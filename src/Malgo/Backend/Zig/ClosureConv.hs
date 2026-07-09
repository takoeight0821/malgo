-- | Pure analyses over (already 'Malgo.Backend.Zig.Normalize.normalizeStatement'd)
-- Join IR, shared by 'Malgo.Backend.Zig.Emit' for lambda lifting and by the
-- future Perceus pass for ownership tracking.
--
-- Two questions this module answers about a function body (a 'Statement'
-- that does not itself descend into nested 'Lambda'\/'Object'\/'Cocase'\/'Mu'
-- bodies for the purpose of these particular computations):
--
--   1. 'freeVarsStatement' etc. — which names does a term reference that it
--      does not bind itself? Used to compute the capture list of a lifted
--      function.
--   2. 'escapingJoins' — which 'Join'-bound consumer names in a statement
--      cannot be compiled as an inline substitution at their (same-function)
--      use site, and must instead be reified as a heap-allocated closure?
--
-- Escaping analysis, in detail: after normalization, every remaining
-- consumer bound by @Join name consumer stmt@ is one of @Apply@, @Project@,
-- @Then@, @Finish@, @Select@, @Destructor@ (never a bare @Label@, since that
-- case was eliminated by substitution). A name bound this way can be
-- compiled as a same-function inline substitution (no allocation at all)
-- as long as every use of it stays within the current function: as the
-- direct target of a @Cut@ or @Primitive@. It must be reified — allocated
-- once, as an ordinary closure value, invoked through the runtime's
-- @applyCovalue@ — when a use crosses into a separately-compiled unit:
--
--   * passed as the continuation argument to 'Invoke' (calls a top-level def)
--   * passed as an element of 'Apply'\'s @returns@ (calls a closure)
--   * passed as the continuation argument to 'Destructor' (calls a codata branch)
--   * passed as the continuation argument to 'Project' (calls a record field)
--   * passed as an element of 'Construct'\'s @returns@ (stored into data;
--     always empty in practice, per 'Malgo.Sequent.ToCore', but handled
--     defensively)
--   * free in the body of a nested 'Lambda'\/'Object' field\/'Cocase' branch\/'Mu'
--     (those bodies are lifted into their own function, so any name they
--     reference from an enclosing scope must be a real captured value)
module Malgo.Backend.Zig.ClosureConv
  ( freeVarsStatement,
    freeVarsProducer,
    freeVarsConsumer,
    escapingNamesStatement,
    classifyJoins,
    classifyJoinsConsumer,
    Ownership (..),
    convertProgram,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Traversable (for)
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Malgo.Backend.Zig.Ir qualified as Ir
import Malgo.Backend.Zig.Normalize (normalizeStatement, substStatement)
import Malgo.Id
import Malgo.Module (ModuleName)
import Malgo.Prelude
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Fun (Name, Pattern (..), Tag (..))

-- | Whether a 'Join'-bound consumer name can be compiled as an inline
-- substitution within its defining function, or must be reified as a
-- heap-allocated closure value.
data Ownership = Local | Escaping
  deriving stock (Eq, Show)

freeVarsProducer :: Producer -> Set Name
freeVarsProducer (Var _ name) = Set.singleton name
freeVarsProducer (Literal _ _) = Set.empty
freeVarsProducer (Construct _ _ ps ks) = Set.unions (map freeVarsProducer ps) <> Set.fromList ks
freeVarsProducer (Lambda _ names stmt) = freeVarsStatement stmt Set.\\ Set.fromList names
freeVarsProducer (Object _ fields) =
  Set.unions [freeVarsStatement stmt Set.\\ Set.singleton ret | (ret, stmt) <- Map.elems fields]
freeVarsProducer (Mu _ name stmt) = Set.delete name (freeVarsStatement stmt)
freeVarsProducer (Cocase _ branches) =
  Set.unions [freeVarsStatement stmt Set.\\ Set.fromList vars | (_, vars, stmt) <- branches]

freeVarsConsumer :: Consumer -> Set Name
freeVarsConsumer (Label _ name) = Set.singleton name
freeVarsConsumer (Apply _ ps ks) = Set.unions (map freeVarsProducer ps) <> Set.fromList ks
freeVarsConsumer (Project _ _ k) = Set.singleton k
freeVarsConsumer (Then _ name stmt) = Set.delete name (freeVarsStatement stmt)
freeVarsConsumer (Finish _) = Set.empty
freeVarsConsumer (Select _ branches) = Set.unions (map freeVarsBranch branches)
freeVarsConsumer (Destructor _ _ ps k) = Set.unions (map freeVarsProducer ps) <> Set.singleton k

freeVarsBranch :: Branch -> Set Name
freeVarsBranch (Branch _ pat stmt) = freeVarsStatement stmt Set.\\ patternVars pat

patternVars :: Pattern -> Set Name
patternVars (PVar _ name) = Set.singleton name
patternVars (PLiteral _ _) = Set.empty
patternVars (Destruct _ _ pats) = Set.unions (map patternVars pats)
patternVars (Expand _ fields) = Set.unions (map patternVars (Map.elems fields))

freeVarsStatement :: Statement -> Set Name
freeVarsStatement (Cut p k) = Set.insert k (freeVarsProducer p)
freeVarsStatement (Join _ name consumer stmt) =
  freeVarsConsumer consumer <> Set.delete name (freeVarsStatement stmt)
freeVarsStatement (Primitive _ _ ps k) = Set.insert k (Set.unions (map freeVarsProducer ps))
freeVarsStatement (Invoke _ _ k) = Set.singleton k
freeVarsStatement (ExternalCall _ _ ps k) = Set.insert k (Set.unions (map freeVarsProducer ps))
freeVarsStatement (BinOp _ _ lhs rhs k) = Set.insert k (freeVarsProducer lhs <> freeVarsProducer rhs)
freeVarsStatement (Ifz _ cond t e) = freeVarsProducer cond <> freeVarsStatement t <> freeVarsStatement e

-- | Names used in a position that forces reification, per the module
-- documentation. Does not descend into nested closure bodies beyond taking
-- their free variables (their internal join structure is irrelevant here;
-- it is analyzed independently once that body is itself lifted).
escapingNamesStatement :: Statement -> Set Name
escapingNamesStatement (Cut p k) = escapingNamesProducer p
  where
    _ = k -- Cut's target is a same-function jump, never escaping on its own.
escapingNamesStatement (Join _ _name consumer stmt) =
  escapingNamesConsumer consumer <> escapingNamesStatement stmt
escapingNamesStatement (Primitive _ _ ps _k) = Set.unions (map escapingNamesProducer ps)
escapingNamesStatement (Invoke _ _ k) = Set.singleton k
escapingNamesStatement (ExternalCall _ _ ps _k) = Set.unions (map escapingNamesProducer ps)
escapingNamesStatement (BinOp _ _ lhs rhs _k) = escapingNamesProducer lhs <> escapingNamesProducer rhs
escapingNamesStatement (Ifz _ cond t e) =
  escapingNamesProducer cond <> escapingNamesStatement t <> escapingNamesStatement e

-- | 'Lambda'\/'Object'\/'Mu'\/'Cocase' are all nested-closure-body
-- producers: an escaping name of the *enclosing* statement is exactly a
-- free variable of theirs (their own join structure is a separate,
-- independently-analyzed scope — see the haddock above), so those cases
-- just delegate to 'freeVarsProducer' rather than duplicating its rules.
escapingNamesProducer :: Producer -> Set Name
escapingNamesProducer (Var _ _) = Set.empty
escapingNamesProducer (Literal _ _) = Set.empty
escapingNamesProducer (Construct _ _ ps ks) = Set.unions (map escapingNamesProducer ps) <> Set.fromList ks
escapingNamesProducer p@(Lambda {}) = freeVarsProducer p
escapingNamesProducer p@(Object {}) = freeVarsProducer p
escapingNamesProducer p@(Mu {}) = freeVarsProducer p
escapingNamesProducer p@(Cocase {}) = freeVarsProducer p

escapingNamesConsumer :: Consumer -> Set Name
escapingNamesConsumer (Label _ _) = Set.empty -- eliminated by Normalize; kept total defensively.
escapingNamesConsumer (Apply _ ps ks) = Set.unions (map escapingNamesProducer ps) <> Set.fromList ks
escapingNamesConsumer (Project _ _ k) = Set.singleton k
escapingNamesConsumer (Then _ _name stmt) = escapingNamesStatement stmt
escapingNamesConsumer (Finish _) = Set.empty
escapingNamesConsumer (Select _ branches) = Set.unions (map escapingNamesBranch branches)
escapingNamesConsumer (Destructor _ _ ps k) = Set.unions (map escapingNamesProducer ps) <> Set.singleton k

escapingNamesBranch :: Branch -> Set Name
escapingNamesBranch (Branch _ _ stmt) = escapingNamesStatement stmt

-- | For every 'Join'-bound name in a function body (not descending into
-- nested closure bodies, which are classified independently once lifted),
-- decide whether it is 'Local' or 'Escaping'.
--
-- 'initialClassifyJoins' alone is not sufficient: it only checks whether a
-- join's /own/ uses escape within its own continuation. But a join @j@
-- classified 'Local' by that rule can still be referenced from inside the
-- *consumer* of some other join @m@ — and once @m@ is itself 'Escaping', its
-- consumer is lifted into a separate function ('Malgo.Backend.Zig.Emit.liftConsumer'),
-- which captures its free variables as ordinary runtime values. A 'Local'
-- join has no runtime value (it exists only as a compile-time substitution
-- table entry), so it cannot be captured this way: it must be promoted to
-- 'Escaping' too. This is a monotone fixpoint over "escaping join → free
-- variable of its consumer that is itself a tracked join, currently Local".
classifyJoins :: Statement -> Map Name Ownership
classifyJoins stmt = promoteEscapingCaptures (Map.fromList (collectJoins stmt)) (initialClassifyJoins stmt)

-- | Like 'classifyJoins', but for a 'Consumer' being independently lifted
-- into its own function (see 'Malgo.Backend.Zig.Emit.liftConsumer'): the
-- fixpoint must be recomputed in this fresh scope using only the joins
-- collected from *this* consumer's body.
classifyJoinsConsumer :: Consumer -> Map Name Ownership
classifyJoinsConsumer consumer = promoteEscapingCaptures (Map.fromList (collectJoinsConsumer consumer)) (initialClassifyJoinsConsumer consumer)

-- | Repeatedly promote any 'Local' join referenced as a free variable of an
-- 'Escaping' join's consumer, until no more changes.
promoteEscapingCaptures :: Map Name Consumer -> Map Name Ownership -> Map Name Ownership
promoteEscapingCaptures consumers = go
  where
    go ownership =
      let escapingConsumers = [c | (n, c) <- Map.toList consumers, Map.lookup n ownership == Just Escaping]
          referenced = Set.unions (map freeVarsConsumer escapingConsumers)
          toPromote = Set.filter (\n -> Map.lookup n ownership == Just Local) referenced
       in if Set.null toPromote
            then ownership
            else go (Map.union (Map.fromSet (const Escaping) toPromote) ownership)

-- | Every 'Join'-bound name paired with its consumer, within a function body
-- (not descending into nested closure bodies — those are collected
-- independently, in their own fresh scope, once lifted).
collectJoins :: Statement -> [(Name, Consumer)]
collectJoins (Cut p _) = collectJoinsProducer p
collectJoins (Join _ name consumer stmt) = (name, consumer) : (collectJoinsConsumer consumer <> collectJoins stmt)
collectJoins (Primitive _ _ ps _) = concatMap collectJoinsProducer ps
collectJoins (Invoke _ _ _) = []
collectJoins (ExternalCall _ _ ps _) = concatMap collectJoinsProducer ps
collectJoins (BinOp _ _ lhs rhs _) = collectJoinsProducer lhs <> collectJoinsProducer rhs
collectJoins (Ifz _ cond t e) = collectJoinsProducer cond <> collectJoins t <> collectJoins e

collectJoinsProducer :: Producer -> [(Name, Consumer)]
collectJoinsProducer (Var _ _) = []
collectJoinsProducer (Literal _ _) = []
collectJoinsProducer (Construct _ _ ps _) = concatMap collectJoinsProducer ps
collectJoinsProducer (Lambda _ _ _) = []
collectJoinsProducer (Object _ _) = []
collectJoinsProducer (Mu _ _ _) = []
collectJoinsProducer (Cocase _ _) = []

collectJoinsConsumer :: Consumer -> [(Name, Consumer)]
collectJoinsConsumer (Label _ _) = []
collectJoinsConsumer (Apply _ ps _) = concatMap collectJoinsProducer ps
collectJoinsConsumer (Project _ _ _) = []
collectJoinsConsumer (Then _ _ stmt) = collectJoins stmt
collectJoinsConsumer (Finish _) = []
collectJoinsConsumer (Select _ branches) = concatMap (\(Branch _ _ stmt) -> collectJoins stmt) branches
collectJoinsConsumer (Destructor _ _ ps _) = concatMap collectJoinsProducer ps

-- | The direct-escaping rule alone (see 'classifyJoins' for why this is not
-- the whole story).
initialClassifyJoins :: Statement -> Map Name Ownership
initialClassifyJoins = fst . initialClassifyJoinsWithEscaping

-- | 'initialClassifyJoins' fused with 'escapingNamesStatement' of the same
-- statement: a chain of nested 'Join's calling 'escapingNamesStatement'
-- freshly on its own (shrinking) continuation at every node would
-- re-traverse that continuation once per enclosing 'Join', making
-- classification quadratic in the chain's length. Computing both bottom-up
-- in one traversal instead means each node's escaping-name set is built
-- once, from its already-computed children, not re-walked from scratch.
initialClassifyJoinsWithEscaping :: Statement -> (Map Name Ownership, Set Name)
initialClassifyJoinsWithEscaping (Cut p _) =
  (initialClassifyJoinsProducer p, escapingNamesProducer p)
initialClassifyJoinsWithEscaping (Join _ name consumer stmt) =
  let (m, esc) = initialClassifyJoinsWithEscaping stmt
      ownership = if name `Set.member` esc then Escaping else Local
   in ( Map.insert name ownership (initialClassifyJoinsConsumer consumer <> m),
        escapingNamesConsumer consumer <> esc
      )
initialClassifyJoinsWithEscaping (Primitive _ _ ps _) =
  (Map.unions (map initialClassifyJoinsProducer ps), Set.unions (map escapingNamesProducer ps))
initialClassifyJoinsWithEscaping (Invoke _ _ k) = (Map.empty, Set.singleton k)
initialClassifyJoinsWithEscaping (ExternalCall _ _ ps _) =
  (Map.unions (map initialClassifyJoinsProducer ps), Set.unions (map escapingNamesProducer ps))
initialClassifyJoinsWithEscaping (BinOp _ _ lhs rhs _) =
  ( initialClassifyJoinsProducer lhs <> initialClassifyJoinsProducer rhs,
    escapingNamesProducer lhs <> escapingNamesProducer rhs
  )
initialClassifyJoinsWithEscaping (Ifz _ cond t e) =
  let (mt, escT) = initialClassifyJoinsWithEscaping t
      (me, escE) = initialClassifyJoinsWithEscaping e
   in ( initialClassifyJoinsProducer cond <> mt <> me,
        escapingNamesProducer cond <> escT <> escE
      )

-- | Only descends into a nested closure's free variables for escaping
-- analysis, but its *own* join structure still needs classifying once it is
-- lifted — which happens via a fresh top-level call to 'classifyJoins' on
-- its body, done by the emitter. This function does not recurse into
-- 'Lambda'\/'Object'\/'Cocase'\/'Mu' bodies for that reason.
initialClassifyJoinsProducer :: Producer -> Map Name Ownership
initialClassifyJoinsProducer (Var _ _) = Map.empty
initialClassifyJoinsProducer (Literal _ _) = Map.empty
initialClassifyJoinsProducer (Construct _ _ ps _) = Map.unions (map initialClassifyJoinsProducer ps)
initialClassifyJoinsProducer (Lambda _ _ _) = Map.empty
initialClassifyJoinsProducer (Object _ _) = Map.empty
initialClassifyJoinsProducer (Mu _ _ _) = Map.empty
initialClassifyJoinsProducer (Cocase _ _) = Map.empty

initialClassifyJoinsConsumer :: Consumer -> Map Name Ownership
initialClassifyJoinsConsumer (Label _ _) = Map.empty
initialClassifyJoinsConsumer (Apply _ ps _) = Map.unions (map initialClassifyJoinsProducer ps)
initialClassifyJoinsConsumer (Project _ _ _) = Map.empty
initialClassifyJoinsConsumer (Then _ _ stmt) = initialClassifyJoins stmt
initialClassifyJoinsConsumer (Finish _) = Map.empty
initialClassifyJoinsConsumer (Select _ branches) = Map.unions (map (\(Branch _ _ stmt) -> initialClassifyJoins stmt) branches)
initialClassifyJoinsConsumer (Destructor _ _ ps _) = Map.unions (map initialClassifyJoinsProducer ps)

-- | Local (same-function) substitution environment: a join name classified
-- 'Local' maps to the 'Consumer' it was bound to, to be inlined at use.
-- Because the same consumer AST is inlined (and converted) once per use
-- site, binders written inside it are duplicated across sites — always on
-- disjoint control-flow paths, since Join IR joins are non-recursive and a
-- linear path expands each local join at most once.
type LocalEnv = Map Name Consumer

-- | Convert a linked Join IR program into the backend 'Ir.Program':
-- normalize each definition, classify its joins, ANF-flatten every nested
-- producer, inline 'Local' joins at their use sites, and lift every
-- 'Lambda'\/escaping join\/'Object' field into its own 'Ir.Func'.
convertProgram :: (State Uniq :> es, Reader ModuleName :> es) => Program -> Eff es Ir.Program
convertProgram program = do
  -- A module reachable via more than one import path (a diamond, e.g. a
  -- test case importing both Builtin and Prelude, which itself imports
  -- Builtin) appears once per path in the linked 'Program.definitions'.
  -- Zig errors on a duplicate struct member name, so definitions are
  -- deduplicated by 'Malgo.Id.Id' (stable across import paths).
  let definitions = nubByName program.definitions
  defFuncs <- concat <$> traverse convertDefinition definitions
  -- A module with no top-level `main` is valid input (a library-style
  -- module): 'Malgo.Sequent.Eval.evalProgram' silently does nothing in
  -- that case, so the generated executable does the same.
  (entryFuncs, entryName) <- case findMain definitions of
    Nothing -> pure ([], Nothing)
    Just found@(entryRange, _) -> do
      entryStmt <- mainEntryStatement found
      fnName <- newTemporalId "zig_main"
      selfVar <- newTemporalId "self"
      (body, lifted) <- convertStatement Map.empty (classifyJoins entryStmt) entryStmt
      let fn = Ir.Func {range = entryRange, name = fnName, kind = Ir.TopLevelFn, selfVar, params = [], body}
      pure (fn : lifted, Just fnName)
  pure Ir.Program {funcs = defFuncs <> entryFuncs, entry = entryName}

convertDefinition :: (State Uniq :> es, Reader ModuleName :> es) => (Range, Name, Name, Statement) -> Eff es [Ir.Func]
convertDefinition (defRange, name, retName, rawStmt) = do
  -- Normalize once per top-level definition: 'normalizeStatement' recurses
  -- into every nested Lambda/Object field/Cocase branch body, so this
  -- single call covers the whole tree reachable from here.
  let stmt = normalizeStatement rawStmt
  selfVar <- newTemporalId "self"
  (body, lifted) <- convertStatement Map.empty (classifyJoins stmt) stmt
  pure (Ir.Func {range = defRange, name, kind = Ir.TopLevelFn, selfVar, params = [retName], body} : lifted)

-- | Keep only the first definition for each 'Malgo.Id.Id', dropping later
-- occurrences reached via a different import path to the same module.
nubByName :: [(Range, Name, Name, Statement)] -> [(Range, Name, Name, Statement)]
nubByName = go Set.empty
  where
    go _ [] = []
    go seen (d@(_, name, _, _) : rest)
      | name `Set.member` seen = go seen rest
      | otherwise = d : go (Set.insert name seen) rest

-- | Find the definition named @main@, matching the interpreter's bootstrap
-- ('Malgo.Sequent.Eval.evalProgram'): the first definition (from any linked
-- module) whose 'Malgo.Id.Id.name' is exactly @"main"@. 'Nothing' is a
-- valid result (a library-style module with no entry point).
findMain :: [(Range, Name, Name, Statement)] -> Maybe (Range, Name)
findMain defs = listToMaybe [(range, name) | (range, name, _, _) <- defs, name.name == "main"]

-- | Build the program's entry-point statement, mirroring
-- 'Malgo.Sequent.Eval.evalProgram'\'s bootstrap exactly: locate @main@,
-- then run @Invoke main afterMain@ where @afterMain@, once handed @main@'s
-- own value (a closure, by the @main : () -> a@ convention), applies it to
-- unit and a fresh @Finish@ continuation.
mainEntryStatement :: (State Uniq :> es, Reader ModuleName :> es) => (Range, Name) -> Eff es Statement
mainEntryStatement (entryRange, mainName) = do
  finishName <- newTemporalId "finish"
  afterMain <- newTemporalId "after_main"
  pure
    $ Join entryRange finishName (Finish entryRange)
    $ Join entryRange afterMain (Apply entryRange [Construct entryRange Tuple [] []] [finishName])
    $ Invoke entryRange mainName afterMain

convertStatement :: (State Uniq :> es, Reader ModuleName :> es) => LocalEnv -> Map Name Ownership -> Statement -> Eff es (Ir.Block, [Ir.Func])
convertStatement env ownership = go
  where
    go (Cut producer k) = do
      (stmts, v, fns1) <- convertProducer env ownership producer
      (Ir.Block rest term, fns2) <- case Map.lookup k env of
        Just consumer -> convertApply env ownership consumer v
        Nothing -> pure (Ir.Block [] (Ir.TApplyCo k v), [])
      pure (Ir.Block (stmts <> rest) term, fns1 <> fns2)
    go (Join joinRange name consumer stmt) = case Map.findWithDefault Local name ownership of
      Local -> convertStatement (Map.insert name consumer env) ownership stmt
      Escaping -> do
        (allocStmts, fns1) <- liftJoinConsumer joinRange name consumer
        (Ir.Block rest term, fns2) <- convertStatement env ownership stmt
        pure (Ir.Block (allocStmts <> rest) term, fns1 <> fns2)
    go (Primitive _ name producers k) = do
      (stmts, vs, fns1) <- convertProducers env ownership producers
      result <- newTemporalId "prim_result"
      let bind = Ir.Let result (Ir.Prim name vs)
      (Ir.Block rest term, fns2) <- case Map.lookup k env of
        Just consumer -> convertApply env ownership consumer result
        Nothing -> pure (Ir.Block [] (Ir.TApplyCo k result), [])
      pure (Ir.Block (stmts <> (bind : rest)) term, fns1 <> fns2)
    go (Invoke _ name k) = pure (Ir.Block [] (Ir.TStaticCall name [k]), [])
    go (ExternalCall exRange name producers k) = go (Primitive exRange name producers k)
    go (BinOp opRange op lhs rhs k) = go (Primitive opRange op [lhs, rhs] k)
    go (Ifz _ cond t e) = do
      (stmts, v, fns1) <- convertProducer env ownership cond
      (tBlock, fns2) <- go t
      (eBlock, fns3) <- go e
      pure (Ir.Block stmts (Ir.TIf (Ir.GIsZero v) tBlock eBlock), fns1 <> fns2 <> fns3)

-- | Apply a 'Consumer' to an already-available variable, inline in the
-- current function scope.
convertApply :: (State Uniq :> es, Reader ModuleName :> es) => LocalEnv -> Map Name Ownership -> Consumer -> Name -> Eff es (Ir.Block, [Ir.Func])
convertApply env ownership consumer v = case consumer of
  Label _ name -> pure (Ir.Block [] (Ir.TApplyCo name v), [])
  Apply _ producers returns -> do
    (stmts, vs, fns) <- convertProducers env ownership producers
    pure (Ir.Block stmts (Ir.TCallClosure v (vs <> returns)), fns)
  Project _ field k -> pure (Ir.Block [] (Ir.TProject v field k), [])
  -- The bound name is renamed to the already-available variable instead of
  -- emitting an alias binding — sound because every Id is globally unique.
  Then _ name stmt -> convertStatement env ownership (substStatement name v stmt)
  Finish _ -> pure (Ir.Block [] (Ir.TReturn v), [])
  Select _ branches -> convertSelect env ownership v branches
  Destructor _ name producers k -> do
    (stmts, vs, fns) <- convertProducers env ownership producers
    pure (Ir.Block stmts (Ir.TDestruct v name (vs <> [k])), fns)

convertProducers :: (State Uniq :> es, Reader ModuleName :> es) => LocalEnv -> Map Name Ownership -> [Producer] -> Eff es ([Ir.Stmt], [Name], [Ir.Func])
convertProducers _ _ [] = pure ([], [], [])
convertProducers env ownership (p : ps) = do
  (stmts, v, fns1) <- convertProducer env ownership p
  (restStmts, vs, fns2) <- convertProducers env ownership ps
  pure (stmts <> restStmts, v : vs, fns1 <> fns2)

convertProducer :: (State Uniq :> es, Reader ModuleName :> es) => LocalEnv -> Map Name Ownership -> Producer -> Eff es ([Ir.Stmt], Name, [Ir.Func])
convertProducer env ownership = \case
  Var _ x -> pure ([], x, [])
  Literal _ lit -> do
    t <- newTemporalId "lit"
    pure ([Ir.Let t (Ir.Lit lit)], t, [])
  Construct _ tag producers returns -> do
    (stmts, vs, fns) <- convertProducers env ownership producers
    t <- newTemporalId "struct"
    pure (stmts <> [Ir.Let t (Ir.MkStruct tag (vs <> returns))], t, fns)
  Lambda lamRange params stmt -> do
    fnId <- newTemporalId "lambda"
    selfVar <- newTemporalId "self"
    let captures = Set.toList (freeVarsStatement stmt Set.\\ Set.fromList params)
        captureLets = [Ir.Let c (Ir.ReadCapture selfVar i) | (i, c) <- zip [0 :: Int ..] captures]
    params' <- dedupParams params
    (Ir.Block bodyStmts term, fns) <- convertStatement Map.empty (classifyJoins stmt) stmt
    let fn =
          Ir.Func
            { range = lamRange,
              name = fnId,
              kind = Ir.ClosureFn,
              selfVar,
              params = params',
              body = Ir.Block (captureLets <> bodyStmts) term
            }
    t <- newTemporalId "closure"
    pure ([Ir.Let t (Ir.MkClosure fnId captures)], t, fn : fns)
  Object objRange fields -> do
    -- A record's fields share ONE captured environment (mirroring the
    -- evaluator's @Record env fields@): the allocation's capture list is
    -- the union of every field's own free variables, but each field
    -- function reads only its own subset out of the shared list.
    let sharedCaptures = Set.toList (Set.unions [freeVarsStatement stmt Set.\\ Set.singleton ret | (ret, stmt) <- Map.elems fields])
        indexedCaptures = zip [0 :: Int ..] sharedCaptures
    fieldFns <- for (Map.toList fields) \(fieldName, (retName, stmt)) -> do
      fnId <- newTemporalId "field"
      selfVar <- newTemporalId "self"
      let usedByThisField = freeVarsStatement stmt Set.\\ Set.singleton retName
          captureLets = [Ir.Let c (Ir.ReadCapture selfVar i) | (i, c) <- indexedCaptures, c `Set.member` usedByThisField]
      (Ir.Block bodyStmts term, fns) <- convertStatement Map.empty (classifyJoins stmt) stmt
      let fn =
            Ir.Func
              { range = objRange,
                name = fnId,
                kind = Ir.FieldFn,
                selfVar,
                params = [retName],
                body = Ir.Block (captureLets <> bodyStmts) term
              }
      pure ((fieldName, fnId), fn : fns)
    t <- newTemporalId "record"
    pure ([Ir.Let t (Ir.MkRecord (map fst fieldFns) sharedCaptures)], t, concatMap snd fieldFns)
  Mu {} -> error "Malgo.Backend.Zig.ClosureConv: Mu in producer position should have been eliminated by Normalize"
  Cocase {} -> do
    -- Not yet implemented: compiles to a runtime panic. 'Ir.PanicExpr' is
    -- @noreturn@, so anything after it in the block is never printed;
    -- variables referenced only by the Cocase become dead bindings.
    t <- newTemporalId "cocase"
    pure ([Ir.Let t (Ir.PanicExpr "Cocase")], t, [])

-- | The renamer can assign the same wildcard 'Malgo.Id.Id' to more than one
-- surface parameter (e.g. two @_@s); Ir binders must be unique per path, so
-- every repeat is replaced with a fresh (never referenced) name.
dedupParams :: (State Uniq :> es, Reader ModuleName :> es) => [Name] -> Eff es [Name]
dedupParams = go Set.empty
  where
    go _ [] = pure []
    go seen (p : ps)
      | p `Set.member` seen = do
          p' <- newTemporalId "unused_param"
          (p' :) <$> go seen ps
      | otherwise = (p :) <$> go (Set.insert p seen) ps

-- | Lift an 'Escaping' join's consumer into its own 'Ir.Func', returning
-- the allocation statement that binds the join's name to the closure.
liftJoinConsumer :: (State Uniq :> es, Reader ModuleName :> es) => Range -> Name -> Consumer -> Eff es ([Ir.Stmt], [Ir.Func])
liftJoinConsumer joinRange name consumer = do
  fnId <- newTemporalId "join"
  selfVar <- newTemporalId "self"
  valueParam <- newTemporalId "value"
  let captures = Set.toList (freeVarsConsumer consumer)
      captureLets = [Ir.Let c (Ir.ReadCapture selfVar i) | (i, c) <- zip [0 :: Int ..] captures]
  -- The join fixpoint must be recomputed in this fresh scope using only
  -- the joins collected from this consumer's body.
  (Ir.Block bodyStmts term, fns) <- convertApply Map.empty (classifyJoinsConsumer consumer) consumer valueParam
  let fn =
        Ir.Func
          { range = joinRange,
            name = fnId,
            kind = Ir.ClosureFn,
            selfVar,
            params = [valueParam],
            body = Ir.Block (captureLets <> bodyStmts) term
          }
  pure ([Ir.Let name (Ir.MkClosure fnId captures)], fn : fns)

convertSelect :: (State Uniq :> es, Reader ModuleName :> es) => LocalEnv -> Map Name Ownership -> Name -> [Branch] -> Eff es (Ir.Block, [Ir.Func])
convertSelect env ownership scrut = go
  where
    go [] = pure (Ir.Block [] (Ir.TPanic "no matching branch"), [])
    go (Branch _ pat stmt : rest) = do
      (tests, binds) <- compilePatternIr scrut pat
      (Ir.Block bodyStmts term, fns1) <- convertStatement env ownership stmt
      (elseBlock, fns2) <- go rest
      pure
        ( Ir.Block [] (Ir.TIf (Ir.GAnd tests) (Ir.Block (binds <> bodyStmts) term) elseBlock),
          fns1 <> fns2
        )

-- | Compile a pattern match against a scrutinee variable into borrowing
-- guard tests and the bindings to splice before the branch body. Mirrors
-- 'Malgo.Sequent.Eval.match'\/@matchDL@.
--
-- The renamer assigns wildcard (@_@) patterns a shared 'Malgo.Id.Id', so a
-- single pattern like @Nil _ _@ can bind the same name more than once; a
-- locally-threaded @seen@ set (scoped to one branch's pattern) replaces
-- every redeclaration after the first with a fresh dead binding (or skips
-- it entirely when the read has no effect).
--
-- 'Expand' (record) sub-patterns must be plain variables: record fields
-- are call-by-name and possibly effectful ('Malgo.Sequent.Eval' re-runs
-- the field statement per force), so each field is forced into a binding
-- exactly once — after the guard commits — which leaves no forced value a
-- literal sub-pattern's guard could inspect. A field absent from the
-- record (which the evaluator's @Map.intersectionWith@ silently treats as
-- still matching) panics at runtime instead; real Malgo programs match
-- only on fields the record's row guarantees are present.
compilePatternIr :: (State Uniq :> es, Reader ModuleName :> es) => Name -> Pattern -> Eff es ([Ir.Test], [Ir.Stmt])
compilePatternIr scrut pat = do
  (tests, binds, _) <- go Set.empty (Ir.PRoot scrut) pat
  pure (tests, binds)
  where
    go seen path = \case
      PVar _ name
        | name `Set.member` seen -> pure ([], [], seen)
        | otherwise -> pure ([], [Ir.Let name (Ir.ReadPath path)], Set.insert name seen)
      PLiteral _ lit -> pure ([Ir.TLitEq path lit], [], seen)
      Destruct _ tag pats -> do
        (subTests, binds, seen') <- goFields seen (zip [0 :: Int ..] pats)
        pure (Ir.TTagEq path tag : subTests, binds, seen')
        where
          goFields seen0 [] = pure ([], [], seen0)
          goFields seen0 ((i, p) : rest) = do
            (g1, b1, seen1) <- go seen0 (Ir.PField path i) p
            (g2, b2, seen2) <- goFields seen1 rest
            pure (g1 <> g2, b1 <> b2, seen2)
      Expand _ fieldPats -> do
        (recordBinds, recordVar) <- case path of
          Ir.PRoot v -> pure ([], v)
          _ -> do
            r <- newTemporalId "record"
            pure ([Ir.Let r (Ir.ReadPath path)], r)
        -- Fields forced in ascending name order (Map.toList), matching
        -- Eval.hs's matchExpand.
        (binds, seen') <- goExpandFields recordVar seen (Map.toList fieldPats)
        pure ([Ir.TKindIs path "record"], recordBinds <> binds, seen')
        where
          goExpandFields recordVar = goF
            where
              goF seen0 [] = pure ([], seen0)
              goF seen0 ((fieldName, p) : rest) = do
                (bind, seen1) <- case p of
                  PVar _ name
                    | name `Set.member` seen0 -> do
                        -- A repeated wildcard Id: the force still runs (the
                        -- field may have effects, and the evaluator forces
                        -- every pattern field), bound to a fresh dead name.
                        t <- newTemporalId "expand"
                        pure ([Ir.Let t (Ir.Force recordVar fieldName)], seen0)
                    | otherwise -> pure ([Ir.Let name (Ir.Force recordVar fieldName)], Set.insert name seen0)
                  _ -> error "Malgo.Backend.Zig.ClosureConv: only variable patterns are supported inside record patterns"
                (restBinds, seen2) <- goF seen1 rest
                pure (bind <> restBinds, seen2)
