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
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Malgo.Prelude
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Fun (Name, Pattern (..))

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

escapingNamesProducer :: Producer -> Set Name
escapingNamesProducer (Var _ _) = Set.empty
escapingNamesProducer (Literal _ _) = Set.empty
escapingNamesProducer (Construct _ _ ps ks) = Set.unions (map escapingNamesProducer ps) <> Set.fromList ks
escapingNamesProducer (Lambda _ names stmt) = freeVarsStatement stmt Set.\\ Set.fromList names
escapingNamesProducer (Object _ fields) =
  Set.unions [freeVarsStatement stmt Set.\\ Set.singleton ret | (ret, stmt) <- Map.elems fields]
escapingNamesProducer (Mu _ name stmt) = Set.delete name (freeVarsStatement stmt)
escapingNamesProducer (Cocase _ branches) =
  Set.unions [freeVarsStatement stmt Set.\\ Set.fromList vars | (_, vars, stmt) <- branches]

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
initialClassifyJoins (Cut p _) = initialClassifyJoinsProducer p
initialClassifyJoins (Join _ name consumer stmt) =
  let ownership = if name `Set.member` escapingNamesStatement stmt then Escaping else Local
   in Map.insert name ownership (initialClassifyJoinsConsumer consumer <> initialClassifyJoins stmt)
initialClassifyJoins (Primitive _ _ ps _) = Map.unions (map initialClassifyJoinsProducer ps)
initialClassifyJoins (Invoke _ _ _) = Map.empty
initialClassifyJoins (ExternalCall _ _ ps _) = Map.unions (map initialClassifyJoinsProducer ps)
initialClassifyJoins (BinOp _ _ lhs rhs _) = initialClassifyJoinsProducer lhs <> initialClassifyJoinsProducer rhs
initialClassifyJoins (Ifz _ cond t e) = initialClassifyJoinsProducer cond <> initialClassifyJoins t <> initialClassifyJoins e

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
