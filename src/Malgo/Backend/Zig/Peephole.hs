-- | Scrutinee-tuple elimination on the backend IR.
--
-- Every multi-parameter clause match is compiled (by
-- 'Malgo.Sequent.ToFun.fromClauses') as a 'Select' on a fresh tuple of the
-- parameters, which reaches this IR as a 'MkStruct' whose only consumers
-- are pattern-match reads ('ReadPath') and guard 'Test's. Materializing
-- that tuple costs one Object + one fields array per call, and — more
-- importantly for the reuse pass — hides the matched value behind the
-- tuple: the scrutinee never gets its own name, so Perceus can never emit
-- a 'Drop' for it.
--
-- This pass removes such tuples: field reads and tests are re-rooted at
-- the tuple's operands and statically-true tests against the tuple itself
-- are deleted. Pure aliases (@Let x = ReadPath (PRoot v)@) left behind by
-- the re-rooting are then substituted away, so downstream passes see the
-- actual matched variable.
--
-- Runs on the output of 'Malgo.Backend.Zig.ClosureConv.convertProgram',
-- BEFORE Perceus: the input carries no 'Dup'\/'Drop', so the rewrite needs
-- no reference-count reasoning — Perceus derives ownership from the
-- rewritten occurrence counts afterwards.
module Malgo.Backend.Zig.Peephole
  ( peepholeProgram,
    peepholeFunc,
  )
where

import Data.Maybe (catMaybes, listToMaybe)
import Data.Set qualified as Set
import Malgo.Backend.Zig.Ir
import Malgo.Prelude
import Malgo.Sequent.Fun (Name, Tag)

peepholeProgram :: Program -> Program
peepholeProgram program = program {funcs = map peepholeFunc program.funcs}

-- | Iterated to a fixpoint: eliminating an outer tuple re-roots reads at
-- its operands, which can expose an inner tuple (nested patterns) or a
-- fresh alias for the next round.
peepholeFunc :: Func -> Func
peepholeFunc fn
  | body' == fn.body = fn
  | otherwise = peepholeFunc fn {body = body'}
  where
    body' = elimAliases (elimTuples fn.body)

-- * Tuple elimination

elimTuples :: Block -> Block
elimTuples (Block stmts terminator) = go stmts
  where
    go [] = Block [] (inBranches terminator)
    go (stmt : rest) = case stmt of
      Let t (MkStruct tag ops)
        | Just (Block rest' terminator') <- elimUses t tag ops (Block rest terminator) ->
            elimTuples (Block rest' terminator')
      _ -> let Block rest' terminator' = go rest in Block (stmt : rest') terminator'
    inBranches = \case
      TIf guard t e -> TIf guard (elimTuples t) (elimTuples e)
      term -> term

-- | Rewrite every use of tuple @t@ (constructed as @MkStruct tag ops@) in
-- the block, or 'Nothing' if any use is not a re-rootable read\/test —
-- i.e. the tuple value itself escapes into an operand, a whole-value
-- read, or a test this pass cannot prove statically.
elimUses :: Name -> Tag -> [Name] -> Block -> Maybe Block
elimUses t tag ops = goBlock
  where
    goBlock (Block stmts terminator) = do
      (stmts', terminator') <- goStmts stmts terminator
      pure (Block stmts' terminator')

    goStmts [] terminator = ([],) <$> goTerm terminator
    goStmts (stmt : rest) terminator = case stmt of
      Let x (ReadPath p)
        | pathRoot p == t -> do
            p' <- reroot p
            first (Let x (ReadPath p') :) <$> goStmts rest terminator
      Let _ e | t `Set.member` freeVarsExpr e -> Nothing
      Dup x | x == t -> Nothing
      Drop x | x == t -> Nothing
      _ -> first (stmt :) <$> goStmts rest terminator

    goTerm = \case
      TIf guard thenB elseB -> TIf <$> goGuard guard <*> goBlock thenB <*> goBlock elseB
      term | t `Set.member` freeVarsTerminator term -> Nothing
      term -> Just term

    goGuard = \case
      GAnd tests -> GAnd . catMaybes <$> traverse goTest tests
      g@(GIsZero v) -> if v == t then Nothing else Just g

    -- 'Nothing' aborts the elimination; 'Just Nothing' deletes a
    -- statically-true test; 'Just (Just test)' keeps a (re-rooted) test.
    goTest test = case test of
      TTagEq (PRoot root) tag'
        | root == t -> if tag' == tag then Just Nothing else Nothing
      TKindIs (PRoot root) kindName
        | root == t -> if kindName == "strukt" then Just Nothing else Nothing
      _ | pathRoot (testPath test) /= t -> Just (Just test)
      TTagEq p tag' -> Just . flip TTagEq tag' <$> reroot p
      TKindIs p kindName -> Just . flip TKindIs kindName <$> reroot p
      TLitEq p lit -> Just . flip TLitEq lit <$> reroot p

    -- @t.fields[i]...@ becomes a path rooted at the i-th operand. A bare
    -- @PRoot t@ (the tuple as a whole) cannot be re-rooted.
    reroot = \case
      PRoot _ -> Nothing
      PField (PRoot _) i
        | Just op <- listToMaybe (drop i ops) -> Just (PRoot op)
        | otherwise -> Nothing
      PField p i -> flip PField i <$> reroot p

    testPath = \case
      TKindIs p _ -> p
      TTagEq p _ -> p
      TLitEq p _ -> p

-- * Alias elimination

-- | @Let x = ReadPath (PRoot v)@ binds nothing new — both names denote the
-- same value (pre-Perceus, a 'ReadPath' is a borrowed alias). Substitute
-- @v@ for @x@ and delete the binding. Ids are globally unique, so the
-- substitution cannot capture.
elimAliases :: Block -> Block
elimAliases (Block stmts terminator) = go stmts
  where
    go [] = Block [] (inBranches terminator)
    go (Let x (ReadPath (PRoot v)) : rest) = elimAliases (substNameBlock x v (Block rest terminator))
    go (stmt : rest) = let Block rest' terminator' = go rest in Block (stmt : rest') terminator'
    inBranches = \case
      TIf guard t e -> TIf guard (elimAliases t) (elimAliases e)
      term -> term

substNameBlock :: Name -> Name -> Block -> Block
substNameBlock from to = goBlock
  where
    goBlock (Block stmts terminator) = Block (map goStmt stmts) (goTerm terminator)

    goStmt = \case
      Let x e -> Let x (goExpr e)
      Dup x -> Dup (rn x)
      Drop x -> Drop (rn x)
      DropReuse {} -> error "Malgo.Backend.Zig.Peephole: input already contains DropReuse (Reuse runs after Peephole)"

    goExpr = \case
      Lit lit -> Lit lit
      MkStruct tag vs -> MkStruct tag (map rn vs)
      MkClosure fn vs -> MkClosure fn (map rn vs)
      MkRecord fields vs -> MkRecord fields (map rn vs)
      Prim name vs -> Prim name (map rn vs)
      ReadPath p -> ReadPath (goPath p)
      ReadCapture self i -> ReadCapture (rn self) i
      Force v field -> Force (rn v) field
      PanicExpr what -> PanicExpr what
      MkStructReuse {} -> error "Malgo.Backend.Zig.Peephole: input already contains MkStructReuse (Reuse runs after Peephole)"

    goTerm = \case
      TApplyCo k v -> TApplyCo (rn k) (rn v)
      TCallClosure f args -> TCallClosure (rn f) (map rn args)
      TStaticCall fn args -> TStaticCall fn (map rn args)
      TProject v field k -> TProject (rn v) field (rn k)
      TDestruct v name args -> TDestruct (rn v) name (map rn args)
      TReturn v -> TReturn (rn v)
      TIf guard t e -> TIf (goGuard guard) (goBlock t) (goBlock e)
      TPanic msg -> TPanic msg

    goGuard = \case
      GAnd tests -> GAnd (map goTest tests)
      GIsZero v -> GIsZero (rn v)

    goTest = \case
      TKindIs p kindName -> TKindIs (goPath p) kindName
      TTagEq p tag -> TTagEq (goPath p) tag
      TLitEq p lit -> TLitEq (goPath p) lit

    goPath = \case
      PRoot v -> PRoot (rn v)
      PField p i -> PField (goPath p) i

    rn x = if x == from then to else x
