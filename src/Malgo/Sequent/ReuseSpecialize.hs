-- | Reuse-hint insertion, an effectful @Fun.Program -> Fun.Program@
-- rewrite that runs at the start of 'Malgo.Sequent.ToCore.toCore', right
-- after 'Malgo.Sequent.SaturateCtor.saturateProgram'.
--
-- Recognizes self-recursive \"match a value, recurse, rebuild the same
-- constructor from the matched fields plus the recursive result\" functions
-- (@mapList@\/@BinaryTree.insert@-shaped: @{ f (Cons x xs) -> Cons (f x)
-- (mapList f xs) }@) and rewrites the rebuilt 'Fun.Construct' so every one
-- of its arguments is forced into its own named binding, in original
-- order, followed by a @Primitive \"reuseHint\" [Var scrutinee]@
-- expression, followed by the 'Fun.Construct' itself over the now-bound
-- names. Binding every argument first (rather than just wrapping the whole
-- 'Fun.Construct' in an outer @Let@) matters: a @Let@ evaluates its bound
-- value before its body, so putting @reuseHint scrutinee@ /before/ the
-- argument bindings would reference the scrutinee before the (possibly
-- recursive) argument computations run — Perceus would then place the
-- scrutinee's @Drop@ right there, long before the recursive call's
-- continuation even exists as a separate @Ir.Func@, which is exactly the
-- bug this pass exists to avoid. With @reuseHint@ genuinely last, the
-- matched-and-about-to-be-discarded scrutinee becomes a free variable of
-- whatever continuation runs right before the reconstruction, so
-- 'Malgo.Backend.Zig.Perceus'\'s existing last-use analysis places the
-- scrutinee's @Drop@ there, and 'Malgo.Backend.Zig.Reuse'\'s existing
-- same-block @Drop@\/@MkStruct@ pairing fires for free. No new IR
-- constructor, no worker\/wrapper split, no change to a function's arity or
-- calling convention, and no changes needed to
-- ClosureConv\/Perceus\/Reuse\/RcCheck: 'Fun.Primitive' already exists at
-- every IR level and is captured across closures exactly like any other
-- free variable.
--
-- Known limitation (M12, confirmed by trace analysis and reconfirmed by an
-- M14 attempt to fix it): this placement only reuses the /innermost/
-- recursion level. An enclosing level's own matched cell is not reused
-- until /after/ its recursive call returns (reconstruction happens
-- post-order), so for the entire duration of that recursive call the
-- enclosing cell's own field keeps the next level's cell at refcount ≥ 2.
-- Moving the hint earlier (right after the match, before recursing) fixes
-- the ordering but requires threading a reuse token, as ordinary data,
-- across the recursive call — which crosses into a different
-- 'Malgo.Backend.Zig.Ir.Func' once closure-converted. An M14 attempt at
-- this (threading the token as an ordinary Fun-IR free variable) crashed a
-- real build: 'Malgo.Backend.Zig.Perceus'\'s dup-before-drop-self rule for
-- 'Ir.ReadCapture' (necessary for ordinary values, since the reader must
-- take its own reference before the capturing closure's own @self@ is
-- dropped) inflates the token's refcount by one per closure hop it
-- transits, so by the time it reaches the reconstruction site its refcount
-- is no longer 1. Fixing this properly needs the runtime's own closure
-- capture representation to distinguish an owned child from a borrowed
-- pass-through (or a full worker/wrapper arity change threading the token
-- as an ordinary call argument instead of a capture) — out of scope here;
-- see the plan file's M14 section for the full writeup.
module Malgo.Sequent.ReuseSpecialize (specializeProgram) where

import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Malgo.Id (newTemporalId)
import Malgo.Module (ModuleName)
import Malgo.Prelude
import Malgo.Sequent.Fun

specializeProgram :: (State Uniq :> es, Reader ModuleName :> es) => Program -> Eff es Program
specializeProgram program = do
  definitions <- traverse specializeDef program.definitions
  pure program {definitions = definitions}

specializeDef :: (State Uniq :> es, Reader ModuleName :> es) => (Range, Name, Expr) -> Eff es (Range, Name, Expr)
specializeDef def@(range, g, body) =
  case recognizeShape body of
    Nothing -> pure def
    Just (params, scrutRange, scrutinee, branches) -> do
      branches' <- traverse (instrumentBranchIfEligible g (length params) (last params)) branches
      pure (range, g, rebuildLambdas range params (Select scrutRange scrutinee branches'))

rebuildLambdas :: Range -> [Name] -> Expr -> Expr
rebuildLambdas range params body = foldr (\p -> Lambda range [p]) body params

-- | A definition eligible for this pass has the shape produced by
-- 'Malgo.Sequent.ToFun.fromClauses': @n@ nested single-argument 'Lambda's
-- ending in @Select scrutinee branches@, where @scrutinee@ is @Var pn@
-- (@n == 1@) or @Construct Tuple [Var p1, .., Var pn]@ (@n > 1@) — i.e. the
-- unmodified output of 'Malgo.Sequent.ToFun.fromClauses', not some
-- already-rewritten variant.
recognizeShape :: Expr -> Maybe ([Name], Range, Expr, [Branch])
recognizeShape body = do
  let (params, inner) = peelLambdas body
  guard (not (null params))
  case inner of
    Select scrutRange scrutinee branches
      | scrutineeMatches params scrutinee ->
          Just (params, scrutRange, scrutinee, branches)
    _ -> Nothing

peelLambdas :: Expr -> ([Name], Expr)
peelLambdas (Lambda _ [p] b) = let (ps, b') = peelLambdas b in (p : ps, b')
peelLambdas e = ([], e)

scrutineeMatches :: [Name] -> Expr -> Bool
scrutineeMatches [p] (Var _ v) = v == p
scrutineeMatches ps (Construct _ Tuple args) =
  length args == length ps && and (zipWith isVar args ps)
  where
    isVar (Var _ v) p = v == p
    isVar _ _ = False
scrutineeMatches _ _ = False

-- | If this branch's pattern destructures the last parameter (@pn@) via a
-- flat, all-'PVar' 'Destruct', instrument its body (see
-- 'instrumentReconstructions'); otherwise leave it byte-for-byte unchanged.
instrumentBranchIfEligible :: (State Uniq :> es, Reader ModuleName :> es) => Name -> Int -> Name -> Branch -> Eff es Branch
instrumentBranchIfEligible g arity scrutinee (Branch r pat body) =
  case lastPositionDestruct arity pat of
    Just (tag, fieldPats) | all isPVar fieldPats -> do
      body' <- instrumentReconstructions g tag (length fieldPats) scrutinee body
      pure (Branch r pat body')
    _ -> pure (Branch r pat body)
  where
    isPVar PVar {} = True
    isPVar _ = False

lastPositionDestruct :: Int -> Pattern -> Maybe (Tag, [Pattern])
lastPositionDestruct 1 (Destruct _ tag pats) = Just (tag, pats)
lastPositionDestruct n (Destruct _ Tuple pats)
  | length pats == n =
      case last pats of
        Destruct _ tag fieldPats -> Just (tag, fieldPats)
        _ -> Nothing
lastPositionDestruct _ _ = Nothing

-- | Walks tail positions of a branch body — through 'Let'-chains and
-- through 'Lambda' thunk arguments of an 'Apply' spine (the shape a
-- built-in control combinator like @if@ compiles to, since it is an
-- ordinary function taking closure-valued arguments, not special syntax) —
-- and inserts a @reuseHint scrutinee@ immediately before any tail
-- 'Construct' that (a) matches the matched pattern's tag\/arity and (b) has
-- exactly one argument that (recursively) mentions the enclosing
-- definition @g@, with no other occurrence of @g@ anywhere else in the
-- reached tail chain. Any other shape (multiple recursive calls, a
-- recursive call outside tail position, a mismatched tag\/arity, a nested
-- pattern match, or any control structure other than a direct
-- @Let@\/thunk-argument @Apply@ chain) is left completely untouched — this
-- is intentionally conservative (see module haddock and the design plan)
-- rather than a general reachability analysis.
instrumentReconstructions :: (State Uniq :> es, Reader ModuleName :> es) => Name -> Tag -> Int -> Name -> Expr -> Eff es Expr
instrumentReconstructions g tag arity scrutinee body = fromMaybe body <$> go body
  where
    go (Let r n v b)
      | occursInvoke g v = pure Nothing
      | otherwise = fmap (Let r n v) <$> go b
    go expr@Apply {} = do
      let (headExpr, args) = unrollSpine expr []
      if occursInvoke g headExpr || any nonThunkOccurs args
        then pure Nothing
        else do
          rewrittenArgs <- traverse rewriteArg args
          if any isJust rewrittenArgs
            then pure $ Just $ rebuildSpine (range expr) headExpr (zipWith fromMaybe args rewrittenArgs)
            else pure Nothing
      where
        nonThunkOccurs Lambda {} = False
        nonThunkOccurs a = occursInvoke g a
        rewriteArg (Lambda lr [p] b) = fmap (Lambda lr [p]) <$> go b
        rewriteArg _ = pure Nothing
    go (Construct r tag' args)
      | tag' == tag,
        length args == arity,
        [_] <- filter (occursInvoke g) args = do
          -- A 'Let' evaluates its bound value before its body, so wrapping
          -- the whole 'Construct' with an outer @Let _ (reuseHint scrutinee)
          -- (Construct ...)@ would put the reuseHint reference to
          -- 'scrutinee' BEFORE the (possibly recursive) argument
          -- computations, not after — Perceus would then place
          -- 'scrutinee'\'s Drop right there, long before the recursive
          -- call's continuation exists as a separate 'Ir.Func', defeating
          -- the whole point. Instead, force every argument into its own
          -- named binding first (preserving left-to-right evaluation
          -- order), THEN reuseHint, THEN the 'Construct' over the
          -- now-immediate temporaries — so 'scrutinee'\'s last mention is
          -- genuinely the last thing evaluated before the reconstruction,
          -- landing it in the same continuation (hence the same 'Ir.Func')
          -- as the 'Construct' itself.
          namedArgs <- traverse bindArg args
          hint <- newTemporalId "reuseHint"
          let reconstruct = Construct r tag' (map (Var r . fst) namedArgs)
              withHint = Let r hint (Primitive r "reuseHint" [Var r scrutinee]) reconstruct
          pure $ Just $ foldr (\(n, val) acc -> Let r n val acc) withHint namedArgs
      | otherwise = pure Nothing
    go _ = pure Nothing

    bindArg val = do
      n <- newTemporalId "reuseArg"
      pure (n, val)

-- | Unrolls an application spine outside-in: @Apply (Apply f [a1]) [a2]@
-- becomes @(f, [a1, a2])@ (left-to-right argument order preserved). Mirrors
-- 'Malgo.Sequent.SaturateCtor.unrollSpine'.
unrollSpine :: Expr -> [Expr] -> (Expr, [Expr])
unrollSpine (Apply _ fn args) acc = unrollSpine fn (args <> acc)
unrollSpine expr acc = (expr, acc)

rebuildSpine :: Range -> Expr -> [Expr] -> Expr
rebuildSpine r = foldl' (\f a -> Apply r f [a])

occursInvoke :: Name -> Expr -> Bool
occursInvoke g = \case
  Var {} -> False
  Literal {} -> False
  Construct _ _ args -> any (occursInvoke g) args
  Let _ _ v b -> occursInvoke g v || occursInvoke g b
  Lambda _ _ b -> occursInvoke g b
  Object _ fields -> any (occursInvoke g) (Map.elems fields)
  Apply _ fn args -> occursInvoke g fn || any (occursInvoke g) args
  Project _ e _ -> occursInvoke g e
  Primitive _ _ args -> any (occursInvoke g) args
  Select _ scrutinee branches -> occursInvoke g scrutinee || any (\(Branch _ _ b) -> occursInvoke g b) branches
  Invoke _ n -> n == g
  Fix _ _ b -> occursInvoke g b
