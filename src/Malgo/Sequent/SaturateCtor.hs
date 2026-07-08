-- | Saturated-constructor inlining, a pure 'Fun.Program' → 'Fun.Program'
-- rewrite that runs at the very start of 'Malgo.Sequent.ToCore.toCore', i.e.
-- before CPS conversion — shared by every backend (Eval, Scheme, Zig) and
-- every direct caller of 'toCore' (tests included), not just the Zig
-- backend.
--
-- 'Malgo.Sequent.ToFun.fromConstructor' compiles every data constructor as
-- a curried function (@Cons = \\x -> \\y -> Cons x y@), so ANY call —
-- @Cons x xs@, @Cons (f x) (mapList f xs)@, arguments immediate or
-- themselves the result of another call — compiles via the generic
-- 'Fun.Apply'\/'Fun.Invoke' machinery, paying for the curried closure's own
-- allocations and indirections on every construction. This pass recognizes
-- curried-constructor definitions structurally and rewrites a fully (or
-- over-) saturated call spine into a direct 'Fun.Construct', regardless of
-- whether its arguments are immediate — 'Malgo.Sequent.Core.Flat.flatProducer'\'s
-- existing @split@\/@isValue@ machinery already sequences non-immediate
-- 'Fun.Construct' arguments correctly, so no CPS-aware bookkeeping is
-- needed here. Partial application (too few arguments in the spine) is
-- left untouched, preserving the constructor's curried closure value.
module Malgo.Sequent.SaturateCtor
  ( saturateProgram,
  )
where

import Data.Map.Strict qualified as Map
import Malgo.Prelude
import Malgo.Sequent.Fun

saturateProgram :: Program -> Program
saturateProgram program = program {definitions = map rewriteDef program.definitions}
  where
    ctorTable = Map.fromList (mapMaybe recognizeDef program.definitions)
    rewriteDef (range, name, body) = (range, name, go body)

    go expr = case trySaturate ctorTable expr' of
      Just rewritten -> rewritten
      Nothing -> expr'
      where
        expr' = case expr of
          Var {} -> expr
          Literal {} -> expr
          Construct r tag args -> Construct r tag (map go args)
          Let r n v b -> Let r n (go v) (go b)
          Lambda r ps b -> Lambda r ps (go b)
          Object r fields -> Object r (Map.map go fields)
          Apply r fn args -> Apply r (go fn) (map go args)
          Project r e field -> Project r (go e) field
          Primitive r op args -> Primitive r op (map go args)
          Select r scrutinee branches -> Select r (go scrutinee) (map goBranch branches)
          Invoke {} -> expr
          Fix r n b -> Fix r n (go b)
        goBranch (Branch r p b) = Branch r p (go b)

-- | A definition compiled from 'Malgo.Sequent.ToFun.fromConstructor' has
-- the shape @\\p1 -> \\p2 -> ... -> \\pn -> Construct tag [p1..pn]@ (arity 0:
-- just @Construct tag []@). This structural test also accepts ordinary
-- functions with exactly this shape (@\\x -> \\y -> (x, y)@) — inlining
-- those too is sound, it is just constructor-shaped inlining either way.
recognizeDef :: (Range, Name, Expr) -> Maybe (Name, (Tag, Int))
recognizeDef (_, name, body) = (name,) <$> peel [] body

peel :: [Name] -> Expr -> Maybe (Tag, Int)
peel params = \case
  Lambda _ [p] b -> peel (params <> [p]) b
  Construct _ tag args
    | length args == length params,
      and (zipWith isParam args params) ->
        Just (tag, length params)
  _ -> Nothing
  where
    isParam (Var _ n) p = n == p
    isParam _ _ = False

-- | Unrolls an application spine outside-in: @Apply (Apply f [a1]) [a2]@
-- becomes @(f, [a1, a2])@ (left-to-right argument order preserved).
unrollSpine :: Expr -> [Expr] -> (Expr, [Expr])
unrollSpine (Apply _ fn args) acc = unrollSpine fn (args <> acc)
unrollSpine expr acc = (expr, acc)

-- | Saturated or over-saturated call of a recognized constructor →
-- 'Construct' (plus any excess arguments re-applied to the constructed
-- value, which fails at runtime exactly as it did before this pass —
-- structs are not callable). Under-saturated (a genuine partial
-- application, e.g. passing the constructor itself as a higher-order
-- value) → 'Nothing', leaving the curried 'Invoke'\/'Apply' chain intact.
trySaturate :: Map Name (Tag, Int) -> Expr -> Maybe Expr
trySaturate ctorTable expr = do
  let (base, args) = unrollSpine expr []
  name <- case base of
    Invoke _ n -> Just n
    _ -> Nothing
  (tag, arity) <- Map.lookup name ctorTable
  guard (length args >= arity)
  let (ctorArgs, extra) = splitAt arity args
      built = Construct (range expr) tag ctorArgs
  pure (foldl' (\f a -> Apply (range expr) f [a]) built extra)
