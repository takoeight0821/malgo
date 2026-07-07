-- | Saturated-constructor inlining, a pure 'Join.Program' → 'Join.Program'
-- rewrite that runs before 'Malgo.Backend.Zig.ClosureConv.convertProgram'.
--
-- 'Malgo.Sequent.ToFun.fromConstructor' compiles every data constructor as
-- a curried function (@Cons = \\x -> \\y -> Cons x y@), so a fully-applied
-- constructor call like @Cons x xs@ reaches Join IR as a call of that
-- shared top-level closure — three extra Object allocations (the
-- intermediate partial-application closures and continuation joins) that
-- also hide the @mkStruct@ from the caller's own block, where a later
-- reuse-analysis pass would need to find it.
--
-- This pass recognizes curried-constructor definitions structurally and,
-- for each fully-saturated call site, replaces the whole call chain with a
-- direct construction. Partial and over-applications are left untouched.
module Malgo.Backend.Zig.SaturateCtor
  ( saturateProgram,
  )
where

import Data.Map.Strict qualified as Map
import Malgo.Prelude
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Fun (Name, Tag)

saturateProgram :: Program -> Program
saturateProgram program = program {definitions = map rewriteDef program.definitions}
  where
    ctorTable = Map.fromList (mapMaybe recognizeDef program.definitions)
    rewriteDef (range, name, ret, stmt) =
      (range, name, ret, rewriteStatement ctorTable (nameUseCounts stmt) Map.empty stmt)

-- * Recognizing curried constructor definitions

-- | A definition compiled from 'Malgo.Sequent.ToFun.fromConstructor' has
-- the shape
--
-- > Cut (Lambda [p1,k1] (Cut (Lambda [p2,k2] (... Cut (Construct tag [p1..pn] []) kn ...) k2)) k1) ret
--
-- (arity 0: @Cut (Construct tag [] []) ret@). This structural test also
-- accepts ordinary functions with exactly this shape (@\\x -> \\y -> (x,
-- y)@) — inlining those too is sound, it is just constructor-shaped
-- inlining either way.
recognizeDef :: (Range, Name, Name, Statement) -> Maybe (Name, (Tag, Int))
recognizeDef (_, name, ret, Cut producer consumer)
  | consumer == ret = (name,) <$> peel [] producer
recognizeDef _ = Nothing

peel :: [Name] -> Producer -> Maybe (Tag, Int)
peel params = \case
  Lambda _ [p, k] (Cut inner k')
    | k == k' -> peel (params <> [p]) inner
  Construct _ tag args []
    | length args == length params,
      and (zipWith isParam args params) ->
        Just (tag, length params)
  _ -> Nothing
  where
    isParam (Var _ n) p = n == p
    isParam _ _ = False

-- * Call-site rewriting

-- | Join bindings visible at the current point in the tree — a 'Join's own
-- name is only in scope inside its body.
type JoinEnv = Map Name Consumer

rewriteStatement :: Map Name (Tag, Int) -> Map Name Int -> JoinEnv -> Statement -> Statement
rewriteStatement ctorTable uses = go
  where
    go env = \case
      Cut producer consumer -> Cut (goProducer env producer) consumer
      Join range name consumer stmt ->
        let consumer' = goConsumer env consumer
            stmt' = go (Map.insert name consumer env) stmt
         in -- Dead-join elimination: a saturated rewrite downstream may
            -- have just consumed the one reference to `name` (its Apply
            -- consumer, replaced by a direct Construct). Without this, the
            -- dead join would still reify as an escaping heap closure
            -- (Malgo.Backend.Zig.ClosureConv), erasing the win.
            if name `elem` nameUses stmt'
              then Join range name consumer' stmt'
              else stmt'
      Primitive range op producers consumer -> Primitive range op (map (goProducer env) producers) consumer
      Invoke range fn consumer
        | Just (tag, arity) <- Map.lookup fn ctorTable,
          Just (args, final) <- saturate env arity consumer ->
            Cut (Construct range tag args []) final
        | otherwise -> Invoke range fn consumer
      ExternalCall range op producers consumer -> ExternalCall range op (map (goProducer env) producers) consumer
      BinOp range op lhs rhs consumer -> BinOp range op (goProducer env lhs) (goProducer env rhs) consumer
      Ifz range cond thenS elseS -> Ifz range (goProducer env cond) (go env thenS) (go env elseS)

    goProducer env = \case
      Construct range tag ps ns -> Construct range tag (map (goProducer env) ps) ns
      Lambda range params stmt -> Lambda range params (go env stmt)
      Object range fields -> Object range (fmap (fmap (go env)) fields)
      Mu range name stmt -> Mu range name (go env stmt)
      Cocase range branches -> Cocase range [(d, vs, go env s) | (d, vs, s) <- branches]
      p@Var {} -> p
      p@Literal {} -> p

    goConsumer env = \case
      Then range name stmt -> Then range name (go env stmt)
      Select range branches -> Select range [Branch r p (go env s) | Branch r p s <- branches]
      c -> c

    -- Follows a chain of exactly `n` single-argument 'Apply' consumers,
    -- each bound by a Join used nowhere else in the definition, collecting
    -- one producer per step. Arity 0 succeeds trivially without consulting
    -- `env`. Fails (aborting the whole rewrite) on a partial application —
    -- a shorter chain, or a join reused elsewhere.
    saturate :: JoinEnv -> Int -> Name -> Maybe ([Producer], Name)
    saturate _ 0 k = Just ([], k)
    saturate env n k = do
      1 <- Map.lookup k uses
      Apply _ [arg] [k'] <- Map.lookup k env
      (args, final) <- saturate env (n - 1) k'
      pure (arg : args, final)

-- * Occurrence counting

nameUseCounts :: Statement -> Map Name Int
nameUseCounts = Map.fromListWith (+) . map (,1 :: Int) . nameUses

nameUses :: Statement -> [Name]
nameUses = \case
  Cut producer consumer -> producerUses producer <> [consumer]
  Join _ _ consumer stmt -> consumerUses consumer <> nameUses stmt
  Primitive _ _ producers consumer -> foldMap producerUses producers <> [consumer]
  Invoke _ fn consumer -> [fn, consumer]
  ExternalCall _ _ producers consumer -> foldMap producerUses producers <> [consumer]
  BinOp _ _ lhs rhs consumer -> producerUses lhs <> producerUses rhs <> [consumer]
  Ifz _ cond thenS elseS -> producerUses cond <> nameUses thenS <> nameUses elseS

producerUses :: Producer -> [Name]
producerUses = \case
  Var _ n -> [n]
  Literal _ _ -> []
  Construct _ _ ps ns -> foldMap producerUses ps <> ns
  Lambda _ _ stmt -> nameUses stmt
  Object _ fields -> foldMap (nameUses . snd) (Map.elems fields)
  Mu _ _ stmt -> nameUses stmt
  Cocase _ branches -> foldMap (\(_, _, s) -> nameUses s) branches

consumerUses :: Consumer -> [Name]
consumerUses = \case
  Label _ n -> [n]
  Apply _ producers ns -> foldMap producerUses producers <> ns
  Project _ _ n -> [n]
  Then _ _ stmt -> nameUses stmt
  Finish _ -> []
  Select _ branches -> foldMap (\(Branch _ _ s) -> nameUses s) branches
  Destructor _ _ producers n -> foldMap producerUses producers <> [n]
