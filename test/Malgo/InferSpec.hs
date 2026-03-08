module Malgo.InferSpec (spec) where

import Data.Either (isRight)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Effectful (runPureEff)
import Effectful.Error.Static (runError)
import Effectful.State.Static.Local (evalState)
import Malgo.Infer.Constraint
import Malgo.Infer.Unify (solveConstraints, unify)
import Malgo.Prelude hiding (State, evalState, get, gets, modify, put, runError)
import Test.Hspec

spec :: Spec
spec = parallel do
  describe "Constraint" do
    describe "applySubst" do
      it "substitutes type variables" do
        let subst = Map.singleton "_t0" tyInt32
        applySubst subst (TVar "_t0" 0) `shouldBe` tyInt32

      it "leaves unrelated variables" do
        let subst = Map.singleton "_t0" tyInt32
        applySubst subst (TVar "_t1" 0) `shouldBe` TVar "_t1" 0

      it "applies to arrow types" do
        let subst = Map.singleton "_t0" tyInt32
        applySubst subst (TArr (TVar "_t0" 0) (TVar "_t1" 0))
          `shouldBe` TArr tyInt32 (TVar "_t1" 0)

    describe "freeVars" do
      it "finds free variables in arrow types" do
        freeVars (TArr (TVar "a" 0) (TVar "b" 0))
          `shouldBe` Set.fromList ["a", "b"]

      it "removes bound variables in forall" do
        freeVars (TForall "a" (TArr (TVar "a" 0) (TVar "b" 0)))
          `shouldBe` Set.fromList ["b"]

    describe "occursIn" do
      it "detects direct occurrence" do
        occursIn "a" (TVar "a" 0) `shouldBe` True

      it "detects nested occurrence" do
        occursIn "a" (TArr (TVar "a" 0) tyInt32) `shouldBe` True

      it "returns False for absent variable" do
        occursIn "a" (TArr tyInt32 tyInt32) `shouldBe` False

    describe "generalize" do
      it "generalizes variables above the given level" do
        let ty = TArr (TVar "_t0" 1) (TVar "_t1" 0)
            scheme = generalize 0 ty
        scheme.vars `shouldBe` ["_t0"]

      it "does not generalize variables at or below the given level" do
        let ty = TArr (TVar "_t0" 0) (TVar "_t1" 0)
            scheme = generalize 0 ty
        scheme.vars `shouldBe` []

  describe "Unify" do
    describe "basic unification" do
      it "unifies identical type constructors" do
        result <- runUnify (dummyRange) tyInt32 tyInt32
        result `shouldSatisfy` isRight

      it "unifies variable with concrete type" do
        result <- runUnify (dummyRange) (TVar "_t0" 0) tyInt32
        case result of
          Right _ -> pure ()
          Left err -> expectationFailure $ show err

      it "fails on different constructors" do
        result <- runUnify (dummyRange) tyInt32 tyString
        case result of
          Left _ -> pure ()
          Right _ -> expectationFailure "Expected unification to fail"

      it "unifies arrow types" do
        result <- runUnify (dummyRange) (TArr (TVar "_t0" 0) tyInt32) (TArr tyString tyInt32)
        case result of
          Right _ -> pure ()
          Left err -> expectationFailure $ show err

      it "detects occurs check" do
        result <- runUnify (dummyRange) (TVar "_t0" 0) (TArr (TVar "_t0" 0) tyInt32)
        case result of
          Left _ -> pure ()
          Right _ -> expectationFailure "Expected occurs check failure"

    describe "bottom type" do
      it "bottom unifies with any type" do
        result <- runUnify (dummyRange) TBottom tyInt32
        result `shouldSatisfy` isRight

      it "any type unifies with bottom" do
        result <- runUnify (dummyRange) tyString TBottom
        result `shouldSatisfy` isRight

    describe "record unification" do
      it "unifies matching closed records" do
        let r1 = TRecord [("x", tyInt32), ("y", tyString)] Nothing
            r2 = TRecord [("x", tyInt32), ("y", tyString)] Nothing
        result <- runUnify (dummyRange) r1 r2
        case result of
          Right _ -> pure ()
          Left err -> expectationFailure $ show err

      it "fails on mismatched closed records" do
        let r1 = TRecord [("x", tyInt32)] Nothing
            r2 = TRecord [("x", tyInt32), ("y", tyString)] Nothing
        result <- runUnify (dummyRange) r1 r2
        case result of
          Left _ -> pure ()
          Right _ -> expectationFailure "Expected failure on mismatched records"

    describe "tuple unification" do
      it "unifies matching tuples" do
        result <- runUnify (dummyRange) (TTuple [tyInt32, tyString]) (TTuple [tyInt32, tyString])
        case result of
          Right _ -> pure ()
          Left err -> expectationFailure $ show err

      it "fails on different-length tuples" do
        result <- runUnify (dummyRange) (TTuple [tyInt32]) (TTuple [tyInt32, tyString])
        case result of
          Left _ -> pure ()
          Right _ -> expectationFailure "Expected failure on different-length tuples"

-- | Helper to run unification in a test context
runUnify :: Range -> Ty -> Ty -> IO (Either InferError Subst)
runUnify pos t1 t2 = pure (stripCallStack result)
  where
    initState =
      GenState
        { nextVar = 100,
          constraints = [],
          currentLevel = 0,
          solvedSubst = Map.empty
        }
    result = runPureEff $ evalState initState $ runError @InferError $ unify pos t1 t2
    stripCallStack (Left (_, err)) = Left err
    stripCallStack (Right x) = Right x
