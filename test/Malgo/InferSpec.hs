module Malgo.InferSpec (spec) where

import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Effectful (runPureEff)
import Effectful.Error.Static (runError)
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local (evalState, runState)
import Malgo.Elaborate (ElaboratePass (..))
import Malgo.Features (Feature (..), FeatureFlags (..))
import Malgo.Id (Id (..), IdSort (..))
import Malgo.Infer (InferPass (..), TyEnv)
import Malgo.Infer.Constraint
import Malgo.Infer.Unify (solveConstraints, unify)
import Malgo.Module (ModuleName (..))
import Malgo.Monad (runMalgoMWith)
import Malgo.Parser (ParserPass (..))
import Malgo.Pass
import Malgo.Prelude
import Malgo.Query (Query (..), fetch)
import Malgo.Query.Database (newDatabase)
import Malgo.Query.Engine (runQueryDB)
import Malgo.Rename
import Malgo.Syntax (Module (..))
import Malgo.TestUtils
import System.Directory
import System.FilePath
import System.Timeout (timeout)
import Test.Hspec

inferFlag :: Flag
inferFlag = flag {useInfer = True}

malgo2025Flags :: FeatureFlags
malgo2025Flags = FeatureFlags (Set.singleton Malgo2025)

spec :: Spec
spec = parallel do
  describe "full-program" do
    testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
    for_ testcases \testcase ->
      it (takeBaseName testcase) $ driveInfer testcase (testcaseDir </> testcase)

  describe "InferredModule export boundary" do
    -- Echo.mlg only defines 'main' at the top level; everything else
    -- (getContents, putStr, ...) comes from Builtin/Prelude.
    let echo = testcaseDir </> "Echo.mlg"

    it "exports only locally-defined names (no dep leakage)" do
      Captured {modName, depsEnv, exported} <- runInferCapturing echo
      Map.keys (Map.intersection depsEnv exported)
        `shouldBe` []
      let leaked =
            [ key
            | key <- Map.keys exported,
              key.moduleName /= modName
            ]
      leaked `shouldBe` []

    it "exports the module's top-level def 'main'" do
      Captured {exported} <- runInferCapturing echo
      let mainNames = filter (\i -> i.name == "main") (Map.keys exported)
      length mainNames `shouldBe` 1

  describe "Constraint" do
    describe "applySubst" do
      it "substitutes type variables" do
        let subst = Map.singleton (mkId "_t0") tyInt32
        applySubst subst (TVar (mkId "_t0") 0) `shouldBe` tyInt32

      it "leaves unrelated variables" do
        let subst = Map.singleton (mkId "_t0") tyInt32
        applySubst subst (TVar (mkId "_t1") 0) `shouldBe` TVar (mkId "_t1") 0

      it "applies to arrow types" do
        let subst = Map.singleton (mkId "_t0") tyInt32
        applySubst subst (TArr (TVar (mkId "_t0") 0) (TVar (mkId "_t1") 0))
          `shouldBe` TArr tyInt32 (TVar (mkId "_t1") 0)

      it "substitutes variables in TMu" do
        let subst = Map.singleton (mkId "_t0") tyInt32
        applySubst subst (TMu (mkId "a") (TArr (TVar (mkId "_t0") 0) (TVar (mkId "a") 0)))
          `shouldBe` TMu (mkId "a") (TArr tyInt32 (TVar (mkId "a") 0))

      it "respects binder in TMu" do
        let subst = Map.singleton (mkId "a") tyInt32
        applySubst subst (TMu (mkId "a") (TVar (mkId "a") 0))
          `shouldBe` TMu (mkId "a") (TVar (mkId "a") 0)

    describe "freeVars" do
      it "finds free variables in arrow types" do
        freeVars (TArr (TVar (mkId "a") 0) (TVar (mkId "b") 0))
          `shouldBe` Set.fromList [mkId "a", mkId "b"]

      it "removes bound variables in forall" do
        freeVars (TForall (mkId "a") (TArr (TVar (mkId "a") 0) (TVar (mkId "b") 0)))
          `shouldBe` Set.fromList [mkId "b"]

      it "removes bound variables in TMu" do
        freeVars (TMu (mkId "a") (TArr (TVar (mkId "a") 0) (TVar (mkId "b") 0)))
          `shouldBe` Set.fromList [mkId "b"]

    describe "occursIn" do
      it "detects direct occurrence" do
        occursIn (mkId "a") (TVar (mkId "a") 0) `shouldBe` True

      it "detects nested occurrence" do
        occursIn (mkId "a") (TArr (TVar (mkId "a") 0) tyInt32) `shouldBe` True

      it "returns False for absent variable" do
        occursIn (mkId "a") (TArr tyInt32 tyInt32) `shouldBe` False

      it "respects binder in TMu" do
        occursIn (mkId "a") (TMu (mkId "a") (TVar (mkId "a") 0)) `shouldBe` False
        occursIn (mkId "a") (TMu (mkId "b") (TVar (mkId "a") 0)) `shouldBe` True

    describe "generalize" do
      it "generalizes variables above the given level" do
        let ty = TArr (TVar (mkId "_t0") 1) (TVar (mkId "_t1") 0)
            scheme = generalize 0 ty
        scheme.vars `shouldBe` [mkId "_t0"]

      it "does not generalize variables at or below the given level" do
        let ty = TArr (TVar (mkId "_t0") 0) (TVar (mkId "_t1") 0)
            scheme = generalize 0 ty
        scheme.vars `shouldBe` []

      it "does not quantify TMu-bound variables" do
        -- μ_t5._t5 -> _t6 at level 2, generalizing at level 0
        -- Only _t6 should be quantified; _t5 is bound by μ.
        let ty = TMu (mkId "_t5") (TArr (TVar (mkId "_t5") 2) (TVar (mkId "_t6") 2))
            scheme = generalize 0 ty
        scheme.vars `shouldBe` [mkId "_t6"]

      it "does not quantify TForall-bound variables" do
        let ty = TForall (mkId "_t5") (TArr (TVar (mkId "_t5") 2) (TVar (mkId "_t6") 2))
            scheme = generalize 0 ty
        scheme.vars `shouldBe` [mkId "_t6"]

  describe "Unify" do
    describe "basic unification" do
      it "unifies identical type constructors" do
        result <- runUnify (dummyRange) tyInt32 tyInt32
        result `shouldSatisfy` isRight

      it "unifies variable with concrete type" do
        result <- runUnify (dummyRange) (TVar (mkId "_t0") 0) tyInt32
        case result of
          Right _ -> pure ()
          Left err -> expectationFailure $ show err

      it "fails on different constructors" do
        result <- runUnify (dummyRange) tyInt32 tyString
        case result of
          Left _ -> pure ()
          Right _ -> expectationFailure "Expected unification to fail"

      it "unifies arrow types" do
        result <- runUnify (dummyRange) (TArr (TVar (mkId "_t0") 0) tyInt32) (TArr tyString tyInt32)
        case result of
          Right _ -> pure ()
          Left err -> expectationFailure $ show err

      it "allows equi-recursive types (relaxed occurs check)" do
        result <- runUnify (dummyRange) (TVar (mkId "_t0") 0) (TArr (TVar (mkId "_t0") 0) tyInt32)
        result `shouldSatisfy` isRight

      it "unifies recursive types to TMu" do
        let v = mkId "_t0"
            t = TArr (TVar v 0) tyInt32
        result <- runUnify (dummyRange) (TVar v 0) t
        case result of
          Right subst -> Map.lookup v subst `shouldBe` Just (TMu v t)
          Left err -> expectationFailure $ show err

      it "rejects (μa.a→Int) vs (Int→Int)" do
        -- μa.a→Int unrolls to (μa.a→Int)→Int, never collapses to Int.
        -- Final mismatch (TArr vs TCon Int) must surface.
        let t1 = TMu (mkId "a") (TArr (TVar (mkId "a") 0) tyInt32)
            t2 = TArr tyInt32 tyInt32
        result <- runUnify (dummyRange) t1 t2
        case result of
          Left _ -> pure ()
          Right _ -> expectationFailure "Expected μa.a→Int vs Int→Int to fail"

      it "accepts α-equivalent recursive types: (μa.a→Int) vs (μb.b→Int)" do
        let t1 = TMu (mkId "a") (TArr (TVar (mkId "a") 0) tyInt32)
            t2 = TMu (mkId "b") (TArr (TVar (mkId "b") 0) tyInt32)
        result <- runUnify (dummyRange) t1 t2
        case result of
          Right _ -> pure ()
          Left err -> expectationFailure $ show err

      it "terminates on α-equivalent recursive types with forall binders" do
        let t1 = TMu (mkId "a") (TArr (TVar (mkId "a") 0) (TForall (mkId "x") (TVar (mkId "x") 0)))
            t2 = TMu (mkId "b") (TArr (TVar (mkId "b") 0) (TForall (mkId "y") (TVar (mkId "y") 0)))
        timed <- timeout 1_000_000 $ runUnify (dummyRange) t1 t2
        case timed of
          Nothing -> expectationFailure "Expected unification to terminate within 1s"
          Just result -> result `shouldSatisfy` isRight

      it "rejects (μa.a→Int) vs (μb.b→String) on tail mismatch" do
        let t1 = TMu (mkId "a") (TArr (TVar (mkId "a") 0) tyInt32)
            t2 = TMu (mkId "b") (TArr (TVar (mkId "b") 0) tyString)
        result <- runUnify (dummyRange) t1 t2
        case result of
          Left _ -> pure ()
          Right _ -> expectationFailure "Expected μa.a→Int vs μb.b→String to fail"

      it "rejects recursive forall codomain mismatch" do
        let t1 = TMu (mkId "a") (TArr (TVar (mkId "a") 0) (TForall (mkId "x") tyInt32))
            t2 = TMu (mkId "b") (TArr (TVar (mkId "b") 0) (TForall (mkId "y") tyString))
        result <- runUnify (dummyRange) t1 t2
        case result of
          Left _ -> pure ()
          Right _ -> expectationFailure "Expected recursive forall codomain mismatch to fail"

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

    describe "constraint solving" do
      it "does not commit substitutions or clear constraints when solving fails" do
        let existing = mkId "_existing"
            inferred = mkId "_inferred"
            initialSubst = Map.singleton existing tyString
            successfulConstraint = CUnify dummyRange (TVar inferred 0) tyInt32
            failingConstraint = CUnify dummyRange tyInt32 tyString
            initialConstraints = [failingConstraint, successfulConstraint]
            initState =
              GenState
                { constraints = initialConstraints,
                  currentLevel = 0,
                  solvedSubst = initialSubst
                }
        (result, finalState) <- runSolveConstraints initState
        case result of
          Left _ -> pure ()
          Right _ -> expectationFailure "Expected constraint solving to fail"
        finalState.solvedSubst `shouldBe` initialSubst
        finalState.constraints `shouldBe` initialConstraints

-- | Module name used by tests when constructing 'Id' values via 'mkId'.
testModule :: ModuleName
testModule = ModuleName "Test"

-- | Build a deterministic 'Id' for a surface name. All test 'Id's share the
-- same module and 'External' sort, so two calls with the same text are equal.
mkId :: Text -> Id
mkId n = Id {name = n, moduleName = testModule, sort = External}

-- | Helper to run unification in a test context
runUnify :: Range -> Ty -> Ty -> IO (Either InferError Subst)
runUnify pos t1 t2 = pure (stripCallStack result)
  where
    initState =
      GenState
        { constraints = [],
          currentLevel = 0,
          solvedSubst = Map.empty
        }
    result =
      runPureEff
        $ runReader testModule
        $ evalState (Uniq 0)
        $ evalState initState
        $ runError @InferError
        $ unify pos t1 t2
    stripCallStack (Left (_, err)) = Left err
    stripCallStack (Right x) = Right x

-- | Helper to run constraint solving while preserving the final GenState.
runSolveConstraints :: GenState -> IO (Either InferError Subst, GenState)
runSolveConstraints initState = pure (stripCallStack result)
  where
    result =
      runPureEff
        $ runReader testModule
        $ evalState (Uniq 0)
        $ runState initState
        $ runError @InferError
        $ solveConstraints
    stripCallStack (Left (_, err), finalState) = (Left err, finalState)
    stripCallStack (Right x, finalState) = (Right x, finalState)

-- | Testcases that are known to fail or hang under InferPass. Tracked
-- in #333; promote a case out of this map once the underlying inferencer
-- bug is fixed. The 'driveInfer' runner deliberately fails when an
-- entry here starts succeeding, so the list cannot drift behind reality.
knownBadInfer :: Map FilePath String
knownBadInfer =
  Map.fromList
    []

-- | Run the full pipeline (Parse -> Rename -> Elaborate -> Infer) on a source file.
-- Success means InferPass completed without throwing a type error.
--
-- Outcomes:
--   * Unlisted testcase succeeds -> pass.
--   * Unlisted testcase times out or fails -> hard 'expectationFailure'.
--     This locks regressions in cross-module inference (#321) and any
--     other case currently expected to pass.
--   * Listed testcase times out / fails -> 'pendingWith' with the recorded
--     reason, so CI surfaces the pending count without going red.
--   * Listed testcase now succeeds -> hard 'expectationFailure' demanding
--     removal from 'knownBadInfer', preventing the allowlist from rotting.
--
-- A wall-clock timeout guards against latent non-termination in the
-- inferencer so a single bad case cannot stall the whole suite.
driveInfer :: FilePath -> FilePath -> IO ()
driveInfer testcaseName srcPath = do
  result <- try @SomeException $ timeout inferTimeoutMicros $ runInfer srcPath
  case (result, Map.lookup testcaseName knownBadInfer) of
    (Right (Just ()), Nothing) -> pure ()
    (Right (Just ()), Just reason) ->
      expectationFailure
        $ testcaseName
        <> " now passes — remove it from knownBadInfer (was: "
        <> reason
        <> ")"
    (Right Nothing, Just reason) ->
      pendingWith
        $ "timed out after "
        <> show timeoutSecs
        <> "s ("
        <> reason
        <> ")"
    (Right Nothing, Nothing) ->
      expectationFailure
        $ "InferPass timed out after "
        <> show timeoutSecs
        <> "s — possible non-termination regression"
    (Left err, Just reason) ->
      pendingWith $ reason <> ": " <> show err
    (Left err, Nothing) ->
      expectationFailure $ "InferPass failed: " <> show err
  where
    inferTimeoutMicros :: Int
    inferTimeoutMicros = 5_000_000
    timeoutSecs :: Int
    timeoutSecs = inferTimeoutMicros `div` 1_000_000

runInfer :: FilePath -> IO ()
runInfer srcPath = void $ runInferCapturing srcPath

-- | Captured intermediate state from running InferPass on a single source file.
-- Lets boundary tests inspect the same envs the engine's 'InferredModule'
-- handler computes ('depsEnv' is unioned dep envs; 'exported' is what the
-- module contributes after 'Map.difference').
data Captured = Captured
  { modName :: ModuleName,
    depsEnv :: TyEnv,
    exported :: TyEnv
  }

runInferCapturing :: FilePath -> IO Captured
runInferCapturing srcPath = do
  src <- convertString <$> BS.readFile srcPath
  db <- newDatabase
  runMalgoMWith inferFlag malgo2025Flags $ runCompileError $ runQueryDB db do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (Module modName def, rnState) <- runPass RenamePass (parsed, rnEnv)
    -- Pull each dep's exported TyEnv via the InferredModule query, which
    -- runs InferPass on the dep and captures all of its sigs/foreigns/data
    -- constructors/inferred bare-def types. Failures propagate so
    -- 'driveInfer' can report the real error via 'pendingWith'.
    depsEnv <-
      foldlM
        ( \acc dep -> do
            depEnv <- fetch (InferredModule dep)
            pure (acc <> depEnv)
        )
        Map.empty
        (Set.toList rnState.dependencies)
    runReader modName do
      elaborated <- runPass ElaboratePass def
      (_, finalEnv) <- runPass InferPass (depsEnv, elaborated)
      pure Captured {modName, depsEnv, exported = Map.difference finalEnv depsEnv}
