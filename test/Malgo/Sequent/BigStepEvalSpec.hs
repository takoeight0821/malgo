module Malgo.Sequent.BigStepEvalSpec (specWith) where

import Control.Exception (SomeException, try)
import Effectful
import Effectful.Error.Static (Error, runError)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local (State)
import Malgo.Module (ArtifactPath, ModuleName)
import Malgo.Monad (runMalgoM)
import Malgo.Pass (runCompileError)
import Malgo.Prelude
import Malgo.Sequent.BigStepEval (bigStepEvalProgram)
import Malgo.Sequent.Core.Join (Program)
import Malgo.Sequent.Eval (EvalError, Handlers (..), evalProgram)
import Malgo.TestUtils
import System.Directory
import System.FilePath
import Test.Hspec

specWith :: ArtifactPath -> ArtifactPath -> Spec
specWith builtin prelude = parallel do
  testcases <- runIO do
    files <- listDirectory testcaseDir
    pure $ filter (isExtensionOf "mlg") files

  describe "golden" do
    for_ testcases \testcase -> do
      (moduleName, program) <- runIO $ compileTestCase builtin prelude (testcaseDir </> testcase)
      golden (takeBaseName testcase) $ runEval bigStepEvalProgram moduleName program

  describe "consistency" do
    for_ testcases \testcase -> do
      (moduleName, program) <- runIO $ compileTestCase builtin prelude (testcaseDir </> testcase)
      it (takeBaseName testcase <> " matches small-step") do
        smallStepResult <- try @SomeException $ runEval evalProgram moduleName program
        bigStepResult <- try @SomeException $ runEval bigStepEvalProgram moduleName program
        case (smallStepResult, bigStepResult) of
          (Right smallStep, Right bigStep) -> bigStep `shouldBe` smallStep
          (Left _, Left _) -> pure () -- Both failed, consistent
          (Left err, Right _) -> expectationFailure $ "Small-step failed but big-step succeeded: " <> show err
          (Right _, Left err) -> expectationFailure $ "Small-step succeeded but big-step failed: " <> show err

-- | Run an evaluator on a compiled program and capture stdout.
runEval ::
  (forall es. (Error EvalError :> es, State Uniq :> es, Reader ModuleName :> es, Reader Handlers :> es, IOE :> es) => Program -> Eff es ()) ->
  ModuleName ->
  Program ->
  IO String
runEval evaluator moduleName program = do
  runMalgoM flag $ runCompileError do
    stdin <- setupTestStdin
    (stdout, stdoutBuilder) <- setupTestStdout
    (stderr, _) <- setupTestStderr

    result <-
      runError @EvalError
        $ runReader moduleName
        $ runReader
          Handlers
            { stdin,
              stdout,
              stderr
            }
        $ evaluator program
    case result of
      Left (_, err) -> error $ show err
      Right _ -> do
        readIORef stdoutBuilder
