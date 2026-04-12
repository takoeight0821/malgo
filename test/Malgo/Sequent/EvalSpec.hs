module Malgo.Sequent.EvalSpec (specWith) where

import Control.Exception (SomeException, try)
import Effectful.Error.Static (runError)
import Effectful.Reader.Static (runReader)
import Malgo.Module (ArtifactPath, ModuleName)
import Malgo.Monad (runMalgoM)
import Malgo.Pass (runCompileError)
import Malgo.Prelude
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
      golden (takeBaseName testcase) (driveEval builtin prelude (testcaseDir </> testcase))

  describe "with-elaborate" do
    for_ testcases \testcase -> do
      it (takeBaseName testcase <> " matches without elaborate") do
        normalResult <- try @SomeException $ driveEval builtin prelude (testcaseDir </> testcase)
        elabResult <- try @SomeException $ driveEvalWithElaborate builtin prelude (testcaseDir </> testcase)
        case (normalResult, elabResult) of
          (Right normal, Right elab) -> elab `shouldBe` normal
          (Left _, Left _) -> pure ()
          (Left err, Right _) -> expectationFailure $ "Normal failed but elaborate succeeded: " <> show err
          (Right _, Left err) -> expectationFailure $ "Normal succeeded but elaborate failed: " <> show err

driveEval :: ArtifactPath -> ArtifactPath -> FilePath -> IO String
driveEval builtinName preludeName srcPath = do
  (moduleName, program) <- compileTestCase builtinName preludeName srcPath
  runEval moduleName program

driveEvalWithElaborate :: ArtifactPath -> ArtifactPath -> FilePath -> IO String
driveEvalWithElaborate builtinName preludeName srcPath = do
  (moduleName, program) <- compileTestCaseWithElaborate builtinName preludeName srcPath
  runEval moduleName program

runEval :: ModuleName -> Program -> IO String
runEval moduleName program = do
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
        $ evalProgram program
    case result of
      Left (_, err) -> error $ show err
      Right _ -> do
        readIORef stdoutBuilder
