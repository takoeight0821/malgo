module Malgo.Sequent.BigStepEvalSpec (specWith) where

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
      golden (takeBaseName testcase) do
        (moduleName, program) <- compileTestCase builtin prelude (testcaseDir </> testcase)
        runEval bigStepEvalProgram moduleName program

  describe "consistency" do
    for_ testcases \testcase -> do
      it (takeBaseName testcase <> " matches small-step")
        $ assertConsistentResults
          ( do
              (moduleName, program) <- compileTestCase builtin prelude (testcaseDir </> testcase)
              runEval evalProgram moduleName program
          )
          ( do
              (moduleName, program) <- compileTestCase builtin prelude (testcaseDir </> testcase)
              runEval bigStepEvalProgram moduleName program
          )

  describe "with-elaborate" do
    for_ testcases \testcase -> do
      let compileAndRun compile = do
            (moduleName, program) <- compile builtin prelude (testcaseDir </> testcase)
            runEval bigStepEvalProgram moduleName program
      it (takeBaseName testcase)
        $ assertConsistentResults
          (compileAndRun compileTestCase)
          (compileAndRun compileTestCaseWithElaborate)

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
              stderr,
              arguments = []
            }
        $ evaluator program
    case result of
      Left (_, err) -> error $ show err
      Right _ -> do
        readIORef stdoutBuilder
