module Malgo.Sequent.EvalSpec (spec) where

import Effectful.Error.Static (runError)
import Effectful.Reader.Static (runReader)
import Malgo.Module (ArtifactPath)
import Malgo.Monad (runMalgoM)
import Malgo.Pass (runCompileError)
import Malgo.Prelude
import Malgo.Sequent.Eval (EvalError, Handlers (..), evalProgram)
import Malgo.TestUtils
import System.Directory
import System.FilePath
import Test.Hspec

spec :: Spec
spec = parallel do
  (builtin, prelude) <- runIO do
    builtin <- setupEvalBuiltin
    prelude <- setupEvalPrelude
    pure (builtin, prelude)
  testcases <- runIO do
    files <- listDirectory testcaseDir
    pure $ filter (isExtensionOf "mlg") files

  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveEval builtin prelude (testcaseDir </> testcase))

driveEval :: ArtifactPath -> ArtifactPath -> FilePath -> IO String
driveEval builtinName preludeName srcPath = do
  (moduleName, program) <- compileTestCase builtinName preludeName srcPath
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
