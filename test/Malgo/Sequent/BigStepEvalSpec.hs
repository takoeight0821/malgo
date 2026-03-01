module Malgo.Sequent.BigStepEvalSpec (spec) where

import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Effectful
import Effectful.Error.Static (runError)
import Effectful.Reader.Static (runReader)
import Malgo.Module
import Malgo.Monad (runMalgoM)
import Malgo.Parser (ParserPass (..), parse)
import Malgo.Pass (runCompileError, runPass)
import Malgo.Prelude
import Malgo.Rename
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.Sequent.BigStepEval (bigStepEvalProgram)
import Malgo.Sequent.Core.Flat (flatProgram)
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Eval (EvalError, Handlers (..), evalProgram)
import Malgo.Sequent.ToCore (toCore)
import Malgo.Sequent.ToFun (ToFunPass (..))
import Malgo.Syntax (Module (..))
import Malgo.TestUtils hiding (setupBuiltin, setupPrelude)
import System.Directory
import System.FilePath
import Test.Hspec

spec :: Spec
spec = parallel do
  (builtin, prelude) <- runIO do
    builtin <- setupBuiltin
    prelude <- setupPrelude
    pure (builtin, prelude)
  testcases <- runIO do
    files <- listDirectory testcaseDir
    pure $ filter (isExtensionOf "mlg") files

  describe "golden" do
    for_ testcases \testcase -> do
      golden (takeBaseName testcase) (driveBigStepEval builtin prelude (testcaseDir </> testcase))

  describe "consistency" do
    for_ testcases \testcase -> do
      it (takeBaseName testcase <> " matches small-step") do
        smallStepResult <- try @SomeException $ driveSmallStepEval builtin prelude (testcaseDir </> testcase)
        bigStepResult <- try @SomeException $ driveBigStepEval builtin prelude (testcaseDir </> testcase)
        case (smallStepResult, bigStepResult) of
          (Right smallStep, Right bigStep) -> bigStep `shouldBe` smallStep
          (Left _, Left _) -> pure () -- Both failed, consistent
          (Left err, Right _) -> expectationFailure $ "Small-step failed but big-step succeeded: " <> show err
          (Right _, Left err) -> expectationFailure $ "Small-step succeeded but big-step failed: " <> show err

setupBuiltin :: IO ArtifactPath
setupBuiltin = do
  src <- convertString <$> BS.readFile builtinPath
  runMalgoM flag $ runCompileError do
    parsed <- runPass ParserPass (builtinPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    program <- runReader renamed.moduleName $ toCore fun >>= flatProgram >>= joinProgram
    saveCore renamed.moduleName program
    getModulePath renamed.moduleName

setupPrelude :: IO ArtifactPath
setupPrelude = do
  src <- convertString <$> BS.readFile preludePath
  runMalgoM flag $ runCompileError do
    parsed <- runPass ParserPass (preludePath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    program <- runReader renamed.moduleName $ toCore fun >>= flatProgram >>= joinProgram
    saveCore renamed.moduleName program
    getModulePath renamed.moduleName

-- | Compile a test case to a linked Join IR program.
compileTestCase :: ArtifactPath -> ArtifactPath -> FilePath -> IO (ModuleName, Program)
compileTestCase builtinName preludeName srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    Program {definitions = program} <- runReader renamed.moduleName $ toCore fun >>= flatProgram >>= joinProgram

    Program {definitions = builtin} <- load builtinName ".sqt"
    Program {definitions = prelude} <- load preludeName ".sqt"

    let linked = Program {definitions = builtin <> prelude <> program, dependencies = []}
    pure (renamed.moduleName, linked)

-- | Run big-step evaluator on a test case and capture stdout.
driveBigStepEval :: ArtifactPath -> ArtifactPath -> FilePath -> IO String
driveBigStepEval builtinName preludeName srcPath = do
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
        $ bigStepEvalProgram program
    case result of
      Left (_, err) -> error $ show err
      Right _ -> do
        readIORef stdoutBuilder

-- | Run small-step (CPS) evaluator on a test case and capture stdout.
driveSmallStepEval :: ArtifactPath -> ArtifactPath -> FilePath -> IO String
driveSmallStepEval builtinName preludeName srcPath = do
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

saveCore :: (Workspace :> es, IOE :> es) => ModuleName -> Program -> Eff es ()
saveCore moduleName program = do
  modulePath <- getModulePath moduleName
  save modulePath ".sqt" program

setupTestStdin :: (MonadIO m) => m (IO (Maybe Char))
setupTestStdin = liftIO do
  ref <- newIORef "Hello\n"
  let stdin :: IO (Maybe Char)
      stdin = do
        str <- readIORef ref
        case str of
          [] -> pure Nothing
          (c : cs) -> do
            writeIORef ref cs
            pure $ Just c
  pure stdin

setupTestStdout :: (MonadIO m) => m (Char -> IO (), IORef String)
setupTestStdout = do
  builder <- newIORef ""
  let stdout c = modifyIORef builder (<> [c])
  pure (stdout, builder)

setupTestStderr :: (MonadIO m) => m (Char -> IO (), IORef String)
setupTestStderr = do
  builder <- newIORef ""
  let stderr c = modifyIORef builder (<> [c])
  pure (stderr, builder)
