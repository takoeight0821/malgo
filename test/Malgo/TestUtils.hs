module Malgo.TestUtils
  ( smallIndentNoColor,
    pShowCompact,
    testcaseDir,
    builtinPath,
    setupBuiltin,
    preludePath,
    setupPrelude,
    flag,
    golden,
    failIfError,
    setupTestStdin,
    setupTestStdout,
    setupTestStderr,
    saveCore,
    setupEvalBuiltin,
    setupEvalPrelude,
    compileTestCase,
  )
where

import Data.ByteString qualified as BS
import Data.Text.Lazy qualified as TL
import Effectful
import Effectful.Reader.Static (runReader)
import GHC.Stack (CallStack, prettyCallStack)
import Malgo.Driver qualified as Driver
import Malgo.Module
import Malgo.Monad
import Malgo.Parser (ParserPass (..), parse)
import Malgo.Pass (runCompileError, runPass)
import Malgo.Prelude
import Malgo.Rename
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.Sequent.Core.Flat (flatProgram)
import Malgo.Sequent.Core.Join (Program (..), joinProgram)
import Malgo.Sequent.ToCore (toCore)
import Malgo.Sequent.ToFun (ToFunPass (..))
import Malgo.Syntax (Module (..))
import System.FilePath ((</>))
import Test.Hspec (Spec, it)
import Test.Hspec.Core.Spec (getSpecDescriptionPath)
import Test.Hspec.Golden (defaultGolden)
import Text.Pretty.Simple

smallIndentNoColor :: OutputOptions
smallIndentNoColor =
  defaultOutputOptionsNoColor
    { outputOptionsIndentAmount = 1,
      outputOptionsStringStyle = Literal
      -- outputOptionsCompact is problematic: https://github.com/cdepillabout/pretty-simple/issues/84
      -- outputOptionsCompactParens = True,
      -- outputOptionsCompact = True
    }

pShowCompact :: (ConvertibleStrings TL.Text b, Show a) => a -> b
pShowCompact x = convertString $ pShowOpt smallIndentNoColor x

testcaseDir :: FilePath
testcaseDir = "./test/testcases/malgo"

builtinPath :: FilePath
builtinPath = "./runtime/malgo/Builtin.mlg"

setupBuiltin :: IO ()
setupBuiltin =
  runMalgoM flag do
    Driver.compile builtinPath

preludePath :: FilePath
preludePath = "./runtime/malgo/Prelude.mlg"

setupPrelude :: IO ()
setupPrelude =
  runMalgoM flag do
    Driver.compile preludePath

flag :: Flag
flag = Flag {noOptimize = False, lambdaLift = False, debugMode = False, testMode = True, target = TargetEval, evalMode = EvalSmallStep, useInfer = False}

golden ::
  -- | Test description
  String ->
  -- | Content (@return content@ for pure functions)
  IO String ->
  Spec
golden description runAction = do
  path <- (<> words description) <$> getSpecDescriptionPath
  it description
    $ defaultGolden (foldr1 (</>) path)
    <$> runAction

failIfError :: (Show e) => Either (CallStack, e) a -> a
failIfError = \case
  Left (callStack, err) -> error $ "Error: " <> show err <> "\nCall stack:\n" <> prettyCallStack callStack
  Right x -> x

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

saveCore :: (Workspace :> es, IOE :> es) => ModuleName -> Program -> Eff es ()
saveCore moduleName program = do
  modulePath <- getModulePath moduleName
  save modulePath ".sqt" program

setupEvalBuiltin :: IO ArtifactPath
setupEvalBuiltin = do
  src <- convertString <$> BS.readFile builtinPath
  runMalgoM flag $ runCompileError do
    parsed <- runPass ParserPass (builtinPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    program <- runReader renamed.moduleName $ toCore fun >>= flatProgram >>= joinProgram
    saveCore renamed.moduleName program
    getModulePath renamed.moduleName

setupEvalPrelude :: IO ArtifactPath
setupEvalPrelude = do
  src <- convertString <$> BS.readFile preludePath
  runMalgoM flag $ runCompileError do
    parsed <- runPass ParserPass (preludePath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    program <- runReader renamed.moduleName $ toCore fun >>= flatProgram >>= joinProgram
    saveCore renamed.moduleName program
    getModulePath renamed.moduleName

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
