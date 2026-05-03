module Malgo.TestUtils
  ( testcaseDir,
    builtinPath,
    setupBuiltin,
    preludePath,
    setupPrelude,
    flag,
    golden,
    representatives,
    validateRepresentatives,
    failIfError,
    setupTestStdin,
    setupTestStdout,
    setupTestStderr,
    saveCore,
    setupEvalBuiltin,
    setupEvalPrelude,
    compileTestCase,
    compileTestCaseWithElaborate,
    assertConsistentResults,
    withFreshQueryDB,
  )
where

import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Data.Text.Lazy qualified as TL
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local (State)
import GHC.Stack (CallStack, prettyCallStack)
import Malgo.Driver qualified as Driver
import Malgo.Elaborate (ElaboratePass (..))
import Malgo.Features (Feature (..), FeatureFlags (..), Features)
import Malgo.Module
import Malgo.Monad
import Malgo.Parser (ParserPass (..))
import Malgo.Pass (CompileError, runCompileError, runPass)
import Malgo.Prelude
import Malgo.Query (QueryDB)
import Malgo.Query.Database (newDatabase)
import Malgo.Query.Engine (runQueryDB)
import Malgo.Rename
import Malgo.Sequent.Core.Flat (flatProgram)
import Malgo.Sequent.Core.Join (Program (..), joinProgram)
import Malgo.Sequent.ToCore (toCore)
import Malgo.Sequent.ToFun (ToFunPass (..))
import Malgo.Syntax (Module (..))
import System.FilePath (takeBaseName, (</>))
import Test.Hspec (Spec, expectationFailure, it, shouldBe)
import Test.Hspec.Core.Spec (getSpecDescriptionPath)
import Test.Hspec.Golden (defaultGolden)

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

-- | Test cases that retain full golden output across all passes.
-- Other test cases use lightweight "compiles" tests instead.
--
-- Selection criteria: one case per independent language feature axis.
-- When adding a new test case that exercises a construct not already
-- covered, add it here. Current axes:
--   primitive ops, recursion/HOF, import, record, row polymorphism,
--   codata, copattern, label/goto, nested pattern, C-style syntax,
--   zero-arity edge, complex control flow, tuple pattern,
--   nested/mutual recursive types
--
-- After updating: mise run reset && mise run test
representatives :: [String]
representatives =
  [ "Primitive",
    "ListOps",
    "HelloImport",
    "RecordTest",
    "RowPoly",
    "CodataE2E",
    "FibCopattern",
    "LabelGoto",
    "NestedMatch",
    "CStyleApply",
    "ZeroArgs",
    "Eventually",
    "TuplePattern",
    "NestedRecursive"
  ]

-- | Verify that all entries in 'representatives' correspond to actual
-- test case files. Call from 'runIO' in each spec to catch typos early.
validateRepresentatives :: [FilePath] -> IO ()
validateRepresentatives testcases = do
  let names = map takeBaseName testcases
  for_ representatives \r ->
    unless (r `elem` names)
      $ error
      $ "Representative test case not found: "
      <> r
      <> "\nAvailable: "
      <> show names

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
setupEvalBuiltin = setupEvalModule builtinPath

setupEvalPrelude :: IO ArtifactPath
setupEvalPrelude = setupEvalModule preludePath

setupEvalModule :: FilePath -> IO ArtifactPath
setupEvalModule srcPath = do
  src <- BS.readFile srcPath
  db <- newDatabase
  runMalgoM flag $ runCompileError $ runQueryDB db do
    srcModulePath <- parseArtifactPathFromPwd srcPath
    save srcModulePath ".mlg" src
    parsed <- runPass ParserPass (srcPath, convertString src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    program <- runReader renamed.moduleName $ toCore fun >>= flatProgram >>= joinProgram
    saveCore renamed.moduleName program
    getModulePath renamed.moduleName

compileTestCase :: ArtifactPath -> ArtifactPath -> FilePath -> IO (ModuleName, Program)
compileTestCase builtinName preludeName srcPath = do
  src <- BS.readFile srcPath
  db <- newDatabase
  runMalgoM flag $ runCompileError $ runQueryDB db do
    srcModulePath <- parseArtifactPathFromPwd srcPath
    save srcModulePath ".mlg" src
    parsed <- runPass ParserPass (srcPath, convertString src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    Program {definitions = program} <- runReader renamed.moduleName $ toCore fun >>= flatProgram >>= joinProgram

    Program {definitions = builtin} <- load builtinName ".sqt"
    Program {definitions = prelude} <- load preludeName ".sqt"

    let linked = Program {definitions = builtin <> prelude <> program, dependencies = []}
    pure (renamed.moduleName, linked)

-- | Like 'compileTestCase' but runs 'ElaboratePass' after 'RenamePass',
-- matching the production path when the Malgo2025 feature is enabled.
compileTestCaseWithElaborate :: ArtifactPath -> ArtifactPath -> FilePath -> IO (ModuleName, Program)
compileTestCaseWithElaborate builtinName preludeName srcPath = do
  src <- BS.readFile srcPath
  db <- newDatabase
  runMalgoMWith flag (FeatureFlags (Set.singleton Malgo2025)) $ runCompileError $ runQueryDB db do
    srcModulePath <- parseArtifactPathFromPwd srcPath
    save srcModulePath ".mlg" src
    parsed <- runPass ParserPass (srcPath, convertString src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    elaborated <- runReader renamed.moduleName $ runPass ElaboratePass renamed.moduleDefinition
    fun <- runReader renamed.moduleName $ runPass ToFunPass elaborated
    Program {definitions = program} <- runReader renamed.moduleName $ toCore fun >>= flatProgram >>= joinProgram

    Program {definitions = builtin} <- load builtinName ".sqt"
    Program {definitions = prelude} <- load preludeName ".sqt"

    let linked = Program {definitions = builtin <> prelude <> program, dependencies = []}
    pure (renamed.moduleName, linked)

-- | Assert that two IO actions produce the same result, or both fail.
assertConsistentResults :: (Show a, Eq a) => IO a -> IO a -> IO ()
assertConsistentResults action1 action2 = do
  result1 <- try @SomeException action1
  result2 <- try @SomeException action2
  case (result1, result2) of
    (Right v1, Right v2) -> v2 `shouldBe` v1
    (Left _, Left _) -> pure ()
    (Left err, Right _) -> expectationFailure $ "First failed but second succeeded: " <> show err
    (Right _, Left err) -> expectationFailure $ "First succeeded but second failed: " <> show err

-- | Wrap an action requiring 'QueryDB' with a fresh database.
-- Convenient for tests that call 'runPass RenamePass' directly.
withFreshQueryDB ::
  ( Reader Flag :> es,
    State Uniq :> es,
    IOE :> es,
    Workspace :> es,
    Features :> es,
    Error CompileError :> es
  ) =>
  Eff (QueryDB : es) a ->
  Eff es a
withFreshQueryDB action = do
  db <- liftIO newDatabase
  runQueryDB db action
