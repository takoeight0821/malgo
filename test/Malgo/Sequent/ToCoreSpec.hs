module Malgo.Sequent.ToCoreSpec (spec) where

import Data.ByteString qualified as BS
import Effectful.Reader.Static (runReader)
import Malgo.Monad (runMalgoM)
import Malgo.Parser (ParserPass (..))
import Malgo.Pass
import Malgo.Prelude
import Malgo.Rename
import Malgo.SExpr (sShow)
import Malgo.Sequent.Core.Fingerprint (fingerprintFlat, fingerprintJoin)
import Malgo.Sequent.Core.Flat qualified as Flat
import Malgo.Sequent.Core.FlatCheck (assertFlat)
import Malgo.Sequent.Core.Full qualified as Full
import Malgo.Sequent.Core.Join qualified as Join
import Malgo.Sequent.Core.JoinCheck (assertJoin)
import Malgo.Sequent.ToCore (toCore)
import Malgo.Sequent.ToFun (ToFunPass (..))
import Malgo.Syntax (Module (..))
import Malgo.TestUtils
import System.Directory
import System.FilePath
import Test.Hspec

data AllIR = AllIR
  { core :: Full.Program,
    flat :: Flat.Program,
    join :: Join.Program
  }

driveAll :: FilePath -> IO AllIR
driveAll srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    coreProgram <- runReader renamed.moduleName $ toCore fun
    flatProgram <- runReader renamed.moduleName $ Flat.flatProgram coreProgram
    joinProgram <- runReader renamed.moduleName $ Join.joinProgram flatProgram
    pure AllIR {core = coreProgram, flat = flatProgram, join = joinProgram}

spec :: Spec
spec = parallel do
  runIO do
    setupBuiltin
    setupPrelude
  testcases <- runIO do
    files <- listDirectory testcaseDir
    pure $ filter (isExtensionOf "mlg") files

  describe "golden" do
    golden "Builtin" (sShow . (.core) <$> driveAll builtinPath)
    golden "Builtin flat" (sShow . (.flat) <$> driveAll builtinPath)
    golden "Builtin join" (sShow . (.join) <$> driveAll builtinPath)
    golden "Prelude" (sShow . (.core) <$> driveAll preludePath)
    golden "Prelude flat" (sShow . (.flat) <$> driveAll preludePath)
    golden "Prelude join" (sShow . (.join) <$> driveAll preludePath)
    for_ testcases \testcase -> do
      ref <- runIO $ newIORef Nothing
      let getAll = do
            cached <- readIORef ref
            case cached of
              Just ir -> pure ir
              Nothing -> do
                ir <- driveAll (testcaseDir </> testcase)
                writeIORef ref (Just ir)
                pure ir
      golden (takeBaseName testcase <> " join") (sShow . (.join) <$> getAll)

  for_ testcases \testcase -> do
    ref <- runIO $ newIORef Nothing
    let getAll = do
          cached <- readIORef ref
          case cached of
            Just ir -> pure ir
            Nothing -> do
              ir <- driveAll (testcaseDir </> testcase)
              writeIORef ref (Just ir)
              pure ir

    describe "flat-invariants" do
      it (takeBaseName testcase) $ getAll >>= assertFlat . (.flat)

    describe "join-invariants" do
      it (takeBaseName testcase) $ getAll >>= assertJoin . (.join)

    describe "flat-fingerprint" do
      golden (takeBaseName testcase) $ fingerprintFlat . (.flat) <$> getAll

    describe "join-fingerprint" do
      golden (takeBaseName testcase) $ fingerprintJoin . (.join) <$> getAll
