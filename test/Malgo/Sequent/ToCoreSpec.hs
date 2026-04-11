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
import Malgo.Sequent.Core.Join qualified as Join
import Malgo.Sequent.Core.JoinCheck (assertJoin)
import Malgo.Sequent.ToCore (toCore)
import Malgo.Sequent.ToFun (ToFunPass (..))
import Malgo.Syntax (Module (..))
import Malgo.TestUtils
import System.Directory
import System.FilePath
import Test.Hspec

spec :: Spec
spec = parallel do
  runIO do
    setupBuiltin
    setupPrelude
  testcases <- runIO do
    files <- listDirectory testcaseDir
    pure $ filter (isExtensionOf "mlg") files

  describe "golden" do
    golden "Builtin" (driveToCore builtinPath)
    golden "Builtin flat" (driveFlat builtinPath)
    golden "Builtin join" (driveJoin builtinPath)
    golden "Prelude" (driveToCore preludePath)
    golden "Prelude flat" (driveFlat preludePath)
    golden "Prelude join" (driveJoin preludePath)
    for_ testcases \testcase -> do
      golden (takeBaseName testcase) (driveToCore (testcaseDir </> testcase))
      golden (takeBaseName testcase <> " flat") (driveFlat (testcaseDir </> testcase))
      golden (takeBaseName testcase <> " join") (driveJoin (testcaseDir </> testcase))

  describe "flat-invariants" do
    for_ testcases \testcase ->
      it (takeBaseName testcase)
        $ driveFlatValidate (testcaseDir </> testcase)

  describe "join-invariants" do
    for_ testcases \testcase ->
      it (takeBaseName testcase)
        $ driveJoinValidate (testcaseDir </> testcase)

  describe "flat-fingerprint" do
    for_ testcases \testcase ->
      golden (takeBaseName testcase)
        $ driveFlatFingerprint (testcaseDir </> testcase)

  describe "join-fingerprint" do
    for_ testcases \testcase ->
      golden (takeBaseName testcase)
        $ driveJoinFingerprint (testcaseDir </> testcase)

driveToCore :: FilePath -> IO String
driveToCore srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    program <- runReader renamed.moduleName $ toCore fun
    pure $ sShow program

driveFlat :: FilePath -> IO String
driveFlat srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    program <- runReader renamed.moduleName $ toCore fun >>= Flat.flatProgram
    pure $ sShow program

driveJoin :: FilePath -> IO String
driveJoin srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    program <- runReader renamed.moduleName $ toCore fun >>= Flat.flatProgram >>= Join.joinProgram
    pure $ sShow program

driveFlatValidate :: FilePath -> IO ()
driveFlatValidate srcPath = do
  src <- convertString <$> BS.readFile srcPath
  program <- runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    runReader renamed.moduleName $ toCore fun >>= Flat.flatProgram
  assertFlat program

driveJoinValidate :: FilePath -> IO ()
driveJoinValidate srcPath = do
  src <- convertString <$> BS.readFile srcPath
  program <- runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    runReader renamed.moduleName $ toCore fun >>= Flat.flatProgram >>= Join.joinProgram
  assertJoin program

driveFlatFingerprint :: FilePath -> IO String
driveFlatFingerprint srcPath = do
  src <- convertString <$> BS.readFile srcPath
  program <- runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    runReader renamed.moduleName $ toCore fun >>= Flat.flatProgram
  pure $ fingerprintFlat program

driveJoinFingerprint :: FilePath -> IO String
driveJoinFingerprint srcPath = do
  src <- convertString <$> BS.readFile srcPath
  program <- runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    runReader renamed.moduleName $ toCore fun >>= Flat.flatProgram >>= Join.joinProgram
  pure $ fingerprintJoin program
