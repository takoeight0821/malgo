module Malgo.Sequent.ToFunSpec (spec) where

import Data.ByteString qualified as BS
import Effectful.Reader.Static (runReader)
import Malgo.Monad (runMalgoM)
import Malgo.Parser (ParserPass (..))
import Malgo.Pass
import Malgo.Prelude
import Malgo.Rename
import Malgo.SExpr (sShow)
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
    golden "Builtin" (driveToFun builtinPath)
    golden "Prelude" (driveToFun preludePath)
    for_ testcases \testcase ->
      when (takeBaseName testcase `elem` representatives)
        $ golden (takeBaseName testcase) (driveToFun (testcaseDir </> testcase))

  describe "compiles" do
    for_ testcases \testcase ->
      when (takeBaseName testcase `notElem` representatives)
        $ it (takeBaseName testcase)
        $ void
        $ driveToFun (testcaseDir </> testcase)

driveToFun :: FilePath -> IO String
driveToFun srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    program <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    pure $ sShow program
