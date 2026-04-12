module Malgo.ElaborateSpec (spec) where

import Data.ByteString qualified as BS
import Effectful.Reader.Static (runReader)
import Malgo.Elaborate (ElaboratePass (..))
import Malgo.Monad (runMalgoM)
import Malgo.Parser (ParserPass (..))
import Malgo.Pass
import Malgo.Prelude
import Malgo.Rename
import Malgo.SExpr (sShow)
import Malgo.Syntax (Module (..))
import Malgo.Syntax.Extension (Malgo, MalgoPhase (..))
import Malgo.TestUtils
import System.Directory
import System.FilePath
import Test.Hspec

spec :: Spec
spec = parallel do
  runIO do
    setupBuiltin
    setupPrelude
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir

  for_ testcases \testcase ->
    if takeBaseName testcase `elem` representatives
      then golden (takeBaseName testcase) (driveElaborate (testcaseDir </> testcase))
      else it (takeBaseName testcase) $ void $ driveElaborate (testcaseDir </> testcase)

driveElaborate :: FilePath -> IO String
driveElaborate srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (Module modName def, _) <- runPass RenamePass (parsed, rnEnv)
    elaborated <- runReader modName $ runPass ElaboratePass def
    pure $ sShow elaborated
