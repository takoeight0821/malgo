module Malgo.RenameSpec (spec) where

import Data.ByteString qualified as BS
import Effectful.Error.Static (catchError)
import Malgo.Monad (runMalgoM)
import Malgo.Parser (ParserPass (..))
import Malgo.Pass
import Malgo.Prelude
import Malgo.Rename
import Malgo.SExpr (sShow)
import Malgo.TestUtils
import System.Directory
import System.FilePath
import Test.Hspec

errorcaseDir :: FilePath
errorcaseDir = "test/Malgo/RenameSpec/errors"

spec :: Spec
spec = parallel do
  runIO do
    setupBuiltin
    setupPrelude
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  golden "Builtin" (driveRename builtinPath)
  golden "Builtin sexpr" (driveRenameSExpr builtinPath)
  golden "Prelude" (driveRename preludePath)
  golden "Prelude sexpr" (driveRenameSExpr preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveRename (testcaseDir </> testcase))
    golden (takeBaseName testcase <> " sexpr") (driveRenameSExpr (testcaseDir </> testcase))
  errorcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory errorcaseDir
  for_ errorcases \errorcase -> do
    golden ("error " <> takeBaseName errorcase) (driveErrorRename (errorcaseDir </> errorcase))

driveRename :: FilePath -> IO String
driveRename srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    pure $ pShowCompact renamed

driveErrorRename :: FilePath -> IO String
driveErrorRename srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    fmap show (runPass RenamePass (parsed, rnEnv))
      `catchError` \_ CompileError {compileError} -> pure $ show compileError

driveRenameSExpr :: FilePath -> IO String
driveRenameSExpr srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    pure $ sShow renamed
