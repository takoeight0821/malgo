module Malgo.LintSpec (spec) where

import Malgo.Lint (lintFile)
import Malgo.Lint.Diagnostic (prettyDiagnostic)
import Malgo.Monad (runMalgoM)
import Malgo.Prelude
import Malgo.TestUtils (flag, golden)
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, takeBaseName, (</>))
import Test.Hspec

caseDir :: FilePath
caseDir = "./test/Malgo/LintSpec/cases"

spec :: Spec
spec = parallel do
  cases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory caseDir

  for_ cases \tc ->
    golden (takeBaseName tc) do
      diags <- runMalgoM flag $ lintFile (caseDir </> tc)
      pure $ unlines $ map (convertString . render . prettyDiagnostic) diags
