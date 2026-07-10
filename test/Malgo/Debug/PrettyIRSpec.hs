module Malgo.Debug.PrettyIRSpec (spec) where

import Data.Text qualified as T
import Malgo.Debug.DiffView (renderUnifiedDiff)
import Malgo.Debug.Pipeline (Stage (..), runTrace)
import Malgo.Prelude
import Malgo.TestUtils
import System.Directory
import System.FilePath
import Test.Hspec

-- | Every '.mlg' file under 'testcaseDir' and 'examplesDir' gets one golden
-- covering the whole trace: every pipeline stage's rendered text, plus the
-- unified diff between each adjacent pair. One golden per source file (not
-- per file-per-stage) keeps the corpus-wide sweep from repeating the
-- golden-output blowup already fixed once for the per-pass Spec suites.
spec :: Spec
spec = parallel do
  testcases <- runIO do
    files <- listDirectory testcaseDir
    pure $ filter (isExtensionOf "mlg") files
  runIO $ validateRepresentatives testcases
  examples <- runIO do
    files <- listDirectory examplesDir
    pure $ filter (isExtensionOf "mlg") files

  for_ testcases \f ->
    golden ("testcases " <> takeBaseName f) (traceReport (testcaseDir </> f))
  for_ examples \f ->
    golden ("examples " <> takeBaseName f) (traceReport (examplesDir </> f))

examplesDir :: FilePath
examplesDir = "./examples/malgo"

traceReport :: FilePath -> IO String
traceReport srcPath = do
  stages <- runTrace srcPath False False
  let stageSection = T.intercalate "\n\n" [heading s.name <> "\n" <> s.rendered | s <- stages]
      diffSection =
        T.intercalate
          "\n\n"
          [ heading (a.name <> " -> " <> b.name) <> "\n" <> renderUnifiedDiff a.rendered b.rendered
          | (a, b) <- zip stages (drop 1 stages)
          ]
  pure $ convertString $ stageSection <> "\n\n" <> heading "DIFFS" <> "\n\n" <> diffSection
  where
    heading label = "=== " <> label <> " ==="
