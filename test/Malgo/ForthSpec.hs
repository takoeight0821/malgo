module Malgo.ForthSpec (specWith) where

import Malgo.Module (ArtifactPath)
import Malgo.Prelude
import Malgo.TestUtils
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, takeBaseName, (</>))
import Test.Hspec

forthTestcaseDir :: FilePath
forthTestcaseDir = "./test/testcases/forth"

specWith :: ArtifactPath -> ArtifactPath -> Spec
specWith builtin prelude = parallel do
  (moduleName, program) <-
    runIO
      $ compileTestCase builtin prelude "examples/malgo/Forth.mlg"

  testcases <- runIO $ do
    files <- listDirectory forthTestcaseDir
    pure $ filter (isExtensionOf "fs") files

  for_ testcases \tc ->
    golden (takeBaseName tc) do
      forthInput <- readFile (forthTestcaseDir </> tc)
      runEvalWithStdin forthInput moduleName program
