module Malgo.ParserSpec (spec) where

import Data.ByteString.Lazy qualified as BL
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parse)
import Malgo.Prelude
import Malgo.SExpr (sShow)
import Malgo.TestUtils
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, takeBaseName, (</>))
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)

errorcaseDir :: FilePath
errorcaseDir = "test/Malgo/ParserSpec/errors"

representatives :: [String]
representatives =
  [ "Primitive",
    "List",
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
    "TuplePattern"
  ]

spec :: Spec
spec = parallel do
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir

  describe "golden" do
    golden "Builtin" (driveParse builtinPath)
    golden "Prelude" (driveParse preludePath)
    for_ testcases \testcase ->
      when (takeBaseName testcase `elem` representatives)
        $ golden (takeBaseName testcase) (driveParse (testcaseDir </> testcase))

  describe "compiles" do
    for_ testcases \testcase ->
      when (takeBaseName testcase `notElem` representatives)
        $ it (takeBaseName testcase)
        $ void
        $ driveParse (testcaseDir </> testcase)

  describe "errors" do
    errorcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory errorcaseDir
    for_ errorcases \errorcase -> do
      golden ("error " <> takeBaseName errorcase) (driveErrorParse (errorcaseDir </> errorcase))

  describe "unified parser entrypoint" do
    describe "accepts regular and C-style syntax" do
      it "parses regular function application with spaces" do
        expectParsed "def main = f x y"

      it "parses C-style function calls with parentheses" do
        expectParsed "def main = f(x, y)"

      it "parses empty C-style function calls" do
        expectParsed "def main = f()"

      it "parses nested C-style calls" do
        expectParsed "def main = f(g(x), h(y, z))"

      it "parses regular tuple syntax with parentheses" do
        expectParsed "def main = (x, y, z)"

      it "parses C-style tuple syntax with braces" do
        expectParsed "def main = {x, y}"

      it "parses regular-style function clauses without parentheses" do
        expectParsed "def f = { x y -> x }"

      it "parses C-style function clauses with parentheses" do
        expectParsed "def f = { (x, y) -> x }"

      it "parses regular-style constructor patterns with tuple arguments" do
        expectParsed "def cond = { (Cons (True, x) _) -> x }"

      it "parses regular-style data and type definitions" do
        expectParsed
          $ unlines
            [ "data List a = Cons a (List a) | Nil",
              "type Id a = a",
              "def main = { (_) -> 42 }"
            ]

      it "parses Malgo 2025 syntax" do
        expectParsed
          $ unlines
            [ "def absurd : _|_ -> a",
              "def absurd = { x -> absurd x }",
              "def idTilde : ~Int64# -> ~Int64#",
              "def idTilde = { x -> x }",
              "def getX : { x: Int64# | r } -> Int64#",
              "def getX = { rec -> rec.x }",
              "def main = { (_) -> label k goto(42i64#, k) }"
            ]

    describe "handles parser pragmas as compatibility no-ops" do
      it "accepts #c-style-apply pragma with C-style syntax" do
        expectParsed "#c-style-apply\ndef main = f(x, y)"

      it "accepts #c-style-apply pragma with regular syntax" do
        expectParsed "#c-style-apply\ndef main = f x y"

      it "accepts #malgo-2025 pragma with C-style syntax" do
        expectParsed "#malgo-2025\ndef main = f(x, y)"

      it "accepts unknown pragmas alongside known pragmas" do
        expectParsed "#experimental-feature\n#c-style-apply\ndef main = f(x, y)"

      it "ignores unknown pragmas while preserving parse behavior" do
        expectParsed "#c-style-apply\n#debug\ndef main = f(x, y)"

driveParse :: FilePath -> IO String
driveParse srcPath = do
  src <- convertString <$> BL.readFile srcPath
  runMalgoM flag do
    parsed <- parse srcPath src
    case parsed of
      Left err -> error $ errorBundlePretty err
      Right parsed ->
        pure $ sShow parsed

driveErrorParse :: FilePath -> IO String
driveErrorParse srcPath = do
  src <- convertString <$> BL.readFile srcPath
  runMalgoM flag do
    parsed <- parse srcPath src
    case parsed of
      Left err -> pure $ errorBundlePretty err
      Right _ -> error "Expected error, but successfully parsed"

expectParsed :: String -> Expectation
expectParsed src = do
  result <- runMalgoM flag do
    parse "test.mlg" (convertString src)
  case result of
    Left err -> expectationFailure $ errorBundlePretty err
    Right _ -> pure ()
