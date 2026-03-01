module Malgo.TestUtils
  ( smallIndentNoColor,
    pShowCompact,
    testcaseDir,
    builtinPath,
    setupBuiltin,
    preludePath,
    setupPrelude,
    flag,
    golden,
    failIfError,
  )
where

import Data.Text.Lazy qualified as TL
import GHC.Stack (CallStack, prettyCallStack)
import Malgo.Driver qualified as Driver
import Malgo.Monad
import Malgo.Prelude
import System.FilePath ((</>))
import Test.Hspec (Spec, it)
import Test.Hspec.Core.Spec (getSpecDescriptionPath)
import Test.Hspec.Golden (defaultGolden)
import Text.Pretty.Simple

smallIndentNoColor :: OutputOptions
smallIndentNoColor =
  defaultOutputOptionsNoColor
    { outputOptionsIndentAmount = 1,
      outputOptionsStringStyle = Literal
      -- outputOptionsCompact is problematic: https://github.com/cdepillabout/pretty-simple/issues/84
      -- outputOptionsCompactParens = True,
      -- outputOptionsCompact = True
    }

pShowCompact :: (ConvertibleStrings TL.Text b, Show a) => a -> b
pShowCompact x = convertString $ pShowOpt smallIndentNoColor x

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
