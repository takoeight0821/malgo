module Main (main) where

import Malgo.Backend.SchemeSpec qualified as SchemeSpec
import Malgo.ElaborateSpec qualified as ElaborateSpec
import Malgo.FeaturesSpec qualified as FeaturesSpec
import Malgo.ForthSpec qualified as ForthSpec
import Malgo.InferSpec qualified as InferSpec
import Malgo.ParserSpec qualified as ParserSpec
import Malgo.Prelude (IO)
import Malgo.Query.EngineSpec qualified as QueryEngineSpec
import Malgo.RenameSpec qualified as RenameSpec
import Malgo.Sequent.BigStepEvalSpec qualified as BigStepEvalSpec
import Malgo.Sequent.EvalSpec qualified as EvalSpec
import Malgo.Sequent.ToCoreSpec qualified as ToCoreSpec
import Malgo.Sequent.ToFunSpec qualified as ToFunSpec
import Malgo.TestUtils (setupEvalBuiltin, setupEvalPrelude)
import Test.Hspec (describe, hspec)

main :: IO ()
main = do
  builtin <- setupEvalBuiltin
  prelude <- setupEvalPrelude
  hspec do
    describe "Malgo.Parser" ParserSpec.spec
    describe "Malgo.Rename" RenameSpec.spec
    describe "Malgo.Infer" InferSpec.spec
    describe "Malgo.Elaborate" ElaborateSpec.spec
    describe "Malgo.Features" FeaturesSpec.spec
    describe "Malgo.Query.Engine" QueryEngineSpec.spec
    describe "Malgo.Backend.Scheme" SchemeSpec.spec
    describe "Malgo.Sequent.ToFun" ToFunSpec.spec
    describe "Malgo.Sequent.ToCore" ToCoreSpec.spec
    describe "Malgo.Sequent.Eval" (EvalSpec.specWith builtin prelude)
    describe "Malgo.Sequent.BigStepEval" (BigStepEvalSpec.specWith builtin prelude)
    describe "Malgo.Forth" (ForthSpec.specWith builtin prelude)
