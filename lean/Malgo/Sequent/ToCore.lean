import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.Monad
import Malgo.Sequent.Fun
import Malgo.Sequent.Core.Full
import Malgo.Sequent.SaturateCtor
import Malgo.Sequent.ReuseSpecialize

/-! Port of `src/Malgo/Sequent/ToCore.hs`: the CPS conversion from the Fun
IR into the sequent-calculus Core (Full) IR.

`toCore = specializeProgram ∘ saturateProgram >=> toCoreFrom`: it runs
`saturateProgram` (pure) then `specializeProgram` (effectful) to completion
first — so saturated constructor applications never reach CPS conversion in
curried form, and self-recursive match-then-rebuild functions carry their
`reuseHint` — then CPS-converts each definition, minting a fresh `return`
continuation per definition.

Effect mapping: Haskell `State Uniq` + `Reader ModuleName` (no errors,
`ErrorType = Void`) becomes `MalgoM` with the `ModuleName` threaded
explicitly. -/

namespace Malgo.Sequent.ToCore

open Malgo
open Malgo.Sequent.Fun (Name)
open Malgo.Sequent.SaturateCtor (saturateProgram)
open Malgo.Sequent.ReuseSpecialize (specializeProgram)

namespace F
export Malgo.Sequent.Fun (Expr Branch Program)
end F

namespace C
export Malgo.Sequent.Core.Full (Producer Consumer Statement Branch Program)
end C

/-- Supports `partial def`s returning `MalgoM α` (inhabited via the monad's
error branch). -/
instance : Inhabited CompileError := ⟨{ passName := "", message := "" }⟩

mutual

partial def toStatement (mn : ModuleName) (expr : F.Expr) (consumer : C.Consumer) :
    MalgoM C.Statement := do
  match expr with
  | .«let» range name value body => do
    let body' ← toStatement mn body consumer
    toStatement mn value (.«then» range name body')
  | .apply range f args => do
    let args' ← args.mapM (toProducer mn)
    toStatement mn f (.apply range args' [consumer])
  | .project range e field => toStatement mn e (.project range field consumer)
  | .primitive range op args => do
    let args' ← args.mapM (toProducer mn)
    pure (.primitive range op args' consumer)
  | .select range scrutinee branches => do
    let branches' ← branches.mapM (convertBranch mn consumer)
    toStatement mn scrutinee (.select range branches')
  | .invoke range name => pure (.invoke range name consumer)
  | _ => do
    let p ← toProducer mn expr
    pure (.cut p consumer)

partial def toProducer (mn : ModuleName) : F.Expr → MalgoM C.Producer
  | .var range name => pure (.var range name)
  | .literal range lit => pure (.literal range lit)
  | .construct range tag arguments => do
    let ps ← arguments.mapM (toProducer mn)
    pure (.construct range tag ps [])
  | .lambda range params body => do
    let ret ← newTemporalId mn "return"
    let body' ← toStatement mn body (.label range ret)
    pure (.lambda range (params ++ [ret]) body')
  | .object range fields => do
    let fields' ← fields.mapM (fun (k, e) => do
      let ret ← newTemporalId mn "return"
      let body ← toStatement mn e (.label range ret)
      pure (k, ret, body))
    pure (.object range fields')
  | .fix range name e => do
    let body ← toStatement mn e (.label range name)
    pure (.mu range name body)
  | e@(.«let» range _ _ _) => do
    let ret ← newTemporalId mn "return"
    let s ← toStatement mn e (.label range ret)
    pure (.«do» range ret s)
  | e@(.apply range _ _) => do
    let ret ← newTemporalId mn "return"
    let s ← toStatement mn e (.label range ret)
    pure (.«do» range ret s)
  | e@(.project range _ _) => do
    let ret ← newTemporalId mn "return"
    let s ← toStatement mn e (.label range ret)
    pure (.«do» range ret s)
  | e@(.primitive range _ _) => do
    let ret ← newTemporalId mn "return"
    let s ← toStatement mn e (.label range ret)
    pure (.«do» range ret s)
  | e@(.select range _ _) => do
    let ret ← newTemporalId mn "return"
    let s ← toStatement mn e (.label range ret)
    pure (.«do» range ret s)
  | e@(.invoke range _) => do
    let ret ← newTemporalId mn "return"
    let s ← toStatement mn e (.label range ret)
    pure (.«do» range ret s)

partial def convertBranch (mn : ModuleName) (consumer : C.Consumer) :
    F.Branch → MalgoM C.Branch
  | .branch range pattern body => do
    let body' ← toStatement mn body consumer
    pure (.branch range pattern body')

end

def convertDefinition (mn : ModuleName) :
    Range × Name × F.Expr → MalgoM (Range × Name × Name × C.Statement)
  | (range, name, body) => do
    let ret ← newTemporalId mn "return"
    let body' ← toStatement mn body (.label range ret)
    pure (range, name, ret, body')

/-- The CPS half of `toCore`, taking a program already run through
`saturateProgram` and `specializeProgram`. -/
def toCoreFrom (mn : ModuleName) (program : F.Program) : MalgoM C.Program := do
  let definitions ← program.definitions.mapM (convertDefinition mn)
  pure { definitions, dependencies := program.dependencies }

/-- Full pass: saturate, specialize (to completion), then CPS-convert. -/
def toCore (mn : ModuleName) (program : F.Program) : MalgoM C.Program := do
  let specialized ← specializeProgram mn (saturateProgram program)
  toCoreFrom mn specialized

end Malgo.Sequent.ToCore
