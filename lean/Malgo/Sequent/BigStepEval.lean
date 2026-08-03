import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.Monad
import Malgo.Sequent.Fun
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Eval

/-! Port of `src/Malgo/Sequent/BigStepEval.hs`: a big-step evaluator for the
Join IR. Where `Malgo.Sequent.Eval` is small-step CPS (every consumer
callback returns `Unit`, control threaded through continuations), this one
descends recursively and *returns* the value that reaches the innermost
`Finish`. Haskell bridges "consumer returns `()`" to "statement returns
`Value`" with an `IORef` that each consumer closure writes and each
`applyConsumer` reads back.

That `IORef` is defunctionalized exactly as in `Eval.lean`: it becomes an
integer store slot (`EvalCtx.slots`). The single top-level ref and every
`Mu`-local ref become fresh slots. The consumer closures Haskell builds are
just two shapes — `mkConsumerValue ref env consumer` (Join labels) and the
`Mu` producer's `\v -> writeIORef ref v` — which map to the shared
`ConsumerK.bigStep`/`ConsumerK.writeSlot` constructors.

Everything else (`Value`, `Env`, `Handlers`, `EvalCtx`, `EvalM`,
`fetchPrimitive`, `lookupEnv`, `extendEnv`, the store, and `matchPat`) is
reused verbatim from `Eval.lean`, mirroring the Haskell module's imports
from `Malgo.Sequent.Eval`. The `slot` argument threaded through the
mutual block is the current result ref: a consumer closure writes its *own*
captured slot, while `applyConsumer`/`applyConsumerDirect` read the current
slot after invoking it — the exact `writeIORef ref result` / `readIORef ref`
asymmetry of the Haskell. -/

namespace Malgo.Sequent.BigStepEval

open Malgo
open Malgo.Sequent.Eval
open Malgo.Sequent.Fun (Name Tag)

abbrev JProducer := Malgo.Sequent.Core.Join.Producer
abbrev JConsumer := Malgo.Sequent.Core.Join.Consumer
abbrev JStatement := Malgo.Sequent.Core.Join.Statement
abbrev JBranch := Malgo.Sequent.Core.Join.Branch

/-- Read the current result slot back after a consumer has written to it.
The slot is always seeded (the top-level ref and every `Mu` slot start at
`Struct Tuple []`), so the default is unreachable — it mirrors Haskell's
`readIORef ref` on a ref initialised to `Struct Tuple []`. -/
private def readSlot (slot : Nat) : EvalM Value :=
  return (← storeGet? slot).getD (.struct .tuple [])

mutual

/-- Big-step evaluation of a statement; returns the value reaching the
innermost `Finish`. `slot` is the current result ref. -/
partial def evalStatement (slot : Nat) (env : Env) : JStatement → EvalM Value
  | .cut (.mu _ name stmt) consumer => do
    let consumerValue ← lookupEnv env (jstmtRange stmt) consumer
    evalStatement slot (extendEnv name consumerValue env) stmt
  | .cut producer consumer => do
    let value ← evalProducer env producer
    applyConsumer slot env (jprodRange producer) consumer value
  | .join _ label consumer statement => do
    let covalue := Value.consumer (.bigStep slot env consumer)
    evalStatement slot (extendEnv label covalue env) statement
  | .primitive range name producers consumer => do
    let vals ← producers.mapM (evalProducer env)
    let result ← fetchPrimitive range name vals
    applyConsumer slot env range consumer result
  | .invoke range name consumer => do
    let (ret, statement) ← lookupToplevel range name
    let covalue ← lookupEnv env range consumer
    evalStatement slot (extendEnv ret covalue env) statement
  | .externalCall range name producers consumer => do
    let vals ← producers.mapM (evalProducer env)
    let result ← fetchPrimitive range name vals
    applyConsumer slot env range consumer result
  | .binOp range op lhs rhs consumer => do
    let l ← evalProducer env lhs
    let r ← evalProducer env rhs
    let result ← fetchPrimitive range op [l, r]
    applyConsumer slot env range consumer result
  | .ifz _ cond thenB elseB => do
    let c ← evalProducer env cond
    if isZeroValue c then evalStatement slot env thenB else evalStatement slot env elseB

/-- Evaluate a producer to a value. Only `Mu` touches a result slot: it
allocates a fresh one, seeds it, binds the `Mu` name to a `writeSlot`
consumer, and returns whatever the body evaluates to. -/
partial def evalProducer (env : Env) : JProducer → EvalM Value
  | .var range name => lookupEnv env range name
  | .literal _ literal => pure (.immediate literal)
  | .construct range tag producers consumers => do
    let ps ← producers.mapM (evalProducer env)
    let cs ← consumers.mapM (lookupEnv env range)
    pure (.struct tag (ps ++ cs))
  | .lambda _ parameters statement => pure (.function env parameters statement)
  | .object _ fields => pure (.record env fields)
  | .mu _ name stmt => do
    let slot ← freshSlot
    storeSet slot (.struct .tuple [])
    evalStatement slot (extendEnv name (.consumer (.writeSlot slot)) env) stmt

/-- Apply a named consumer to a value: invoke its closure (which writes to
its own captured slot), then read the current slot back. -/
partial def applyConsumer (slot : Nat) (env : Env) (range : Range) (name : Name)
    (value : Value) : EvalM Value := do
  match ← lookupEnv env range name with
  | .consumer k => do applyConsumerRepr k value; readSlot slot
  | _ => throw (.expectConsumer range value)

/-- Run a defunctionalized consumer closure for its effect (Haskell's
`repr value`, returning `()`). `bigStep` mirrors `mkConsumerValue`: it runs
`applyConsumerDirect` over the *captured* env/slot, then writes the result
to that captured slot. `writeSlot`/`writeSlotOpt` write the value directly.
`run` cannot occur here (BigStepEval never builds a small-step consumer). -/
partial def applyConsumerRepr : ConsumerK → Value → EvalM Unit
  | .bigStep capturedSlot env consumer, value => do
    let result ← applyConsumerDirect capturedSlot env consumer value
    storeSet capturedSlot result
  | .writeSlot capturedSlot, value => storeSet capturedSlot value
  | .writeSlotOpt capturedSlot, value => storeSet capturedSlot value
  | .run env k, value => Malgo.Sequent.Eval.evalConsumer env k value

/-- Directly apply a `Consumer` AST node to a value, returning the result
without going through the named-lookup + slot-read bridge. `slot` is the
captured result ref this consumer runs against. -/
partial def applyConsumerDirect (slot : Nat) (env : Env) : JConsumer → Value → EvalM Value
  | .label range label, given => do
    match ← lookupEnv env range label with
    | .consumer k => do applyConsumerRepr k given; readSlot slot
    | covalue => throw (.expectConsumer range covalue)
  | .apply range producers consumers, given => do
    let values ← producers.mapM (evalProducer env)
    let covalues ← consumers.mapM (lookupEnv env range)
    match given with
    | .function fenv parameters statement =>
      evalStatement slot (extendEnv' (parameters.zip (values ++ covalues)) fenv) statement
    | _ => throw (.expectFunction range given)
  | .project range field consumer, given => do
    let covalue ← lookupEnv env range consumer
    match given with
    | .record renv fields =>
      match lookupField field fields with
      | some (name, statement) => evalStatement slot (extendEnv name covalue renv) statement
      | none => throw (.noSuchField range field given)
    | _ => throw (.expectRecord range given)
  | .«then» _ name statement, given =>
    evalStatement slot (extendEnv name given env) statement
  | .finish _, given => pure given
  | .select range branches, given => selectGo slot env range given branches

partial def selectGo (slot : Nat) (env : Env) (range : Range) (given : Value) :
    List JBranch → EvalM Value
  | [] => throw (.noMatch range given)
  | .branch _ pattern statement :: rest => do
    match ← Malgo.Sequent.Eval.matchPat env pattern given with
    | some bindings => evalStatement slot (extendEnv' bindings env) statement
    | none => selectGo slot env range given rest

end

/-! ## Program entry -/

/-- Port of `bigStepEvalProgram`/`BigStepEvalPass.runPassImpl`. Identical to
`Eval.evalProgram` except the statement is driven by the big-step
`evalStatement`: it locates `main`, mints the single `finish` continuation
(the only eval-time uniq — same as small-step, so uniq parity holds), builds
the same synthetic entry statement, allocates and seeds the top-level result
slot (Haskell's `newIORef (Struct Tuple [])`), and runs. -/
def bigStepEvalProgram (moduleName : ModuleName) (handlers : Handlers)
    (program : Malgo.Sequent.Core.Join.Program) : MalgoM Unit := do
  let toplevels : Toplevels :=
    program.definitions.foldl (fun m d => m.insert d.name (d.ret, d.body)) {}
  match toplevels.toList.find? (fun (k, _) => k.name == "main") with
  | none => pure ()
  | some (_, (ret, statement)) => do
    let finish ← newTemporalId moduleName "finish"
    let slots ← IO.mkRef (∅ : Std.HashMap Nat Value)
    let nextSlot ← IO.mkRef 0
    let ctx : EvalCtx := { slots, nextSlot, handlers, toplevels }
    let r := jstmtRange statement
    let entry : JStatement :=
      .join r finish (.finish r)
        (.join r ret (.apply r [.construct r .tuple [] []] [finish]) statement)
    let run : EvalM Value := do
      let slot0 ← freshSlot
      storeSet slot0 (.struct .tuple [])
      evalStatement slot0 emptyEnv entry
    let result ← MalgoM.io ((run.run ctx).run)
    match result with
    | .ok _ => pure ()
    | .error .exitSuccess => pure ()
    | .error e => throw { passName := "BigStepEval", message := e.render, range? := e.rangeOf }

end Malgo.Sequent.BigStepEval
