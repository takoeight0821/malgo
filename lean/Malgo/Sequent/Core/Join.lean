import Malgo.Prelude
import Malgo.Id
import Malgo.Monad
import Malgo.Module
import Malgo.SExpr
import Malgo.Sequent.Fun
import Malgo.Sequent.Core.Flat

/-! Port of `src/Malgo/Sequent/Core/Join.hs`: the Join IR (explicit join
points; consumers become `Name`s referring to hoisted `Join` bindings)
plus the Flat→Join hoisting pass.

The Haskell pass runs each statement in `Writer (Endo Statement)`: every
`joinConsumer` that is not a bare label emits (`tell`) an `Endo` that
prepends a `Join` binding, and `runJoin` applies the accumulated `Endo` to
the produced statement. The monoid is `Endo`, whose `<>` is function
composition (`f <> g = f ∘ g`), and `Writer.tell` accumulates on the
right, so the *first* consumer processed becomes the *outermost* `Join`:
`runJoin` yields `join₁ (join₂ (… (jointₙ result)))`.

Here that `Writer (Endo Statement)` is modeled as an explicit accumulator
`Statement → Statement` threaded through `StateT`, updated by right
composition (`acc ∘ frame`) so the order matches exactly; `runJoin` starts
from `id` and applies the final accumulator to the result. Fresh names use
`Malgo.newTemporalId` (the `ModuleName` is an explicit parameter); the
uniq order is observable, so control flow is translated mechanically and
`Object` fields are traversed in ascending key order (matching
`Data.Map`).

`Show`/`Repr`/`Binary` are skipped. -- TODO(M2): ToJson/FromJson -/

namespace Malgo.Sequent.Core.Join

open Malgo.Sequent.Fun (Name Literal Tag Pattern)

mutual

inductive Producer where
  | var (range : Range) (name : Name)
  | literal (range : Range) (lit : Literal)
  | construct (range : Range) (tag : Tag) (producers : List Producer) (consumers : List Name)
  | lambda (range : Range) (names : List Name) (statement : Statement)
  | object (range : Range) (fields : List (String × Name × Statement))
  | mu (range : Range) (name : Name) (statement : Statement)
  | cocase (range : Range) (branches : List (String × List Name × Statement))

inductive Consumer where
  | label (range : Range) (name : Name)
  | apply (range : Range) (producers : List Producer) (consumers : List Name)
  | project (range : Range) (field : String) (ret : Name)
  | «then» (range : Range) (name : Name) (statement : Statement)
  | finish (range : Range)
  | select (range : Range) (branches : List Branch)
  | destructor (range : Range) (name : String) (producers : List Producer) (consumer : Name)

inductive Statement where
  | cut (producer : Producer) (consumer : Name)
  | join (range : Range) (name : Name) (consumer : Consumer) (statement : Statement)
  | primitive (range : Range) (name : String) (producers : List Producer) (consumer : Name)
  | invoke (range : Range) (name : Name) (consumer : Name)
  | externalCall (range : Range) (name : String) (producers : List Producer) (consumer : Name)
  | binOp (range : Range) (op : String) (lhs : Producer) (rhs : Producer) (consumer : Name)
  | ifz (range : Range) (cond : Producer) (thenS : Statement) (elseS : Statement)

inductive Branch where
  | branch (range : Range) (pattern : Pattern) (statement : Statement)

end

def Producer.range : Producer → Range
  | .var r _ => r
  | .literal r _ => r
  | .construct r _ _ _ => r
  | .lambda r _ _ => r
  | .object r _ => r
  | .mu r _ _ => r
  | .cocase r _ => r

instance : HasRange Producer := ⟨Producer.range⟩

def Statement.range : Statement → Range
  | .cut producer _ => Producer.range producer
  | .join r _ _ _ => r
  | .primitive r _ _ _ => r
  | .invoke r _ _ => r
  | .externalCall r _ _ _ => r
  | .binOp r _ _ _ _ => r
  | .ifz r _ _ _ => r

instance : HasRange Statement := ⟨Statement.range⟩

private def sym (s : String) : SExpr := .atom (.symbol s)

mutual

def Producer.toSExpr : Producer → SExpr
  | .var _ name => Malgo.toSExpr name
  | .literal _ literal => Malgo.toSExpr literal
  | .construct _ tag producers consumers =>
    .list [sym "construct", Malgo.toSExpr tag,
      .list (producers.map Producer.toSExpr), .list (consumers.map Malgo.toSExpr)]
  | .lambda _ names statement =>
    .list [sym "lambda", .list (names.map Malgo.toSExpr), statement.toSExpr]
  | .object _ kvs =>
    .list ((sortAssocAscending kvs).attach.map fun ⟨kns, hkns⟩ =>
      .list [sym kns.1, .list [Malgo.toSExpr kns.2.1, kns.2.2.toSExpr]])
  | .mu _ name statement => .list [sym "mu", Malgo.toSExpr name, statement.toSExpr]
  | .cocase _ branches =>
    .list (sym "cocase" :: branches.attach.map fun ⟨dvs, hdvs⟩ =>
      .list [Malgo.toSExpr dvs.1, .list (dvs.2.1.map Malgo.toSExpr), dvs.2.2.toSExpr])
termination_by p => sizeOf p
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_snd_lt_of_mem (mem_sortAssocAscending hkns)) (by omega)
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_snd_lt_of_mem hdvs) (by omega)
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def Consumer.toSExpr : Consumer → SExpr
  | .label _ name => Malgo.toSExpr name
  | .apply _ producers consumers =>
    .list [sym "apply", .list (producers.map Producer.toSExpr), .list (consumers.map Malgo.toSExpr)]
  | .project _ field ret => .list [sym "project", Malgo.toSExpr field, Malgo.toSExpr ret]
  | .«then» _ name statement => .list [sym "then", Malgo.toSExpr name, statement.toSExpr]
  | .finish _ => sym "finish"
  | .select _ branches => .list (sym "select" :: branches.map Branch.toSExpr)
  | .destructor _ name producers consumer =>
    .list [sym "destructor", Malgo.toSExpr name, .list (producers.map Producer.toSExpr), Malgo.toSExpr consumer]
termination_by c => sizeOf c
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def Statement.toSExpr : Statement → SExpr
  | .cut producer consumer => .list [sym "cut", producer.toSExpr, Malgo.toSExpr consumer]
  | .join _ name consumer statement =>
    .list [sym "join", Malgo.toSExpr name, consumer.toSExpr, statement.toSExpr]
  | .primitive _ name producers consumer =>
    .list [sym "prim", Malgo.toSExpr name, .list (producers.map Producer.toSExpr), Malgo.toSExpr consumer]
  | .invoke _ name consumer => .list [sym "invoke", Malgo.toSExpr name, Malgo.toSExpr consumer]
  | .externalCall _ name producers consumer =>
    .list [sym "external-call", Malgo.toSExpr name, .list (producers.map Producer.toSExpr), Malgo.toSExpr consumer]
  | .binOp _ op lhs rhs consumer =>
    .list [sym "binop", Malgo.toSExpr op, lhs.toSExpr, rhs.toSExpr, Malgo.toSExpr consumer]
  | .ifz _ cond thenS elseS => .list [sym "ifz", cond.toSExpr, thenS.toSExpr, elseS.toSExpr]
termination_by s => sizeOf s
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def Branch.toSExpr : Branch → SExpr
  | .branch _ pattern statement => .list [Malgo.toSExpr pattern, statement.toSExpr]
termination_by b => sizeOf b

end

instance : ToSExpr Producer := ⟨Producer.toSExpr⟩
instance : ToSExpr Consumer := ⟨Consumer.toSExpr⟩
instance : ToSExpr Statement := ⟨Statement.toSExpr⟩
instance : ToSExpr Branch := ⟨Branch.toSExpr⟩

structure Program where
  definitions : List (Range × Name × Name × Statement)
  dependencies : List ModuleName

instance : ToSExpr Program where
  toSExpr p :=
    .list <|
      (p.definitions.map fun (_, name, ret, body) =>
        .list [Malgo.toSExpr name, Malgo.toSExpr ret, Malgo.toSExpr body])
      ++ [.list (p.dependencies.map Malgo.toSExpr)]

/-- The hoisting accumulator: Haskell's `Writer (Endo Statement)`. The
state is the accumulated `Endo`, i.e. a `Statement → Statement`. -/
abbrev JoinM := StateT (Statement → Statement) MalgoM

/-- Port of `runJoin`: run a hoisting computation with an empty accumulator
(`id`) and apply the collected `Join` frames to the produced statement.
The accumulator is `frame₁ ∘ frame₂ ∘ … ∘ frameₙ`, so the first frame
emitted (the first consumer processed) ends up the outermost `Join`. -/
def runJoin (m : JoinM Statement) : MalgoM Statement := do
  let (result, acc) ← m.run id
  pure (acc result)

/-- Port of `tellJoin`: allocate a fresh name for a hoisted continuation,
append its `Join` frame to the accumulator (on the right, matching
`Writer.tell`'s `acc <> Endo frame`), and return the name. -/
def tellJoin (moduleName : ModuleName) (range : Range) (name : String) (consumer : Consumer) :
    JoinM Name := do
  let name ← newTemporalId moduleName name
  modify fun acc => acc ∘ (Statement.join range name consumer)
  pure name

mutual

partial def joinStatement (moduleName : ModuleName) : Flat.Statement → JoinM Statement
  | .cut producer consumer => do
    let producer ← joinProducer moduleName producer
    let consumer ← joinConsumer moduleName consumer
    pure <| .cut producer consumer
  | .join range name consumer statement => do
    let statement ← runJoin (joinStatement moduleName statement)
    let consumer ← joinConsumer' moduleName consumer
    pure <| .join range name consumer statement
  | .primitive range name producers ret => do
    let producers ← producers.mapM (joinProducer moduleName)
    let ret ← joinConsumer moduleName ret
    pure <| .primitive range name producers ret
  | .invoke range name ret => do
    let ret ← joinConsumer moduleName ret
    pure <| .invoke range name ret
  | .externalCall range name producers ret => do
    let producers ← producers.mapM (joinProducer moduleName)
    let ret ← joinConsumer moduleName ret
    pure <| .externalCall range name producers ret
  | .binOp range op lhs rhs ret => do
    let lhs ← joinProducer moduleName lhs
    let rhs ← joinProducer moduleName rhs
    let ret ← joinConsumer moduleName ret
    pure <| .binOp range op lhs rhs ret
  | .ifz range cond thenS elseS => do
    let cond ← joinProducer moduleName cond
    let thenS ← runJoin (joinStatement moduleName thenS)
    let elseS ← runJoin (joinStatement moduleName elseS)
    pure <| .ifz range cond thenS elseS

partial def joinProducer (moduleName : ModuleName) : Flat.Producer → JoinM Producer
  | .var range name => pure <| .var range name
  | .literal range literal => pure <| .literal range literal
  | .construct range tag producers returns => do
    let producers ← producers.mapM (joinProducer moduleName)
    let returns ← returns.mapM (joinConsumer moduleName)
    pure <| .construct range tag producers returns
  | .lambda range names statement => do
    let statement ← runJoin (joinStatement moduleName statement)
    pure <| .lambda range names statement
  | .object range fields => do
    let fields ← (sortAssocAscending fields).mapM fun (k, name, s) => do
      pure (k, name, ← runJoin (joinStatement moduleName s))
    pure <| .object range fields
  | .mu range name statement => do
    let statement ← runJoin (joinStatement moduleName statement)
    pure <| .mu range name statement
  | .cocase range branches => do
    let branches ← branches.mapM fun (d, vars, s) => do
      pure (d, vars, ← runJoin (joinStatement moduleName s))
    pure <| .cocase range branches

/-- Consumers other than a bare `Label` are hoisted: they emit a `Join`
frame via `tellJoin` and reduce to the fresh name that binds it. -/
partial def joinConsumer (moduleName : ModuleName) : Flat.Consumer → JoinM Name
  | .label _ name => pure name
  | .apply range producers returns => do
    let producers ← producers.mapM (joinProducer moduleName)
    let returns ← returns.mapM (joinConsumer moduleName)
    tellJoin moduleName range "apply" (.apply range producers returns)
  | .project range field ret => do
    let ret ← joinConsumer moduleName ret
    tellJoin moduleName range "project" (.project range field ret)
  | .«then» range name statement => do
    let statement ← runJoin (joinStatement moduleName statement)
    tellJoin moduleName range "then" (.«then» range name statement)
  | .finish range => tellJoin moduleName range "finish" (.finish range)
  | .select range branches => do
    let branches ← branches.mapM fun b => do pure (← joinBranch moduleName b)
    tellJoin moduleName range "select" (.select range branches)
  | .destructor range name producers ret => do
    let producers ← producers.mapM (joinProducer moduleName)
    let ret ← joinConsumer moduleName ret
    tellJoin moduleName range "destructor" (.destructor range name producers ret)

/-- The `Join`-binding position keeps a full `Consumer` rather than
hoisting it to a name (port of `joinConsumer'`). -/
partial def joinConsumer' (moduleName : ModuleName) : Flat.Consumer → JoinM Consumer
  | .label range name => pure <| .label range name
  | .apply range producers returns => do
    let producers ← producers.mapM (joinProducer moduleName)
    let returns ← returns.mapM (joinConsumer moduleName)
    pure <| .apply range producers returns
  | .project range field ret => do
    let ret ← joinConsumer moduleName ret
    pure <| .project range field ret
  | .«then» range name statement => do
    let statement ← runJoin (joinStatement moduleName statement)
    pure <| .«then» range name statement
  | .finish range => pure <| .finish range
  | .select range branches => do
    let branches ← branches.mapM fun b => do pure (← joinBranch moduleName b)
    pure <| .select range branches
  | .destructor range name producers ret => do
    let producers ← producers.mapM (joinProducer moduleName)
    let ret ← joinConsumer moduleName ret
    pure <| .destructor range name producers ret

partial def joinBranch (moduleName : ModuleName) : Flat.Branch → MalgoM Branch
  | .branch range pattern statement => do
    let statement ← runJoin (joinStatement moduleName statement)
    pure <| .branch range pattern statement

end

def joinDefinition (moduleName : ModuleName) :
    Range × Name × Name × Flat.Statement → MalgoM (Range × Name × Name × Statement)
  | (range, name, ret, statement) => do
    let statement ← runJoin (joinStatement moduleName statement)
    pure (range, name, ret, statement)

def joinProgram (moduleName : ModuleName) (program : Flat.Program) : MalgoM Program := do
  let definitions ← program.definitions.mapM (joinDefinition moduleName)
  pure { definitions, dependencies := program.dependencies }

private def r0 : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
private def nm (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }

-- Dump-format checks (the golden-observable surface).
#guard Malgo.sShow (Statement.cut (.var r0 (nm "x")) (nm "k")) == "(cut x k)"
#guard Malgo.sShow (Statement.invoke r0 (nm "f") (nm "k")) == "(invoke f k)"
-- Nested `Join`: the first-emitted frame (`a`) is the outermost, which is
-- exactly the shape `runJoin` produces from the accumulator order.
#guard Malgo.sShow
    (Statement.join r0 (nm "a") (.finish r0)
      (.join r0 (nm "b") (.finish r0) (.cut (.var r0 (nm "x")) (nm "k"))))
  == "(join a finish (join b finish (cut x k)))"
-- Object fields dump in ascending key order (b before r).
#guard Malgo.sShow
    (Producer.object r0
      [("return", nm "k", .cut (.var r0 (nm "x")) (nm "k")),
       ("bind", nm "j", .cut (.var r0 (nm "y")) (nm "j"))])
  == "((bind (j (cut y j))) (return (k (cut x k))))"

end Malgo.Sequent.Core.Join
