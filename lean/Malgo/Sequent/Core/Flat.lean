import Malgo.Prelude
import Malgo.Id
import Malgo.Monad
import Malgo.Module
import Malgo.SExpr
import Malgo.Sequent.Fun
import Malgo.Sequent.Core.Full

/-! Port of `src/Malgo/Sequent/Core/Flat.hs`: the Flat IR (no nested `Do`
producers) plus the Full→Flat flattening pass.

Constructor renames follow Full.lean: `Consumer.Then` → `«then»`. The Flat
IR drops Full's `Do` producer (it becomes `Statement.Join`) and adds
`Statement.Join`.

Fresh names: Haskell's `newTemporalId` runs under `Reader ModuleName +
State Uniq`; here the `ModuleName` is an explicit parameter and the uniq
supply is `Malgo.getUniq` inside `MalgoM`. The uniq order is observable
(it appears in dumps), so the control flow is translated mechanically:
every `traverse`/`for`/`<-` sequences left-to-right exactly as the
Haskell does, and `Object` fields are traversed in ascending key order to
match `Data.Map`'s `traverse`.

`Show`/`Repr` is not derived (only the golden-checked `ToSExpr` dumps are
observable). `Binary` is skipped. -- TODO(M2): ToJson/FromJson -/

namespace Malgo.Sequent.Core.Flat

open Malgo.Sequent.Fun (Name Literal Tag Pattern)

/-- Orphan `Inhabited` so the `partial def` flattening functions (which
return `MalgoM _`) type-check; the value is never observed. -/
instance : Inhabited CompileError := ⟨{ passName := "", message := "" }⟩

mutual

inductive Producer where
  | var (range : Range) (name : Name)
  | literal (range : Range) (lit : Literal)
  | construct (range : Range) (tag : Tag) (producers : List Producer) (consumers : List Consumer)
  | lambda (range : Range) (names : List Name) (statement : Statement)
  | object (range : Range) (fields : List (String × Name × Statement))
  | mu (range : Range) (name : Name) (statement : Statement)
  | cocase (range : Range) (branches : List (String × List Name × Statement))

inductive Consumer where
  | label (range : Range) (name : Name)
  | apply (range : Range) (producers : List Producer) (consumers : List Consumer)
  | project (range : Range) (field : String) (ret : Consumer)
  | «then» (range : Range) (name : Name) (statement : Statement)
  | finish (range : Range)
  | select (range : Range) (branches : List Branch)
  | destructor (range : Range) (name : String) (producers : List Producer) (consumer : Consumer)

inductive Statement where
  | cut (producer : Producer) (consumer : Consumer)
  | join (range : Range) (name : Name) (consumer : Consumer) (statement : Statement)
  | primitive (range : Range) (name : String) (producers : List Producer) (consumer : Consumer)
  | invoke (range : Range) (name : Name) (consumer : Consumer)
  | externalCall (range : Range) (name : String) (producers : List Producer) (consumer : Consumer)
  | binOp (range : Range) (op : String) (lhs : Producer) (rhs : Producer) (consumer : Consumer)
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
      .list (producers.map Producer.toSExpr), .list (consumers.map Consumer.toSExpr)]
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
    .list [sym "apply", .list (producers.map Producer.toSExpr), .list (consumers.map Consumer.toSExpr)]
  | .project _ field ret => .list [sym "project", Malgo.toSExpr field, ret.toSExpr]
  | .«then» _ name statement => .list [sym "then", Malgo.toSExpr name, statement.toSExpr]
  | .finish _ => sym "finish"
  | .select _ branches => .list (sym "select" :: branches.map Branch.toSExpr)
  | .destructor _ name producers consumer =>
    .list [sym "destructor", Malgo.toSExpr name, .list (producers.map Producer.toSExpr), consumer.toSExpr]
termination_by c => sizeOf c
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def Statement.toSExpr : Statement → SExpr
  | .cut producer consumer => .list [sym "cut", producer.toSExpr, consumer.toSExpr]
  | .join _ name consumer statement =>
    .list [sym "join", Malgo.toSExpr name, consumer.toSExpr, statement.toSExpr]
  | .primitive _ name producers consumer =>
    .list [sym "prim", Malgo.toSExpr name, .list (producers.map Producer.toSExpr), consumer.toSExpr]
  | .invoke _ name consumer => .list [sym "invoke", Malgo.toSExpr name, consumer.toSExpr]
  | .externalCall _ name producers consumer =>
    .list [sym "external-call", Malgo.toSExpr name, .list (producers.map Producer.toSExpr), consumer.toSExpr]
  | .binOp _ op lhs rhs consumer =>
    .list [sym "binop", Malgo.toSExpr op, lhs.toSExpr, rhs.toSExpr, consumer.toSExpr]
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

/-- Temporary state of the flattening process: `do'` marks a producer that
must become a `Join` (a bound continuation); `zero` is a rank-zero
producer that can appear directly. Port of Haskell's `Wip`. -/
inductive Wip where
  | do' (range : Range) (name : Name) (statement : Statement)
  | zero (producer : Producer)

def Wip.cut : Wip → Consumer → Statement
  | .do' range name statement, consumer => .join range name consumer statement
  | .zero producer, consumer => .cut producer consumer

/-- A producer is a value (rank zero) when it can be used directly as an
argument without being bound to a continuation first. -/
partial def isValue : Full.Producer → Bool
  | .var .. => true
  | .literal .. => true
  | .construct _ _ ps _ => ps.all isValue
  | .lambda .. => true
  | .object .. => true
  | .«do» .. => false
  | .mu .. => false
  | .cocase .. => true

/-- Split at the first non-value producer: `(valuePrefix, firstNonValue?,
rest)`. Pure — it consumes no fresh names. -/
def split (producers : List Full.Producer) :
    List Full.Producer × Option Full.Producer × List Full.Producer :=
  aux [] producers
where
  aux (acc : List Full.Producer) :
      List Full.Producer → List Full.Producer × Option Full.Producer × List Full.Producer
    | [] => (acc.reverse, none, [])
    | p :: ps => if isValue p then aux (p :: acc) ps else (acc.reverse, some p, ps)

mutual

partial def flatStatement (moduleName : ModuleName) : Full.Statement → MalgoM Statement
  | .cut producer consumer => do
    let producer ← flatProducer moduleName producer
    let consumer ← flatConsumer moduleName consumer
    match producer with
    | .do' range name statement => pure <| .join range name consumer statement
    | .zero producer => pure <| .cut producer consumer
  | .primitive range name producers consumer => do
    let (zeros, mproducer, rest) := split producers
    match mproducer with
    | some producer =>
      let var ← newTemporalId moduleName "var"
      let producer' ← flatProducer moduleName producer
      let primitive ← flatStatement moduleName
        (.primitive range name (zeros ++ [.var range var] ++ rest) consumer)
      pure <| producer'.cut <| .«then» range var primitive
    | none =>
      let producers' ← flatZeros moduleName zeros
      let consumer' ← flatConsumer moduleName consumer
      pure <| .primitive range name producers' consumer'
  | .invoke range name consumer => do
    let consumer' ← flatConsumer moduleName consumer
    pure <| .invoke range name consumer'
  | .externalCall range name producers consumer => do
    let (zeros, mproducer, rest) := split producers
    match mproducer with
    | some producer =>
      let var ← newTemporalId moduleName "var"
      let producer' ← flatProducer moduleName producer
      let stmt ← flatStatement moduleName
        (.externalCall range name (zeros ++ [.var range var] ++ rest) consumer)
      pure <| producer'.cut <| .«then» range var stmt
    | none =>
      let producers' ← flatZeros moduleName zeros
      let consumer' ← flatConsumer moduleName consumer
      pure <| .externalCall range name producers' consumer'
  | .binOp range op lhs rhs consumer => do
    -- lhs', rhs', consumer' are computed (and consume fresh names) up front
    -- even in the recursing branches, matching the Haskell order exactly.
    let lhs' ← flatProducer moduleName lhs
    let rhs' ← flatProducer moduleName rhs
    let consumer' ← flatConsumer moduleName consumer
    match lhs' with
    | .do' r1 _ _ =>
      let var1 ← newTemporalId moduleName "var"
      let stmt ← flatStatement moduleName (.binOp range op (.var range var1) rhs consumer)
      pure <| lhs'.cut <| .«then» r1 var1 stmt
    | .zero lhsP =>
      match rhs' with
      | .do' r2 _ _ =>
        let var2 ← newTemporalId moduleName "var"
        let stmt ← flatStatement moduleName (.binOp range op lhs (.var range var2) consumer)
        pure <| rhs'.cut <| .«then» r2 var2 stmt
      | .zero rhsP => pure <| .binOp range op lhsP rhsP consumer'
  | .ifz range cond thenS elseS => do
    let cond' ← flatProducer moduleName cond
    let then' ← flatStatement moduleName thenS
    let else' ← flatStatement moduleName elseS
    match cond' with
    | .do' r _ _ =>
      let var ← newTemporalId moduleName "var"
      pure <| cond'.cut <| .«then» r var <| .ifz range (.var range var) then' else'
    | .zero condP => pure <| .ifz range condP then' else'

partial def flatProducer (moduleName : ModuleName) : Full.Producer → MalgoM Wip
  | .var range name => pure <| .zero (.var range name)
  | .literal range literal => pure <| .zero (.literal range literal)
  | .construct range tag producers consumers => do
    let (zeros, mproducer, rest) := split producers
    match mproducer with
    | some producer =>
      let label ← newTemporalId moduleName "label"
      let var ← newTemporalId moduleName "var"
      let constructor ← flatProducer moduleName
        (.construct range tag (zeros ++ [.var range var] ++ rest) consumers)
      let producer' ← flatProducer moduleName producer
      pure <| .do' range label <| producer'.cut <| .«then» range var
        <| constructor.cut <| .label range label
    | none =>
      let producers' ← flatZeros moduleName zeros
      let consumers' ← consumers.mapM (flatConsumer moduleName)
      pure <| .zero (.construct range tag producers' consumers')
  | .lambda range names statement => do
    let statement ← flatStatement moduleName statement
    pure <| .zero (.lambda range names statement)
  | .object range fields => do
    let fields ← (sortAssocAscending fields).mapM fun (k, name, s) => do
      pure (k, name, ← flatStatement moduleName s)
    pure <| .zero (.object range fields)
  | .«do» range name statement => do
    let statement' ← flatStatement moduleName statement
    pure <| .do' range name statement'
  | .mu range name statement => do
    let statement' ← flatStatement moduleName statement
    pure <| .zero (.mu range name statement')
  | .cocase range branches => do
    let branches' ← branches.mapM fun (d, vars, s) => do
      pure (d, vars, ← flatStatement moduleName s)
    pure <| .zero (.cocase range branches')

partial def flatConsumer (moduleName : ModuleName) : Full.Consumer → MalgoM Consumer
  | .label range name => pure <| .label range name
  | .apply range producers consumers => do
    let (zeros, mproducer, rest) := split producers
    match mproducer with
    | some producer =>
      let outer ← newTemporalId moduleName "outer"
      let inner ← newTemporalId moduleName "inner"
      let apply ← flatConsumer moduleName
        (.apply range (zeros ++ [.var range inner] ++ rest) consumers)
      let producer' ← flatProducer moduleName producer
      pure <| .«then» range outer <| producer'.cut <| .«then» range inner
        <| (Wip.zero (.var range outer)).cut apply
    | none =>
      let producers' ← flatZeros moduleName zeros
      let consumers' ← consumers.mapM (flatConsumer moduleName)
      pure <| .apply range producers' consumers'
  | .project range field ret => do
    let ret' ← flatConsumer moduleName ret
    pure <| .project range field ret'
  | .«then» range name statement => do
    let statement' ← flatStatement moduleName statement
    pure <| .«then» range name statement'
  | .finish range => pure <| .finish range
  | .select range branches => do
    let branches ← branches.mapM (flatBranch moduleName)
    pure <| .select range branches
  | .destructor range name producers consumer => do
    let (zeros, mproducer, rest) := split producers
    match mproducer with
    | some producer =>
      let outer ← newTemporalId moduleName "outer"
      let inner ← newTemporalId moduleName "inner"
      let destr ← flatConsumer moduleName
        (.destructor range name (zeros ++ [.var range inner] ++ rest) consumer)
      let producer' ← flatProducer moduleName producer
      pure <| .«then» range outer <| producer'.cut <| .«then» range inner
        <| (Wip.zero (.var range outer)).cut destr
    | none =>
      let producers' ← flatZeros moduleName zeros
      let consumer' ← flatConsumer moduleName consumer
      pure <| .destructor range name producers' consumer'

partial def flatBranch (moduleName : ModuleName) : Full.Branch → MalgoM Branch
  | .branch range pattern statement => do
    let statement ← flatStatement moduleName statement
    pure <| .branch range pattern statement

/-- Flatten a list of value (rank-zero) producers, left-to-right. `split`
guarantees each is a value, so `flatProducer` returns `Wip.zero`; the
`do'` case is unreachable (mirrors Haskell's `error "impossible"`). -/
partial def flatZeros (moduleName : ModuleName) (zeros : List Full.Producer) :
    MalgoM (List Producer) :=
  zeros.mapM fun zero => do
    match ← flatProducer moduleName zero with
    | .zero producer' => pure producer'
    | .do' .. => throw { passName := "Flat", message := "impossible: non-value producer in zeros" }

end

def flatDefinition (moduleName : ModuleName) :
    Range × Name × Name × Full.Statement → MalgoM (Range × Name × Name × Statement)
  | (range, name, ret, statement) => do
    let statement ← flatStatement moduleName statement
    pure (range, name, ret, statement)

/-- Flattens a program into one with no nested `Do` producers. -/
def flatProgram (moduleName : ModuleName) (program : Full.Program) : MalgoM Program := do
  let definitions ← program.definitions.mapM (flatDefinition moduleName)
  pure { definitions, dependencies := program.dependencies }

private def r0 : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
private def nm (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }

-- Dump-format checks (the golden-observable surface).
#guard Malgo.sShow (Statement.cut (.var r0 (nm "x")) (.finish r0)) == "(cut x finish)"
#guard Malgo.sShow (Statement.invoke r0 (nm "f") (.finish r0)) == "(invoke f finish)"
-- A `Join` nesting: the outer binding (`a`) wraps the inner (`b`); this is
-- the shape the hoisting produces (first continuation bound is outermost).
#guard Malgo.sShow
    (Statement.join r0 (nm "a") (.finish r0)
      (.join r0 (nm "b") (.finish r0) (.cut (.var r0 (nm "x")) (.label r0 (nm "k")))))
  == "(join a finish (join b finish (cut x k)))"
-- Object fields dump in ascending key order (b before r).
#guard Malgo.sShow
    (Producer.object r0
      [("return", nm "k", .cut (.var r0 (nm "x")) (.finish r0)),
       ("bind", nm "j", .cut (.var r0 (nm "y")) (.finish r0))])
  == "((bind (j (cut y finish))) (return (k (cut x finish))))"

end Malgo.Sequent.Core.Flat
