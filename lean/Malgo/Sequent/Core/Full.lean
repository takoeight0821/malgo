import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.SExpr
import Malgo.Sequent.Fun
import Malgo.Sequent.Core.Common

/-! Port of `src/Malgo/Sequent/Core/Full.hs`: the sequent-calculus Core IR
(`Producer`/`Consumer`/`Statement`/`Branch`), with explicit control flow.

Keyword-forced renames: `Producer.Do` → `«do»`, `Consumer.Then` → `«then»`.
`Statement.Ifz`'s `then_`/`else_` branch fields are `thenS`/`elseS`.
The Haskell `Object` field `Map Text (Name, Statement)` becomes
`List (String × Name × Statement)` in ascending key order (a `Std.TreeMap`
cannot nest in a recursive inductive); dumps sort with
`sortAssocAscending` to match `Data.Map`'s ascending `toList`.

`Show`/`Repr` is not derived for the mutually-recursive types (not
observable; only the `ToSExpr` dumps are, and they are golden-checked).
`Binary` is skipped. -- TODO(M2): ToJson/FromJson -/

namespace Malgo.Sequent.Core.Full

open Malgo.Sequent.Fun (Name Literal Tag Pattern)

mutual

inductive Producer where
  | var (range : Range) (name : Name)
  | literal (range : Range) (lit : Literal)
  | construct (range : Range) (tag : Tag) (producers : List Producer) (consumers : List Consumer)
  | lambda (range : Range) (names : List Name) (statement : Statement)
  | object (range : Range) (fields : List (String × Name × Statement))
  | «do» (range : Range) (name : Name) (statement : Statement)
  | mu (range : Range) (name : Name) (statement : Statement)

inductive Consumer where
  | label (range : Range) (name : Name)
  | apply (range : Range) (producers : List Producer) (consumers : List Consumer)
  | project (range : Range) (field : String) (ret : Consumer)
  | «then» (range : Range) (name : Name) (statement : Statement)
  | finish (range : Range)
  | select (range : Range) (branches : List Branch)

inductive Statement where
  | cut (producer : Producer) (consumer : Consumer)
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
  | .«do» r _ _ => r
  | .mu r _ _ => r

instance : HasRange Producer := ⟨Producer.range⟩

def Statement.range : Statement → Range
  | .cut producer _ => Producer.range producer
  | .primitive r _ _ _ => r
  | .invoke r _ _ => r
  | .externalCall r _ _ _ => r
  | .binOp r _ _ _ _ => r
  | .ifz r _ _ _ => r

instance : HasRange Statement := ⟨Statement.range⟩

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
  | .«do» _ name statement => .list [sym "do", Malgo.toSExpr name, statement.toSExpr]
  | .mu _ name statement => .list [sym "mu", Malgo.toSExpr name, statement.toSExpr]
termination_by p => sizeOf p
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_snd_lt_of_mem (mem_sortAssocAscending hkns)) (by omega)
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
termination_by c => sizeOf c
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def Statement.toSExpr : Statement → SExpr
  | .cut producer consumer => .list [sym "cut", producer.toSExpr, consumer.toSExpr]
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

abbrev Definition := DefinitionOf Statement
abbrev Program := ProgramOf Statement

end Malgo.Sequent.Core.Full
