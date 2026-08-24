import Lean.Data.Json
import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.Interface
import Malgo.Sequent.Fun
import Malgo.Sequent.Core.Join

/-! JSON codecs for the Join IR (`.sqt` artifacts).

Haskell derives `Binary` for every IR type; deriving `ToJson`/`FromJson` on
Lean's mutually-recursive `Producer`/`Consumer`/`Statement`/`Branch` does
not work, so the codecs are hand-written as compact tagged arrays
(`#[tag, field₀, …]`). Only the Join IR is persisted (`.sqt`); the Full/Flat
IRs are transient and get no codec.

`ModuleName`/`ArtifactPath` codecs come from `Malgo/Interface.lean`. Floats
round-trip through their raw bits (`toBits`/`ofBits`) to stay exact — a
decimal JSON number would lose precision. `Id` round-trips exactly, so the
interpreter's `Id`-keyed environment behaves identically after load. -/

namespace Malgo.Sequent.Core.Json

open Lean (Json ToJson FromJson toJson fromJson?)
open Malgo
open Malgo.Sequent.Fun (Name Literal Tag Pattern)
open Malgo.Sequent.Core.Join

/-- Serialize a `List` to a JSON array. -/
private def jList (f : α → Json) (xs : List α) : Json := Json.arr (xs.map f).toArray

/-- Parse a JSON array into a `List` with an element parser. -/
private def jParseList (f : Json → Except String α) (j : Json) : Except String (List α) := do
  (← j.getArr?).toList.mapM f

/-- `Json.getArr?` succeeding pins the input down to that exact array — its
2-arm match (`Lean/Data/Json/Basic.lean`) makes this a one-line case split. -/
private theorem getArr?_eq_ok {j : Json} {a : Array Json} (h : j.getArr? = .ok a) :
    j = .arr a := by
  cases j <;> simp_all [Json.getArr?, Except.pure, pure]

/-- Parser-side counterpart to `List.attach`, used in place of `jParseList`
wherever an array's elements recurse into one of the mutually-recursive
`fromJson?` codecs below. `toJson`'s `decreasing_by` blocks get a
`sizeOf`-decrease for free by recursing over the already-typed AST
(`pats.attach.map fun ⟨p, hp⟩ => ...`); `fromJson?` instead recurses over the
*untyped* `Json` being decoded, so this attaches the same kind of proof to
every array element up front, from `getArr?_eq_ok` plus the core-library
`Array.sizeOf_lt_of_mem`. -/
private def jParseArrAttach (j : Json) :
    Except String (List {e : Json // sizeOf e < sizeOf j}) :=
  match h : j.getArr? with
  | .error e => .error e
  | .ok arr =>
    .ok (arr.toList.attach.map fun ⟨e, he⟩ =>
      ⟨e, by
        have hj : j = Json.arr arr := getArr?_eq_ok h
        have hmem : e ∈ arr := Array.mem_toList_iff.mp he
        have h1 : sizeOf e < sizeOf arr := Array.sizeOf_lt_of_mem hmem
        subst hj
        simp only [Json.arr.sizeOf_spec]
        omega⟩)

/-! ## Leaf codecs -/

instance : ToJson SourcePos where
  toJson p := Json.arr #[Json.str p.sourceName, toJson p.line, toJson p.column]

instance : FromJson SourcePos where
  fromJson? j := do
    match (← j.getArr?).toList with
    | [n, l, c] => return { sourceName := ← n.getStr?, line := ← l.getNat?, column := ← c.getNat? }
    | _ => .error "SourcePos: expected a 3-element array"

instance : ToJson Range where
  toJson r := Json.arr #[toJson r.start, toJson r.stop]

instance : FromJson Range where
  fromJson? j := do
    match (← j.getArr?).toList with
    | [s, e] => return { start := ← fromJson? s, stop := ← fromJson? e }
    | _ => .error "Range: expected a 2-element array"

instance : ToJson IdSort where
  toJson
    | .external => Json.arr #[Json.str "e"]
    | .internal u => Json.arr #[Json.str "i", toJson u]
    | .temporal u => Json.arr #[Json.str "t", toJson u]

instance : FromJson IdSort where
  fromJson? j := do
    match (← j.getArr?).toList with
    | [tag] => match ← tag.getStr? with
      | "e" => return .external
      | other => .error s!"IdSort: bad nullary tag {other}"
    | [tag, u] => match ← tag.getStr? with
      | "i" => return .internal (← u.getNat?)
      | "t" => return .temporal (← u.getNat?)
      | other => .error s!"IdSort: bad unary tag {other}"
    | _ => .error "IdSort: unexpected shape"

instance : ToJson Id where
  toJson i := Json.arr #[Json.str i.name, toJson i.moduleName, toJson i.sort]

instance : FromJson Id where
  fromJson? j := do
    match (← j.getArr?).toList with
    | [n, m, s] => return { name := ← n.getStr?, moduleName := ← fromJson? m, sort := ← fromJson? s }
    | _ => .error "Id: expected a 3-element array"

instance : ToJson Tag where
  toJson
    | .tuple => Json.arr #[Json.str "tuple"]
    | .tag t => Json.arr #[Json.str "tag", Json.str t]

instance : FromJson Tag where
  fromJson? j := do
    match (← j.getArr?).toList with
    | [tag] => match ← tag.getStr? with
      | "tuple" => return .tuple
      | other => .error s!"Tag: bad nullary tag {other}"
    | [tag, v] => match ← tag.getStr? with
      | "tag" => return .tag (← v.getStr?)
      | other => .error s!"Tag: bad unary tag {other}"
    | _ => .error "Tag: unexpected shape"

instance : ToJson Literal where
  toJson
    | .int32 n => Json.arr #[Json.str "i32", toJson n.toInt]
    | .int64 n => Json.arr #[Json.str "i64", toJson n.toInt]
    | .float n => Json.arr #[Json.str "f32", toJson n.toBits.toNat]
    | .double n => Json.arr #[Json.str "f64", toJson n.toBits.toNat]
    | .char c => Json.arr #[Json.str "c", toJson c.toNat]
    | .string s => Json.arr #[Json.str "s", Json.str s]

instance : FromJson Literal where
  fromJson? j := do
    match (← j.getArr?).toList with
    | [tag, v] => match ← tag.getStr? with
      | "i32" => return .int32 (Int32.ofInt (← v.getInt?))
      | "i64" => return .int64 (Int64.ofInt (← v.getInt?))
      | "f32" => return .float (Float32.ofBits (UInt32.ofNat (← v.getNat?)))
      | "f64" => return .double (Float.ofBits (UInt64.ofNat (← v.getNat?)))
      | "c" => return .char (Char.ofNat (← v.getNat?))
      | "s" => return .string (← v.getStr?)
      | other => .error s!"Literal: unknown tag {other}"
    | _ => .error "Literal: expected a 2-element array"

def patternToJson : Pattern → Json
  | .pvar r name => Json.arr #[Json.str "pvar", toJson r, toJson name]
  | .pliteral r lit => Json.arr #[Json.str "plit", toJson r, toJson lit]
  | .destruct r tag pats =>
    Json.arr #[Json.str "destruct", toJson r, toJson tag,
      Json.arr (pats.attach.map fun ⟨p, hp⟩ => patternToJson p).toArray]
  | .expand r fields =>
    Json.arr #[Json.str "expand", toJson r,
      Json.arr (fields.attach.map fun ⟨kp, hkp⟩ =>
        Json.arr #[Json.str kp.1, patternToJson kp.2]).toArray]
termination_by pt => sizeOf pt
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_lt_of_mem hkp) (by omega)
    | exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem hp) (by omega)

def patternFromJson (j : Json) : Except String Pattern := do
  match ← jParseArrAttach j with
  | [⟨tag, _⟩, ⟨r, _⟩, ⟨a, ha⟩] => match ← tag.getStr? with
    | "pvar" => return .pvar (← fromJson? r) (← fromJson? a)
    | "plit" => return .pliteral (← fromJson? r) (← fromJson? a)
    | "expand" => return .expand (← fromJson? r) (← (← jParseArrAttach a).mapM fun ⟨e, he⟩ => do
        match ← jParseArrAttach e with
        | [⟨k, _⟩, ⟨p, hp⟩] => return (← k.getStr?, ← patternFromJson p)
        | _ => .error "Pattern.expand: bad field")
    | other => .error s!"Pattern: unknown 3-element tag {other}"
  | [⟨tag, _⟩, ⟨r, _⟩, ⟨a, _⟩, ⟨b, hb⟩] => match ← tag.getStr? with
    | "destruct" => return .destruct (← fromJson? r) (← fromJson? a) (← (← jParseArrAttach b).mapM fun ⟨p, hp⟩ => patternFromJson p)
    | other => .error s!"Pattern: unknown 4-element tag {other}"
  | _ => .error "Pattern: unexpected shape"
termination_by sizeOf j
decreasing_by
  all_goals simp_wf
  all_goals omega

instance : ToJson Pattern := ⟨patternToJson⟩
instance : FromJson Pattern := ⟨patternFromJson⟩

/-! ## Join IR codecs (mutual) -/

mutual

def producerToJson : Producer → Json
  | .var r name => Json.arr #[Json.str "var", toJson r, toJson name]
  | .literal r lit => Json.arr #[Json.str "lit", toJson r, toJson lit]
  | .construct r tag ps cs =>
    Json.arr #[Json.str "con", toJson r, toJson tag,
      Json.arr (ps.attach.map fun ⟨p, hp⟩ => producerToJson p).toArray, jList toJson cs]
  | .lambda r names s => Json.arr #[Json.str "lam", toJson r, jList toJson names, statementToJson s]
  | .object r fields =>
    Json.arr #[Json.str "obj", toJson r,
      Json.arr (fields.attach.map fun ⟨kns, hkns⟩ =>
        Json.arr #[Json.str kns.1, toJson kns.2.1, statementToJson kns.2.2]).toArray]
  | .mu r name s => Json.arr #[Json.str "mu", toJson r, toJson name, statementToJson s]
termination_by p => sizeOf p
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_snd_lt_of_mem hkns) (by omega)
    | exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem hp) (by omega)

def consumerToJson : Consumer → Json
  | .label r name => Json.arr #[Json.str "label", toJson r, toJson name]
  | .apply r ps cs =>
    Json.arr #[Json.str "apply", toJson r,
      Json.arr (ps.attach.map fun ⟨p, hp⟩ => producerToJson p).toArray, jList toJson cs]
  | .project r field ret => Json.arr #[Json.str "proj", toJson r, Json.str field, toJson ret]
  | .«then» r name s => Json.arr #[Json.str "then", toJson r, toJson name, statementToJson s]
  | .finish r => Json.arr #[Json.str "finish", toJson r]
  | .select r branches =>
    Json.arr #[Json.str "select", toJson r,
      Json.arr (branches.attach.map fun ⟨b, hb⟩ => branchToJson b).toArray]
termination_by c => sizeOf c
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem hp) (by omega)
    | exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem hb) (by omega)

def statementToJson : Statement → Json
  | .cut p c => Json.arr #[Json.str "cut", producerToJson p, toJson c]
  | .join r name c s => Json.arr #[Json.str "join", toJson r, toJson name, consumerToJson c, statementToJson s]
  | .primitive r name ps c =>
    Json.arr #[Json.str "prim", toJson r, Json.str name,
      Json.arr (ps.attach.map fun ⟨p, hp⟩ => producerToJson p).toArray, toJson c]
  | .invoke r name c => Json.arr #[Json.str "invoke", toJson r, toJson name, toJson c]
  | .externalCall r name ps c =>
    Json.arr #[Json.str "extern", toJson r, Json.str name,
      Json.arr (ps.attach.map fun ⟨p, hp⟩ => producerToJson p).toArray, toJson c]
  | .binOp r op l rhs c => Json.arr #[Json.str "binop", toJson r, Json.str op, producerToJson l, producerToJson rhs, toJson c]
  | .ifz r cond t e => Json.arr #[Json.str "ifz", toJson r, producerToJson cond, statementToJson t, statementToJson e]
termination_by s => sizeOf s
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem hp) (by omega)

def branchToJson : Branch → Json
  | .branch r pat s => Json.arr #[Json.str "branch", toJson r, patternToJson pat, statementToJson s]
termination_by b => sizeOf b

end

mutual

def producerFromJson (j : Json) : Except String Producer := do
  match ← jParseArrAttach j with
  | [⟨tag, _⟩, ⟨r, _⟩, ⟨a, ha⟩] => match ← tag.getStr? with
    | "var" => return .var (← fromJson? r) (← fromJson? a)
    | "lit" => return .literal (← fromJson? r) (← fromJson? a)
    | "obj" => return .object (← fromJson? r) (← (← jParseArrAttach a).mapM fun ⟨e, he⟩ => do
        match ← jParseArrAttach e with
        | [⟨k, _⟩, ⟨name, _⟩, ⟨s, hs⟩] => return (← k.getStr?, ← fromJson? name, ← statementFromJson s)
        | _ => .error "Producer.object: bad field")
    | other => .error s!"Producer: unknown 3-element tag {other}"
  | [⟨tag, _⟩, ⟨r, _⟩, ⟨a, ha⟩, ⟨b, hb⟩] => match ← tag.getStr? with
    | "mu" => return .mu (← fromJson? r) (← fromJson? a) (← statementFromJson b)
    | "lam" => return .lambda (← fromJson? r) (← jParseList fromJson? a) (← statementFromJson b)
    | other => .error s!"Producer: unknown 4-element tag {other}"
  | [⟨tag, _⟩, ⟨r, _⟩, ⟨a, _⟩, ⟨b, hb⟩, ⟨c, _⟩] => match ← tag.getStr? with
    | "con" => return .construct (← fromJson? r) (← fromJson? a) (← (← jParseArrAttach b).mapM fun ⟨p, hp⟩ => producerFromJson p) (← jParseList fromJson? c)
    | other => .error s!"Producer: unknown 5-element tag {other}"
  | _ => .error "Producer: unexpected shape"
termination_by sizeOf j
decreasing_by
  all_goals simp_wf
  all_goals omega

def consumerFromJson (j : Json) : Except String Consumer := do
  match ← jParseArrAttach j with
  | [⟨tag, _⟩, ⟨r, _⟩] => match ← tag.getStr? with
    | "finish" => return .finish (← fromJson? r)
    | other => .error s!"Consumer: unknown 2-element tag {other}"
  | [⟨tag, _⟩, ⟨r, _⟩, ⟨a, ha⟩] => match ← tag.getStr? with
    | "label" => return .label (← fromJson? r) (← fromJson? a)
    | "select" => return .select (← fromJson? r) (← (← jParseArrAttach a).mapM fun ⟨p, hp⟩ => branchFromJson p)
    | other => .error s!"Consumer: unknown 3-element tag {other}"
  | [⟨tag, _⟩, ⟨r, _⟩, ⟨a, ha⟩, ⟨b, _⟩] => match ← tag.getStr? with
    | "apply" => return .apply (← fromJson? r) (← (← jParseArrAttach a).mapM fun ⟨p, hp⟩ => producerFromJson p) (← jParseList fromJson? b)
    | "proj" => return .project (← fromJson? r) (← a.getStr?) (← fromJson? b)
    | "then" => return .«then» (← fromJson? r) (← fromJson? a) (← statementFromJson b)
    | other => .error s!"Consumer: unknown 4-element tag {other}"
  | _ => .error "Consumer: unexpected shape"
termination_by sizeOf j
decreasing_by
  all_goals simp_wf
  all_goals omega

def statementFromJson (j : Json) : Except String Statement := do
  match ← jParseArrAttach j with
  | [⟨tag, _⟩, ⟨a, ha⟩, ⟨b, _⟩] => match ← tag.getStr? with
    | "cut" => return .cut (← producerFromJson a) (← fromJson? b)
    | other => .error s!"Statement: unknown 3-element tag {other}"
  | [⟨tag, _⟩, ⟨r, _⟩, ⟨a, _⟩, ⟨b, _⟩] => match ← tag.getStr? with
    | "invoke" => return .invoke (← fromJson? r) (← fromJson? a) (← fromJson? b)
    | other => .error s!"Statement: unknown 4-element tag {other}"
  | [⟨tag, _⟩, ⟨r, _⟩, ⟨a, ha⟩, ⟨b, hb⟩, ⟨c, hc⟩] => match ← tag.getStr? with
    | "join" => return .join (← fromJson? r) (← fromJson? a) (← consumerFromJson b) (← statementFromJson c)
    | "prim" => return .primitive (← fromJson? r) (← a.getStr?) (← (← jParseArrAttach b).mapM fun ⟨p, hp⟩ => producerFromJson p) (← fromJson? c)
    | "extern" => return .externalCall (← fromJson? r) (← a.getStr?) (← (← jParseArrAttach b).mapM fun ⟨p, hp⟩ => producerFromJson p) (← fromJson? c)
    | "ifz" => return .ifz (← fromJson? r) (← producerFromJson a) (← statementFromJson b) (← statementFromJson c)
    | other => .error s!"Statement: unknown 5-element tag {other}"
  | [⟨tag, _⟩, ⟨r, _⟩, ⟨op, _⟩, ⟨l, hl⟩, ⟨rhs, hrhs⟩, ⟨c, _⟩] => match ← tag.getStr? with
    | "binop" => return .binOp (← fromJson? r) (← op.getStr?) (← producerFromJson l) (← producerFromJson rhs) (← fromJson? c)
    | other => .error s!"Statement: unknown 6-element tag {other}"
  | _ => .error "Statement: unexpected shape"
termination_by sizeOf j
decreasing_by
  all_goals simp_wf
  all_goals omega

def branchFromJson (j : Json) : Except String Branch := do
  match ← jParseArrAttach j with
  | [⟨tag, _⟩, ⟨r, _⟩, ⟨a, _⟩, ⟨b, hb⟩] => match ← tag.getStr? with
    | "branch" => return .branch (← fromJson? r) (← patternFromJson a) (← statementFromJson b)
    | other => .error s!"Branch: unknown tag {other}"
  | _ => .error "Branch: expected a 4-element array"
termination_by sizeOf j
decreasing_by
  all_goals simp_wf
  all_goals omega

end

instance : ToJson Producer := ⟨producerToJson⟩
instance : FromJson Producer := ⟨producerFromJson⟩
instance : ToJson Consumer := ⟨consumerToJson⟩
instance : FromJson Consumer := ⟨consumerFromJson⟩
instance : ToJson Statement := ⟨statementToJson⟩
instance : FromJson Statement := ⟨statementFromJson⟩
instance : ToJson Branch := ⟨branchToJson⟩
instance : FromJson Branch := ⟨branchFromJson⟩

instance : ToJson Join.Program where
  toJson p :=
    Json.arr #[
      jList (fun d =>
        Json.arr #[toJson d.range, toJson d.name, toJson d.ret, statementToJson d.body]) p.definitions,
      jList toJson p.dependencies ]

instance : FromJson Join.Program where
  fromJson? j := do
    match (← j.getArr?).toList with
    | [defsJ, depsJ] =>
      let definitions ← jParseList (fun e => do
        match (← e.getArr?).toList with
        | [r, name, ret, body] =>
          return ({ range := (← fromJson? r), name := (← fromJson? name), ret := (← fromJson? ret),
                     body := (← statementFromJson body) } : Join.Definition)
        | _ => .error "Program.definition: expected a 4-element array") defsJ
      return { definitions, dependencies := ← jParseList fromJson? depsJ }
    | _ => .error "Program: expected a 2-element array"

instance : Resource Join.Program where
  toBytes p := (toJson p).compress.toUTF8
  ofBytes b :=
    match String.fromUTF8? b with
    | none => .error "Join.Program: invalid UTF-8"
    | some s => Lean.Json.parse s >>= fromJson?

/-! ## Round-trip checks -/

section Guards
private def r0 : Range := ⟨SourcePos.initial "t", SourcePos.initial "t"⟩
private def nm (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }
private def ap : ArtifactPath :=
  { rawPath := "a/B.mlg", originPath := ⟨"/root/a/B.mlg"⟩, relPath := ⟨"a/B.mlg"⟩, targetPath := ⟨"/root/.w/a/B.mlg"⟩ }
private def artId : Name := { name := "x", moduleName := .artifact ap, sort := .internal 7 }

private def roundtrips [ToJson α] [FromJson α] [BEq α] (x : α) : Bool :=
  match fromJson? (toJson x) with
  | .ok y => x == y
  | .error _ => false

-- `Id` round-trips exactly, including `.artifact` module names (eval keys on `Id`).
#guard roundtrips artId
#guard roundtrips (nm "k")
#guard roundtrips (Literal.int32 (-5))
#guard roundtrips (Literal.int64 42)
#guard roundtrips (Literal.string "hi")
#guard roundtrips (Literal.char 'a')
#guard roundtrips (Literal.float 1.5)
#guard roundtrips (Literal.double 3.25)

-- `Producer`/`Consumer`/`Statement`/`Branch`/`Pattern` have no `BEq` (their
-- mutual `inductive`s derive none), so these check shape by pattern match
-- instead of `roundtrips`, mirroring the `prog`/`Resource` guards below.
-- Covers every previously-untested tag — a decode arity bug in one of these
-- (`.object`/`.cocase` misrouted to the wrong arity branch) was only caught
-- because a disk-based golden gate happened to exercise it (commit 90410d4b).
#guard match (fromJson? (toJson (Producer.construct r0 .tuple [.var r0 (nm "x")] [nm "k"])) : Except String Producer) with
  | .ok (.construct _ tag [.var _ name] [k]) => tag == .tuple && name == nm "x" && k == nm "k"
  | _ => false
#guard match (fromJson? (toJson (Producer.object r0 [("f", nm "x", .cut (.var r0 (nm "y")) (nm "k"))])) : Except String Producer) with
  | .ok (.object _ [(field, name, _)]) => field == "f" && name == nm "x"
  | _ => false
#guard match (fromJson? (toJson (Producer.lambda r0 [nm "x"] (.cut (.var r0 (nm "x")) (nm "k")))) : Except String Producer) with
  | .ok (.lambda _ [name] _) => name == nm "x"
  | _ => false
#guard match (fromJson? (toJson (Consumer.select r0 [.branch r0 (.pvar r0 (nm "x")) (.cut (.var r0 (nm "x")) (nm "k"))])) : Except String Consumer) with
  | .ok (.select _ [.branch _ (.pvar _ name) _]) => name == nm "x"
  | _ => false
#guard match (fromJson? (toJson (Statement.join r0 (nm "j") (.finish r0) (.cut (.var r0 (nm "x")) (nm "k")))) : Except String Statement) with
  | .ok (.join _ name (.finish _) _) => name == nm "j"
  | _ => false
#guard match (fromJson? (toJson (Statement.primitive r0 "add" [.var r0 (nm "x"), .var r0 (nm "y")] (nm "k"))) : Except String Statement) with
  | .ok (.primitive _ name [.var _ _, .var _ _] consumer) => name == "add" && consumer == nm "k"
  | _ => false
#guard match (fromJson? (toJson (Statement.externalCall r0 "puts" [.var r0 (nm "x")] (nm "k"))) : Except String Statement) with
  | .ok (.externalCall _ name [.var _ _] consumer) => name == "puts" && consumer == nm "k"
  | _ => false
#guard match (fromJson? (toJson (Statement.binOp r0 "+" (.var r0 (nm "x")) (.var r0 (nm "y")) (nm "k"))) : Except String Statement) with
  | .ok (.binOp _ op (.var _ l) (.var _ r) consumer) => op == "+" && l == nm "x" && r == nm "y" && consumer == nm "k"
  | _ => false
#guard match (fromJson? (toJson (Statement.ifz r0 (.var r0 (nm "c")) (.cut (.var r0 (nm "x")) (nm "k")) (.cut (.var r0 (nm "y")) (nm "k")))) : Except String Statement) with
  | .ok (.ifz _ (.var _ cond) (.cut (.var _ t) _) (.cut (.var _ e) _)) => cond == nm "c" && t == nm "x" && e == nm "y"
  | _ => false
-- Pattern round-trip: nests `.expand` inside `.destruct` to cover both tags.
#guard match (fromJson? (toJson (Pattern.destruct r0 .tuple [.expand r0 [("f", .pvar r0 (nm "x"))]])) : Except String Pattern) with
  | .ok (.destruct _ tag [.expand _ [(k, .pvar _ name)]]) => tag == .tuple && k == "f" && name == nm "x"
  | _ => false

-- A small Program round-trips.
private def prog : Join.Program :=
  { definitions := [{ range := r0, name := nm "main", ret := nm "ret",
                       body := .cut (.var r0 (nm "x")) (nm "ret") }],
    dependencies := [.moduleName "Builtin", .artifact ap] }
#guard match (fromJson? (toJson prog) : Except String Join.Program) with
  | .ok p => p.definitions.length == 1 && p.dependencies.length == 2
  | .error _ => false
-- Resource round-trip (bytes → Program).
#guard match Resource.ofBytes (Resource.toBytes prog) with
  | .ok (p : Join.Program) => p.dependencies.length == 2
  | .error _ => false
end Guards

end Malgo.Sequent.Core.Json
