import Std.Data.TreeMap
import Malgo.Prelude
import Malgo.Id
import Malgo.Sequent.Fun

/-! Port of `src/Malgo/Sequent/SaturateCtor.hs`: a pure `Fun.Program →
Fun.Program` rewrite run at the very start of `ToCore.toCore` (before CPS
conversion). It recognizes curried-constructor definitions structurally and
rewrites a fully-(or over-)saturated call spine into a direct
`Fun.Construct`, leaving genuine partial applications untouched. Shared by
every backend and every direct caller of `toCore`. -/

namespace Malgo.Sequent.SaturateCtor

open Malgo.Sequent.Fun

/-- Structural test for a curried-constructor definition
(`\p1 -> .. -> \pn -> Construct tag [p1..pn]`). Also matches plain
tuple-returning functions of the same shape (sound to inline either way). -/
def peel (params : List Name) : Expr → Option (Tag × Nat)
  | .lambda _ [p] b => peel (params ++ [p]) b
  | .construct _ tag args =>
    if args.length == params.length
        && (args.zip params).all (fun (a, p) => match a with | .var _ n => n == p | _ => false)
    then some (tag, params.length) else none
  | _ => none

def recognizeDef : Range × Name × Expr → Option (Name × Tag × Nat)
  | (_, name, body) => (peel [] body).map (fun (tag, n) => (name, tag, n))

/-- Unroll an application spine outside-in, preserving left-to-right
argument order: `Apply (Apply f [a1]) [a2]` becomes `(f, [a1, a2])`. -/
def unrollSpine : Expr → List Expr → Expr × List Expr
  | .apply _ fn args, acc => unrollSpine fn (args ++ acc)
  | expr, acc => (expr, acc)

/-- Wrapping `start` in a left-to-right chain of single-argument `Apply`s
(as `trySaturate` does for its `extra` re-application) is exactly what
`unrollSpine` undoes: it peels every `Apply` layer the chain added and
lands back on `start`, with the chain's arguments prepended to `acc` in
original order. Holds regardless of `start`'s own shape — if `start` is
itself `Apply`-headed, `unrollSpine` simply keeps unrolling into it. -/
theorem unrollSpine_foldl_apply (r : Range) (start : Expr) :
    ∀ (extra acc : List Expr),
      unrollSpine (extra.foldl (fun f a => Expr.apply r f [a]) start) acc =
        unrollSpine start (extra ++ acc)
  | [], acc => rfl
  | a :: rest, acc => by
    show unrollSpine (rest.foldl (fun f a => Expr.apply r f [a]) (Expr.apply r start [a])) acc = _
    rw [unrollSpine_foldl_apply r (Expr.apply r start [a]) rest acc]
    simp [unrollSpine, List.cons_append]

/-- A saturated/over-saturated call of a recognized constructor becomes a
`Construct` (with any excess arguments re-applied). Under-saturated calls
stay as-is (`none`). -/
def trySaturate (ctorTable : Std.TreeMap Name (Tag × Nat)) (expr : Expr) : Option Expr :=
  let (base, args) := unrollSpine expr []
  match base with
  | .invoke _ name =>
    match ctorTable.get? name with
    | some (tag, arity) =>
      if args.length ≥ arity then
        let ctorArgs := args.take arity
        let extra := args.drop arity
        let built := Expr.construct expr.range tag ctorArgs
        some (extra.foldl (fun f a => Expr.apply expr.range f [a]) built)
      else none
    | none => none
  | _ => none

/-- Whenever `trySaturate` fires, its output is always a `Construct` (with
any excess arguments re-applied around it) — never a bare `Invoke` spine.
The exact shape doesn't matter to callers; what matters is that
`unrollSpine`'s base is `Construct`, so `trySaturate` can never fire a
second time on the result (see `trySaturate_result_none` below). -/
theorem trySaturate_some_shape {ctorTable : Std.TreeMap Name (Tag × Nat)} {expr rewritten : Expr}
    (h : trySaturate ctorTable expr = some rewritten) :
    ∃ (tag : Tag) (ctorArgs extra : List Expr),
      rewritten =
        extra.foldl (fun f a => Expr.apply expr.range f [a])
          (Expr.construct expr.range tag ctorArgs) := by
  unfold trySaturate at h
  split at h
  next base args heq =>
    split at h
    · next name _ =>
      split at h
      · next tag arity _ =>
        split at h
        · next hlen =>
          simp only [Option.some.injEq] at h
          exact ⟨tag, args.take arity, args.drop arity, h.symm⟩
        · simp at h
      · simp at h
    · simp at h

/-- `trySaturate` never fires twice: whatever it produces already has a
`Construct` (not `Invoke`) at the head of its call spine, so applying
`trySaturate` again is a no-op. -/
theorem trySaturate_result_none {ctorTable : Std.TreeMap Name (Tag × Nat)} {expr rewritten : Expr}
    (h : trySaturate ctorTable expr = some rewritten) :
    trySaturate ctorTable rewritten = none := by
  obtain ⟨tag, ctorArgs, extra, rfl⟩ := trySaturate_some_shape h
  unfold trySaturate
  rw [unrollSpine_foldl_apply]
  simp [unrollSpine]

mutual

def goExpr (ctorTable : Std.TreeMap Name (Tag × Nat)) (expr : Expr) : Expr :=
  let expr' := match expr with
    | .var .. => expr
    | .literal .. => expr
    | .construct r tag args => .construct r tag (args.map (goExpr ctorTable))
    | .«let» r n v b => .«let» r n (goExpr ctorTable v) (goExpr ctorTable b)
    | .lambda r ps b => .lambda r ps (goExpr ctorTable b)
    | .object r fields =>
      .object r (fields.attach.map fun ⟨kv, hkv⟩ => (kv.1, goExpr ctorTable kv.2))
    | .apply r fn args => .apply r (goExpr ctorTable fn) (args.map (goExpr ctorTable))
    | .project r e field => .project r (goExpr ctorTable e) field
    | .primitive r op args => .primitive r op (args.map (goExpr ctorTable))
    | .select r s branches => .select r (goExpr ctorTable s) (branches.map (goBranch ctorTable))
    | .invoke .. => expr
    | .fix r n b => .fix r n (goExpr ctorTable b)
  match trySaturate ctorTable expr' with
  | some rewritten => rewritten
  | none => expr'
termination_by sizeOf expr
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_lt_of_mem hkv) (by omega)
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def goBranch (ctorTable : Std.TreeMap Name (Tag × Nat)) : Branch → Branch
  | .branch r p b => .branch r p (goExpr ctorTable b)
termination_by b => sizeOf b

end

/-- The shape every `goExpr` equation ends with: apply `trySaturate` to
some already-computed `expr'` and use it if it fired, else keep `expr'`
as-is. Isolated as its own lemma (quantified over an arbitrary `expr'`)
so the proof doesn't need to case on what `expr'` actually is — that's
exactly what makes `goExpr_trySaturate_none` below not need induction on
`expr`'s shape. -/
theorem trySaturate_match_none (ctorTable : Std.TreeMap Name (Tag × Nat)) (expr' : Expr) :
    trySaturate ctorTable
        (match trySaturate ctorTable expr' with
          | some rewritten => rewritten
          | none => expr') =
      none := by
  cases h : trySaturate ctorTable expr' with
  | none => exact h
  | some rewritten => exact trySaturate_result_none h

/-- The invariant `saturateProgram` is meant to establish: after
`goExpr`, no fully-(or over-)saturated curried-constructor call spine
remains anywhere in the tree. Stated as "one more pass of `trySaturate`
never fires" rather than induction on `expr`'s shape — `goExpr`'s
recursive processing of `expr`'s children only decides what `expr'` is;
`trySaturate_match_none` already covers both of what happens next
regardless. -/
theorem goExpr_trySaturate_none (ctorTable : Std.TreeMap Name (Tag × Nat)) (expr : Expr) :
    trySaturate ctorTable (goExpr ctorTable expr) = none := by
  unfold goExpr
  exact trySaturate_match_none ctorTable _

/-- The constructor table `saturateProgram` builds from a program's own
recognized curried-constructor definitions — factored out so the
invariant below can refer to the exact same table `saturateProgram`
uses internally. -/
def ctorTableOf (program : Program) : Std.TreeMap Name (Tag × Nat) :=
  (program.definitions.filterMap recognizeDef).foldl (fun m (name, tp) => m.insert name tp) {}

def saturateProgram (program : Program) : Program :=
  { program with
    definitions := program.definitions.map (fun (r, n, b) => (r, n, goExpr (ctorTableOf program) b)) }

/-- The invariant #359 tracks: at the root of each definition body,
`saturateProgram`'s output has no remaining fully-(or over-)saturated
curried-constructor call spine. (The stronger tree-wide fact — no
saturated spine survives anywhere within `b`, not just at its root — is
already available for free from `goExpr_trySaturate_none`, universally
quantified over `expr`; instantiate it directly at any subexpression of
`b` if a later proof needs that form instead.) -/
theorem saturateProgram_no_saturated_spine (program : Program) {r : Range} {n : Name} {b : Expr}
    (hmem : (r, n, b) ∈ (saturateProgram program).definitions) :
    trySaturate (ctorTableOf program) b = none := by
  unfold saturateProgram at hmem
  simp only [List.mem_map, Prod.mk.injEq] at hmem
  obtain ⟨⟨r', n', b'⟩, _, rfl, rfl, rfl⟩ := hmem
  exact goExpr_trySaturate_none (ctorTableOf program) b'

/-! Sanity check: a saturated `Cons a b` call (via `Invoke`) collapses into
a direct `Construct`. -/
section Test
private def r0 : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
private def extId (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }
private def testProg : Program :=
  { definitions :=
      [ (r0, extId "Cons",
          .lambda r0 [extId "x"] (.lambda r0 [extId "y"]
            (.construct r0 (.tag "Cons") [.var r0 (extId "x"), .var r0 (extId "y")]))),
        (r0, extId "main",
          .apply r0 (.apply r0 (.invoke r0 (extId "Cons")) [.var r0 (extId "a")])
            [.var r0 (extId "b")]) ],
    dependencies := [] }

#guard
  match (saturateProgram testProg).definitions with
  | [_, (_, _, body)] => Malgo.sShow body == "(Cons (a b))"
  | _ => false
end Test

end Malgo.Sequent.SaturateCtor
