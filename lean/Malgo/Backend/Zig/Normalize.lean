import Malgo.Prelude
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Fun

/-! Port of `src/Malgo/Backend/Zig/Normalize.hs`: eliminate two forms of
pure aliasing from Join IR before closure conversion.

  * `Cut (Mu x s) k` binds `x` to the covalue of `k` and runs `s` inline
    (Eval's special case for producer-position `Mu`). Substituting `x := k`
    throughout `s` is equivalent and removes `Mu` entirely.
  * `Join m (Label j) s` makes `m` a pure alias for `j`. Substituting
    `m := j` throughout `s` removes the indirection.

Both eliminations are capture-avoiding for free because every `Id` in the
pipeline is already globally unique (same invariant as the Haskell). -/

namespace Malgo.Backend.Zig.Normalize

open Malgo.Sequent.Core.Join
open Malgo.Sequent.Fun (Name)

private def dummyRange : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
private def dummyName : Name := { name := "", moduleName := .moduleName "", sort := .external }

instance : Inhabited Producer := ⟨.literal dummyRange (.int32 0)⟩
instance : Inhabited Statement := ⟨.cut default dummyName⟩
instance : Inhabited Consumer := ⟨.finish dummyRange⟩
instance : Inhabited Branch := ⟨.branch dummyRange (.pvar dummyRange dummyName) default⟩

private def substName (src tgt n : Name) : Name :=
  if n == src then tgt else n

mutual

/-- Substitute one `Name` for another throughout a `Statement`. -/
def substStatement (src tgt : Name) : Statement → Statement
  | .cut p k => .cut (substProducer src tgt p) (substName src tgt k)
  -- `name` is a fresh binder here; it can never equal `src` because `src`
  -- was already bound (and eliminated) further out.
  | .join range name consumer stmt =>
    .join range name (substConsumer src tgt consumer) (substStatement src tgt stmt)
  | .primitive range name ps k =>
    .primitive range name (ps.map (substProducer src tgt)) (substName src tgt k)
  | .invoke range name k => .invoke range name (substName src tgt k)
  | .externalCall range name ps k =>
    .externalCall range name (ps.map (substProducer src tgt)) (substName src tgt k)
  | .binOp range op lhs rhs k =>
    .binOp range op (substProducer src tgt lhs) (substProducer src tgt rhs) (substName src tgt k)
  | .ifz range cond t e =>
    .ifz range (substProducer src tgt cond) (substStatement src tgt t) (substStatement src tgt e)
termination_by s => sizeOf s

def substProducer (src tgt : Name) : Producer → Producer
  | .var range n => .var range (substName src tgt n)
  | .literal range lit => .literal range lit
  | .construct range tag ps ks =>
    .construct range tag (ps.map (substProducer src tgt)) (ks.map (substName src tgt))
  | .lambda range names stmt => .lambda range names (substStatement src tgt stmt)
  | .object range fields =>
    .object range (fields.attach.map fun ⟨krs, hkrs⟩ =>
      (krs.1, krs.2.1, substStatement src tgt krs.2.2))
  | .mu range name stmt => .mu range name (substStatement src tgt stmt)
  | .cocase range branches =>
    .cocase range (branches.attach.map fun ⟨dvs, hdvs⟩ =>
      (dvs.1, dvs.2.1, substStatement src tgt dvs.2.2))
termination_by p => sizeOf p
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_snd_lt_of_mem hkrs) (by omega)
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_snd_lt_of_mem hdvs) (by omega)
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def substConsumer (src tgt : Name) : Consumer → Consumer
  | .label range n => .label range (substName src tgt n)
  | .apply range ps ks =>
    .apply range (ps.map (substProducer src tgt)) (ks.map (substName src tgt))
  | .project range field k => .project range field (substName src tgt k)
  | .«then» range name stmt => .«then» range name (substStatement src tgt stmt)
  | .finish range => .finish range
  | .select range branches => .select range (branches.map (substBranch src tgt))
  | .destructor range name ps k =>
    .destructor range name (ps.map (substProducer src tgt)) (substName src tgt k)
termination_by c => sizeOf c

def substBranch (src tgt : Name) : Branch → Branch
  | .branch range pat stmt => .branch range pat (substStatement src tgt stmt)
termination_by b => sizeOf b

end

mutual

/-- Eliminate `Mu` (under `Cut`) and `Label`-forwarding `Join` bindings from
a `Statement`, bottom-up. -/
partial def normalizeStatement : Statement → Statement
  | .cut p k =>
    match normalizeProducer p with
    | .mu _ x s => normalizeStatement (substStatement x k s)
    | p' => .cut p' k
  | .join range name consumer stmt =>
    match normalizeConsumer consumer with
    | .label _ j => normalizeStatement (substStatement name j stmt)
    | consumer' => .join range name consumer' (normalizeStatement stmt)
  | .primitive range name ps k => .primitive range name (ps.map normalizeProducer) k
  | .invoke range name k => .invoke range name k
  | .externalCall range name ps k => .externalCall range name (ps.map normalizeProducer) k
  | .binOp range op lhs rhs k => .binOp range op (normalizeProducer lhs) (normalizeProducer rhs) k
  | .ifz range cond t e => .ifz range (normalizeProducer cond) (normalizeStatement t) (normalizeStatement e)

partial def normalizeProducer : Producer → Producer
  | .var range n => .var range n
  | .literal range lit => .literal range lit
  | .construct range tag ps ks => .construct range tag (ps.map normalizeProducer) ks
  | .lambda range names stmt => .lambda range names (normalizeStatement stmt)
  | .object range fields =>
    .object range (fields.map (fun (k, ret, s) => (k, ret, normalizeStatement s)))
  | .mu range name stmt => .mu range name (normalizeStatement stmt)
  | .cocase range branches =>
    .cocase range (branches.map (fun (d, vs, s) => (d, vs, normalizeStatement s)))

partial def normalizeConsumer : Consumer → Consumer
  | .label range n => .label range n
  | .apply range ps ks => .apply range (ps.map normalizeProducer) ks
  | .project range field k => .project range field k
  | .«then» range name stmt => .«then» range name (normalizeStatement stmt)
  | .finish range => .finish range
  | .select range branches => .select range (branches.map normalizeBranch)
  | .destructor range name ps k => .destructor range name (ps.map normalizeProducer) k

partial def normalizeBranch : Branch → Branch
  | .branch range pat stmt => .branch range pat (normalizeStatement stmt)

end

section Test
private def r0 : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
private def nm (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }

-- `Cut (Mu x s) k` collapses to `s[x := k]`.
#guard Malgo.sShow (normalizeStatement
    (.cut (.mu r0 (nm "x") (.cut (.var r0 (nm "y")) (nm "x"))) (nm "k")))
  == "(cut y k)"
-- `Join m (Label j) s` collapses to `s[m := j]`.
#guard Malgo.sShow (normalizeStatement
    (.join r0 (nm "m") (.label r0 (nm "j")) (.cut (.var r0 (nm "y")) (nm "m"))))
  == "(cut y j)"
-- A non-forwarding `Join` is preserved.
#guard Malgo.sShow (normalizeStatement
    (.join r0 (nm "m") (.finish r0) (.cut (.var r0 (nm "y")) (nm "m"))))
  == "(join m finish (cut y m))"
end Test

end Malgo.Backend.Zig.Normalize
