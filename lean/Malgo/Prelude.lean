import Std.Data.TreeSet

/-! Port of `src/Malgo/Prelude.hs`: source positions, ranges, the `Pretty`
class, and the compiler-wide `Flag` record. The Haskell module is mostly
re-exports; only the project-defined pieces are ported. -/

namespace Malgo

/-- Port of megaparsec `SourcePos`. `line`/`column` are 1-based. -/
structure SourcePos where
  sourceName : String
  line : Nat
  column : Nat
  deriving BEq, Ord, Repr, DecidableEq

namespace SourcePos

/-- Port of megaparsec `initialPos`. -/
def initial (name : String) : SourcePos :=
  { sourceName := name, line := 1, column := 1 }

def min (a b : SourcePos) : SourcePos :=
  if compare a b == .gt then b else a

def max (a b : SourcePos) : SourcePos :=
  if compare a b == .lt then b else a

end SourcePos

/-- Range of a token. Field names follow the Haskell `_start`/`_end`. -/
structure Range where
  start : SourcePos
  stop : SourcePos
  deriving BEq, Ord, Repr, DecidableEq

namespace Range

/-- `Semigroup Range`: min of starts, max of ends. -/
def append (a b : Range) : Range :=
  { start := a.start.min b.start, stop := a.stop.max b.stop }

instance : Append Range := ⟨append⟩

end Range

class HasRange (α : Type u) where
  range : α → Range

export HasRange (range)

instance : HasRange Range := ⟨id⟩

instance : HasRange Empty := ⟨fun e => nomatch e⟩

/-- Insert `(k, v)` into an assoc list kept in ascending key order. An
existing equal key wins: `sortAssocAscending` folds from the right, so the
LAST occurrence in the source list is inserted first and kept — matching
`Data.Map.fromList`'s last-occurrence-wins semantics for duplicates. -/
def insertAssocAscending [Ord κ] (x : κ × α) : List (κ × α) → List (κ × α)
  | [] => [x]
  | y :: ys =>
    match compare x.1 y.1 with
    | .gt => y :: insertAssocAscending x ys
    | .eq => y :: ys
    | .lt => x :: y :: ys

/-- Emulate `Data.Map.fromList`/`Map.toList`: an assoc list presented in
ascending key order. Used for the sequent IRs' `Object`/`Expand` fields,
which are `Map Text _` in Haskell but must be stored as assoc lists in
Lean (a `Std.TreeMap` cannot nest inside a recursive inductive). -/
def sortAssocAscending [Ord κ] (xs : List (κ × α)) : List (κ × α) :=
  xs.foldr insertAssocAscending []

private theorem mem_insertAssocAscending {κ α : Type} [Ord κ] (x : κ × α) :
    ∀ (ys : List (κ × α)) {y}, y ∈ insertAssocAscending x ys → y = x ∨ y ∈ ys
  | [], y, h => by
    simp only [insertAssocAscending, List.mem_singleton] at h
    exact Or.inl h
  | y' :: ys, y, h => by
    simp only [insertAssocAscending] at h
    cases hc : compare x.1 y'.1 with
    | gt =>
      rw [hc] at h
      cases h with
      | head => exact Or.inr (List.mem_cons.mpr (Or.inl rfl))
      | tail _ h =>
        cases mem_insertAssocAscending x ys h with
        | inl h' => exact Or.inl h'
        | inr h' => exact Or.inr (List.mem_cons.mpr (Or.inr h'))
    | eq =>
      rw [hc] at h
      cases h with
      | head => exact Or.inr (List.mem_cons.mpr (Or.inl rfl))
      | tail _ h => exact Or.inr (List.mem_cons.mpr (Or.inr h))
    | lt =>
      rw [hc] at h
      cases h with
      | head => exact Or.inl rfl
      | tail _ h => exact Or.inr h

/-- `sortAssocAscending` only ever keeps or drops elements of its input
(dropping shadowed duplicate keys) — it never invents a new pair. Lets a
termination proof recurse into a `sortAssocAscending`-filtered field
exactly as if it had recursed into the field directly. -/
theorem mem_sortAssocAscending {κ α : Type} [Ord κ] {xs : List (κ × α)} {p : κ × α}
    (h : p ∈ sortAssocAscending xs) : p ∈ xs := by
  induction xs with
  | nil => simp [sortAssocAscending] at h
  | cons x xs ih =>
    simp only [sortAssocAscending, List.foldr_cons] at h
    cases mem_insertAssocAscending x (xs.foldr insertAssocAscending []) h with
    | inl h' => exact List.mem_cons.mpr (Or.inl h')
    | inr h' => exact List.mem_cons.mpr (Or.inr (ih h'))

/-! ## `Std.TreeSet` size/membership helpers

`Std.TreeSet`/`TreeMap` expose no direct "membership-subset implies size
monotonicity" lemma (`s ⊆ t → s.size ≤ t.size`), and no `TreeSet`-level
extensionality principle usable without `LawfulEqCmp` (which a key type
like `ModuleName` can genuinely fail — two `ModuleName.artifact` values
with the same `relPath` but different other `ArtifactPath` fields compare
`.eq` without being `=`). Both facts below are derived purely from
existing size/diff/inter identities (`isEmpty_diff_iff`,
`size_diff_add_size_inter_eq_size_left`, `size_inter_le_size_right`,
`size_erase`), so they need only `[Std.TransCmp cmp]`. -/

theorem Std.TreeSet.size_le_of_forall_mem {α : Type} {cmp : α → α → Ordering}
    [Std.TransCmp cmp] {s t : Std.TreeSet α cmp} (h : ∀ x, x ∈ s → x ∈ t) :
    s.size ≤ t.size := by
  have hempty : (s \ t).isEmpty = true := Std.TreeSet.isEmpty_diff_iff.mpr h
  have hsize0 : (s \ t).size = 0 := by
    have hb := Std.TreeSet.isEmpty_eq_size_eq_zero (t := s \ t)
    rw [hempty] at hb
    simpa using hb.symm
  have heq := Std.TreeSet.size_diff_add_size_inter_eq_size_left (t₁ := s) (t₂ := t)
  rw [hsize0, Nat.zero_add] at heq
  calc s.size = (s ∩ t).size := heq.symm
    _ ≤ t.size := Std.TreeSet.size_inter_le_size_right

/-- Strict version: needed whenever a decreasing measure is a `TreeSet`
size that must shrink by at least the one known witness `x`. -/
theorem Std.TreeSet.size_lt_of_forall_mem_of_not_mem {α : Type} {cmp : α → α → Ordering}
    [Std.TransCmp cmp] {s t : Std.TreeSet α cmp} {x : α}
    (h : ∀ y, y ∈ s → y ∈ t) (hx : x ∈ t) (hxs : x ∉ s) :
    s.size < t.size := by
  have hle : s.size ≤ (t.erase x).size := by
    apply Std.TreeSet.size_le_of_forall_mem
    intro y hy
    rw [Std.TreeSet.mem_erase]
    refine ⟨?_, h y hy⟩
    intro hceq
    exact hxs ((Std.TreeSet.mem_congr hceq).mpr hy)
  have hcontains : t.contains x = true := Std.TreeSet.mem_iff_contains.mp hx
  have herase : (t.erase x).size < t.size := by
    rw [Std.TreeSet.size_erase, if_pos hcontains]
    have hpos : 0 < t.size := by
      rcases Nat.eq_zero_or_pos t.size with h0 | hpos
      · exfalso
        have hempty : t.isEmpty = true := by
          simp [Std.TreeSet.isEmpty_eq_size_eq_zero, h0]
        rw [Std.TreeSet.isEmpty_iff_forall_not_mem] at hempty
        exact hempty x hx
      · exact hpos
    omega
  omega

/-- Membership in a `cond`-filtered insert-`foldl` (the
`depsOf.foldl (init:={}) fun s m ds => if cond m ds then s.insert m else
s` shape `Query.Engine.reverseDepClosureGo` and similar passes build a
result set with) implies membership in the seed set or a witness pair
from the source list — analogous to `mem_sortAssocAscending` above, but
for `Std.TreeMap.foldl`/`Std.TreeSet.insert` instead of `List`. -/
theorem mem_foldl_filter_insert {α β : Type} {cmp : α → α → Ordering} [Std.TransCmp cmp]
    (cond : α × β → Bool) (x : α) :
    ∀ (xs : List (α × β)) (s0 : Std.TreeSet α cmp),
      x ∈ xs.foldl (fun s p => if cond p then s.insert p.1 else s) s0 →
      x ∈ s0 ∨ ∃ p ∈ xs, cond p = true ∧ cmp p.1 x = .eq
  | [], s0, h => Or.inl h
  | p :: rest, s0, h => by
    simp only [List.foldl_cons] at h
    rcases mem_foldl_filter_insert cond x rest _ h with h1 | h2
    · by_cases hc : cond p = true
      · simp only [hc, if_true] at h1
        rw [Std.TreeSet.mem_insert] at h1
        rcases h1 with h1 | h1
        · exact Or.inr ⟨p, List.mem_cons_self .., hc, h1⟩
        · exact Or.inl h1
      · simp only [hc] at h1
        exact Or.inl h1
    · obtain ⟨q, hq, hqc, hqx⟩ := h2
      exact Or.inr ⟨q, List.mem_cons_of_mem _ hq, hqc, hqx⟩

theorem mem_foldl_insert_of_mem_init {α β : Type} {cmp : α → α → Ordering} [Std.TransCmp cmp] :
    ∀ (xs : List (α × β)) (s0 : Std.TreeSet α cmp) (x : α), x ∈ s0 →
      x ∈ xs.foldl (fun s p => s.insert p.1) s0
  | [], s0, x, h => h
  | p :: rest, s0, x, h => by
    simp only [List.foldl_cons]
    exact mem_foldl_insert_of_mem_init rest (s0.insert p.1) x
      (by rw [Std.TreeSet.mem_insert]; exact Or.inr h)

/-- Companion to `mem_foldl_filter_insert`: a plain (unfiltered)
insert-`foldl` carries every key from its source list forward. -/
theorem mem_foldl_insert_forward {α β : Type} {cmp : α → α → Ordering} [Std.TransCmp cmp]
    (x : α) :
    ∀ (xs : List (α × β)) (s0 : Std.TreeSet α cmp) (p : α × β), p ∈ xs → cmp p.1 x = .eq →
      x ∈ xs.foldl (fun s p => s.insert p.1) s0
  | p :: rest, s0, q, hq, heq => by
    rcases List.mem_cons.mp hq with hq | hq
    · simp only [List.foldl_cons]
      apply mem_foldl_insert_of_mem_init rest (s0.insert p.1) x
      rw [Std.TreeSet.mem_insert]
      exact Or.inl (hq ▸ heq)
    · simp only [List.foldl_cons]
      exact mem_foldl_insert_forward x rest (s0.insert p.1) q hq heq

/-! ## `sizeOf`/termination helpers

Reused across every hand-written `termination_by`/`decreasing_by` proof
for a `partial def` whose recursion is hidden behind a pair/triple
destructuring lambda (`fun (k, v) => ...`) or an opaque function call
before a `.map` (`sortAssocAscending`/`NEList.toList`) — both patterns
recur throughout the sequent IRs' `object`/`record`/`cocase`-style
named-fields lists. -/

/-- A pair's second component is never bigger than the pair itself. -/
theorem sizeOf_snd_le {α β : Type} [SizeOf α] [SizeOf β] (p : α × β) :
    sizeOf p.snd ≤ sizeOf p := by
  cases p with
  | mk a b => simp

theorem sizeOf_snd_lt_of_mem {α β : Type} [SizeOf α] [SizeOf β] {p : α × β}
    {l : List (α × β)} (h : p ∈ l) : sizeOf p.snd < sizeOf l :=
  Nat.lt_of_le_of_lt (sizeOf_snd_le p) (List.sizeOf_lt_of_mem h)

/-- Composed via `Nat.le_trans` for the third component of a triple. -/
theorem sizeOf_snd_snd_lt_of_mem {α β γ : Type} [SizeOf α] [SizeOf β] [SizeOf γ]
    {p : α × β × γ} {l : List (α × β × γ)} (h : p ∈ l) : sizeOf p.2.2 < sizeOf l :=
  Nat.lt_of_le_of_lt (Nat.le_trans (sizeOf_snd_le p.2) (sizeOf_snd_le p)) (List.sizeOf_lt_of_mem h)

theorem sizeOf_fst_le {α β : Type} [SizeOf α] [SizeOf β] (p : α × β) :
    sizeOf p.fst ≤ sizeOf p := by
  cases p with
  | mk a b => simp <;> omega

theorem sizeOf_fst_lt_of_mem {α β : Type} [SizeOf α] [SizeOf β] {p : α × β}
    {l : List (α × β)} (h : p ∈ l) : sizeOf p.fst < sizeOf l :=
  Nat.lt_of_le_of_lt (sizeOf_fst_le p) (List.sizeOf_lt_of_mem h)

/-- Two-level version of `sizeOf_snd_lt_of_mem`: for a doubly-nested field
like `List (String × List α)`, bounds an element of the INNER list
against the OUTER list's size. -/
theorem sizeOf_lt_of_mem_snd_of_mem {α γ : Type} [SizeOf α] [SizeOf γ]
    {x : γ} {p : α × List γ} {l : List (α × List γ)} (hx : x ∈ p.snd) (hp : p ∈ l) :
    sizeOf x < sizeOf l :=
  Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem hx)
    (Nat.le_trans (sizeOf_snd_le p) (Nat.le_of_lt (List.sizeOf_lt_of_mem hp)))

/-- Haskell `Data.List.NonEmpty`. -/
structure NEList (α : Type u) where
  head : α
  tail : List α
  deriving BEq, Ord, Repr

namespace NEList

def toList (xs : NEList α) : List α := xs.head :: xs.tail

def map (f : α → β) (xs : NEList α) : NEList β := ⟨f xs.head, xs.tail.map f⟩

def singleton (a : α) : NEList α := ⟨a, []⟩

def length (xs : NEList α) : Nat := 1 + xs.tail.length

def ofList : List α → Option (NEList α)
  | [] => none
  | x :: xs => some ⟨x, xs⟩

instance [Inhabited α] : Inhabited (NEList α) := ⟨⟨default, []⟩⟩

end NEList

/-- `NEList.toList`'s recursion target — every element is either the head
or in the tail, both strictly smaller than the whole `NEList`. -/
theorem sizeOf_lt_of_mem_toList {α : Type} [SizeOf α] {x : α} {xs : NEList α}
    (h : x ∈ xs.toList) : sizeOf x < sizeOf xs := by
  cases xs with
  | mk head tail =>
    simp only [NEList.toList, List.mem_cons] at h
    cases h with
    | inl h => subst h; simp; omega
    | inr h => exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by simp)

/-- Bounds the combined size of an `NEList`'s two projections — needed when
a caller destructures `xs` into `xs.head`/`xs.tail` and hands both to a
function whose own termination measure sums its two arguments. -/
theorem sizeOf_head_add_tail_lt {α : Type} [SizeOf α] (xs : NEList α) :
    sizeOf xs.head + sizeOf xs.tail < sizeOf xs := by
  cases xs with
  | mk head tail => simp

/-- Replaces Haskell's `prettyprinter`-based `Pretty`. Rendering is plain
`String`; layout combinators are introduced only if a golden demands them. -/
class Pretty (α : Type u) where
  pretty : α → String

export Pretty (pretty)

instance : Pretty String := ⟨id⟩
instance : Pretty Nat := ⟨toString⟩
instance : Pretty Int := ⟨toString⟩

instance : Pretty Range where
  pretty r :=
    s!"{r.start.sourceName}: line {r.start.line}, column {r.start.column}" ++
    s!" - line {r.stop.line}, column {r.stop.column}"

/-- Compilation target backend. -/
inductive Target where
  | eval
  | zig
  deriving BEq, Repr

/-- Evaluation mode (small-step CPS or big-step). -/
inductive EvalMode where
  | smallStep
  | bigStep
  deriving BEq, Repr

structure Flag where
  noOptimize : Bool
  lambdaLift : Bool
  debugMode : Bool
  testMode : Bool
  target : Target
  evalMode : EvalMode
  useInfer : Bool
  programArgs : List String
  deriving Repr

end Malgo
