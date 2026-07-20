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
  | scheme
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
