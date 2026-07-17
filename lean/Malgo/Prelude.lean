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

/-- Insert `(k, v)` into an assoc list kept in ascending key order.
Duplicate keys are not expected (the Haskell source uses `Data.Map`). -/
def insertAssocAscending [Ord κ] (x : κ × α) : List (κ × α) → List (κ × α)
  | [] => [x]
  | y :: ys => if compare x.1 y.1 == .gt then y :: insertAssocAscending x ys else x :: y :: ys

/-- Emulate `Data.Map.fromList`/`Map.toList`: an assoc list presented in
ascending key order. Used for the sequent IRs' `Object`/`Expand` fields,
which are `Map Text _` in Haskell but must be stored as assoc lists in
Lean (a `Std.TreeMap` cannot nest inside a recursive inductive). -/
def sortAssocAscending [Ord κ] (xs : List (κ × α)) : List (κ × α) :=
  xs.foldr insertAssocAscending []

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
