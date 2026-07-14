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
