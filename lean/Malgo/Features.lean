import Std.Data.TreeSet

/-! Port of `src/Malgo/Features.hs`. The `Features` effect becomes plain
data here; the mutable-state accessors live on `Ctx` in `Malgo.Monad`. -/

namespace Malgo

inductive Feature where
  | cStyleApply
  | malgo2025
  | experimental (name : String)
  deriving BEq, Ord, Repr

structure FeatureFlags where
  toSet : Std.TreeSet Feature := {}

namespace FeatureFlags

def empty : FeatureFlags := {}

def contains (flags : FeatureFlags) (f : Feature) : Bool :=
  flags.toSet.contains f

def union (a b : FeatureFlags) : FeatureFlags :=
  ⟨b.toSet.foldl (init := a.toSet) (·.insert ·)⟩

def ofList (fs : List Feature) : FeatureFlags :=
  ⟨fs.foldl (init := {}) (·.insert ·)⟩

end FeatureFlags

/-- Port of `parseFeature`; Haskell calls `error` on unknown names, the
Lean port surfaces it to the caller. -/
def parseFeature (name : String) : Except String Feature :=
  if name == "c-style-apply" then .ok .cStyleApply
  else if name == "malgo-2025" then .ok .malgo2025
  else if name.startsWith "experimental-" then
    .ok (.experimental (toString (name.drop "experimental-".length)))
  else .error s!"Unknown feature: {name}"

def parseFeatures (names : List String) : Except String FeatureFlags := do
  return FeatureFlags.ofList (← names.mapM parseFeature)

end Malgo
