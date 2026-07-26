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

/-! ## Checks (port of `test/Malgo/FeaturesSpec.hs`)

Haskell wraps these in the `Features` effect and asserts through `runEff`;
here the flags are plain data (see this file's header), so membership is a
direct `contains`. `parseFeature`'s unknown-name case is the one real
behavioural difference: Haskell `error`s, the port returns `.error`, so the
Haskell spec's `shouldThrow anyErrorCall` becomes a check on the `Except`. -/

private def fCStyle : FeatureFlags := .ofList [.cStyleApply]
private def fWithExp : FeatureFlags := .ofList [.cStyleApply, .experimental "X"]
private def fMalgo2025 : FeatureFlags := .ofList [.malgo2025]

#guard fCStyle.contains .cStyleApply
#guard !(fCStyle.contains (.experimental "X"))
#guard fWithExp.contains (.experimental "X")
#guard fWithExp.contains .cStyleApply

-- `malgo-2025` is its own feature, not an `experimental-` name.
#guard fMalgo2025.contains .malgo2025
#guard !(fCStyle.contains .malgo2025)

-- `FeatureFlags` has no `BEq` (it wraps a `TreeSet`), so parse results are
-- compared by membership rather than structurally.
#guard (match parseFeatures ["c-style-apply", "experimental-X"] with
  | .ok f => f.contains .cStyleApply && f.contains (.experimental "X") && !(f.contains .malgo2025)
  | .error _ => false)
#guard (match parseFeatures ["malgo-2025"] with
  | .ok f => f.contains .malgo2025 && !(f.contains .cStyleApply)
  | .error _ => false)

-- Unknown names must be rejected, not silently ignored.
#guard (match parseFeatures ["unknown-feature"] with | .error _ => true | .ok _ => false)

end Malgo
