import Malgo.Prelude
import Malgo.Module
import Malgo.Features

/-! Port of `src/Malgo/Monad.hs` and the effect stack it runs
(`Reader Flag / State Uniq / Features / State Pragma / Workspace / IOE`).
All mutable state lives in `IO.Ref`s inside `Ctx`; per-pass errors are
wrapped into `CompileError` by `Malgo.Pass.wrapError`. -/

namespace Malgo

/-- Uniform compile error: every pass renders its own error type into
this record (the Haskell existential is only ever shown). -/
structure CompileError where
  passName : String
  message : String
  range? : Option Range := none

def CompileError.render (e : CompileError) : String :=
  match e.range? with
  | some r => s!"{pretty r}: [{e.passName}] {e.message}"
  | none => s!"[{e.passName}] {e.message}"

structure Ctx where
  flag : Flag
  workspace : Workspace
  uniqRef : IO.Ref Nat
  featuresRef : IO.Ref FeatureFlags
  pragmaRef : IO.Ref Pragma

abbrev MalgoM := ReaderT Ctx (EIO CompileError)

namespace MalgoM

/-- Lift IO, converting unexpected IO errors into a `CompileError`. -/
def io (act : IO α) : MalgoM α := fun _ =>
  act.toEIO fun e => { passName := "io", message := toString e }

instance : MonadLift IO MalgoM := ⟨io⟩

def run (flag : Flag) (features : FeatureFlags) (act : MalgoM α) : IO α := do
  let ctx : Ctx := {
    flag
    workspace := ← Workspace.setup
    uniqRef := ← IO.mkRef 0
    featuresRef := ← IO.mkRef features
    pragmaRef := ← IO.mkRef {}
  }
  (ReaderT.run act ctx).toIO fun e => IO.userError e.render

end MalgoM

def getFlag : MalgoM Flag :=
  return (← read).flag

def getWorkspace : MalgoM Workspace :=
  return (← read).workspace

/-- Port of `getUniq`: the fresh-name supply. -/
def getUniq : MalgoM Nat := do
  (← read).uniqRef.modifyGet fun u => (u, u + 1)

def getFeatureFlags : MalgoM FeatureFlags := do
  (← read).featuresRef.get

def hasFeature (f : Feature) : MalgoM Bool := do
  return (← getFeatureFlags).contains f

def addFeatures (flags : FeatureFlags) : MalgoM Unit := do
  (← read).featuresRef.modify (·.union flags)

def isMalgo2025Enabled : MalgoM Bool :=
  hasFeature .malgo2025

def insertPragmas (name : ModuleName) (values : List String) : MalgoM Unit := do
  (← read).pragmaRef.modify (Pragma.insert name values)

end Malgo
