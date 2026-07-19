import Malgo.Driver
import Malgo.Debug.PrettyIR

/-! Port of `src/Malgo/Debug/Pipeline.hs`: drives a single `.mlg` file through
the whole compilation pipeline — Parse through the Zig backend's Reuse pass —
capturing every stage's rendered text (`Malgo.Debug.PrettyIR`). Used by the
MET (M-exp-Tracer) debug tool and by its golden tests.

Mirrors the exact pass sequence `Malgo.Query.Engine`'s `LinkedProgram` handler
and `Malgo.Backend.Zig` run in production, just without discarding the
intermediate values. Only the target module's own definitions are traced (no
Builtin/Prelude linking): linking in every dependency's definitions would
bury the interesting diff in Prelude noise, and each pass rewrites
definitions independently anyway. -/

namespace Malgo.Debug.Pipeline

open Malgo

/-- One pipeline stage: its label and its Malgo-ish rendered text. -/
structure Stage where
  name : String
  rendered : String

/-- Haskell's `Pipeline.hs` builds a fresh `Query.Database` per `runTrace`
call too, but its `Query.Engine` first checks an ON-DISK `.mlgi` cache
(`tryLoadInterfaceFromDisk`) before ever re-renaming a dependency — since
`test/Spec.hs`'s fixtures already persisted Builtin/Prelude's `.mlgi` before
any golden test runs, `runTrace` never actually renames them, so the traced
module's own uniqs start at 0. The Lean query engine doesn't (yet, M7) port
on-disk `.mlgi` reuse, so seeding a brand-new, empty `InterfaceCache` here
(as an earlier version of this file did) makes `loadInterface` genuinely
re-rename Builtin+Prelude from scratch on every call, burning ~800+ uniqs
from the SAME run's counter before the traced module's own definitions are
renamed — confirmed empirically: a golden mismatch where every uniq was
offset by a constant, structure otherwise identical. Mirror `Test/Main.lean`'s
`getPrebuilt` instead: pre-build Builtin/Prelude's interfaces once, each in
its OWN throwaway `MalgoM.run` (so their internal uniqs never touch this
module's shared counter), memoized process-wide since `runTrace` is called
once per golden case (89 times in the full corpus). -/
initialize prebuiltRef : IO.Ref (Option (Std.TreeMap ModuleName Malgo.Rename.Interface)) ←
  IO.mkRef none

private def tracePrebuildFlag : Flag :=
  { noOptimize := false, lambdaLift := false, debugMode := false, testMode := true,
    target := .eval, evalMode := .smallStep, useInfer := false, programArgs := [] }

private def getPrebuiltInterfaces : IO (Std.TreeMap ModuleName Malgo.Rename.Interface) := do
  match ← prebuiltRef.get with
  | some c => return c
  | none =>
    let ws ← Workspace.setup
    let cache ← IO.mkRef ({} : Std.TreeMap ModuleName Malgo.Rename.Interface)
    MalgoM.run tracePrebuildFlag {} (Malgo.Driver.prebuildInterface ws cache "runtime/malgo/Builtin.mlg")
    MalgoM.run tracePrebuildFlag {} (Malgo.Driver.prebuildInterface ws cache "runtime/malgo/Prelude.mlg")
    let c ← cache.get
    prebuiltRef.set (some c)
    return c

/-- Run `srcPath` through the pipeline, rendering every stage. `useInfer`/
`malgo2025` mirror the `malgo eval` flags of the same name — like Haskell's
`runTrace`, these are plain parameters that drive the branch decisions
directly (not re-derived from the ambient `FeatureFlags` via `hasFeature`),
though the ambient `FeatureFlags` is still set to match, for any pass that
does consult it. -/
def runTrace (srcPath : System.FilePath) (useInfer : Bool) (malgo2025 : Bool) :
    IO (List Stage) := do
  let ws ← Workspace.setup
  let flag : Flag :=
    { noOptimize := false, lambdaLift := false, debugMode := false, testMode := true,
      target := .eval, evalMode := .smallStep, useInfer, programArgs := [] }
  let features : FeatureFlags := if malgo2025 then FeatureFlags.ofList [.malgo2025] else {}
  MalgoM.run flag features do
    let text ← IO.FS.readFile srcPath
    let srcModulePath ← ws.parseArtifactPathFromPwd srcPath
    Resource.save srcModulePath ".mlg" (← IO.FS.readBinFile srcPath)
    match ← Malgo.Parser.pass ws srcPath text with
    | (.error e, _) => throw ({ passName := "Parser", message := e.render, range? := none } : CompileError)
    | (.ok parsed, _) => do
      let cache : Malgo.Driver.InterfaceCache ← IO.mkRef (← MalgoM.io getPrebuiltInterfaces)
      let (renamedModule, _) ←
        Malgo.Rename.pass (Malgo.Driver.loadInterface ws cache) (parsed, Malgo.Rename.genBuiltinRnEnv)
      let mn := renamedModule.moduleName
      let renamedBindGroup := renamedModule.moduleDefinition
      let elaborated ← if malgo2025 then Elaborate.pass mn renamedBindGroup else pure renamedBindGroup
      let bindGroup ← if useInfer then
          let (bg, _) ← Infer.pass mn ({} : Infer.TyEnv) elaborated
          pure bg
        else pure elaborated
      let fun_ ← Malgo.Sequent.ToFun.pass mn bindGroup
      -- Saturated-constructor inlining and reuse-hint insertion now run
      -- first thing inside toCore (shared by every backend), not as
      -- Join-IR-local Zig passes; traced here at the Fun IR level, where
      -- the rewrites actually happen. `toCoreFrom specializedFun` reuses
      -- this same specialization, so `core` reflects the real
      -- post-specialization pipeline without re-minting its temporaries.
      let saturatedFun := Malgo.Sequent.SaturateCtor.saturateProgram fun_
      let specializedFun ← Malgo.Sequent.ReuseSpecialize.specializeProgram mn saturatedFun
      let core ← Malgo.Sequent.ToCore.toCoreFrom mn specializedFun
      let flat ← Malgo.Sequent.Core.Flat.flatProgram mn core
      let joined ← Malgo.Sequent.Core.Join.joinProgram mn flat
      let stages ← Malgo.Backend.Zig.runZigStages mn joined
      let reuseNote := match Malgo.Backend.Zig.RcCheck.checkProgram stages.reuse with
        | .ok () => ""
        | .error violations =>
          "-- RC CHECK VIOLATIONS:\n" ++ String.join (violations.map fun v => s!"--   {repr v}\n")
      pure <|
        [ { name := "Parse", rendered := PrettyIR.renderParsedModule parsed },
          { name := "Rename", rendered := PrettyIR.renderBindGroup renamedBindGroup } ] ++
        (if malgo2025 then [{ name := "Elaborate", rendered := PrettyIR.renderBindGroup elaborated }] else []) ++
        [ { name := "ToFun", rendered := PrettyIR.renderFun fun_ },
          { name := "SaturateCtor", rendered := PrettyIR.renderFun saturatedFun },
          { name := "ReuseSpecialize", rendered := PrettyIR.renderFun specializedFun },
          { name := "ToCore", rendered := PrettyIR.renderCoreFull core },
          { name := "Flat", rendered := PrettyIR.renderFlat flat },
          { name := "Join", rendered := PrettyIR.renderJoin joined },
          { name := "ClosureConv", rendered := PrettyIR.renderZigIr stages.closureConv },
          { name := "Peephole", rendered := PrettyIR.renderZigIr stages.peephole },
          { name := "Perceus", rendered := PrettyIR.renderZigIr stages.perceus },
          { name := "Reuse", rendered := reuseNote ++ PrettyIR.renderZigIr stages.reuse } ]

end Malgo.Debug.Pipeline
