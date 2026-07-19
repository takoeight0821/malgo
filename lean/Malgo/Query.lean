import Std.Data.TreeMap
import Malgo.Prelude
import Malgo.Module
import Malgo.Interface
import Malgo.Syntax
import Malgo.Rename.RnState
import Malgo.Sequent.Core.Join
import Malgo.Infer

/-! Port of `src/Malgo/Query.hs` + `src/Malgo/Query/Database.hs`.

The Haskell `Query` GADT and the `QueryDB` dynamic effect flatten, in Lean,
to memoized methods over a `QueryDB` structure of `IO.Ref` caches (one per
query kind) — the union of Haskell's `Query.hs` (the effect surface) and
`Query/Database.hs` (the caches). The methods themselves live in
`Malgo/Query/Engine.lean`.

Deviations from Haskell:
- no `cacheInferredModule` yet — type inference is M3 (`useInfer = false`
  everywhere for now), so the inferred cache is a `-- TODO(M3)`;
- `buildInterface` lives here (it projects `RnState`, which this module
  already imports) rather than in `Interface.lean`, which stays free of the
  rename layer. -/

namespace Malgo.Query

open Malgo Malgo.Syntax Malgo.Rename

/-- Per-query-kind `IO.Ref` caches plus the in-memory source registry
(port of `Malgo.Query.Database.Database`). `cacheLinkedProgram` holds the
*linked* program (dependencies concatenated); the single-module program is
the `.sqt` artifact on disk. -/
structure QueryDB where
  cacheParsedModule : IO.Ref (Std.TreeMap ModuleName (Module .parse))
  cacheRenamedModule : IO.Ref (Std.TreeMap ModuleName (Module .rename × RnState))
  cacheModuleInterface : IO.Ref (Std.TreeMap ModuleName Interface)
  cacheLinkedProgram : IO.Ref (Std.TreeMap ModuleName Sequent.Core.Join.Program)
  /-- Each module's exported `TyEnv` (inference results). -/
  cacheInferredModule : IO.Ref (Std.TreeMap ModuleName Malgo.Infer.TyEnv)
  /-- In-memory source registry; populated by `updateSource` (e.g. from LSP). -/
  sourceMap : IO.Ref (Std.TreeMap ModuleName (System.FilePath × String))

/-- Create a fresh empty `QueryDB` (port of `newDatabase`). -/
def newQueryDB : IO QueryDB := do
  return {
    cacheParsedModule := ← IO.mkRef {},
    cacheRenamedModule := ← IO.mkRef {},
    cacheModuleInterface := ← IO.mkRef {},
    cacheLinkedProgram := ← IO.mkRef {},
    cacheInferredModule := ← IO.mkRef {},
    sourceMap := ← IO.mkRef {} }

/-- A `QueryDB` sharing an existing interface cache. The M1 test harness
seeds prebuilt Builtin/Prelude interfaces into an `IO.Ref (TreeMap …)`; this
lets the driver route the renamer through the engine while those prebuilt
entries (and the uniq order they imply) are preserved. -/
def QueryDB.ofInterfaceCache (cache : IO.Ref (Std.TreeMap ModuleName Interface)) : IO QueryDB := do
  return {
    cacheParsedModule := ← IO.mkRef {},
    cacheRenamedModule := ← IO.mkRef {},
    cacheModuleInterface := cache,
    cacheLinkedProgram := ← IO.mkRef {},
    cacheInferredModule := ← IO.mkRef {},
    sourceMap := ← IO.mkRef {} }

/-- Port of `Malgo.Interface.buildInterface`: project the renamer's final
`RnState` into the `Interface` importers consume. `infixInfo` is re-keyed
from resolved `Id` to raw name (Haskell `Map.mapKeys (·.name)`). -/
def buildInterface (moduleName : ModuleName) (rnState : RnState) : Interface :=
  { moduleName,
    infixInfo := rnState.infixInfo.foldl (fun acc id v => acc.insert id.name v) {},
    dependencies := rnState.dependencies,
    exportedIdentList := rnState.exportedIdentifiers,
    exportedTypeIdentList := rnState.exportedTypeIdentifiers }

end Malgo.Query
