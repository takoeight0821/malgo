import Malgo.Monad
import Malgo.Module
import Malgo.Parser
import Malgo.Parser.Prim
import Malgo.Rename.Pass

/-! M1 mini-driver: a direct, in-memory compile pipeline up to Rename.

This is deliberately NOT the query engine (that is M2). It ports the
observable behavior of Haskell's `Malgo.Query.Engine` for the two queries
the renamer needs — `RenamedModule` and `ModuleInterface` — as a memoized
recursive `loadInterface` callback, plus a `buildInterface` mirroring
`Malgo.Interface.buildInterface`.

Parity note on uniqs: in Haskell the golden specs pre-build `Builtin.mlgi`
and `Prelude.mlgi` on disk (`setupBuiltin`/`setupPrelude`), so a testcase's
`RenamePass` loads those interfaces from disk and consumes NO uniqs for
them — the testcase's own `Id`s start at uniq 0. The test harness mirrors
this by pre-building the Builtin/Prelude interfaces in separate,
uniq-isolated `MalgoM.run`s and sharing them through the interface cache;
`loadInterface` returns a cached interface without re-renaming. -/

namespace Malgo.Driver

open Malgo Malgo.Syntax Malgo.Rename Malgo.Parser

/-- Shared, memoized interface cache. Keyed by `ModuleName`; the same module
can be reached both by `.artifact` path (testcase imports) and by
`.moduleName` name (Prelude's `import Builtin`), so pre-built entries are
stored under both keys (see `prebuildInterface`). -/
abbrev InterfaceCache := IO.Ref (Std.TreeMap ModuleName Interface)

instance : Inhabited CompileError := ⟨{ passName := "", message := "" }⟩
/-- Lets the recursive `partial def loadInterface` synthesize its fixpoint;
the witness is a `throw`, never evaluated on the success path. -/
instance {α} : Inhabited (MalgoM α) := ⟨throw default⟩

/-- Port of `Malgo.Interface.buildInterface`: project the renamer's final
`RnState` into the `Interface` the renamer consumes for imports. `infixInfo`
is re-keyed from resolved `Id` to raw name (Haskell `Map.mapKeys (·.name)`). -/
def buildInterface (moduleName : ModuleName) (rnState : RnState) : Interface :=
  { moduleName,
    infixInfo := rnState.infixInfo.foldl (fun acc id v => acc.insert id.name v) {},
    dependencies := rnState.dependencies,
    exportedIdentList := rnState.exportedIdentifiers,
    exportedTypeIdentList := rnState.exportedTypeIdentifiers }

private def parseError (e : PError) : CompileError :=
  { passName := "Parser", message := e.render, range? := none }

/-- Memoized `loadInterface` callback (Haskell `ModuleInterface`/
`RenamedModule`). On a cache miss it resolves the module path, parses,
recursively renames (which itself may call `loadInterface` for nested
imports), builds the interface, and caches it under the queried name.

The recursive rename consumes uniqs from the current `MalgoM` run, matching
Haskell's on-demand `RenamedModule` path (used only when no pre-built
interface is cached). -/
partial def loadInterface (ws : Workspace) (cache : InterfaceCache) (modName : ModuleName) :
    MalgoM Interface := do
  match (← cache.get).get? modName with
  | some inf => return inf
  | none =>
    let apath ← ws.getModulePath modName
    let path := apath.originPath.toFilePath
    let text ← IO.FS.readFile path
    match ← Malgo.Parser.pass ws path text with
    | (.error e, _) => throw (parseError e)
    | (.ok parsed, _) =>
      let (_, rnState) ← Malgo.Rename.pass (loadInterface ws cache) (parsed, genBuiltinRnEnv)
      let inf := buildInterface parsed.moduleName rnState
      cache.modify (·.insert modName inf)
      return inf

/-- Parse + rename a source file directly (mirrors the specs' `driveRename`:
the top-level module is not routed through the interface cache; only its
imports are). -/
def compileToRenamed (ws : Workspace) (cache : InterfaceCache) (path : System.FilePath) :
    MalgoM (Module .rename × RnState) := do
  let text ← IO.FS.readFile path
  match ← Malgo.Parser.pass ws path text with
  | (.error e, _) => throw (parseError e)
  | (.ok parsed, _) => Malgo.Rename.pass (loadInterface ws cache) (parsed, genBuiltinRnEnv)

/-- Pre-build a module's interface and store it in the cache under both its
`.artifact` module name and its bare `.moduleName <digest>` alias, so later
`loadInterface` lookups hit regardless of how the importer names it. Run in
a uniq-isolated `MalgoM.run` so the pre-built module contributes no uniqs to
importers. Mirrors `setupBuiltin`/`setupPrelude`. -/
def prebuildInterface (ws : Workspace) (cache : InterfaceCache) (path : System.FilePath) :
    MalgoM Unit := do
  let text ← IO.FS.readFile path
  match ← Malgo.Parser.pass ws path text with
  | (.error e, _) => throw (parseError e)
  | (.ok parsed, _) =>
    let (_, rnState) ← Malgo.Rename.pass (loadInterface ws cache) (parsed, genBuiltinRnEnv)
    let inf := buildInterface parsed.moduleName rnState
    cache.modify (·.insert parsed.moduleName inf)
    cache.modify (·.insert (.moduleName parsed.moduleName.digest) inf)

end Malgo.Driver
