import Malgo.Monad
import Malgo.Module
import Malgo.Parser
import Malgo.Parser.Prim
import Malgo.Rename.Pass
import Malgo.Sequent.Fun
import Malgo.Sequent.ToFun
import Malgo.Sequent.ToCore
import Malgo.Sequent.Core.Full
import Malgo.Sequent.Core.Flat
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Eval
import Malgo.Sequent.BigStepEval
import Malgo.Query
import Malgo.Query.Engine
import Malgo.Backend.Scheme
import Malgo.Backend.Zig
import Malgo.Backend.Zig.Toolchain

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

/-! ## Lowering to the sequent IRs (single module, unlinked)

Mirrors Haskell `TestUtils`/`Query.Engine` for one module: ToFun → ToCore
(runs saturate+specialize internally — not re-run here) → Flat → Join.
Cross-module linking (concatenating Builtin/Prelude programs) comes with
Eval integration; these produce the single module's IR, as the ToFun/ToCore
specs dump. -/

/-- Parse + rename + ToFun. -/
def compileToFun (ws : Workspace) (cache : InterfaceCache) (path : System.FilePath) :
    MalgoM Malgo.Sequent.Fun.Program := do
  let (renamed, _) ← compileToRenamed ws cache path
  Malgo.Sequent.ToFun.pass renamed.moduleName renamed.moduleDefinition

/-- All three Core-level IRs for one module, matching `ToCoreSpec`'s `AllIR`,
plus the module name and dependency list needed for linking and eval. -/
structure AllIR where
  moduleName : ModuleName
  dependencies : List ModuleName
  core : Malgo.Sequent.Core.Full.Program
  flat : Malgo.Sequent.Core.Flat.Program
  join : Malgo.Sequent.Core.Join.Program

/-- Parse + rename + ToFun + ToCore + Flat + Join, keeping every stage. -/
def compileToJoin (ws : Workspace) (cache : InterfaceCache) (path : System.FilePath) :
    MalgoM AllIR := do
  let (renamed, rnState) ← compileToRenamed ws cache path
  let mn := renamed.moduleName
  let fn ← Malgo.Sequent.ToFun.pass mn renamed.moduleDefinition
  let core ← Malgo.Sequent.ToCore.toCore mn fn
  let flat ← Malgo.Sequent.Core.Flat.flatProgram mn core
  let join ← Malgo.Sequent.Core.Join.joinProgram mn flat
  pure { moduleName := mn, dependencies := rnState.dependencies.toList, core, flat, join }

/-! ## Linking (M1 test harness: direct in-memory linking, no artifacts)

`linkPrograms` backs the golden gates in `Test/Main.lean`, which link a
fixed `[builtin, prelude, program]` list rather than a discovered dependency
closure (each precompiled once, in a uniq-isolated run, for golden byte
parity). The CLI's `compileAndEval` below instead resolves and links an
arbitrary dependency closure through the query engine
(`Malgo.Query.Engine.fetchLinkedProgram`). -/

/-- Concatenate Join programs, dependencies first (the order Haskell's
`compileTestCase` uses: `builtin <> prelude <> program`). -/
def linkPrograms (progs : List Malgo.Sequent.Core.Join.Program) :
    Malgo.Sequent.Core.Join.Program :=
  { definitions := progs.flatMap (·.definitions), dependencies := [] }

/-- Deposit a module's source into the workspace mirror (Haskell `save
srcModulePath ".mlg" src` in `Driver.compile`). Bare-name imports (`import
Builtin`) resolve by searching the mirror, so evaluating
`runtime/malgo/Builtin.mlg` once seeds it for later runs — the protocol
`scripts/zig-golden.sh` and the selfhost scripts rely on. Resolves `modName`
via the workspace (works for both `.artifact`- and `.moduleName`-keyed
modules), independent of any particular IR representation. -/
private def seedMirrorFor (ws : Workspace) (modName : ModuleName) : IO Unit := do
  let ap ← ws.getModulePath modName
  let target := ap.targetPath.toFilePath
  IO.FS.createDirAll (target.parent.getD (System.FilePath.mk "."))
  -- Atomic tmp+rename via a uniquely-named temp file (same pattern as
  -- `Resource.save`): concurrent `malgo eval` runs (the golden sweep
  -- scripts run cases in parallel) must not observe a half-written mirror,
  -- and must not race each other for the same temp filename either.
  let bytes ← IO.FS.readBinFile ap.originPath.toFilePath
  let mut written := false
  for n in [0:1000] do
    let tmp := System.FilePath.mk s!"{target}.{n}.tmp"
    let handle? ← try
        some <$> IO.FS.Handle.mk tmp .writeNew
      catch _ => pure none
    if let some handle := handle? then
      try
        handle.write bytes
        handle.flush
        IO.FS.rename tmp target
      catch e =>
        try IO.FS.removeFile tmp catch _ => pure ()
        throw e
      written := true
      break
  unless written do
    throw (IO.userError s!"seedMirrorFor: could not create a temp file for {target}")

/-- CLI entry for `malgo eval --target eval`: compile through the query
engine (`Malgo.Query.Engine.fetchLinkedProgram` — memoized fetch, each
dependency parsed/renamed/lowered exactly once and persisted as `.sqt`,
unlike `compileClosure`'s M1-era double-rename) and run the interpreter on
real handles.

The top-level file is parsed directly (mirroring `compileToRenamed`) and its
already-parsed module seeded into the engine's `cacheParsedModule` under its
own resolved name, so `fetchLinkedProgram` starts from a cache hit instead
of re-deriving the module's identity.

`compileAndEval`/`compileScheme`/`compileZig`/`compileToNativeExecutable`
(all four CLI entries below) share this exact parse+link+seed sequence —
`linkForCli` is the one place it lives, so a future fix only has one call
site to update. -/
private def linkForCli (ws : Workspace) (path : System.FilePath) :
    MalgoM (ModuleName × Malgo.Sequent.Core.Join.Program) := do
  let db ← MalgoM.io Malgo.Query.newQueryDB
  let text ← MalgoM.io (IO.FS.readFile path)
  match ← Malgo.Parser.pass ws path text with
  | (.error e, _) => throw (parseError e)
  | (.ok parsed, flags) =>
    addFeatures flags
    MalgoM.io (db.cacheParsedModule.modify (·.insert parsed.moduleName parsed))
    let linked ← Malgo.Query.Engine.fetchLinkedProgram ws db parsed.moduleName
    -- Seed the `.mlg` mirror for every module the query touched (top module
    -- + transitive deps) so a later, separate `malgo eval` invocation's
    -- bare-name imports can find them — orthogonal to the `.sqt`/`.mlgi`
    -- artifacts the query engine persists.
    let touched := (← MalgoM.io db.cacheRenamedModule.get).keys
    for m in touched do
      MalgoM.io (seedMirrorFor ws m)
    pure (parsed.moduleName, linked)

def compileAndEval (flag : Flag) (path : System.FilePath) : IO UInt32 := do
  let ws ← Workspace.setup
  MalgoM.run flag {} do
    let (moduleName, linked) ← linkForCli ws path
    let handlers := Malgo.Sequent.Eval.Handlers.real flag.programArgs
    match flag.evalMode with
    | .smallStep => Malgo.Sequent.Eval.evalProgram moduleName handlers linked
    | .bigStep => Malgo.Sequent.BigStepEval.bigStepEvalProgram moduleName handlers linked
  return 0

/-- CLI entry for `malgo eval --target scheme`: link exactly as
`compileAndEval` but, instead of interpreting, emit the Scheme source for the
linked Join program. Mirrors Haskell `Driver.compileFromAST`'s `TargetScheme`
branch. -/
def compileScheme (flag : Flag) (path : System.FilePath) : IO UInt32 := do
  let ws ← Workspace.setup
  MalgoM.run flag {} do
    let (moduleName, linked) ← linkForCli ws path
    MalgoM.io (IO.print (Malgo.Backend.Scheme.compileToScheme moduleName linked))
  return 0

/-- CLI entry for `malgo eval --target zig`: link exactly as `compileScheme`,
then run the Zig lowering pipeline (`Malgo.Backend.Zig.compileToZigText`, which
runs ClosureConv → Peephole → Perceus → Reuse, the linearity check, and Emit)
and print the generated Zig source. Mirrors Haskell `Driver.compileFromAST`'s
`TargetZig` branch. -/
def compileZig (flag : Flag) (path : System.FilePath) : IO UInt32 := do
  let ws ← Workspace.setup
  MalgoM.run flag {} do
    let (moduleName, linked) ← linkForCli ws path
    let zigText ← Malgo.Backend.Zig.compileToZigText moduleName linked
    MalgoM.io (IO.print zigText)
  return 0

/-- CLI entry for `malgo compile SOURCE -o OUT`: link and lower to Zig exactly
as `compileZig`, then write the generated source to `OUT.zig` and invoke the
`zig` toolchain to produce a native executable at `OUT`. Mirrors Haskell
`Driver.compileToExecutable` (cache root = the workspace dir). -/
def compileToNativeExecutable (flag : Flag) (path : System.FilePath)
    (outPath : System.FilePath) (optMode : Malgo.Backend.Zig.Toolchain.OptMode) : IO UInt32 := do
  let ws ← Workspace.setup
  MalgoM.run flag {} do
    let (moduleName, linked) ← linkForCli ws path
    let zigText ← Malgo.Backend.Zig.compileToZigText moduleName linked
    let zigPath := outPath.toString ++ ".zig"
    MalgoM.io (IO.FS.writeFile zigPath zigText)
    MalgoM.io (Malgo.Backend.Zig.Toolchain.buildExecutable
      (toString ws.dir) zigPath outPath.toString optMode)
  return 0

end Malgo.Driver
