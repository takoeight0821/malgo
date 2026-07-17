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

/-! ## Linking and evaluation (M1: direct in-memory linking, no artifacts) -/

/-- Concatenate Join programs, dependencies first (the order Haskell's
`compileTestCase` uses: `builtin <> prelude <> program`). -/
def linkPrograms (progs : List Malgo.Sequent.Core.Join.Program) :
    Malgo.Sequent.Core.Join.Program :=
  { definitions := progs.flatMap (·.definitions), dependencies := [] }

/-- Compile a module and its dependency closure to Join programs, dependencies
first (depth-first postorder, deduplicated). Each module is lowered once; a
dependency is re-renamed for lowering even though `loadInterface` renamed it
for its interface — wasteful but sound, since cross-module references use
uniq-free `External` ids (the query engine removes the duplication in M2). -/
partial def compileClosure (ws : Workspace) (cache : InterfaceCache)
    (joins : IO.Ref (Std.TreeMap ModuleName AllIR)) (path : System.FilePath) :
    MalgoM AllIR := do
  let ir ← compileToJoin ws cache path
  for dep in ir.dependencies do
    unless (← joins.get).contains dep do
      let depPath ← MalgoM.io (ws.getModulePath dep)
      let depIR ← compileClosure ws cache joins depPath.originPath.toFilePath
      joins.modify (·.insert dep depIR)
  pure ir

/-- Deposit a compiled module's source into the workspace mirror (Haskell
`save srcModulePath ".mlg" src` in `Driver.compile`). Bare-name imports
(`import Builtin`) resolve by searching the mirror, so evaluating
`runtime/malgo/Builtin.mlg` once seeds it for later runs — the protocol
`scripts/zig-golden.sh` and the selfhost scripts rely on. -/
private def seedMirror (ir : AllIR) : IO Unit := do
  if let .artifact ap := ir.moduleName then
    let target := ap.targetPath.toFilePath
    IO.FS.createDirAll (target.parent.getD (System.FilePath.mk "."))
    IO.FS.writeBinFile target (← IO.FS.readBinFile ap.originPath.toFilePath)

/-- CLI entry for `malgo eval --target eval`: compile, link the dependency
closure, and run the interpreter on real handles. -/
def compileAndEval (flag : Flag) (path : System.FilePath) : IO UInt32 := do
  let ws ← Workspace.setup
  MalgoM.run flag {} do
    let cache : InterfaceCache ← MalgoM.io (IO.mkRef {})
    let joins ← MalgoM.io (IO.mkRef ({} : Std.TreeMap ModuleName AllIR))
    let ir ← compileClosure ws cache joins path
    let deps := (← MalgoM.io joins.get).values
    for m in deps ++ [ir] do
      MalgoM.io (seedMirror m)
    let linked := linkPrograms (deps.map (·.join) ++ [ir.join])
    let handlers := Malgo.Sequent.Eval.Handlers.real flag.programArgs
    Malgo.Sequent.Eval.evalProgram ir.moduleName handlers linked
  return 0

end Malgo.Driver
