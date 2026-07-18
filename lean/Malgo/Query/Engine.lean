import Std.Data.TreeMap
import Std.Data.TreeSet
import Malgo.Prelude
import Malgo.Monad
import Malgo.Module
import Malgo.Interface
import Malgo.Query
import Malgo.Parser
import Malgo.Rename.RnEnv
import Malgo.Rename.Pass
import Malgo.Elaborate
import Malgo.Sequent.ToFun
import Malgo.Sequent.ToCore
import Malgo.Sequent.Core.Flat
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Core.Json
import Malgo.Infer

/-! Port of `src/Malgo/Query/Engine.hs`: the memoized query handlers over a
`QueryDB`, plus reverse-dependency invalidation (`reverseDepClosure`,
`invalidateModule`, `updateSource`) that the LSP needs in M7.

Design notes on parity with the current Lean M1 driver (the goldens'
oracle):

- **Miss on an interface = rename in-run.** Haskell's `ModuleInterface`
  tries a pre-built `.mlgi` from disk *first*; doing that here would make a
  second `lake test` run (against a dirty `.malgo-work-lean`) load a
  0-uniq `.mlgi` an earlier run wrote instead of re-renaming, shifting an
  importer's uniqs. So the engine renames on a miss and treats the `.mlgi`
  it writes as **write-only**. Cross-invocation `.mlgi` reuse is a
  `-- TODO(M7)`. (Under the current test suite every import resolves to a
  prebuilt Builtin/Prelude interface, so the miss path is dead code there;
  this keeps it safe regardless.)
- **`.sqt` linking is self-contained.** `fetchLinkedProgram` renders each
  transitive dependency once (memoized), saving its single-module `.sqt`
  before linking, then `linkDeps` loads those `.sqt` back and concatenates —
  exercising the Join codec end-to-end.
- **No `buildDepsEnv`/inferred cache.** Type inference is M3; `useInfer`
  is `false`, so `LinkedProgram` never elaborates or infers. -/

namespace Malgo.Query.Engine

open Malgo Malgo.Query Malgo.Rename Malgo.Syntax
open Malgo.Sequent

instance {α} : Inhabited (MalgoM α) := ⟨throw { passName := "Query.Engine", message := "uninhabited" }⟩
instance : Inhabited (Std.TreeSet ModuleName) := ⟨{}⟩

private def parseError (e : Parser.PError) : CompileError :=
  { passName := "Parser", message := e.render, range? := none }

/-- Read source text for a module: in-memory registry first, then disk
(port of `fetchSource`). -/
def fetchSource (ws : Workspace) (db : QueryDB) (modName : ModuleName) :
    MalgoM (System.FilePath × String) := do
  match (← db.sourceMap.get).get? modName with
  | some result => return result
  | none =>
    let modPath ← ws.getModulePath modName
    let path := modPath.originPath.toFilePath
    return (path, ← IO.FS.readFile path)

/-- Best-effort `.mlgi` write (write-only; see the module note). Only a
failure to resolve the module's path is swallowed — mirroring Haskell's
`tryGetModulePath`, which catches solely `WorkspaceError` from
`getModulePath` and lets `save` fail loudly. A real `Resource.save` failure
(disk full, permission error, a codec bug) must propagate, not vanish. -/
private def saveInterfaceBestEffort (ws : Workspace) (modName : ModuleName) (inf : Interface) :
    MalgoM Unit := do
  let path? ← try
    pure (some (← ws.getModulePath modName))
  catch _ =>
    pure none
  match path? with
  | some path => Resource.save path ".mlgi" inf
  | none => pure ()

/-- Parsed AST for a module (Haskell `ParsedModule`). -/
partial def fetchParsedModule (ws : Workspace) (db : QueryDB) (modName : ModuleName) :
    MalgoM (Module .parse) := do
  match (← db.cacheParsedModule.get).get? modName with
  | some m => return m
  | none =>
    let (path, text) ← fetchSource ws db modName
    match ← Malgo.Parser.pass ws path text with
    | (.error e, _) => throw (parseError e)
    | (.ok parsed, _) =>
      db.cacheParsedModule.modify (·.insert modName parsed)
      return parsed

mutual

/-- Renamed AST + `RnState` (Haskell `RenamedModule`). Builds and caches the
interface, then writes it to `.mlgi` (write-only). -/
partial def fetchRenamedModule (ws : Workspace) (db : QueryDB) (modName : ModuleName) :
    MalgoM (Module .rename × RnState) := do
  match (← db.cacheRenamedModule.get).get? modName with
  | some result => return result
  | none =>
    let parsed ← fetchParsedModule ws db modName
    let result ← Malgo.Rename.pass (fetchInterface ws db) (parsed, genBuiltinRnEnv)
    db.cacheRenamedModule.modify (·.insert modName result)
    -- Use the parsed module's own name so External Ids match a direct
    -- compile regardless of the alias the query key used.
    let inf := buildInterface parsed.moduleName result.2
    db.cacheModuleInterface.modify (·.insert modName inf)
    saveInterfaceBestEffort ws modName inf
    return result

/-- Interface for a module (Haskell `ModuleInterface`). Cache hit returns
immediately; a miss renames in-run (see the module note). The fallback (only
reachable if `fetchRenamedModule` is ever changed to not always populate
`cacheModuleInterface` before returning — it does today) keys the interface
by the renamed module's own name, exactly like `fetchRenamedModule` does, so
an alias-keyed query never produces an Interface with the wrong exported
name. -/
partial def fetchInterface (ws : Workspace) (db : QueryDB) (modName : ModuleName) :
    MalgoM Interface := do
  match (← db.cacheModuleInterface.get).get? modName with
  | some inf => return inf
  | none =>
    let (renamed, rnState) ← fetchRenamedModule ws db modName
    match (← db.cacheModuleInterface.get).get? modName with
    | some inf => return inf
    | none => return buildInterface renamed.moduleName rnState

end

/-- Load dependency programs from disk and merge into a single linked program
(port of `linkDeps`). Each dependency's single-module `.sqt` is loaded and
its definitions concatenated AFTER the module's own — Haskell
`Query/Engine.hs`'s `linkDeps` (the actual ported function): `program.
definitions <> concatMap ... deps`. (Not `TestUtils.compileTestCase`'s
unrelated `builtin <> prelude <> program`, which links a fixed test-harness
list, not a discovered closure — an earlier version of this comment cited
that by mistake and had the order backwards.) `Toplevels` is a map built by
folding `insert` over this list (`evalProgram`), so on a qualified-name
collision the LAST occurrence wins; own-first/deps-last here means a
dependency's definition would win such a collision, matching Haskell
exactly (an edge case with no legal source today, since External `Id`s are
qualified by module — but the concatenation order must still match the
oracle byte-for-byte in case it ever becomes observable). `dependencies` is
transitive, so this covers the whole closure with no duplication. -/
def linkDeps (ws : Workspace) (dependencies : Std.TreeSet ModuleName)
    (program : Sequent.Core.Join.Program) : MalgoM Sequent.Core.Join.Program := do
  let deps ← dependencies.toList.mapM fun dep => do
    let path ← ws.getModulePath dep
    (Resource.load path ".sqt" : IO Sequent.Core.Join.Program)
  pure {
    definitions := program.definitions ++ deps.flatMap (·.definitions),
    dependencies := [] }

mutual

/-- Union each dependency's exported `TyEnv` (port of `buildDepsEnv`). Each
`InferredModule` result already covers signatures, foreigns, data
constructors, and inferred bare defs.

Haskell surfaces a two-deps-export-the-same-`Id` collision loudly (plain
`error`, via `Map.intersectionWith` on the KEYS — it does not special-case
"same Id, same value", any repeated key is fatal), since a genuine
cross-module `Id` clash indicates an upstream invariant violation. This is
deliberately reproduced exactly, not softened: the query engine can key the
same underlying module under two `ModuleName` aliases (`.moduleName
"Builtin"` and the artifact-path form path-string imports resolve to), and a
module reached both ways lists as two distinct dependency-set entries —
Haskell hits the identical crash in that situation (confirmed empirically:
`test/testcases/malgo/error/{ConstructorArity,StringPatIsNotSupported}.mlg`,
which import Builtin/Prelude via relative path *and* transitively via
Prelude's own bare-name `import Builtin`, crash under `--infer` on exactly
this). Silently unioning here would diverge from the oracle on those two
fixtures and mask a real upstream aliasing defect rather than surfacing it —
see `test/testcases/malgo/error/README.md`. -/
partial def buildDepsEnv (ws : Workspace) (db : QueryDB) (deps : Std.TreeSet ModuleName) :
    MalgoM Malgo.Infer.TyEnv := do
  deps.toList.foldlM (init := ({} : Malgo.Infer.TyEnv)) fun acc dep => do
    let depEnv ← fetchInferredModule ws db dep
    let collisions := depEnv.foldl (fun ks k _ => if acc.contains k then k :: ks else ks) []
    unless collisions.isEmpty do
      let msg := s!"buildDepsEnv: dependency {dep.toStr} redefines names already exported by " ++
        s!"an earlier dep: {collisions.map Malgo.Id.toText}"
      throw { passName := "Query.Engine", message := msg }
    pure (depEnv.foldl (fun m k v => m.insert k v) acc)

/-- Exported `TyEnv` for a module (Haskell `InferredModule`): rename →
buildDepsEnv → elaborate (malgo2025) → infer → export only the entries this
module contributes (`finalEnv \ depsEnv`). -/
partial def fetchInferredModule (ws : Workspace) (db : QueryDB) (modName : ModuleName) :
    MalgoM Malgo.Infer.TyEnv := do
  match (← db.cacheInferredModule.get).get? modName with
  | some result => return result
  | none =>
    let (renamed, rnState) ← fetchRenamedModule ws db modName
    let mn := renamed.moduleName
    let depsEnv ← buildDepsEnv ws db rnState.dependencies
    let bindGroup ← if (← Malgo.hasFeature .malgo2025)
      then Malgo.Elaborate.pass mn renamed.moduleDefinition
      else pure renamed.moduleDefinition
    let (_, finalEnv) ← Malgo.Infer.pass mn depsEnv bindGroup
    let exported := finalEnv.foldl (fun m k v => if depsEnv.contains k then m else m.insert k v) {}
    db.cacheInferredModule.modify (·.insert modName exported)
    return exported

end

/-- Linked Join program for a module (Haskell `LinkedProgram`): rename →
elaborate (malgo2025) → [infer if `useInfer`] → ToFun → ToCore → Flat →
Join → save `.sqt` → link the transitive dependency closure. -/
partial def fetchLinkedProgram (ws : Workspace) (db : QueryDB) (modName : ModuleName) :
    MalgoM Sequent.Core.Join.Program := do
  match (← db.cacheLinkedProgram.get).get? modName with
  | some p => return p
  | none =>
    let (renamed, rnState) ← fetchRenamedModule ws db modName
    let mn := renamed.moduleName
    -- Elaborate (codata desugaring), gated on the malgo2025 feature, mirroring
    -- Haskell `Query/Engine.hs`'s `LinkedProgram` handler. (infer-port adds the
    -- `useInfer` branch chronologically after this, before ToFun.)
    let bindGroup ← if (← Malgo.hasFeature .malgo2025)
      then Malgo.Elaborate.pass mn renamed.moduleDefinition
      else pure renamed.moduleDefinition
    let bindGroup' ← if (← getFlag).useInfer then do
        let importedEnv ← buildDepsEnv ws db rnState.dependencies
        let (bg, _) ← Malgo.Infer.pass mn importedEnv bindGroup
        pure bg
      else pure bindGroup
    let fn ← Sequent.ToFun.pass mn bindGroup'
    let core ← Sequent.ToCore.toCore mn fn
    let flat ← Sequent.Core.Flat.flatProgram mn core
    let program ← Sequent.Core.Join.joinProgram mn flat
    -- Save the single-module program, then ensure every dependency's `.sqt`
    -- exists (each module rendered once) before linkDeps reads them back.
    let path ← ws.getModulePath modName
    Resource.save path ".sqt" program
    for dep in rnState.dependencies.toList do
      let _ ← fetchLinkedProgram ws db dep
    let linked ← linkDeps ws rnState.dependencies program
    db.cacheLinkedProgram.modify (·.insert modName linked)
    return linked

/-! ## Invalidation (LSP, M7) -/

/-- Register an in-memory source for a module (port of `updateSource`). -/
def updateSource (db : QueryDB) (modName : ModuleName) (path : System.FilePath) (text : String) :
    IO Unit :=
  db.sourceMap.modify (·.insert modName (path, text))

/-- Transitive set of modules depending on `target`, from a forward dep-edge
map `M ↦ M's deps`. Excludes `target`. Pure — unit-testable without a
populated `QueryDB` (port of `reverseDepClosure`). -/
private partial def reverseDepClosureGo (depsOf : Std.TreeMap ModuleName (Std.TreeSet ModuleName))
    (target : ModuleName) (acc frontier : Std.TreeSet ModuleName) : Std.TreeSet ModuleName :=
  if frontier.isEmpty then acc
  else
    let next : Std.TreeSet ModuleName := depsOf.foldl (init := {}) fun s m ds =>
      if m != target && !acc.contains m && ds.toList.any frontier.contains
      then s.insert m else s
    reverseDepClosureGo depsOf target (acc.union next) next

def reverseDepClosure (depsOf : Std.TreeMap ModuleName (Std.TreeSet ModuleName))
    (target : ModuleName) : Std.TreeSet ModuleName :=
  reverseDepClosureGo depsOf target {} (({} : Std.TreeSet ModuleName).insert target)

/-- Invalidate `modName` and every cached module that transitively depends on
it (port of `invalidateWithRdeps`). Reverse-dep edges are reconstructed from
`cacheRenamedModule` (each entry's `RnState.dependencies`). -/
def invalidateModule (db : QueryDB) (modName : ModuleName) : IO Unit := do
  let renamed ← db.cacheRenamedModule.get
  let depsOf : Std.TreeMap ModuleName (Std.TreeSet ModuleName) :=
    renamed.foldl (init := {}) fun acc m (_, st) => acc.insert m st.dependencies
  let victims := (reverseDepClosure depsOf modName).insert modName
  for m in victims.toList do
    db.cacheParsedModule.modify (·.erase m)
    db.cacheRenamedModule.modify (·.erase m)
    db.cacheModuleInterface.modify (·.erase m)
    db.cacheLinkedProgram.modify (·.erase m)
    db.cacheInferredModule.modify (·.erase m)

/-! ## `reverseDepClosure` unit checks (port of `Malgo.Query.EngineSpec`) -/

section Guards
private def a : ModuleName := .moduleName "A"
private def b : ModuleName := .moduleName "B"
private def c : ModuleName := .moduleName "C"
private def d : ModuleName := .moduleName "D"
private def e : ModuleName := .moduleName "E"

private def mkDeps (xs : List (ModuleName × List ModuleName)) :
    Std.TreeMap ModuleName (Std.TreeSet ModuleName) :=
  xs.foldl (init := {}) fun acc (m, ds) =>
    acc.insert m (ds.foldl (fun s x => s.insert x) {})

private def closureList (xs : List (ModuleName × List ModuleName)) (target : ModuleName) :
    List ModuleName :=
  (reverseDepClosure (mkDeps xs) target).toList

-- empty when nothing depends on the target
#guard closureList [(a, []), (b, [])] a == []
-- direct importers
#guard closureList [(a, []), (b, [a])] a == [b]
-- transitive importers (C → B → A)
#guard closureList [(a, []), (b, [a]), (c, [b])] a == [b, c]
-- excludes the target itself
#guard closureList [(a, [b]), (b, [])] b == [a]
-- ignores siblings that do not depend on the target
#guard closureList [(a, []), (b, [a]), (d, []), (e, [d])] a == [b]
-- terminates on cyclic graphs
#guard closureList [(a, [b]), (b, [a])] a == [b]
end Guards

end Malgo.Query.Engine
