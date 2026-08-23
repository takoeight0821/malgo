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
- **Inference is gated behind `useInfer`.** `fetchLinkedProgram` only calls
  `buildDepsEnv`/`Malgo.Infer.pass` when `(← getFlag).useInfer` is set;
  `fetchInferredModule` populates `cacheInferredModule` on that path. -/

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
    | (.ok parsed, flags) =>
      addFeatures flags
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

/-- Every key `depsOf` maps. `reverseDepClosureGo`'s `acc`/`frontier`/`next`
sets are always drawn from this fixed universe, which is what makes
termination provable: `acc`'s complement within it can only shrink. -/
private def depsUniverse (depsOf : Std.TreeMap ModuleName (Std.TreeSet ModuleName)) :
    Std.TreeSet ModuleName :=
  depsOf.foldl (init := (∅ : Std.TreeSet ModuleName)) fun s m _ => s.insert m

/-- One step of the reverse-dependency BFS: every module (other than
`target`, not already in `acc`) whose own deps intersect `frontier`.
Factored out of `reverseDepClosureGo` so its termination proof can name
this computation directly instead of re-deriving it from an inline
`let`. -/
private def nextOf (depsOf : Std.TreeMap ModuleName (Std.TreeSet ModuleName))
    (target : ModuleName) (acc frontier : Std.TreeSet ModuleName) : Std.TreeSet ModuleName :=
  depsOf.foldl (init := (∅ : Std.TreeSet ModuleName)) fun s m ds =>
    if m != target && !acc.contains m && ds.toList.any frontier.contains
    then s.insert m else s

private theorem mem_nextOf_elim (depsOf : Std.TreeMap ModuleName (Std.TreeSet ModuleName))
    (target : ModuleName) (acc frontier : Std.TreeSet ModuleName) {x : ModuleName}
    (hx : x ∈ nextOf depsOf target acc frontier) :
    ∃ p ∈ depsOf.toList,
      (p.1 != target && !acc.contains p.1 && p.2.toList.any frontier.contains) = true ∧
        compare p.1 x = .eq := by
  unfold nextOf at hx
  rw [Std.TreeMap.foldl_eq_foldl_toList] at hx
  rcases mem_foldl_filter_insert _ x depsOf.toList (∅ : Std.TreeSet ModuleName) hx with h1 | h2
  · simp at h1
  · exact h2

/-- Every module `nextOf` adds is a key of `depsOf`, hence in
`depsUniverse`. -/
private theorem next_subset_universe (depsOf : Std.TreeMap ModuleName (Std.TreeSet ModuleName))
    (target : ModuleName) (acc frontier : Std.TreeSet ModuleName) {x : ModuleName}
    (hx : x ∈ nextOf depsOf target acc frontier) :
    x ∈ depsUniverse depsOf := by
  obtain ⟨p, hp, _hpc, hpx⟩ := mem_nextOf_elim depsOf target acc frontier hx
  have hkey : p.1 ∈ depsUniverse depsOf := by
    unfold depsUniverse
    rw [Std.TreeMap.foldl_eq_foldl_toList]
    exact mem_foldl_insert_forward p.1 depsOf.toList (∅ : Std.TreeSet ModuleName) p hp
      Std.ReflCmp.compare_self
  exact (Std.TreeSet.mem_congr hpx).mp hkey

/-- Every module `nextOf` adds is disjoint from the current `acc` (the
filter's own `!acc.contains m` guard). -/
private theorem next_disjoint_acc (depsOf : Std.TreeMap ModuleName (Std.TreeSet ModuleName))
    (target : ModuleName) (acc frontier : Std.TreeSet ModuleName) {x : ModuleName}
    (hx : x ∈ nextOf depsOf target acc frontier) :
    x ∉ acc := by
  obtain ⟨p, _hp, hpc, hpx⟩ := mem_nextOf_elim depsOf target acc frontier hx
  have hnc : ¬ acc.contains p.1 := by
    have h := hpc
    simp only [Bool.and_eq_true, Bool.not_eq_true'] at h
    simp [h.1.2]
  rw [Std.TreeSet.mem_iff_contains, ← Std.TreeSet.contains_congr hpx]
  simpa using hnc

/-- The decreasing measure `reverseDepClosureGo`'s termination proof
needs: adding a nonempty, `acc`-disjoint `next` (drawn from the fixed
`depsUniverse`) strictly shrinks `acc`'s complement within that universe.
Built from `Std.TreeSet.size_lt_of_forall_mem_of_not_mem` (`Prelude.lean`)
with `next`'s own witness element as the one known shrinking point. -/
private theorem depsUniverse_diff_acc_union_next_lt
    (depsOf : Std.TreeMap ModuleName (Std.TreeSet ModuleName))
    (target : ModuleName) (acc frontier : Std.TreeSet ModuleName)
    (hne : ¬ (nextOf depsOf target acc frontier).isEmpty) :
    (depsUniverse depsOf \ (acc.union (nextOf depsOf target acc frontier))).size <
      (depsUniverse depsOf \ acc).size := by
  have hne' : (nextOf depsOf target acc frontier).isEmpty = false := by
    cases h : (nextOf depsOf target acc frontier).isEmpty with
    | true => exact absurd h hne
    | false => rfl
  obtain ⟨x, hxnext⟩ := Std.TreeSet.isEmpty_eq_false_iff_exists_mem.mp hne'
  have hxu : x ∈ depsUniverse depsOf := next_subset_universe depsOf target acc frontier hxnext
  have hxacc : x ∉ acc := next_disjoint_acc depsOf target acc frontier hxnext
  have hxdiff : x ∈ depsUniverse depsOf \ acc := Std.TreeSet.mem_diff_iff.mpr ⟨hxu, hxacc⟩
  refine Std.TreeSet.size_lt_of_forall_mem_of_not_mem
    (s := depsUniverse depsOf \ (acc.union (nextOf depsOf target acc frontier)))
    (t := depsUniverse depsOf \ acc) (x := x) ?_ hxdiff ?_
  · intro y hy
    rw [Std.TreeSet.mem_diff_iff] at hy ⊢
    refine ⟨hy.1, ?_⟩
    intro hyacc
    exact hy.2 (Std.TreeSet.mem_union_of_left hyacc)
  · rw [Std.TreeSet.mem_diff_iff]
    intro hcon
    exact hcon.2 (Std.TreeSet.mem_union_of_right hxnext)

/-- Transitive set of modules depending on `target`, from a forward dep-edge
map `M ↦ M's deps`. Excludes `target`. Pure — unit-testable without a
populated `QueryDB` (port of `reverseDepClosure`).

Terminates because `depsUniverse depsOf \ acc` strictly shrinks whenever
`nextOf` is nonempty (`depsUniverse_diff_acc_union_next_lt`), and
`frontier.size` strictly shrinks to 0 on the one further step where
`nextOf` is empty (`acc` unchanged, `frontier` becomes that empty
`next`) — a lexicographic pair of the two. -/
private def reverseDepClosureGo (depsOf : Std.TreeMap ModuleName (Std.TreeSet ModuleName))
    (target : ModuleName) (acc frontier : Std.TreeSet ModuleName) : Std.TreeSet ModuleName :=
  if frontier.isEmpty then acc
  else
    let next := nextOf depsOf target acc frontier
    reverseDepClosureGo depsOf target (acc.union next) next
termination_by ((depsUniverse depsOf \ acc).size, frontier.size)
decreasing_by
  rename_i hfrontier
  simp only [Bool.not_eq_true] at hfrontier
  cases hemp : (nextOf depsOf target acc frontier).isEmpty with
  | false =>
    apply Prod.Lex.left
    exact depsUniverse_diff_acc_union_next_lt depsOf target acc frontier (by simp [hemp])
  | true =>
    have hle1 : (depsUniverse depsOf \ (acc.union (nextOf depsOf target acc frontier))).size ≤
        (depsUniverse depsOf \ acc).size := by
      apply Std.TreeSet.size_le_of_forall_mem
      intro y hy
      rw [Std.TreeSet.mem_diff_iff] at hy ⊢
      exact ⟨hy.1, fun hcon => hy.2 (Std.TreeSet.mem_union_of_left hcon)⟩
    have hle2 : (depsUniverse depsOf \ acc).size ≤
        (depsUniverse depsOf \ (acc.union (nextOf depsOf target acc frontier))).size := by
      apply Std.TreeSet.size_le_of_forall_mem
      intro y hy
      rw [Std.TreeSet.mem_diff_iff] at hy ⊢
      refine ⟨hy.1, ?_⟩
      intro hcon
      rcases Std.TreeSet.mem_union_iff.mp hcon with hcon | hcon
      · exact hy.2 hcon
      · rw [Std.TreeSet.isEmpty_iff_forall_not_mem] at hemp
        exact hemp y hcon
    have heq : (depsUniverse depsOf \ (acc.union (nextOf depsOf target acc frontier))).size =
        (depsUniverse depsOf \ acc).size := Nat.le_antisymm hle1 hle2
    rw [heq]
    apply Prod.Lex.right
    have hnsize : (nextOf depsOf target acc frontier).size = 0 := by
      rw [Std.TreeSet.isEmpty_eq_size_eq_zero] at hemp
      simpa using hemp
    have hfsize : frontier.size ≠ 0 := by
      intro hz
      rw [Std.TreeSet.isEmpty_eq_size_eq_zero, hz] at hfrontier
      simp at hfrontier
    omega

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

/-! ## `reverseDepClosure` unit checks -/

namespace RdepGuards

/-! Port of `test/Malgo/Query/EngineSpec.hs`. `reverseDepClosure` decides
what the LSP invalidates when a file changes; getting it wrong means either
stale results or invalidating the world. The cyclic case is the one this
port already proved terminates (see `reverseDepClosureGo`'s
`termination_by`) -- these pin that it also returns the right answer. -/

private def mn (s : String) : ModuleName := .moduleName s
private def a := mn "A"
private def b := mn "B"
private def c := mn "C"
private def d := mn "D"
private def e := mn "E"

private def deps (xs : List (ModuleName × List ModuleName)) :
    Std.TreeMap ModuleName (Std.TreeSet ModuleName) :=
  xs.foldl (init := {}) (fun m (k, vs) => m.insert k (Std.TreeSet.ofList vs))

-- Nothing imports A.
#guard reverseDepClosure (deps [(a, []), (b, [])]) a == ({} : Std.TreeSet ModuleName)

-- B imports A, so invalidating A must reach B.
#guard reverseDepClosure (deps [(a, []), (b, [a])]) a == Std.TreeSet.ofList [b]

-- C -> B -> A: C must be reached even though it never names A.
#guard reverseDepClosure (deps [(a, []), (b, [a]), (c, [b])]) a == Std.TreeSet.ofList [b, c]

-- The target itself is excluded; `invalidateWithRdeps` adds it separately.
#guard reverseDepClosure (deps [(a, [b]), (b, [])]) b == Std.TreeSet.ofList [a]

-- A sibling subtree (E -> D) must not be dragged in.
#guard reverseDepClosure (deps [(a, []), (b, [a]), (d, []), (e, [d])]) a
  == Std.TreeSet.ofList [b]

-- Defensive: a cycle must neither loop nor over-report.
#guard reverseDepClosure (deps [(a, [b]), (b, [a])]) a == Std.TreeSet.ofList [b]

end RdepGuards

end Malgo.Query.Engine
