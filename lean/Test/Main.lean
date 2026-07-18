import Malgo
import Test.Golden
import Test.Fingerprint

/-! Test driver. Parser golden cases run the real C-style parser
(`Malgo.Parser.pass`) and dump the parsed+resolved module with
`Malgo.sShow`, comparing byte-for-byte against the committed
`.golden/Malgo.Parser/<Case>/golden`. -/

namespace Malgo.Test

open Malgo

/-- Parse `test/testcases/malgo/<name>.mlg` and dump it. The workspace is
rooted at the repository root (`main` chdirs there), so artifact relPaths
match the Haskell goldens. -/
private def parseGolden (name : String) : IO String := do
  let ws ← Workspace.setup
  let path := System.FilePath.mk s!"test/testcases/malgo/{name}.mlg"
  let text ← IO.FS.readFile path
  let (result, _flags) ← Malgo.Parser.pass ws path text
  match result with
  | .error e => return s!"PARSE ERROR: {e.render}"
  | .ok m => return sShow m

private def parserCase (name : String) : GoldenCase :=
  { group := "Malgo.Parser", name, run := parseGolden name }

/-! ## Rename golden cases (the first real uniq-order-parity gate)

Each case runs the M1 mini-driver (`Malgo.Driver.compileToRenamed`) and
dumps `sShow` of the renamed module — exactly what the Haskell
`RenameSpec`'s `driveRename` dumps. Builtin/Prelude interfaces are
pre-built once in uniq-isolated runs (mirroring `setupBuiltin`/
`setupPrelude`) so a testcase's own `Id`s start at uniq 0. -/

/-- Test flag, matching Haskell `TestUtils.flag`. -/
def flag : Flag :=
  { noOptimize := false, lambdaLift := false, debugMode := false, testMode := true,
    target := .eval, evalMode := .smallStep, useInfer := false, programArgs := [] }

initialize prebuiltRef : IO.Ref (Option (Std.TreeMap ModuleName Malgo.Rename.Interface)) ←
  IO.mkRef none

/-- Pre-build (once) the Builtin then Prelude interfaces, each in its own
`MalgoM.run` so their uniqs never leak into a testcase's numbering. -/
def getPrebuilt : IO (Std.TreeMap ModuleName Malgo.Rename.Interface) := do
  match ← prebuiltRef.get with
  | some c => return c
  | none =>
    let ws ← Workspace.setup
    let cache ← IO.mkRef ({} : Std.TreeMap ModuleName Malgo.Rename.Interface)
    MalgoM.run flag {} (Malgo.Driver.prebuildInterface ws cache "runtime/malgo/Builtin.mlg")
    MalgoM.run flag {} (Malgo.Driver.prebuildInterface ws cache "runtime/malgo/Prelude.mlg")
    let c ← cache.get
    prebuiltRef.set (some c)
    return c

private def renameGolden (path : System.FilePath) : IO String := do
  try
    let ws ← Workspace.setup
    let cache ← IO.mkRef (← getPrebuilt)
    let (renamed, _) ← MalgoM.run flag {} (Malgo.Driver.compileToRenamed ws cache path)
    return sShow renamed
  catch e => return s!"ERROR: {toString e}"

private def renameCase (name : String) (path : System.FilePath) : GoldenCase :=
  { group := "Malgo.Rename", name, run := renameGolden path }

private def testcasePath (name : String) : System.FilePath :=
  System.FilePath.mk s!"test/testcases/malgo/{name}.mlg"

/-- The full-golden representatives (Haskell `TestUtils.representatives`);
the Rename and ToFun specs cut full goldens only for these (plus Builtin/
Prelude). -/
def representatives : List String :=
  [ "Primitive", "ListOps", "HelloImport", "RecordTest", "RowPoly", "CodataE2E",
    "FibCopattern", "LabelGoto", "NestedMatch", "CStyleApply", "ZeroArgs", "Eventually",
    "TuplePattern", "NestedRecursive", "StringPattern", "LetPattern" ]

/-- The 18 non-error Rename goldens: Builtin/Prelude (from `runtime/malgo/`)
plus the 16 representative testcases. -/
def renameCases : List GoldenCase :=
  [ renameCase "Builtin" "runtime/malgo/Builtin.mlg",
    renameCase "Prelude" "runtime/malgo/Prelude.mlg" ] ++
  representatives.map (fun n => renameCase n (testcasePath n))

/-! ## Elaborate gate (`sShow` of the elaborated BindGroup, per `ElaborateSpec`)

Each case runs `compileToRenamed` then `Elaborate.pass` and dumps `sShow` of
the resulting `BindGroup .rename` — exactly what the Haskell `ElaborateSpec`'s
`driveElaborate` dumps. Only the 16 representatives get full goldens (no
Builtin/Prelude here). -/

private def elaborateGolden (path : System.FilePath) : IO String := do
  try
    let ws ← Workspace.setup
    let cache ← IO.mkRef (← getPrebuilt)
    let bg ← MalgoM.run flag {}
      (do let (r, _) ← Malgo.Driver.compileToRenamed ws cache path
          Malgo.Elaborate.pass r.moduleName r.moduleDefinition)
    return sShow bg
  catch e => return s!"ERROR: {toString e}"

private def elaborateCase (name : String) (path : System.FilePath) : GoldenCase :=
  { group := "Malgo.Elaborate", name, run := elaborateGolden path }

def elaborateCases : List GoldenCase :=
  representatives.map (fun n => elaborateCase n (testcasePath n))

/-! ## ToFun gate (`sShow` of the Fun.Program, per `ToFunSpec`) -/

private def toFunGolden (path : System.FilePath) : IO String := do
  try
    let ws ← Workspace.setup
    let cache ← IO.mkRef (← getPrebuilt)
    let fn ← MalgoM.run flag {} (Malgo.Driver.compileToFun ws cache path)
    return sShow fn
  catch e => return s!"ERROR: {toString e}"

private def toFunCase (name : String) (path : System.FilePath) : GoldenCase :=
  { group := "Malgo.Sequent.ToFun", name, run := toFunGolden path }

def toFunCases : List GoldenCase :=
  [ toFunCase "Builtin" "runtime/malgo/Builtin.mlg",
    toFunCase "Prelude" "runtime/malgo/Prelude.mlg" ] ++
  representatives.map (fun n => toFunCase n (testcasePath n))

/-! ## ToCore gate (Core/Flat/Join, per `ToCoreSpec`)

Layout (subpath carried in the `name` field):
- `golden/<M>[/flat|/join]` — Builtin/Prelude dump core, flat, join (`sShow`).
- `golden/<Case>/join` — every testcase dumps its Join program (`sShow`).
- `flat-fingerprint/<Case>` / `join-fingerprint/<Case>` — format-immune
  constructor counts for every testcase.

`AllIR` per source path is memoized so the three per-testcase gates share
one lowering (mirrors `ToCoreSpec`'s per-testcase `IORef` cache). -/

def getAllIR (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR))
    (path : System.FilePath) : IO Malgo.Driver.AllIR := do
  let key := path.toString
  match (← memo.get).get? key with
  | some ir => return ir
  | none =>
    let ws ← Workspace.setup
    let cache ← IO.mkRef (← getPrebuilt)
    let ir ← MalgoM.run flag {} (Malgo.Driver.compileToJoin ws cache path)
    memo.modify (·.insert key ir)
    return ir

private def toCoreGolden (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR))
    (path : System.FilePath) (proj : Malgo.Driver.AllIR → String) : IO String := do
  try return proj (← getAllIR memo path)
  catch e => return s!"ERROR: {toString e}"

private def coreCase (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR))
    (name : String) (path : System.FilePath) (proj : Malgo.Driver.AllIR → String) : GoldenCase :=
  { group := "Malgo.Sequent.ToCore", name, run := toCoreGolden memo path proj }

/-- Build every ToCore golden case for the given testcase names. -/
def toCoreCases (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR))
    (names : List String) : List GoldenCase :=
  ([ ("Builtin", (⟨"runtime/malgo/Builtin.mlg"⟩ : System.FilePath)),
     ("Prelude", (⟨"runtime/malgo/Prelude.mlg"⟩ : System.FilePath)) ].flatMap fun (m, p) =>
    [ coreCase memo s!"golden/{m}" p (fun ir => sShow ir.core),
      coreCase memo s!"golden/{m}/flat" p (fun ir => sShow ir.flat),
      coreCase memo s!"golden/{m}/join" p (fun ir => sShow ir.join) ]) ++
  names.flatMap fun n =>
    let p := testcasePath n
    [ coreCase memo s!"golden/{n}/join" p (fun ir => sShow ir.join),
      coreCase memo s!"flat-fingerprint/{n}" p (fun ir => Malgo.Test.Fingerprint.fingerprintFlat ir.flat),
      coreCase memo s!"join-fingerprint/{n}" p (fun ir => Malgo.Test.Fingerprint.fingerprintJoin ir.join) ]

/-! ## Eval gate (captured stdout, per `EvalSpec`)

Linking mirrors Haskell `compileTestCase`: `builtin <> prelude <> program`
(Builtin/Prelude Join programs come from the same uniq-isolated lowerings
the ToCore gate validated), stdin is the fixed `"Hello\n"` of
`setupTestStdin`, and the golden is the captured stdout. -/

private def evalGolden (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR))
    (name : String) : IO String := do
  try
    let builtin ← getAllIR memo (System.FilePath.mk "runtime/malgo/Builtin.mlg")
    let prel ← getAllIR memo (System.FilePath.mk "runtime/malgo/Prelude.mlg")
    let ir ← getAllIR memo (testcasePath name)
    let linked := Malgo.Driver.linkPrograms [builtin.join, prel.join, ir.join]
    let (handlers, outRef) ← Malgo.Sequent.Eval.Handlers.buffered "Hello\n"
    MalgoM.run flag {} (Malgo.Sequent.Eval.evalProgram ir.moduleName handlers linked)
    outRef.get
  catch e => return s!"ERROR: {toString e}"

def evalCases (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR))
    (names : List String) : List GoldenCase :=
  names.map fun n => { group := "Malgo.Sequent.Eval", name := n, run := evalGolden memo n }

/-! ## BigStepEval gate (captured stdout, per `BigStepEvalSpec`)

Identical linking/stdin to `evalGolden`, but drives the program through the
big-step evaluator. The Haskell `BigStepEvalSpec` nests these under
`describe "golden"`, so the golden path is
`.golden/Malgo.Sequent.BigStepEval/golden/<Case>/golden` — hence the
`golden/` prefix in `name`. The goldens are byte-identical to small-step's;
the consistency/with-elaborate `describe` blocks are not ported (they assert
evaluator agreement, not stdout). -/

private def bigStepEvalGolden (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR))
    (name : String) : IO String := do
  try
    let builtin ← getAllIR memo (System.FilePath.mk "runtime/malgo/Builtin.mlg")
    let prel ← getAllIR memo (System.FilePath.mk "runtime/malgo/Prelude.mlg")
    let ir ← getAllIR memo (testcasePath name)
    let linked := Malgo.Driver.linkPrograms [builtin.join, prel.join, ir.join]
    let (handlers, outRef) ← Malgo.Sequent.Eval.Handlers.buffered "Hello\n"
    MalgoM.run flag {} (Malgo.Sequent.BigStepEval.bigStepEvalProgram ir.moduleName handlers linked)
    outRef.get
  catch e => return s!"ERROR: {toString e}"

def bigStepEvalCases (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR))
    (names : List String) : List GoldenCase :=
  names.map fun n =>
    { group := "Malgo.Sequent.BigStepEval", name := s!"golden/{n}", run := bigStepEvalGolden memo n }

/-! ## Forth gate (captured stdout, per `ForthSpec`)

`ForthSpec` compiles `examples/malgo/Forth.mlg` once, then for every
`test/testcases/forth/<case>.fs` feeds that file's contents as **stdin**
(Forth.mlg reads its program via `getContents`, not argv) to the small-step
`evalProgram`, capturing stdout. Golden path is
`.golden/Malgo.Forth/<case>/golden`. -/

private def forthMlgPath : System.FilePath := System.FilePath.mk "examples/malgo/Forth.mlg"
private def forthTestcaseDir : System.FilePath := System.FilePath.mk "test/testcases/forth"

private def forthGolden (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR))
    (caseName : String) : IO String := do
  try
    let builtin ← getAllIR memo (System.FilePath.mk "runtime/malgo/Builtin.mlg")
    let prel ← getAllIR memo (System.FilePath.mk "runtime/malgo/Prelude.mlg")
    let forth ← getAllIR memo forthMlgPath
    let linked := Malgo.Driver.linkPrograms [builtin.join, prel.join, forth.join]
    let input ← IO.FS.readFile (forthTestcaseDir / s!"{caseName}.fs")
    let (handlers, outRef) ← Malgo.Sequent.Eval.Handlers.buffered input
    MalgoM.run flag {} (Malgo.Sequent.Eval.evalProgram forth.moduleName handlers linked)
    outRef.get
  catch e => return s!"ERROR: {toString e}"

/-- Base names of `test/testcases/forth/*.fs`, sorted for stable output. -/
def enumerateForthTestcases : IO (List String) := do
  let entries ← forthTestcaseDir.readDir
  let names := entries.toList.filterMap fun e =>
    if e.fileName.endsWith ".fs" then (System.FilePath.mk e.fileName).fileStem else none
  return (names.toArray.qsort (· < ·)).toList

def forthCases (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR))
    (names : List String) : List GoldenCase :=
  names.map fun n => { group := "Malgo.Forth", name := n, run := forthGolden memo n }

/-- Testcase base names under `test/testcases/malgo/` (Haskell
`listDirectory testcaseDir`), sorted for stable output. -/
def enumerateTestcases : IO (List String) := do
  let entries ← (System.FilePath.mk "test/testcases/malgo").readDir
  let names := entries.toList.filterMap fun e =>
    if e.fileName.endsWith ".mlg" then (System.FilePath.mk e.fileName).fileStem else none
  return (names.toArray.qsort (· < ·)).toList

def cases : List GoldenCase :=
  [parserCase "Primitive", parserCase "HelloImport", parserCase "Eventually"]
    ++ renameCases ++ elaborateCases ++ toFunCases

end Malgo.Test

def main (args : List String) : IO UInt32 := do
  -- Root the runner (and the workspace) at the repository root; `lake test`
  -- starts in `lean/`, so `MALGO_REPO_ROOT` (default "..") points there.
  IO.Process.setCurrentDir ((← IO.getEnv "MALGO_REPO_ROOT").getD "..")
  match Malgo.Test.parseArgs args with
  | .error e =>
    IO.eprintln e
    return 2
  | .ok cfg =>
    let memo ← IO.mkRef ({} : Std.HashMap String Malgo.Driver.AllIR)
    let names ← Malgo.Test.enumerateTestcases
    let forthNames ← Malgo.Test.enumerateForthTestcases
    let allCases := Malgo.Test.cases ++ Malgo.Test.toCoreCases memo names
      ++ Malgo.Test.evalCases memo names
      ++ Malgo.Test.bigStepEvalCases memo names
      ++ Malgo.Test.forthCases memo forthNames
    Malgo.Test.runSuite cfg allCases
