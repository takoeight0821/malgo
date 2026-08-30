import Malgo
import Test.Golden
import Test.Fingerprint
import Test.MetPage

/-! Test driver. Parser golden cases run the real C-style parser
(`Malgo.Parser.pass`) and dump the parsed+resolved module with
`Malgo.sShow`, comparing byte-for-byte against the committed
`.golden/Malgo.Parser/<Case>/golden`. -/

namespace Malgo.Test

open Malgo

/-- Parse `test/testcases/malgo/<name>.mlg` and dump it. The workspace is
rooted at the repository root (`main` chdirs there), so artifact relPaths
match the Haskell goldens. -/
private def parseGoldenAt (path : System.FilePath) : IO String := do
  let ws ← Workspace.setup
  let text ← IO.FS.readFile path
  let (result, _flags) ← Malgo.Parser.pass ws path text
  match result with
  | .error e => return s!"PARSE ERROR: {e.render}"
  | .ok m => return sShow m

private def parserCaseAt (name : String) (path : System.FilePath) : GoldenCase :=
  { group := "Malgo.Parser", name, run := parseGoldenAt path }

/-! ### Parser error cases (`test/Malgo/ParserSpec/errors/*.mlg`)

The goldens here are `PError.render`'s single line. Haskell's
`driveErrorParse` dumped megaparsec's `errorBundlePretty` — the offending
source line with a caret under the column — so these were the last cases to
transfer when that implementation retired. -/

private def parserErrorCaseDir : System.FilePath :=
  System.FilePath.mk "test/Malgo/ParserSpec/errors"

private def parseErrorGolden (path : System.FilePath) : IO String := do
  let ws ← Workspace.setup
  let text ← IO.FS.readFile path
  let (result, _flags) ← Malgo.Parser.pass ws path text
  match result with
  | .error e => return e.render
  | .ok m => return s!"PARSED WITHOUT ERROR: {sShow m}"

private def parserErrorCase (name : String) : GoldenCase :=
  { group := "Malgo.Parser", name := s!"error/{name}",
    run := parseErrorGolden (parserErrorCaseDir / s!"{name}.mlg") }

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

/-! ### Rename error cases (`test/Malgo/RenameSpec/errors/*.mlg`)

Haskell's `driveErrorRename` `show`s the `CompileError` it caught; Lean's
`CompileError.render` names the pass in the text and formats the range
differently, so these transferred alongside the parser errors above. -/

private def renameErrorCaseDir : System.FilePath :=
  System.FilePath.mk "test/Malgo/RenameSpec/errors"

private def renameErrorGolden (path : System.FilePath) : IO String := do
  try
    let ws ← Workspace.setup
    let cache ← IO.mkRef (← getPrebuilt)
    let (renamed, _) ← MalgoM.run flag {} (Malgo.Driver.compileToRenamed ws cache path)
    return s!"RENAMED WITHOUT ERROR: {sShow renamed}"
  catch e => return toString e

private def renameErrorCase (name : String) : GoldenCase :=
  { group := "Malgo.Rename", name := s!"error/{name}",
    run := renameErrorGolden (renameErrorCaseDir / s!"{name}.mlg") }

/-- Base names of an error-fixture directory, sorted (Haskell
`listDirectory errorcaseDir`). -/
def enumerateErrorCases (dir : System.FilePath) : IO (List String) := do
  let entries ← dir.readDir
  let names := entries.toList.filterMap fun e =>
    if e.fileName.endsWith ".mlg" then (System.FilePath.mk e.fileName).fileStem else none
  return (names.toArray.qsort (· < ·)).toList

def enumerateParserErrorCases : IO (List String) := enumerateErrorCases parserErrorCaseDir
def enumerateRenameErrorCases : IO (List String) := enumerateErrorCases renameErrorCaseDir

def parserErrorCases (names : List String) : List GoldenCase := names.map parserErrorCase
def renameErrorCases (names : List String) : List GoldenCase := names.map renameErrorCase

private def testcasePath (name : String) : System.FilePath :=
  System.FilePath.mk s!"test/testcases/malgo/{name}.mlg"

/-- The full-golden representatives (Haskell `TestUtils.representatives`);
the Rename and ToFun specs cut full goldens only for these (plus Builtin/
Prelude). -/
def representatives : List String :=
  [ "Primitive", "ListOps", "HelloImport", "RecordTest", "RowPoly", "CodataE2E",
    "FibCopattern", "LabelGoto", "NestedMatch", "CStyleApply", "ZeroArgs", "Eventually",
    "TuplePattern", "NestedRecursive", "StringPattern", "LetPattern",
    "TaggedRecordCrossModuleUse" ]

/-- The 19 non-error Rename goldens: Builtin/Prelude (from `runtime/malgo/`)
plus the 17 representative testcases. -/
def renameCases : List GoldenCase :=
  [ renameCase "Builtin" "runtime/malgo/Builtin.mlg",
    renameCase "Prelude" "runtime/malgo/Prelude.mlg" ] ++
  representatives.map (fun n => renameCase n (testcasePath n))

/-! ## Elaborate gate (`sShow` of the elaborated BindGroup, per `ElaborateSpec`)

Each case runs `compileToRenamed` then `Elaborate.pass` and dumps `sShow` of
the resulting `BindGroup .rename` — exactly what the Haskell `ElaborateSpec`'s
`driveElaborate` dumps. Only the 17 representatives get full goldens (no
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

/-- Testcases excluded from `evalCases`/`bigStepEvalCases` (and therefore
from ever getting a `.golden/Malgo.Sequent.Eval/<name>/golden` file), each
for its own reason:

- `TaggedRecordCrossModuleUse` imports a third module
  (`TaggedRecordCrossModuleDef.mlg`) beyond Builtin/Prelude to
  regression-test a #422 cross-module fix; `linkPrograms [builtin, prelude,
  ir]` above has no way to see that third module's compiled program, so
  evaluating the linked result always fails with "Undefined variable:
  Point" here regardless of whether the compiler itself is correct. The
  real CLI's `Query.Engine.fetchLinkedProgram` does link the full
  dependency closure and evaluates this case correctly (verified manually).
- `Panic` and `CondPanic` (both malgo#426) call `malgo_panic` -- the former
  directly, the latter via `Prelude.mlg`'s `cond`'s `Nil -> panic "no
  branch"` fallback (the specific path #426's own issue text called out as
  untested) -- which *by design* terminates evaluation nonzero with a
  message on stderr, matching the Zig (`runtime/zig/runtime.zig`'s `panic`)
  and Scheme (`Backend/Scheme.lean`'s `(error 'panic ...)`) backends —
  there is no successful stdout to pin as a golden. `PanicGate.run` below
  exercises both directly instead.
- `PanicNamedImport` (malgo#452) is `Panic`'s named-import twin (`module
  {panic} = import ...` instead of `module {..} = import ...`), added to
  regression-test the self-hosted evaluator's `import Builtin.mlg`
  substitution (see `scripts/selfhost-golden.sh`'s panic-import gate) --
  excluded from this harness for the same reason as `Panic`.

Excluding these here keeps `scripts/cli-gate.sh`'s `eval` mode, and
`scripts/zig-golden.sh`/`scripts/scheme-golden.sh`/`scripts/selfhost-golden.sh`
(all of which discover their cases by listing `.golden/Malgo.Sequent.Eval/*/`
rather than scanning `test/testcases/malgo/` directly), from ever seeing
any of these names — no per-script skip-list needed. -/
def evalHarnessUnsupported : List String :=
  ["TaggedRecordCrossModuleUse", "Panic", "CondPanic", "PanicNamedImport"]

private def evalGolden (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR))
    (name : String) : IO String := do
  try
    let builtin ← getAllIR memo (System.FilePath.mk "runtime/malgo/Builtin.mlg")
    let prel ← getAllIR memo (System.FilePath.mk "runtime/malgo/Prelude.mlg")
    let ir ← getAllIR memo (testcasePath name)
    let linked := Malgo.Driver.linkPrograms [builtin.join, prel.join, ir.join]
    let (handlers, outRef) ← Malgo.Sequent.Eval.Handlers.buffered "Hello\n"
    let _exitCode ← MalgoM.run flag {} (Malgo.Sequent.Eval.evalProgram ir.moduleName handlers linked)
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
    let _exitCode ← MalgoM.run flag {} (Malgo.Sequent.BigStepEval.bigStepEvalProgram ir.moduleName handlers linked)
    outRef.get
  catch e => return s!"ERROR: {toString e}"

def bigStepEvalCases (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR))
    (names : List String) : List GoldenCase :=
  names.map fun n =>
    { group := "Malgo.Sequent.BigStepEval", name := s!"golden/{n}", run := bigStepEvalGolden memo n }

/-! ## Panic gate (regression test for malgo#426, not golden — see
`evalHarnessUnsupported` for why `Panic`/`CondPanic`/`PanicNamedImport` are
excluded from `evalCases`/`bigStepEvalCases`).

Checks both entry points against each linked program: `BigStepEval.lean`
calls `Eval.fetchPrimitive` directly rather than reimplementing it, so this
also confirms the big-step evaluator didn't need its own `malgo_panic`
case. Three scenarios: `Panic` calls `panic(...)` directly; `CondPanic` walks
one `Cons (False, _) xs` step of `Prelude.mlg`'s `cond` before hitting its
`Nil -> panic "no branch"` fallback -- the specific path #426's issue text
called out as untested; `PanicNamedImport` (malgo#452) is `Panic`'s
named-import twin, confirming the Lean interpreters -- unlike the
self-hosted evaluator -- have no trouble with either import form. Both
stdout and the full rendered error message are pinned exactly:
`EvalError.rangeOf`'s `.panic` case returns `none` (its
foreign-import declaration in Builtin.mlg, never the caller's actual
`panic(...)` site), so the message is just `[<passName>] panic: <msg>` with
no location prefix to make an exact match brittle -- `<passName>` differs
per evaluator (`Eval.lean`/`BigStepEval.lean` each `throw`s their own
`CompileError` with their own module name as `passName`), so it's supplied
per evaluator below rather than baked into `Scenario.expectedMessage`. -/
namespace PanicGate

private structure Scenario where
  testcase : String
  expectedStdout : String
  /-- Suffix after `[<passName>] `, e.g. `"panic: no branch"`. -/
  expectedMessage : String

private def scenarios : List Scenario :=
  [ { testcase := "Panic", expectedStdout := "before panic\n",
      expectedMessage := "panic: malgo#426 regression check" },
    { testcase := "CondPanic", expectedStdout := "before cond\n",
      expectedMessage := "panic: no branch" },
    { testcase := "PanicNamedImport", expectedStdout := "before panic\n",
      expectedMessage := "panic: malgo#452 named-import regression check" } ]

def run (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR)) : IO Nat := do
  let evaluators :=
    [ ("Malgo.Sequent.Eval", "Eval", Malgo.Sequent.Eval.evalProgram),
      ("Malgo.Sequent.BigStepEval", "BigStepEval", Malgo.Sequent.BigStepEval.bigStepEvalProgram) ]
  let mut failed := 0
  let mut total := 0
  for scenario in scenarios do
    let builtin ← getAllIR memo (System.FilePath.mk "runtime/malgo/Builtin.mlg")
    let prel ← getAllIR memo (System.FilePath.mk "runtime/malgo/Prelude.mlg")
    let ir ← getAllIR memo (testcasePath scenario.testcase)
    let linked := Malgo.Driver.linkPrograms [builtin.join, prel.join, ir.join]
    for (label, passName, runProgram) in evaluators do
      total := total + 1
      let expectedMessage := s!"[{passName}] {scenario.expectedMessage}"
      let (handlers, outRef) ← Malgo.Sequent.Eval.Handlers.buffered "Hello\n"
      try
        let _ ← MalgoM.run flag {} (runProgram ir.moduleName handlers linked)
        IO.println s!"FAIL {label}/{scenario.testcase}: malgo_panic did not terminate evaluation"
        failed := failed + 1
      catch e =>
        let out ← outRef.get
        let msg := toString e
        if out != scenario.expectedStdout then
          IO.println
            s!"FAIL {label}/{scenario.testcase}: stdout was {out}, expected {scenario.expectedStdout}"
          failed := failed + 1
        else if msg != expectedMessage then
          IO.println
            s!"FAIL {label}/{scenario.testcase}: message was \"{msg}\", expected \"{expectedMessage}\""
          failed := failed + 1
        else
          IO.println s!"ok {label}/{scenario.testcase}"
  IO.println s!"=== panic-gate {total - failed}/{total} passed ==="
  return failed

end PanicGate

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
    let _exitCode ← MalgoM.run flag {} (Malgo.Sequent.Eval.evalProgram forth.moduleName handlers linked)
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

/-- The 19 non-error Parser goldens, mirroring `renameCases`: Builtin/Prelude
from `runtime/malgo/` plus the 17 representative testcases. Previously only
three were registered, leaving 15 of the committed `.golden/Malgo.Parser`
files ungated on the Lean side. The `error/*` cases are registered
separately (`parserErrorCases`) because their goldens are Lean-owned. -/
def parserCases : List GoldenCase :=
  [ parserCaseAt "Builtin" "runtime/malgo/Builtin.mlg",
    parserCaseAt "Prelude" "runtime/malgo/Prelude.mlg" ] ++
  representatives.map (fun n => parserCaseAt n (testcasePath n))

def cases : List GoldenCase :=
  parserCases
    ++ renameCases ++ elaborateCases ++ toFunCases

/-! ## Lint gate (per `Malgo.LintSpec`)

Each `test/Malgo/LintSpec/cases/*.mlg` runs `Malgo.Lint.lintFile` (a relative
path, matching the Haskell spec's own `caseDir </> tc` — NOT the CLI's
`makeAbsolute`, so the golden text's path prefix is `./test/...`, not an
absolute path) and the golden is `unlines` of every diagnostic's rendered
text (`unlines [] = ""`, matching a clean/parse-error case's empty golden). -/

private def lintCaseDir : System.FilePath := System.FilePath.mk "./test/Malgo/LintSpec/cases"

private def lintGolden (name : String) : IO String := do
  try
    let diags ← MalgoM.run flag {} (Malgo.Lint.lintFile (lintCaseDir / s!"{name}.mlg"))
    return String.join (diags.map (fun d => Malgo.Doc.render (Malgo.Lint.prettyDiagnostic d) ++ "\n"))
  catch e => return s!"ERROR: {toString e}"

private def lintCase (name : String) : GoldenCase :=
  { group := "Malgo.Lint", name, run := lintGolden name }

/-- Base names of `test/Malgo/LintSpec/cases/*.mlg` (Haskell `listDirectory
caseDir`), sorted for stable output. -/
def enumerateLintCases : IO (List String) := do
  let entries ← lintCaseDir.readDir
  let names := entries.toList.filterMap fun e =>
    if e.fileName.endsWith ".mlg" then (System.FilePath.mk e.fileName).fileStem else none
  return (names.toArray.qsort (· < ·)).toList

def lintCases (names : List String) : List GoldenCase := names.map lintCase

/-! ## PrettyIR trace gate (port of `Malgo.Debug.PrettyIRSpec`)

Every `.mlg` file under `test/testcases/malgo/` and `examples/malgo/` gets one
golden covering the whole trace: every pipeline stage's rendered text, plus
the unified diff between each adjacent pair. One golden per source file (not
per file-per-stage) keeps the corpus-wide sweep from repeating the
golden-output blowup already fixed once for the per-pass Spec suites. -/

private def examplesDir : System.FilePath := System.FilePath.mk "examples/malgo"

private def traceReport (srcPath : System.FilePath) : IO String := do
  let stages ← Malgo.Debug.Pipeline.runTrace srcPath false false
  let heading (label : String) : String := s!"=== {label} ==="
  let stageSection := String.intercalate "\n\n"
    (stages.map fun s => heading s.name ++ "\n" ++ s.rendered)
  let diffSection := String.intercalate "\n\n"
    ((stages.zip stages.tail).map fun (a, b) =>
      heading s!"{a.name} -> {b.name}" ++ "\n" ++ Malgo.Debug.DiffView.renderUnifiedDiff a.rendered b.rendered)
  return stageSection ++ "\n\n" ++ heading "DIFFS" ++ "\n\n" ++ diffSection

private def prettyIRGolden (srcPath : System.FilePath) : IO String := do
  try traceReport srcPath
  catch e => return s!"ERROR: {toString e}"

/-- Haskell's `golden` helper splits its description string on whitespace
into separate path components (`words description`) before joining with the
enclosing spec path — so `golden ("testcases " <> name) ...` lands at
`.golden/Malgo.Debug.PrettyIR/testcases/<name>/golden`, a nested directory,
not a literal space in one component. Mirror that with an explicit `/`
(the same convention `bigStepEvalCases` already uses via `s!"golden/{n}"`). -/
private def prettyIRCase (subdir name : String) (srcPath : System.FilePath) : GoldenCase :=
  { group := "Malgo.Debug.PrettyIR", name := s!"{subdir}/{name}", run := prettyIRGolden srcPath }

/-- Base names of `examples/malgo/*.mlg` (Haskell `listDirectory examplesDir`),
sorted for stable output. -/
def enumerateExamples : IO (List String) := do
  let entries ← examplesDir.readDir
  let names := entries.toList.filterMap fun e =>
    if e.fileName.endsWith ".mlg" then (System.FilePath.mk e.fileName).fileStem else none
  return (names.toArray.qsort (· < ·)).toList

def prettyIRCases (testcaseNames exampleNames : List String) : List GoldenCase :=
  testcaseNames.map (fun n => prettyIRCase "testcases" n (testcasePath n)) ++
  exampleNames.map (fun n => prettyIRCase "examples" n (examplesDir / s!"{n}.mlg"))

/-! ## MET page gate (M8, authored fresh — no Haskell reference test
exists; see `Test/MetPage.lean`'s module doc). -/

/-! ## Zig Reuse placement gate (port of `ReuseSpec.hs`'s `placementSpec`)

`reuseFunc` mints fresh token names, so it lives in `MalgoM` and cannot be a
`#guard` the way the pass's pure siblings (`Peephole`, `RcCheck`) are. Same
determinism the Haskell spec relies on: `MalgoM.run` starts `Uniq` at 0, so
within one call the tokens are `reuse_0`, `reuse_1`, ... in pairing order.

The four negative cases matter most -- an unpaired drop that got swallowed
instead of flushed back out would leak, and the golden sweep would not
notice. -/

/-! ## Zig corpus linearity oracle (port of `PerceusSpec.hs`'s `corpusSpec`)

Every golden testcase is lowered through the real backend pipeline
(ClosureConv → Peephole → Perceus → Reuse) and handed to the `RcCheck`
linearity checker. That covers **every control-flow path** of the whole
corpus, including paths no golden input actually executes, with no Zig
toolchain in the loop -- which is exactly what `zig-golden.sh` cannot do,
since it only observes the stdout of the paths a run happens to take.

Two properties, both from the Haskell spec:

  * ClosureConv alone must not insert any RC op (they are Perceus's job);
  * the full pipeline must stay linear, reuse-token obligations included.

Plus the guard against vacuity: a pairing pass that silently matched nothing
corpus-wide would satisfy every linearity check above, so at least one
`dropReuse` must fire somewhere. -/

/-! ## Flat/Join structural invariants (port of `FlatCheck.hs`/`JoinCheck.hs`)

`ToCoreSpec` drives these over every testcase alongside its goldens. The
goldens pin *what* the IR looks like; these pin a property that must hold
whatever it looks like.

**Flat**: every producer in an argument position (`primitive`,
`externalCall`, `binOp`, `construct`) must be a *value* -- a var, a literal,
or a recursively-value construct. That is the entire point of the Flat pass;
a nested computation surviving there would be a miscompile that the goldens
would happily record.

**Join**: the Haskell checker walks the tree calling `evaluate` to force
bottoms out of a lazily-built structure. That has no meaning in Lean, where
the program is already a total value by the time we hold it -- porting the
traversal would assert nothing. What does carry over is the non-vacuity
check: a Join program with no definitions means the pipeline dropped
everything on the floor. -/

/-! ## ReuseSpecialize (port of `test/Malgo/Sequent/ReuseSpecializeSpec.hs`)

`specializeProgram` inserts the `reuseHint` primitive that lets the Zig
backend recycle a matched cell in place. It runs in `MalgoM` (it mints
`reuseArg`/`reuseHint` names), so this is a gate rather than a `#guard`.

Five of the six cases assert the pass leaves a definition **alone**. That is
the important direction: #354 records two attempts at more aggressive
hint placement that both crashed the runtime's `rc == 1` assertion, so a pass
that fires where it should not is a live failure mode, not a hypothetical
one. Compared via `sShow`, exactly as the Haskell spec does. -/

namespace ReuseSpec

open Malgo.Sequent.Fun

private def rr : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
private def sn (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }
private def vr (s : String) : Expr := .var rr (sn s)

/-- `sShow` of the named definition after specialization. -/
private def specialized (defs : List (Range × Name × Expr)) (target : String) : IO String := do
  let prog : Program := { definitions := defs, dependencies := [] }
  let out ← MalgoM.run Malgo.Test.flag {}
    (Malgo.Sequent.ReuseSpecialize.specializeProgram (.moduleName "t") prog)
  match out.definitions.find? (fun d => d.2.1 == sn target) with
  | some (_, _, b) => return Malgo.sShow b
  | none => return "<not found>"

private structure Case where
  name : String
  defs : List (Range × Name × Expr)
  target : String
  /-- `none` means "must come back unchanged". -/
  expected : Option Expr

-- mapList: `Cons (f x) (mapList f xs)` in the recursive arm.
private def mapListBody : Expr :=
  .lambda rr [sn "f"] (.lambda rr [sn "xs"]
    (.select rr (.construct rr .tuple [vr "f", vr "xs"])
      [ .branch rr (.destruct rr .tuple [.pvar rr (sn "_"),
          .destruct rr (.tag "Nil") []]) (.construct rr (.tag "Nil") []),
        .branch rr (.destruct rr .tuple [.pvar rr (sn "f2"),
          .destruct rr (.tag "Cons") [.pvar rr (sn "x2"), .pvar rr (sn "xs2")]])
          (.construct rr (.tag "Cons")
            [ .apply rr (vr "f2") [vr "x2"],
              .apply rr (.apply rr (.invoke rr (sn "mapList")) [vr "f2"]) [vr "xs2"] ]) ]))

-- Two recursive calls into one Construct: which cell would be reused?
private def mirrorBody : Expr :=
  .lambda rr [sn "tree"] (.select rr (vr "tree")
    [ .branch rr (.destruct rr (.tag "Node")
        [.pvar rr (sn "val"), .pvar rr (sn "left"), .pvar rr (sn "right")])
        (.construct rr (.tag "Node")
          [ vr "val",
            .apply rr (.invoke rr (sn "mirror")) [vr "right"],
            .apply rr (.invoke rr (sn "mirror")) [vr "left"] ]) ])

-- Rebuilt through a different tag than the one matched.
private def snocBody : Expr :=
  .lambda rr [sn "xs"] (.select rr (vr "xs")
    [ .branch rr (.destruct rr (.tag "Cons") [.pvar rr (sn "x2"), .pvar rr (sn "xs2")])
        (.construct rr (.tag "Snoc")
          [vr "x2", .apply rr (.invoke rr (sn "f")) [vr "xs2"]]) ])

-- Models Map.mlg's AVL insert: rebuilt via a rebalancing helper, not a
-- direct Construct, so there is no cell to recycle in place.
private def smartCtorBody : Expr :=
  .lambda rr [sn "x"] (.lambda rr [sn "tree"] (.select rr (vr "tree")
    [ .branch rr (.destruct rr (.tag "Node")
        [.pvar rr (sn "val"), .pvar rr (sn "left"), .pvar rr (sn "right")])
        (.apply rr (.apply rr (.apply rr (.invoke rr (sn "mkNode"))
          [.apply rr (.apply rr (.invoke rr (sn "insert")) [vr "x"]) [vr "left"]])
          [vr "val"]) [vr "right"]) ]))

private def baseCaseBody : Expr :=
  .lambda rr [sn "xs"] (.select rr (vr "xs")
    [ .branch rr (.destruct rr (.tag "Nil") []) (.construct rr (.tag "Nil") []) ])

private def notLambdaSelectBody : Expr := .construct rr (.tag "Unit") []

private def cases : List Case :=
  [ { name := "instruments a mapList-shaped self-recursive rebuild"
    , defs := [(rr, sn "mapList", mapListBody)], target := "mapList", expected := none }
  , { name := "leaves two recursive calls into one Construct untouched (ambiguous)"
    , defs := [(rr, sn "mirror", mirrorBody)], target := "mirror", expected := some mirrorBody }
  , { name := "leaves a rebuild through a different tag untouched"
    , defs := [(rr, sn "f", snocBody)], target := "f", expected := some snocBody }
  , { name := "leaves a smart-constructor rebuild untouched"
    , defs := [(rr, sn "insert", smartCtorBody)], target := "insert", expected := some smartCtorBody }
  , { name := "leaves a non-recursive base case untouched"
    , defs := [(rr, sn "f", baseCaseBody)], target := "f", expected := some baseCaseBody }
  , { name := "leaves a definition that isn't Lambda+Select shaped untouched"
    , defs := [(rr, sn "f", notLambdaSelectBody)], target := "f", expected := some notLambdaSelectBody } ]

def run : IO Nat := do
  let mut failed := 0
  for c in cases do
    let actual ← specialized c.defs c.target
    match c.expected with
    | some e =>
      if actual == Malgo.sShow e then
        IO.println s!"ok Malgo.Sequent.ReuseSpecialize/{c.name}"
      else
        IO.println s!"FAIL Malgo.Sequent.ReuseSpecialize/{c.name}: changed when it should not have"
        failed := failed + 1
    | none =>
      -- The positive case: assert the hint was actually inserted rather
      -- than pinning the exact fresh-name numbering.
      if (actual.splitOn "reuseHint").length > 1 then
        IO.println s!"ok Malgo.Sequent.ReuseSpecialize/{c.name}"
      else
        IO.println s!"FAIL Malgo.Sequent.ReuseSpecialize/{c.name}: no reuseHint inserted"
        failed := failed + 1
  IO.println s!"=== reuse-specialize {cases.length - failed}/{cases.length} passed ==="
  return failed

end ReuseSpec

namespace IrInvariants

open Malgo.Sequent.Core

private partial def isValueProducer : Flat.Producer → Bool
  | .var _ _ => true
  | .literal _ _ => true
  | .construct _ _ ps _ => ps.all isValueProducer
  | .lambda .. => true
  | .object .. => true
  | .mu .. => false

mutual

/-- Collects a description of each argument position holding a non-value. -/
private partial def badFlatStmt : Flat.Statement → List String
  | .cut p c => badFlatProd p ++ badFlatCons c
  | .join _ _ c s => badFlatCons c ++ badFlatStmt s
  | .primitive _ nm ps c =>
    (ps.filter (fun p => !isValueProducer p) |>.map (fun _ => s!"primitive {nm}")) ++ badFlatCons c
  | .invoke _ _ c => badFlatCons c
  | .externalCall _ nm ps c =>
    (ps.filter (fun p => !isValueProducer p) |>.map (fun _ => s!"externalCall {nm}")) ++ badFlatCons c
  | .binOp _ op l r c =>
    (if isValueProducer l then [] else [s!"binOp {op} lhs"]) ++
    (if isValueProducer r then [] else [s!"binOp {op} rhs"]) ++ badFlatCons c
  | .ifz _ cond t e => badFlatProd cond ++ badFlatStmt t ++ badFlatStmt e

private partial def badFlatProd : Flat.Producer → List String
  | .var .. => []
  | .literal .. => []
  | .construct _ _ ps cs =>
    (ps.filter (fun p => !isValueProducer p) |>.map (fun _ => "construct arg")) ++
      cs.flatMap badFlatCons
  | .lambda _ _ s => badFlatStmt s
  | .object _ fields => fields.flatMap (fun f => badFlatStmt f.2.2)
  | .mu _ _ s => badFlatStmt s

private partial def badFlatCons : Flat.Consumer → List String
  | .label .. => []
  | .apply _ ps cs => ps.flatMap badFlatProd ++ cs.flatMap badFlatCons
  | .project _ _ c => badFlatCons c
  | .«then» _ _ s => badFlatStmt s
  | .finish _ => []
  | .select _ branches => branches.flatMap (fun b => match b with | .branch _ _ s => badFlatStmt s)

end

def run (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR)) (names : List String) :
    IO Nat := do
  let mut failed := 0
  for name in names do
    try
      let ir ← Malgo.Test.getAllIR memo (Malgo.Test.testcasePath name)
      let bad := ir.flat.definitions.flatMap (fun d => badFlatStmt d.body)
      if !bad.isEmpty then
        IO.println s!"FAIL Malgo.Sequent.Core.Flat/{name}: non-value in {bad.length} argument position(s): {bad.take 3}"
        failed := failed + 1
      else if ir.join.definitions.isEmpty then
        IO.println s!"FAIL Malgo.Sequent.Core.Join/{name}: no definitions"
        failed := failed + 1
      else
        IO.println s!"ok Malgo.Sequent.Core.FlatJoin/{name}"
    catch e =>
      IO.println s!"FAIL Malgo.Sequent.Core.FlatJoin/{name}: {toString e}"
      failed := failed + 1
  IO.println s!"=== ir-invariants {names.length - failed}/{names.length} passed ==="
  return failed

end IrInvariants

/-! ## Parser surface behaviour (selected from `test/Malgo/ParserSpec.hs`)

That spec has 19 unit cases; most are "this string parses" smoke checks for
syntax the golden corpus already exercises end to end -- C-style calls
(`CStyleApply`), tuples and clauses, data/type defs (`CStyleDataDef`,
`CStyleTypeSynonym`), the Malgo 2025 bundle (`BottomTilde`, `RowPoly`,
`LabelGoto`), and pragmas (8 corpus files open with `#c-style-apply` or
`#malgo-2025`). Re-asserting those here would add coverage of nothing.

Four carry weight and are ported:

  * The three `#345` field-access precedence cases. `.field` binds to the
    adjacent atom only when no whitespace precedes the dot. The corpus has
    the no-space form (`xs.tail.tail` in CodataE2E) but **not one instance
    of the spaced form**, so half the rule is currently unpinned -- and the
    two halves produce different trees, not a parse error, which is the kind
    of difference a golden over a whole file will not localise.
  * Unknown pragmas must be ignored rather than rejected. Every pragma in
    the corpus is one of the two known ones, so nothing else covers the
    fallthrough. -/

namespace ParserSurface

private def parses (src : String) : IO (Except String String) := do
  let ws ← Workspace.setup
  let (res, _) ← Malgo.Parser.pass ws "<test>" src
  match res with
  | .error e => return .error e.render
  | .ok m => return .ok (Malgo.sShow m)

private structure Case where
  name : String
  src : String
  /-- Substring the rendered parse tree must contain. -/
  expect : Option String := none

private def cases : List Case :=
  [ { name := "binds .field to the adjacent atom when no space precedes the dot"
    , src := "def main = f a state.dict"
    , expect := some "(apply (apply f a) (project state \"dict\"))" }
  , { name := "treats .field as a postfix on the whole application when a space precedes it"
    , src := "def main = f a state .dict"
    , expect := some "(project (apply (apply f a) state) \"dict\")" }
  , { name := "chains adjacent .field projections tightly"
    , src := "def main = f state.dict.head"
    , expect := some "(apply f (project (project state \"dict\") \"head\"))" }
  , { name := "ignores unknown pragmas alongside known ones"
    , src := "#experimental-feature\n#c-style-apply\ndef main = f(x, y)" }
  , { name := "binds .field to a parenthesized call argument when no space precedes the dot (#424)"
    , src := "def main = f (g x).field"
    , expect := some "(apply f (project (parens (apply g x)) \"field\"))" }
  , { name := "chains adjacent .field projections onto a parenthesized call argument (#424)"
    , src := "def main = f (g x).a.b"
    , expect := some
        "(apply f (project (project (parens (apply g x)) \"a\") \"b\"))" }
  , { name := "treats .field as a postfix on the whole call when a space precedes it after a parenthesized argument"
    , src := "def main = f (g x) .field"
    , expect := some "(project (apply f (apply g x)) \"field\")" }
  , { name := "does not disturb a two-argument call written tight: f(a, b)"
    , src := "def main = f(a, b)"
    , expect := some "(apply (apply f a) b)" }
  , { name := "does not disturb a two-argument call written with a space: f (a, b)"
    , src := "def main = f (a, b)"
    , expect := some "(apply (apply f a) b)" }
  , { name := "keeps a tight C-style call's .field bound to the call result, not the argument (#424)"
    , src := "def main = nats(0).tail"
    , expect := some "(project (apply nats (int32 0)) \"tail\")" }
  , { name := "keeps a tight call's .field bound to the call result even with a compound argument (#424)"
    , src := "def main = f(g x).field"
    , expect := some "(project (apply f (apply g x)) \"field\")" }
  , { name := "a tight call's closing paren may be on its own line before a tight dot (#424)"
    , src := "def main = nats(\n  0\n).tail"
    , expect := some "(project (apply nats (int32 0)) \"tail\")" }
  , { name := "a loose call's closing paren may be on its own line before a tight dot (#424)"
    , src := "def main = f (\n  g x\n).field"
    , expect := some "(apply f (project (parens (apply g x)) \"field\"))" }
  , { name := "binds .field to a parenthesized argument in a non-first position too (#424)"
    , src := "def main = f a (g x).field"
    , expect := some
        "(apply (apply f a) (project (parens (apply g x)) \"field\"))" }
  , { name := "a tight call's .field binds to the call result for a lambda-headed atom too (#424 follow-up)"
    , src := "def main = { x -> x }(5).field"
    , expect := some
        "(project (apply (fn ((clause (x) (seq (do x))))) (int32 5)) \"field\")" }
  , { name := "a tight call's .field binds to the call result for a tuple-headed atom too (#424 follow-up)"
    , src := "def main = (a, b)(5).field"
    , expect := some "(project (apply (tuple a b) (int32 5)) \"field\")" }
  , { name := "a tight call's .field binds to the call result for a record-headed atom too (#424 follow-up)"
    , src := "def main = { .a -> 1, .b -> 2 }(5).field"
    , expect := some
        "(project (apply (record (a (int32 1)) (b (int32 2))) (int32 5)) \"field\")" }
  , { name := "keeps a tight two-argument call's .field bound to the call result: f(a, b).field"
    , src := "def main = f(a, b).field"
    , expect := some "(project (apply (apply f a) b) \"field\")" }
  , { name := "keeps a tight nullary call's .field bound to the call result: f().field"
    , src := "def main = f().field"
    , expect := some "(project (apply f (tuple)) \"field\")" } ]

def run : IO Nat := do
  let mut failed := 0
  for c in cases do
    match ← parses c.src with
    | .error e =>
      IO.println s!"FAIL Malgo.Parser.surface/{c.name}: parse error: {e}"
      failed := failed + 1
    | .ok rendered =>
      match c.expect with
      | none => IO.println s!"ok Malgo.Parser.surface/{c.name}"
      | some needle =>
        if (rendered.splitOn needle).length > 1 then
          IO.println s!"ok Malgo.Parser.surface/{c.name}"
        else
          IO.println s!"FAIL Malgo.Parser.surface/{c.name}: missing {needle}"
          failed := failed + 1
  IO.println s!"=== parser-surface {cases.length - failed}/{cases.length} passed ==="
  return failed

end ParserSurface

/-! ## Primitive coverage gate (#453)

`Builtin.mlg` declares primitives via `foreign import`; `fetchPrimitive`
(`Malgo.Sequent.Eval`) handles a subset of them via literal-match arms and
prefix/suffix family rules (`malgo_add_*`, `*to_string`, etc.). The two
lists can silently diverge in either direction:

- a declared primitive with no `fetchPrimitive` case fails at eval time with
  `Primitive ... is not implemented` (`malgo_exit_failure` is the repro
  #453 opened with);
- a handled name with no `foreign import` declaration is dead code no
  `.mlg` source can ever reach (8 legacy `malgo_str_*`/`malgo_rune_*`/
  `malgo_int_to_*` arms, confirmed unreferenced under `runtime/`).

This gate parses `Builtin.mlg`'s declared names, mirrors `fetchPrimitive`'s
dispatch as data, and fails on any name present in exactly one side unless
it is named in `knownMissing`/`knownUndeclared` below. The allowlist
documents currently-acknowledged gaps — it is not a way to silence the gate
permanently, so each entry is also checked for staleness (still actually a
gap) on every run; closing a gap for real means deleting its entry. -/
namespace PrimitiveCoverage

/-- Names `fetchPrimitive` matches literally, i.e. neither a prefix nor a
suffix family. Mirrors every `if name == ...` / `| "..." =>` arm in
`Malgo.Sequent.Eval.fetchPrimitive` by hand (Lean has no reflection over
`match` arms) — keep this in sync whenever that function gains or loses a
literal case. -/
def handledLiteralNames : List String :=
  [ "reuseHint", "malgo_unsafe_cast"
  , "malgo_print_string", "malgo_newline", "malgo_print_char", "malgo_get_contents"
  , "malgo_string_append", "malgo_string_length", "malgo_string_at", "malgo_string_cons"
  , "malgo_substring", "malgo_string_reverse", "malgo_print"
  , "malgo_str_len", "malgo_str_at", "malgo_str_sub", "malgo_str_to_int"
  , "malgo_int_to_str", "malgo_rune_to_str", "malgo_int_to_rune", "malgo_rune_to_int"
  , "malgo_is_digit", "malgo_is_lower", "malgo_is_upper", "malgo_is_alphanum"
  , "malgo_char_ord", "malgo_int32_t_to_char", "malgo_read_file", "malgo_write_file"
  , "malgo_get_line", "malgo_get_args", "malgo_panic", "malgo_exit_success"
  , "malgo_exit_with_code", "malgo_has_env", "malgo_get_env", "malgo_stderr_string"
  , "malgo_run_process", "malgo_string_to_int32", "malgo_string_to_int64" ]

/-- Prefixes `fetchPrimitive` dispatches on via `String.startsWith`. Keep in
sync with the `else if name.startsWith "..."` chain there. -/
def handledPrimitivePrefixes : List String :=
  [ "malgo_add_", "malgo_sub_", "malgo_mul_", "malgo_div_", "malgo_mod_", "malgo_neg_"
  , "malgo_eq_", "malgo_ne_", "malgo_lt_", "malgo_le_", "malgo_gt_", "malgo_ge_" ]

/-- Does `fetchPrimitive` have a case that would dispatch on `name`? Prefix
and suffix families make this a superset of any finite name list, which is
why the reverse (handled-but-undeclared) direction below only walks
`handledLiteralNames`: a prefix family has no single concrete name to check
for a matching declaration. -/
def isHandled (name : String) : Bool :=
  handledLiteralNames.contains name
  || handledPrimitivePrefixes.any (fun p => name.startsWith p)
  || (name.startsWith "malgo_" && name.endsWith "to_string")

/-- One entry in the allowlist: a primitive name plus why the gap is
currently acknowledged rather than a regression. -/
structure KnownGap where
  name : String
  reason : String

/-- Declared via `foreign import` in `Builtin.mlg` but `fetchPrimitive` has
no case for them (yet). Implementing these is explicitly out of scope for
#453. -/
def knownMissing : List KnownGap :=
  [ { name := "malgo_exit_failure"
    , reason := "eval fails at runtime with \"not implemented\" (#453's original repro)" }
  , { name := "malgo_flush", reason := "declared, never wired into fetchPrimitive" }
  , { name := "malgo_get_char", reason := "declared, never wired into fetchPrimitive" }
  , { name := "sqrt", reason := "declared, never wired into fetchPrimitive" }
  , { name := "sqrtf", reason := "declared, never wired into fetchPrimitive" } ]

/-- Handled by `fetchPrimitive` but no `foreign import` declares them: dead
legacy arms with zero references anywhere under `runtime/`. Removing these
arms is explicitly out of scope for #453. -/
def knownUndeclared : List KnownGap :=
  [ { name := "malgo_str_len", reason := "dead legacy arm, zero references under runtime/" }
  , { name := "malgo_str_at", reason := "dead legacy arm, zero references under runtime/" }
  , { name := "malgo_str_sub", reason := "dead legacy arm, zero references under runtime/" }
  , { name := "malgo_str_to_int", reason := "dead legacy arm, zero references under runtime/" }
  , { name := "malgo_int_to_str", reason := "dead legacy arm, zero references under runtime/" }
  , { name := "malgo_rune_to_str", reason := "dead legacy arm, zero references under runtime/" }
  , { name := "malgo_int_to_rune", reason := "dead legacy arm, zero references under runtime/" }
  , { name := "malgo_rune_to_int", reason := "dead legacy arm, zero references under runtime/" } ]

/-- Handled by `fetchPrimitive`, undeclared by design rather than by
oversight: primitives the compiler inserts internally that no `.mlg` source
ever spells, so they can never carry a `foreign import`. Unlike
`knownMissing`/`knownUndeclared`, these are not gaps to close, so they are
checked in the reverse direction but excluded from the staleness pass
below. -/
def internalPrimitives : List String :=
  [ "reuseHint" ]

/-- Primitive names declared via `foreign import` in `Builtin.mlg`'s text,
in the style of `Malgo.Parser.extractPragmas`. -/
def declaredPrimitives (text : String) : List String :=
  (text.splitOn "\n").filterMap fun line =>
    if line.startsWith "foreign import " then
      some (toString ((line.drop "foreign import ".length).takeWhile (fun c => c != ' ' && c != ':')))
    else none

def run : IO Nat := do
  let text ← IO.FS.readFile (System.FilePath.mk "runtime/malgo/Builtin.mlg")
  let declared := declaredPrimitives text
  let mut failed := 0
  let mut total := 0

  -- Sanity floor: if the parse above ever regresses to empty (or near-empty
  -- — `Builtin.mlg` moved, the `foreign import ` marker changed, cwd is
  -- wrong), every check below would vacuously pass. Guard against that
  -- silent no-op explicitly, pinned comfortably under the current count of
  -- 99 declared primitives.
  if declared.length < 90 then
    failed := failed + 1
    IO.println
      s!"FAIL Malgo.PrimitiveCoverage/declaredPrimitives: parsed only {declared.length} foreign imports from Builtin.mlg — the parse or the path is broken"

  -- Forward: every declared primitive must be handled, unless it is a
  -- known-missing gap.
  for name in declared do
    total := total + 1
    if isHandled name then
      IO.println s!"ok Malgo.PrimitiveCoverage/declared/{name}"
    else if knownMissing.any (·.name == name) then
      IO.println s!"ok Malgo.PrimitiveCoverage/declared/{name} (known-missing)"
    else
      failed := failed + 1
      IO.println
        s!"FAIL Malgo.PrimitiveCoverage/declared/{name}: no fetchPrimitive case, and not in knownMissing"

  -- Reverse: every literally-handled name must be declared, unless it is a
  -- known-undeclared gap or an internal (undeclarable-by-design) primitive.
  for name in handledLiteralNames do
    total := total + 1
    if declared.contains name then
      IO.println s!"ok Malgo.PrimitiveCoverage/handled/{name}"
    else if knownUndeclared.any (·.name == name) then
      IO.println s!"ok Malgo.PrimitiveCoverage/handled/{name} (known-undeclared)"
    else if internalPrimitives.contains name then
      IO.println s!"ok Malgo.PrimitiveCoverage/handled/{name} (internal)"
    else
      failed := failed + 1
      IO.println
        s!"FAIL Malgo.PrimitiveCoverage/handled/{name}: no foreign import in Builtin.mlg, and not in knownUndeclared"

  -- Allowlist staleness: a known-missing name must still be
  -- declared-but-unhandled, and a known-undeclared name must still be
  -- handled-but-undeclared, or the entry has rotted and must be deleted.
  -- `internalPrimitives` is exempt: it is undeclarable by design, not a gap
  -- that could close.
  for gap in knownMissing do
    total := total + 1
    if declared.contains gap.name && !isHandled gap.name then
      IO.println s!"ok Malgo.PrimitiveCoverage/knownMissing/{gap.name}"
    else
      failed := failed + 1
      IO.println
        s!"FAIL Malgo.PrimitiveCoverage/knownMissing/{gap.name}: allowlist entry is stale (no longer both declared and unhandled) — delete it"
  for gap in knownUndeclared do
    total := total + 1
    if isHandled gap.name && !declared.contains gap.name then
      IO.println s!"ok Malgo.PrimitiveCoverage/knownUndeclared/{gap.name}"
    else
      failed := failed + 1
      IO.println
        s!"FAIL Malgo.PrimitiveCoverage/knownUndeclared/{gap.name}: allowlist entry is stale (no longer both handled and undeclared) — delete it"

  IO.println s!"=== primitive-coverage {total - failed}/{total} passed ==="
  return failed

end PrimitiveCoverage

namespace ZigCorpus

open Malgo.Backend.Zig.Ir

private def hasRcOps : Block → Bool
  | .mk stmts term =>
    stmts.any (fun st => match st with
      | .dup _ => true | .drop _ => true | _ => false)
    || (match term with
        | .«if» _ t e => hasRcOps t || hasRcOps e
        | _ => false)

private def hasReuseOp : Block → Bool
  | .mk stmts term =>
    stmts.any (fun st => match st with | .dropReuse .. => true | _ => false)
    || (match term with
        | .«if» _ t e => hasReuseOp t || hasReuseOp e
        | _ => false)

def run (memo : IO.Ref (Std.HashMap String Malgo.Driver.AllIR)) (names : List String) :
    IO Nat := do
  let mut failed := 0
  let mut reuseFired := false
  for name in names do
    let path := Malgo.Test.testcasePath name
    try
      let ir ← Malgo.Test.getAllIR memo path
      let stages ← MalgoM.run Malgo.Test.flag {}
        (Malgo.Backend.Zig.runZigStages ir.moduleName ir.join)
      if stages.closureConv.program.funcs.any (fun fn => hasRcOps fn.body) then
        IO.println s!"FAIL Malgo.Backend.Zig.corpus/{name}: ClosureConv inserted an RC op"
        failed := failed + 1
      else match Malgo.Backend.Zig.RcCheck.checkProgram stages.reuse with
        | .ok () =>
          if stages.reuse.program.funcs.any (fun fn => hasReuseOp fn.body) then
            reuseFired := true
          IO.println s!"ok Malgo.Backend.Zig.corpus/{name}"
        | .error vs =>
          IO.println s!"FAIL Malgo.Backend.Zig.corpus/{name}: {vs.length} linearity violation(s)"
          failed := failed + 1
    catch e =>
      IO.println s!"FAIL Malgo.Backend.Zig.corpus/{name}: {toString e}"
      failed := failed + 1
  if reuseFired then
    IO.println "ok Malgo.Backend.Zig.corpus/reuse fired at least once"
  else
    IO.println "FAIL Malgo.Backend.Zig.corpus: no Drop/MkStruct pairing anywhere in the corpus"
    failed := failed + 1
  IO.println s!"=== zig-corpus {names.length + 1 - failed}/{names.length + 1} passed ==="
  return failed

end ZigCorpus

namespace ZigReuse

open Malgo.Backend.Zig.Ir
open Malgo.Sequent.Fun (Name Tag)

private def rr : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩

private def rn (s : String) (uniq : Nat) : Name :=
  { name := s, moduleName := .moduleName "ReuseTest", sort := .temporal uniq }

private def vA : Name := rn "a" 10
private def vB : Name := rn "b" 11
private def vK : Name := rn "k" 12
private def vS1 : Name := rn "s1" 13
private def vS2 : Name := rn "s2" 14

/-- The Nth fresh token `reuseFunc` mints, 0-indexed. -/
private def tok (i : Nat) : Name := rn "reuse" i

private def runReuse (params : List Name) (body : Block) : IO Block := do
  let fn : Func := { range := rr, name := rn "fn" 0, kind := .topLevelFn,
                     selfVar := rn "self" 1, params, body }
  let fn' ← MalgoM.run Malgo.Test.flag {} (Malgo.Backend.Zig.Reuse.reuseFunc (.moduleName "ReuseTest") fn)
  return fn'.body

private structure Case where
  name : String
  params : List Name
  input : Block
  expected : Block

private def cases : List Case :=
  [ { name := "pairs a Drop with a later same-shape MkStruct"
    , params := [vA, vB, vK]
    , input := .mk [.drop vA, .let vS1 (.mkStruct .tuple [vB])] (.applyCo vK vS1)
    , expected := .mk [.dropReuse (tok 0) vA 1,
                       .let vS1 (.mkStructReuse (tok 0) .tuple [vB])] (.applyCo vK vS1) }
    -- vB was dropped last, so it pairs with the nearest MkStruct.
  , { name := "pairs LIFO: the most recently dropped wins the nearest MkStruct"
    , params := [vA, vB, vK]
    , input := .mk [.drop vA, .drop vB, .let vS1 (.mkStruct .tuple [vK]),
                    .let vS2 (.mkStruct .tuple [vK])] (.callClosure vK [vS1, vS2])
    , expected := .mk [.dropReuse (tok 0) vB 1, .let vS1 (.mkStructReuse (tok 0) .tuple [vK]),
                       .dropReuse (tok 1) vA 1, .let vS2 (.mkStructReuse (tok 1) .tuple [vK])]
                    (.callClosure vK [vS1, vS2]) }
  , { name := "flushes an unpaired Drop (no later MkStruct) unchanged"
    , params := [vA, vK]
    , input := .mk [.drop vA] (.applyCo vK vA)
    , expected := .mk [.drop vA] (.applyCo vK vA) }
    -- Losing the leftover pending drop here would leak.
  , { name := "flushes the extra pending drop when drops outnumber MkStructs"
    , params := [vA, vB, vK]
    , input := .mk [.drop vA, .drop vB, .let vS1 (.mkStruct .tuple [vK])] (.callClosure vK [vS1])
    , expected := .mk [.dropReuse (tok 0) vB 1, .let vS1 (.mkStructReuse (tok 0) .tuple [vK]),
                       .drop vA] (.callClosure vK [vS1]) }
  , { name := "does not pair a Drop that comes after the MkStruct"
    , params := [vA, vB, vK]
    , input := .mk [.let vS1 (.mkStruct .tuple [vB]), .drop vA] (.callClosure vK [vS1])
    , expected := .mk [.let vS1 (.mkStruct .tuple [vB]), .drop vA] (.callClosure vK [vS1]) }
  , { name := "treats a bare PanicExpr as a barrier"
    , params := [vA, vK]
    , input := .mk [.drop vA, .let vB (.panicExpr "no match")] (.applyCo vK vB)
    , expected := .mk [.drop vA, .let vB (.panicExpr "no match")] (.applyCo vK vB) }
  , { name := "pairs within each TIf branch independently"
    , params := [vA, vB, vK]
    , input := .mk [] (.«if» (.isZero vA)
        (.mk [.drop vA, .let vS1 (.mkStruct .tuple [vB])] (.applyCo vK vS1))
        (.mk [.drop vB] (.applyCo vK vA)))
    , expected := .mk [] (.«if» (.isZero vA)
        (.mk [.dropReuse (tok 0) vA 1, .let vS1 (.mkStructReuse (tok 0) .tuple [vB])]
          (.applyCo vK vS1))
        (.mk [.drop vB] (.applyCo vK vA))) } ]

def run : IO Nat := do
  let mut failed := 0
  for c in cases do
    let actual ← runReuse c.params c.input
    if actual == c.expected then
      IO.println s!"ok Malgo.Backend.Zig.Reuse/{c.name}"
    else
      IO.println s!"FAIL Malgo.Backend.Zig.Reuse/{c.name}"
      failed := failed + 1
  IO.println s!"=== zig-reuse {cases.length - failed}/{cases.length} passed ==="
  return failed

end ZigReuse

def runMetPageGate : IO Nat := do
  match ← Malgo.Test.MetPage.run with
  | .ok () => IO.println "ok Malgo.Debug.MetPage/index"; return 0
  | .error msg => IO.println s!"FAIL Malgo.Debug.MetPage/index: {msg}"; return 1

/-! ## Infer full-program gate (port of `Malgo.InferSpec`)

Each testcase is driven Parse → Rename → Elaborate → Infer (via the engine's
`fetchInferredModule`, mirroring the Haskell `InferredModule` handler) with
`useInfer := true` and `malgo2025` enabled. Success = inference completes
without throwing; there is no golden (Haskell's `knownBadInfer` is empty, so
every testcase must succeed). -/

def inferFlag : Flag := { flag with useInfer := true }
def malgo2025 : FeatureFlags := FeatureFlags.ofList [.malgo2025]

private def parseError' (e : Malgo.Parser.PError) : CompileError :=
  { passName := "Parser", message := e.render, range? := none }

/-- Parse a source file and seed it into `cacheParsedModule` under its own
module name; returns the parsed module. -/
private def seedParsed (ws : Workspace) (db : Malgo.Query.QueryDB) (path : System.FilePath) :
    MalgoM (Malgo.Syntax.Module .parse) := do
  let text ← MalgoM.io (IO.FS.readFile path)
  match ← Malgo.Parser.pass ws path text with
  | (.error e, _) => throw (parseError' e)
  | (.ok parsed, flags) =>
    -- Mirrors `Malgo.Driver.linkForCli`: a source's own pragmas (e.g.
    -- `#malgo-2025`, `#c-style-apply`) must be folded into the ambient
    -- `FeatureFlags` the same way the real CLI does, or a testcase that
    -- only requests a feature via its own pragma (rather than via the
    -- flags this call was seeded with) infers under a weaker feature set
    -- than `malgo eval` actually gives it.
    addFeatures flags
    MalgoM.io (db.cacheParsedModule.modify (·.insert parsed.moduleName parsed))
    return parsed

/-- Register Builtin/Prelude module paths so the engine's bare-name import
resolution (`getModulePath`) finds them at their `runtime/malgo/` origin
without needing a pre-seeded workspace mirror. -/
private def registerRuntime (ws : Workspace) : MalgoM Unit := do
  ws.registerModule (.moduleName "Builtin") (← ws.parseArtifactPathFromPwd "runtime/malgo/Builtin.mlg")
  ws.registerModule (.moduleName "Prelude") (← ws.parseArtifactPathFromPwd "runtime/malgo/Prelude.mlg")

/-- Accumulate each dependency's exported `TyEnv`, left-biased on collision
(port of `InferSpec.runInferCapturing`'s own inline `foldlM ... <> ...`
fold — NOT `Query.Engine.buildDepsEnv`, which is a different, stricter
function used only by the query engine's own per-module internal
accumulation and the CLI's `LinkedProgram` handler).

Haskell's test harness deliberately does not route the top-level testcase's
OWN direct dependencies through `Query.Engine.buildDepsEnv`: a testcase that
directly imports both Prelude and Builtin (the overwhelmingly common case —
Prelude does `module {..} = import Builtin`, re-exporting every Builtin name)
would otherwise always collide, since Prelude's own exported env already
contains Builtin's names. `Map.<>`'s left-bias silently prefers the
earlier-listed dependency's copy, which is fine since re-exported names are
identical regardless of which dependency contributed them.

The engine's `buildDepsEnv` stays genuinely strict, matching what the
Haskell `Query/Engine.hs` did, and `scripts/cli-gate.sh`'s error mode
asserts that: `malgo eval --infer` on a real testcase (e.g. `Undefined.mlg`,
which has this exact Prelude+Builtin diamond) is expected to fail. The
Haskell binary failed on it identically — confirmed empirically before that
implementation was retired — so this is a shared latent defect being pinned
down, not a Lean-side regression. -/
private def buildDepsEnvLenient (ws : Workspace) (db : Malgo.Query.QueryDB)
    (deps : Std.TreeSet ModuleName) : MalgoM Malgo.Infer.TyEnv :=
  deps.toList.foldlM (init := ({} : Malgo.Infer.TyEnv)) fun acc dep => do
    let depEnv ← Malgo.Query.Engine.fetchInferredModule ws db dep
    pure (depEnv.foldl (fun m k v => if m.contains k then m else m.insert k v) acc)

/-- Shared Infer pipeline body: seed the parsed module → rename → (Elaborate,
if `malgo2025`) → infer, under an explicit feature set and against an
explicit source `path`. Discards the result -- callers only care whether it
throws. Factored out so `driveInferWithFeatures` and `inferErrorGolden`
below (positive and negative golden-error drivers over the same pipeline)
can't silently drift out of sync. -/
private def runInferPipeline (ws : Workspace) (features : FeatureFlags)
    (path : System.FilePath) : IO Unit :=
  MalgoM.run inferFlag features do
    let db ← MalgoM.io Malgo.Query.newQueryDB
    registerRuntime ws
    let parsed ← seedParsed ws db path
    let (renamed, rnState) ← Malgo.Query.Engine.fetchRenamedModule ws db parsed.moduleName
    let depsEnv ← buildDepsEnvLenient ws db rnState.dependencies
    let bindGroup ← if (← Malgo.hasFeature .malgo2025)
      then Malgo.Elaborate.pass renamed.moduleName renamed.moduleDefinition
      else pure renamed.moduleDefinition
    let _ ← Malgo.Infer.pass renamed.moduleName depsEnv bindGroup
    pure ()

/-- Run inference on one testcase under an explicit feature set; `.ok ()` on
success, `.error msg` on any compile error. Generalizes `driveInfer` (below,
which fixes `malgo2025` on) so the CLI-default configuration can be driven
the same way -- `malgo eval`'s `eval` subcommand has no `--feature` option
at all (only the separate `debug-trace` subcommand takes `--malgo2025`), so
every real invocation of `malgo eval --infer` runs with an *empty*
`FeatureFlags`, never the `malgo2025`-on one this test suite otherwise
always exercises. Renames the target directly (never calling
`fetchInferredModule` ON the top-level testcase itself — only on each of
its dependencies, via `buildDepsEnvLenient`), then infers with the
accumulated deps env. -/
def driveInferWithFeatures (features : FeatureFlags) (name : String) :
    IO (Except String Unit) := do
  let ws ← Workspace.setup
  try
    runInferPipeline ws features (testcasePath name)
    return .ok ()
  catch e => return .error (toString e)

/-- Run inference on one testcase with `malgo2025` on; `.ok ()` on success,
`.error msg` on any compile error. Mirrors Haskell
`InferSpec.runInfer`/`driveInfer`. See `driveInferWithFeatures` for the
CLI-default (`malgo2025`-off) counterpart. -/
def driveInfer (name : String) : IO (Except String Unit) := driveInferWithFeatures malgo2025 name

/-- Capture the module name, dependency env, and exported env from inference
(port of `runInferCapturing`), for the export-boundary tests. -/
def runInferCapturing (name : String) :
    IO (ModuleName × Malgo.Infer.TyEnv × Malgo.Infer.TyEnv) := do
  let ws ← Workspace.setup
  MalgoM.run inferFlag malgo2025 do
    let db ← MalgoM.io Malgo.Query.newQueryDB
    registerRuntime ws
    let parsed ← seedParsed ws db (testcasePath name)
    let (renamed, rnState) ← Malgo.Query.Engine.fetchRenamedModule ws db parsed.moduleName
    let mn := renamed.moduleName
    let depsEnv ← buildDepsEnvLenient ws db rnState.dependencies
    let bindGroup ← if (← Malgo.hasFeature .malgo2025)
      then Malgo.Elaborate.pass mn renamed.moduleDefinition
      else pure renamed.moduleDefinition
    let (_, finalEnv) ← Malgo.Infer.pass mn depsEnv bindGroup
    let exported := finalEnv.foldl (fun m k v => if depsEnv.contains k then m else m.insert k v) {}
    pure (mn, depsEnv, exported)

/-- Run the two `InferredModule export boundary` unit tests on Echo.mlg.
Returns the number of failures (0 = both pass). -/
def runBoundaryTests : IO Nat := do
  let mut failed := 0
  let (mn, depsEnv, exported) ← runInferCapturing "Echo"
  let leakedDep := exported.toList.filter (fun (k, _) => depsEnv.contains k)
  let leakedMod := exported.toList.filter (fun (k, _) => k.moduleName != mn)
  if leakedDep.isEmpty && leakedMod.isEmpty then
    IO.println "ok Malgo.Infer/boundary/no-dep-leakage"
  else
    failed := failed + 1
    IO.println s!"FAIL Malgo.Infer/boundary/no-dep-leakage: {leakedDep.length} dep, {leakedMod.length} foreign-module"
  let mainNames := exported.toList.filter (fun (k, _) => k.name == "main")
  if mainNames.length == 1 then
    IO.println "ok Malgo.Infer/boundary/single-main"
  else
    failed := failed + 1
    IO.println s!"FAIL Malgo.Infer/boundary/single-main: found {mainNames.length}"
  return failed

/-! ## Infer error golden cases (`test/Malgo/InferSpec/errors/*.mlg`)

Mirrors the Parser/Rename error-golden pattern above: each fixture is
driven through the same Parse → Rename → (Elaborate) → Infer pipeline as
`driveInfer`, and the caught `CompileError`'s rendered text (i.e.
`InferError.render` wrapped by `Malgo.wrapError`) becomes the golden.
Unlike `runInferGate`, these testcases are deliberately negative. -/

private def inferErrorCaseDir : System.FilePath :=
  System.FilePath.mk "test/Malgo/InferSpec/errors"

private def inferErrorGolden (path : System.FilePath) : IO String := do
  let ws ← Workspace.setup
  try
    runInferPipeline ws malgo2025 path
    return "INFERRED WITHOUT ERROR"
  catch e => return toString e

private def inferErrorCase (name : String) : GoldenCase :=
  { group := "Malgo.Infer", name := s!"error/{name}",
    run := inferErrorGolden (inferErrorCaseDir / s!"{name}.mlg") }

def enumerateInferErrorCases : IO (List String) := enumerateErrorCases inferErrorCaseDir

def inferErrorCases (names : List String) : List GoldenCase := names.map inferErrorCase

/-! ## Constraint/Unify unit tests (port of the `Malgo.InferSpec` unit cases)

`applySubst`/`composeSubst`/`unify` are `partial` (equi-recursive), so these
run as runtime assertions rather than `#guard`. -/

open Malgo.Infer in
private def mkId (n : String) : Id :=
  { name := n, moduleName := .moduleName "Test", sort := .external }

open Malgo.Infer in
/-- Run `unify` in a fresh inference state; `.ok subst` or `.error msg`. -/
private def runUnify (t1 t2 : Ty) : IO (Except String Subst) := do
  try
    let s ← MalgoM.run flag {} do
      let ctx : InferCtx := { moduleName := .moduleName "Test" }
      let act := ((unify dummyRange t1 t2).run ctx).run' initGenState
      Malgo.wrapError "Unify" InferError.render InferError.rangeOf act
    return .ok s
  catch e => return .error (toString e)

open Malgo.Infer in
/-- Constraint/Unify unit checks; returns `(failures, total)`. -/
def runUnitTests : IO (Nat × Nat) := do
  let t0 := mkId "_t0"
  let t1 := mkId "_t1"
  let s1 : Subst := ({} : Subst).insert t0 (.tMu (.tArr (.tBound 0) (.tVar t1 0)))
  let s2 : Subst := ({} : Subst).insert t1 (.tArr (.tVar t0 0) tyInt32)
  let composed := composeSubst s2 s1
  let composed' := composeSubst (({} : Subst).insert t1 tyString) (({} : Subst).insert t0 tyInt32)
  let checks : List (String × IO Bool) :=
    [ ("applySubst-var", pure (applySubst (({} : Subst).insert t0 tyInt32) (.tVar t0 0) == tyInt32)),
      ("applySubst-unrelated", pure (applySubst (({} : Subst).insert t0 tyInt32) (.tVar t1 0) == .tVar t1 0)),
      ("applySubst-nosub-tbound",
        pure (applySubst (({} : Subst).insert (mkId "a") tyInt32) (.tMu (.tBound 0)) == .tMu (.tBound 0))),
      ("freeVars-forall",
        pure ((freeVars (.tForall (.tArr (.tBound 0) (.tVar (mkId "b") 0)))).toList == [mkId "b"])),
      ("occursIn-nested", pure (occursIn (mkId "a") (.tArr (.tVar (mkId "a") 0) tyInt32))),
      ("occursIn-mu-bound", pure (occursIn (mkId "a") (.tMu (.tBound 0)) == false)),
      ("generalize-above-level", pure ((generalize 0 (.tArr (.tVar t0 1) (.tVar t1 0))).vars == [t0])),
      ("generalize-mu-not-quantified",
        pure ((generalize 0 (.tMu (.tArr (.tBound 0) (.tVar (mkId "_t6") 2)))).vars == [mkId "_t6"])),
      ("composeSubst-330-no-self-ref",
        pure (match composed.get? t0 with | some v => occursIn t0 v == false | none => false)),
      ("composeSubst-idempotent",
        pure (composed'.get? t0 == some tyInt32 && composed'.get? t1 == some tyString)),
      ("unify-con-eq", do pure (← runUnify tyInt32 tyInt32).isOk),
      ("unify-con-neq", do pure (!(← runUnify tyInt32 tyString).isOk)),
      ("unify-var-concrete", do pure (← runUnify (.tVar t0 0) tyInt32).isOk),
      ("unify-recursive-to-mu", do
        pure (match (← runUnify (.tVar t0 0) (.tArr (.tVar t0 0) tyInt32)) with
          | .ok subst => subst.get? t0 == some (.tMu (.tArr (.tBound 0) tyInt32))
          | .error _ => false)),
      ("unify-reject-mu-vs-int", do
        pure (!(← runUnify (.tMu (.tArr (.tBound 0) tyInt32)) (.tArr tyInt32 tyInt32)).isOk)),
      ("unify-alpha-equiv-mu", do
        pure (← runUnify (.tMu (.tArr (.tBound 0) tyInt32)) (.tMu (.tArr (.tBound 0) tyInt32))).isOk),
      ("unify-bottom", do pure (← runUnify .tBottom tyInt32).isOk),
      ("unify-tuple-len-mismatch", do
        pure (!(← runUnify (.tTuple [tyInt32]) (.tTuple [tyInt32, tyString])).isOk)),
      ("unify-record-match", do
        pure (← runUnify (.tRecord [("x", tyInt32), ("y", tyString)] none)
                         (.tRecord [("x", tyInt32), ("y", tyString)] none)).isOk),
      -- Regression coverage for the commonSubst/row-tail interaction: a
      -- common field's type is a metavariable that ALSO appears in a
      -- leftover field on the other side. Unifying the common field solves
      -- the metavariable; that solution must propagate into the leftover
      -- field before it's folded into the open row tail's unification, or
      -- the row tail resolves to a stale, unsubstituted type.
      ("unify-record-open-row-propagates-commonSubst", do
        let commonMeta := mkId "_commonR"
        let rowVar := mkId "_rowR"
        pure (match (← runUnify
            (.tRecord [("x", .tVar commonMeta 0)] (some (.tVar rowVar 0)))
            (.tRecord [("x", tyInt32), ("y", .tVar commonMeta 0)] none)) with
          | .ok subst => subst.get? rowVar == some (.tRecord [("y", tyInt32)] none)
          | .error _ => false)),
      ("unify-variant-open-row-propagates-commonSubst", do
        let commonMeta := mkId "_commonV"
        let rowVar := mkId "_rowV"
        pure (match (← runUnify
            (.tVariant [("X", [.tVar commonMeta 0])] (some (.tVar rowVar 0)))
            (.tVariant [("X", [tyInt32]), ("Y", [.tVar commonMeta 0])] none)) with
          | .ok subst => subst.get? rowVar == some (.tVariant [("Y", [tyInt32])] none)
          | .error _ => false)) ]
  let mut failed := 0
  for (label, act) in checks do
    if ← act then IO.println s!"ok Malgo.Infer/unit/{label}"
    else
      failed := failed + 1
      IO.println s!"FAIL Malgo.Infer/unit/{label}"
  return (failed, checks.length)

/-! ## Infer error constructor coverage

Guards against a newly added `InferError` constructor going silently
untested: `inferErrorCoverage` is written as an exhaustive match with no
wildcard arm, so Lean's own exhaustiveness checker forces every case to be
addressed here the moment a constructor is added or removed to
`Malgo.Infer.InferError` -- a compile error, not a runtime gap that could
go unnoticed. -/

/-- Whether an `InferError` constructor is exercised by a golden fixture
under `test/Malgo/InferSpec/errors/`, or explicitly known-unreachable
(with why). -/
inductive InferErrorCoverage where
  | tested (fixture : String)
  | unreachable (reason : String)

open Malgo.Infer in
/-- One entry per `InferError` constructor. Field values are irrelevant
here (matched via `..`) -- only the constructor shape decides coverage. -/
def inferErrorCoverage : InferError → InferErrorCoverage
  | .unificationError .. => .tested "ConstructorMismatch"
  | .unboundVariable .. =>
    .unreachable "Rename's own scope resolution guarantees every renamed \
term-level reference already resolves to a binding before Infer ever sees \
it; the one plausible non-recursive-scope gap (a local self-referencing \
`let`) is already rejected at Rename, not Infer. No construct was found \
that reaches Infer.lean's `env.get? name == none` branches while Rename \
still succeeds -- see docs/plans/2026-08-30-type-system-error-test-coverage.md."
  | .occursCheckError .. =>
    .unreachable "the unifier resolves a genuine occurs-check situation \
into an equirecursive `tMu` type (Infer/Unify.lean's `unifyInternal`, the \
`.tVar x _, t` case) instead of throwing -- this constructor is never \
constructed by current code."
  | .notImplemented .. =>
    .unreachable "exists only as the `Inhabited InferError` default \
witness (Infer/Constraint.lean); never thrown by real inference code."
  | .cyclicSynonym .. => .tested "CyclicSynonym"
  | .synonymArityMismatch .. => .tested "SynonymArityMissing"

open Malgo.Infer in
/-- Cross-checks every `.tested` fixture name above against the real
fixture directory, catching a rename on either side that forgot the
other. The `InferError` sample values are throwaways: only their
constructor shape reaches `inferErrorCoverage`. -/
def runInferErrorCoverageGate (fixtureNames : List String) : IO UInt32 := do
  let samples : List (String × InferErrorCoverage) :=
    [ ("unificationError", inferErrorCoverage (.unificationError dummyRange .tBottom .tBottom "sample"))
    , ("unboundVariable", inferErrorCoverage (.unboundVariable dummyRange (mkId "sample")))
    , ("occursCheckError", inferErrorCoverage (.occursCheckError dummyRange (mkId "sample") .tBottom))
    , ("notImplemented", inferErrorCoverage (.notImplemented dummyRange "sample"))
    , ("cyclicSynonym", inferErrorCoverage (.cyclicSynonym dummyRange (mkId "sample")))
    , ("synonymArityMismatch", inferErrorCoverage (.synonymArityMismatch dummyRange (mkId "sample") 0 0)) ]
  let mut failed := 0
  for (tag, cov) in samples do
    match cov with
    | .tested fixture =>
      if fixtureNames.contains fixture then
        IO.println s!"ok Malgo.InferErrorCoverage/{tag} (fixture: {fixture})"
      else
        failed := failed + 1
        IO.println s!"FAIL Malgo.InferErrorCoverage/{tag}: fixture '{fixture}' not found under {inferErrorCaseDir}"
    | .unreachable reason =>
      IO.println s!"ok Malgo.InferErrorCoverage/{tag} (known-unreachable: {reason})"
  return if failed == 0 then 0 else 1

/-- One entry in a known-gap allowlist for a corpus-wide Infer sweep: `name`
currently fails under that sweep's feature set but succeeds under another,
so it's an acknowledged gap rather than a regression. Each entry is checked
for staleness (still actually failing) on every run; closing a gap for real
means deleting its entry, same discipline as
`PrimitiveCoverage.knownMissing`. -/
structure InferKnownGap where
  name : String
  reason : String

/-- Fixtures that currently fail Infer under the CLI-default (`malgo2025`
off) configuration but pass with `malgo2025` on -- i.e. real gaps in what
`malgo eval --infer` can type-check today, not artifacts of the
`runInferGateCliDefault` gate below. -/
def inferMalgo2025OffKnownGaps : List InferKnownGap :=
  [ { name := "Bool"
    , reason := "without Elaborate's desugaring, inference reports a \
Type error (Expected: (() -> ...), Actual: Bool) -- a real gap in `malgo \
eval --infer`'s default behavior, not a test-harness artifact." }
  , { name := "FibCopattern"
    , reason := "without Elaborate, this copattern-based codata \
definition fails to unify -- same class of real gap as Bool above." } ]

/-- Run `driveInferWithFeatures features n` for every name in `selected`,
printing `ok`/`FAIL` under `label/<name>` and treating any name in
`knownGaps` as an expected failure. Shared by `runInferGate`
(`malgo2025`-on, no allowlist) and `runInferGateCliDefault` (CLI-default,
`inferMalgo2025OffKnownGaps`) so the two corpus sweeps can't drift apart.
Returns `(failed, total)`. -/
def runInferCorpusLoop (label : String) (features : FeatureFlags)
    (knownGaps : List InferKnownGap) (selected : List String) : IO (Nat × Nat) := do
  let mut failed := 0
  let mut total := 0
  for n in selected do
    total := total + 1
    match (← driveInferWithFeatures features n), knownGaps.find? (·.name == n) with
    | .ok (), some _ =>
      failed := failed + 1
      IO.println s!"FAIL {label}/{n}: passes now -- known-gap entry is stale, delete it"
    | .ok (), none => IO.println s!"ok {label}/{n}"
    | .error _, some gap => IO.println s!"ok {label}/{n} (known-gap: {gap.reason})"
    | .error msg, none =>
      failed := failed + 1
      IO.println s!"FAIL {label}/{n}: {msg}"
  return (failed, total)

/-- Full-program inference gate + boundary tests. Respects `--match`. -/
def runInferGate (cfg : Config) (names : List String) : IO UInt32 := do
  let selected := names.filter fun n => match cfg.match? with
    | none => true
    | some pat => (s!"Malgo.Infer/{n}".splitOn pat).length > 1
  let runBoundary := match cfg.match? with
    | none => true
    | some pat => (("Malgo.Infer/boundary").splitOn pat).length > 1
  let runUnit := match cfg.match? with
    | none => true
    | some pat => (("Malgo.Infer/unit").splitOn pat).length > 1
  if selected.isEmpty && !runBoundary && !runUnit then return 0
  let mut failed := 0
  let mut total := 0
  if runUnit then
    let (unitFail, unitTotal) ← runUnitTests
    failed := failed + unitFail
    total := total + unitTotal
  let (corpusFailed, corpusTotal) ← runInferCorpusLoop "Malgo.Infer" malgo2025 [] selected
  failed := failed + corpusFailed
  total := total + corpusTotal
  if runBoundary then
    total := total + 2
    failed := failed + (← runBoundaryTests)
  IO.println s!"=== infer {total - failed}/{total} passed ==="
  return if failed == 0 then 0 else 1

/-- Full-program inference gate under the CLI-DEFAULT configuration: empty
`FeatureFlags` (`malgo2025` off), matching what `malgo eval --infer`
actually runs with in every real invocation (see
`driveInferWithFeatures`'s doc comment). `runInferGate` above only ever
exercises `malgo2025`-on, a configuration unreachable through the `eval`
subcommand at all -- this gate is what actually stands behind the CLI's
default type-checking behavior. Two known, real gaps are allowlisted (see
`inferMalgo2025OffKnownGaps`); any other regression fails the gate.
Respects `--match` against the `Malgo.InferCliDefault/<name>` label. -/
def runInferGateCliDefault (cfg : Config) (names : List String) : IO UInt32 := do
  let selected := names.filter fun n => match cfg.match? with
    | none => true
    | some pat => (s!"Malgo.InferCliDefault/{n}".splitOn pat).length > 1
  if selected.isEmpty then return 0
  let (failed, total) ←
    runInferCorpusLoop "Malgo.InferCliDefault" {} inferMalgo2025OffKnownGaps selected
  IO.println s!"=== infer-cli-default {total - failed}/{total} passed ==="
  return if failed == 0 then 0 else 1

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
    let evalNames := names.filter (!Malgo.Test.evalHarnessUnsupported.contains ·)
    let forthNames ← Malgo.Test.enumerateForthTestcases
    let lintNames ← Malgo.Test.enumerateLintCases
    let exampleNames ← Malgo.Test.enumerateExamples
    let parserErrorNames ← Malgo.Test.enumerateParserErrorCases
    let renameErrorNames ← Malgo.Test.enumerateRenameErrorCases
    let inferErrorNames ← Malgo.Test.enumerateInferErrorCases
    let allCases := Malgo.Test.cases
      ++ Malgo.Test.parserErrorCases parserErrorNames
      ++ Malgo.Test.renameErrorCases renameErrorNames
      ++ Malgo.Test.inferErrorCases inferErrorNames
      ++ Malgo.Test.toCoreCases memo names
      ++ Malgo.Test.evalCases memo evalNames
      ++ Malgo.Test.bigStepEvalCases memo evalNames
      ++ Malgo.Test.forthCases memo forthNames
      ++ Malgo.Test.lintCases lintNames
      ++ Malgo.Test.prettyIRCases names exampleNames
    let goldenCode ← Malgo.Test.runSuite cfg allCases
    let inferCode ← Malgo.Test.runInferGate cfg names
    let inferCliDefaultCode ← Malgo.Test.runInferGateCliDefault cfg names
    let inferErrorCoverageFailures ← Malgo.Test.runInferErrorCoverageGate inferErrorNames
    let metPageFailures ← Malgo.Test.runMetPageGate
    let reuseFailures ← Malgo.Test.ZigReuse.run
    let corpusFailures ← Malgo.Test.ZigCorpus.run memo names
    let irFailures ← Malgo.Test.IrInvariants.run memo names
    let specFailures ← Malgo.Test.ReuseSpec.run
    let parserFailures ← Malgo.Test.ParserSurface.run
    let panicFailures ← Malgo.Test.PanicGate.run memo
    let primitiveCoverageFailures ← Malgo.Test.PrimitiveCoverage.run
    return (if goldenCode == 0 && inferCode == 0 && inferCliDefaultCode == 0
        && inferErrorCoverageFailures == 0
        && metPageFailures == 0
        && reuseFailures == 0 && corpusFailures == 0 && irFailures == 0
        && specFailures == 0 && parserFailures == 0 && panicFailures == 0
        && primitiveCoverageFailures == 0
      then 0 else 1)
