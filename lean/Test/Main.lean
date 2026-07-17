import Malgo
import Test.Golden

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

/-- The 18 non-error Rename goldens: Builtin/Prelude (from `runtime/malgo/`)
plus the 16 representative testcases. -/
def renameCases : List GoldenCase :=
  [ renameCase "Builtin" "runtime/malgo/Builtin.mlg",
    renameCase "Prelude" "runtime/malgo/Prelude.mlg" ] ++
  ([ "Primitive", "ListOps", "HelloImport", "RecordTest", "RowPoly", "CodataE2E",
     "FibCopattern", "LabelGoto", "NestedMatch", "CStyleApply", "ZeroArgs", "Eventually",
     "TuplePattern", "NestedRecursive", "StringPattern", "LetPattern" ].map
    (fun n => renameCase n (testcasePath n)))

def cases : List GoldenCase :=
  [parserCase "Primitive", parserCase "HelloImport", parserCase "Eventually"] ++ renameCases

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
    Malgo.Test.runSuite cfg Malgo.Test.cases
