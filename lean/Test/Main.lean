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

def cases : List GoldenCase :=
  [parserCase "Primitive", parserCase "HelloImport", parserCase "Eventually"]

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
