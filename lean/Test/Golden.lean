/-! Hand-rolled golden runner, layout-compatible with hspec-golden:
`.golden/<Group>/<Case>/golden` holds the expected bytes, `actual` is
written next to it on mismatch. Groups that legitimately diverge from the
Haskell output live in `.golden-lean/` with the same layout; the runner
checks `.golden/` first and falls back. -/

namespace Malgo.Test

structure GoldenCase where
  group : String
  name : String
  run : IO String

def repoRoot : IO System.FilePath := do
  return System.FilePath.mk ((← IO.getEnv "MALGO_REPO_ROOT").getD "..")

private def goldenDirs (c : GoldenCase) : IO (Array System.FilePath) := do
  let root ← repoRoot
  return #[root / ".golden" / c.group / c.name, root / ".golden-lean" / c.group / c.name]

/-- Locate the golden file for a case: prefer `.golden/`, fall back to
`.golden-lean/`; with `update = true` a missing case is created under
`.golden-lean/` (never under the shared `.golden/`). -/
private def resolveGoldenDir (update : Bool) (c : GoldenCase) : IO (Option System.FilePath) := do
  for dir in (← goldenDirs c) do
    if (← (dir / "golden").pathExists) then
      return some dir
  if update then
    return some ((← goldenDirs c)[1]!)
  return none

structure Outcome where
  passed : Bool
  message : String

def checkGolden (update : Bool) (c : GoldenCase) : IO Outcome := do
  let label := s!"{c.group}/{c.name}"
  match ← resolveGoldenDir update c with
  | none => return ⟨false, s!"MISSING {label}: no golden file (run with --update to create)"⟩
  | some dir =>
    let goldenPath := dir / "golden"
    let actual ← c.run
    let existing ← if (← goldenPath.pathExists) then some <$> IO.FS.readFile goldenPath else pure none
    if existing == some actual then
      return ⟨true, s!"ok {label}"⟩
    else if update then
      IO.FS.createDirAll dir
      IO.FS.writeFile goldenPath actual
      return ⟨true, s!"UPDATED {label}"⟩
    else
      IO.FS.createDirAll dir
      IO.FS.writeFile (dir / "actual") actual
      return ⟨false, s!"FAIL {label}: output differs from {goldenPath} (actual written alongside)"⟩

structure Config where
  match? : Option String := none
  update : Bool := false

def parseArgs : List String → Except String Config
  | [] => .ok {}
  | "" :: rest => parseArgs rest
  | "--update" :: rest => do
    let cfg ← parseArgs rest
    return { cfg with update := true }
  | "--match" :: pat :: rest => do
    let cfg ← parseArgs rest
    return { cfg with match? := some pat }
  | arg :: _ => .error s!"unknown argument: {arg} (usage: [--match PATTERN] [--update])"

def runSuite (cfg : Config) (cases : List GoldenCase) : IO UInt32 := do
  let selected := match cfg.match? with
    | none => cases
    | some pat => cases.filter fun c => (s!"{c.group}/{c.name}".splitOn pat).length > 1
  if selected.isEmpty then
    IO.eprintln "no test cases selected"
    return 1
  let mut failed := 0
  for c in selected do
    let outcome ← checkGolden cfg.update c
    IO.println outcome.message
    unless outcome.passed do
      failed := failed + 1
  IO.println s!"=== {selected.length - failed}/{selected.length} passed ==="
  return if failed == 0 then 0 else 1

end Malgo.Test
