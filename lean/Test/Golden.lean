/-! Hand-rolled golden runner, layout-compatible with hspec-golden:
`.golden/<Group>/<Case>/golden` holds the expected bytes, `actual` is
written next to it on mismatch. Cases whose Lean output legitimately
diverges from Haskell get an override in `.golden-lean/` with the same
layout: the runner checks `.golden-lean/` first (an explicit override
wins), then the shared `.golden/`. `--update` only ever writes to
`.golden-lean/` — the shared tree stays Haskell-owned. -/

namespace Malgo.Test

structure GoldenCase where
  group : String
  name : String
  run : IO String

/-- The runner's cwd is the repository root: `main` chdirs there before
running the suite (so `Workspace` and artifact relPaths resolve against the
root). Golden paths are therefore relative to ".". -/
def repoRoot : IO System.FilePath := do
  return System.FilePath.mk "."

/-- Override directory (`.golden-lean/`) first, shared `.golden/` second. -/
private def goldenDirs (c : GoldenCase) : IO (Array System.FilePath) := do
  let root ← repoRoot
  return #[root / ".golden-lean" / c.group / c.name, root / ".golden" / c.group / c.name]

private def resolveGoldenDir (c : GoldenCase) : IO (Option System.FilePath) := do
  for dir in (← goldenDirs c) do
    if (← (dir / "golden").pathExists) then
      return some dir
  return none

structure Outcome where
  passed : Bool
  message : String

def checkGolden (update : Bool) (c : GoldenCase) : IO Outcome := do
  let label := s!"{c.group}/{c.name}"
  let overrideDir := (← goldenDirs c)[0]!
  match ← resolveGoldenDir c with
  | none =>
    if update then
      IO.FS.createDirAll overrideDir
      IO.FS.writeFile (overrideDir / "golden") (← c.run)
      return ⟨true, s!"CREATED {label} (in .golden-lean)"⟩
    return ⟨false, s!"MISSING {label}: no golden file (run with --update to create)"⟩
  | some dir =>
    let goldenPath := dir / "golden"
    let actual ← c.run
    if (← IO.FS.readFile goldenPath) == actual then
      return ⟨true, s!"ok {label}"⟩
    else if update then
      -- Never clobber the shared, Haskell-owned .golden/ tree: a
      -- divergent case gets (or refreshes) a .golden-lean override.
      IO.FS.createDirAll overrideDir
      IO.FS.writeFile (overrideDir / "golden") actual
      return ⟨true, s!"UPDATED {label} (override in .golden-lean)"⟩
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
  let root ← repoRoot
  unless (← (root / ".golden").isDir) do
    IO.eprintln s!"golden root not found at {root / ".golden"} — run from lean/ (lake test) \
or set MALGO_REPO_ROOT to the repository root"
    return 2
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
