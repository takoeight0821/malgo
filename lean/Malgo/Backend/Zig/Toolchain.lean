/-! Port of `src/Malgo/Backend/Zig/Toolchain.hs`: invokes the system `zig`
toolchain to turn generated Zig source into a native executable, for the
`malgo compile` subcommand.

Haskell shells out with `System.Process.readProcessWithExitCode`, but first
calls `findExecutable "zig"` and only attempts the spawn if that succeeds.
Lean uses `IO.Process.output`, which has no Lean-stdlib `findExecutable`
equivalent AND, unlike Haskell's `readProcessWithExitCode`, does not raise an
`IO.Error` when the command is missing — empirically (verified against this
binary) it instead returns `.ok` with a nonzero exit code and a `stderr` of
"could not execute external process 'zig'". Checking `PATH` up front with
`findOnPath` (re-implementing `findExecutable`'s search) and never spawning
at all when `zig` is absent avoids depending on that message's exact text,
mirrors Haskell's actual control flow instead of guessing at Lean's
process-spawn error shape, and — critically — still lets a REAL spawn/exit
failure (a `zig` that IS on `PATH` but errors for some other reason:
permissions, a full cache-dir disk, a resource limit) fall through to the
generic "zig build-exe failed" branch with its own stderr intact, rather
than being misdiagnosed as "not installed". -/

namespace Malgo.Backend.Zig.Toolchain

/-- Port of `System.Directory.findExecutable "zig"`: search `$PATH` for an
executable file named `zig`. Returns `none` if `$PATH` is unset or no
directory on it contains one. -/
private def findOnPath (name : String) : IO Bool := do
  let some path ← IO.getEnv "PATH" | return false
  for dir in path.splitOn ":" do
    if dir.isEmpty then continue
    let candidate := System.FilePath.mk dir / name
    if (← candidate.pathExists) then
      return true
  return false

inductive OptMode where
  | debug
  | releaseSafe
  | releaseFast
  deriving BEq, Repr

def parseOptMode : String → Except String OptMode
  | "debug" => .ok .debug
  | "release-safe" => .ok .releaseSafe
  | "release-fast" => .ok .releaseFast
  | m => .error s!"Unknown --opt mode: {m}"

def optModeFlag : OptMode → String
  | .debug => "Debug"
  | .releaseSafe => "ReleaseSafe"
  | .releaseFast => "ReleaseFast"

/-- Compile a `.zig` source file to a native executable at `outPath`, using a
per-invocation cache directory under `zigCacheRoot` so repeated builds do not
touch any path outside the workspace. On failure prints `zig`'s stderr and
exits the process with code 1 (mirroring Haskell's `exitFailure`). -/
def buildExecutable (zigCacheRoot srcPath outPath : String) (mode : OptMode) : IO Unit := do
  unless (← findOnPath "zig") do
    IO.eprintln "zig not found on PATH."
    IO.eprintln "Install it via 'mise install' (pinned in mise.toml) or https://ziglang.org/download/"
    IO.Process.exit 1
  let args : Array String :=
    #[ "build-exe",
       srcPath,
       "-femit-bin=" ++ outPath,
       "--cache-dir", zigCacheRoot ++ "/zig-cache",
       "--global-cache-dir", zigCacheRoot ++ "/zig-global-cache",
       "-O", optModeFlag mode,
       -- The runtime calls std.c.write/std.c.read (deliberately bypassing
       -- Zig's async std.Io; see runtime.zig's module doc). macOS links libc
       -- unconditionally (part of libSystem), but Zig does not link libc by
       -- default on Linux, so every generated program would fail at link time
       -- there without this.
       "-lc" ]
  let out ← (IO.Process.output { cmd := "zig", args }).toBaseIO
  match out with
  | .error e =>
    -- `zig` is on PATH (checked above) yet the spawn itself raised an
    -- `IO.Error` — an OS-level failure distinct from "not installed" (e.g. a
    -- resource limit during fork/exec). Surface it rather than guessing.
    IO.eprintln s!"failed to run zig: {e}"
    IO.Process.exit 1
  | .ok result =>
    if result.exitCode == 0 then
      pure ()
    else do
      IO.eprintln s!"zig build-exe failed (source kept at {srcPath}):"
      IO.eprintln result.stderr
      IO.Process.exit 1

end Malgo.Backend.Zig.Toolchain
