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
`findOnPath` and never spawning at all when no `zig` is present avoids
depending on that message's exact text, and — critically — still lets a REAL
spawn/exit failure (a `zig` that IS on `PATH` but errors for some other
reason: permissions, a full cache-dir disk, a resource limit) fall through
to the generic "zig build-exe failed" branch with its own stderr intact,
rather than being misdiagnosed as "not installed".

`findOnPath` is a coarser check than Haskell's `findExecutable`: it only
confirms a file named `zig` exists on `PATH`, not that it is executable.
Lean's `IO.FS.Metadata` (v4.32.0) has no field exposing POSIX access rights
(`IO.setAccessRights` exists to set permissions, but there is no matching
getter) — checked directly against the pinned toolchain's source, not
assumed — so there is no portable way to check the execute bit from here. A
`zig` on `PATH` that exists but lacks the execute bit is therefore still
misreported as "found"; the resulting spawn failure surfaces as whatever
`IO.Process.output` produces for that case (see the `.error e` branch below),
not as the "not installed" message. -/

namespace Malgo.Backend.Zig.Toolchain

/-- Search `$PATH` for a file named `name`. Only checks existence, not
executability (see the module doc for why). Returns `false` if `$PATH` is
unset or no directory on it contains a matching file. -/
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
       -- Deliberately NOT `-fsingle-threaded`, though nothing in the runtime or
       -- in generated code spawns a thread: `std.heap.SmpAllocator` opens with
       -- `assert(!builtin.single_threaded)` ("you're holding it wrong"), so the
       -- flag is a comptime error while `runtime.zig` uses `smp_allocator` for
       -- ReleaseFast. Reaching for it again means first giving the runtime an
       -- allocator that does not require threads -- an Object pool would serve
       -- both goals at once, since Object is a single fixed size (#385).
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
