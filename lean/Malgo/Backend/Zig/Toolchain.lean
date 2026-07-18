/-! Port of `src/Malgo/Backend/Zig/Toolchain.hs`: invokes the system `zig`
toolchain to turn generated Zig source into a native executable, for the
`malgo compile` subcommand.

Haskell shells out with `System.Process.readProcessWithExitCode`; Lean uses
`IO.Process.output`. `findExecutable "zig"` has no Lean-stdlib equivalent, so
a missing `zig` surfaces as the spawn `IO.Error` from `IO.Process.output`,
which is caught and reported with the same install hint. -/

namespace Malgo.Backend.Zig.Toolchain

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
  | .error _ => do
    IO.eprintln "zig not found on PATH."
    IO.eprintln "Install it via 'mise install' (pinned in mise.toml) or https://ziglang.org/download/"
    IO.Process.exit 1
  | .ok result =>
    if result.exitCode == 0 then
      pure ()
    else do
      IO.eprintln s!"zig build-exe failed (source kept at {srcPath}):"
      IO.eprintln result.stderr
      IO.Process.exit 1

end Malgo.Backend.Zig.Toolchain
