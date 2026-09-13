import Malgo.Backend.OptMode

/-! Invokes the system `go` toolchain to turn generated Go source into a
native executable, for `malgo compile --target go`.

Two things keep the build off the network, which matters because the Bash
sandbox this project develops under has no egress:

* `GOTOOLCHAIN=local`. When a `go.mod`'s `go` directive names a release
  newer than the installed toolchain, Go downloads that toolchain. The
  generated module asks for an old floor so this never triggers, and the
  environment variable forbids it outright even if the floor is raised.
* The generated program imports only the standard library, so there is
  nothing to fetch from a module proxy.

`findOnPath` is checked up front for the same reason as the Zig toolchain:
Lean's `IO.Process.output` does not raise when the command is missing — it
returns `.ok` with a nonzero exit code — so without the check a missing `go`
would be reported as a build failure with a confusing message. -/

namespace Malgo.Backend.Go.Toolchain

open Malgo.Backend (OptMode)

/-- The `go` directive written into the generated module. Deliberately an
old floor rather than the installed version: a floor never triggers a
toolchain download, while naming the exact installed release would start
doing so the moment someone builds with an older `go`. -/
def goDirective : String := "1.24"

private def findOnPath (name : String) : IO Bool := do
  let some path ← IO.getEnv "PATH" | return false
  for dir in path.splitOn ":" do
    if dir.isEmpty then continue
    let candidate := System.FilePath.mk dir / name
    if (← candidate.pathExists) then
      return true
  return false

/-- Go has one code generator: there is no `-O` and no bounds-check switch,
so `release-safe` and `release-fast` produce identical machine code and
differ only in whether the binary keeps its symbol table. `debug` is the
only level that changes codegen, and it changes it a lot — `-N -l` disables
optimization and inlining, which makes the trampoline's dispatch loop
several times slower. -/
def optModeFlags : OptMode → Array String
  | .debug => #["-gcflags=all=-N -l"]
  | .releaseSafe => #[]
  | .releaseFast => #["-ldflags=-s -w"]

private def absolutise (p : String) : IO String := do
  if p.startsWith "/" then return p
  let cwd ← IO.currentDir
  return (cwd / p).toString

/-- Compile Go source text to a native executable at `outPath`.

The source is written twice: once as `<outPath>.go` so a failing build leaves
something to read, and once as `main.go` inside a throwaway module under
`goCacheRoot`. The module is what `go build` needs — a bare `.go` file with
no `go.mod` in scope is not buildable — and keeping it under the cache root
means the build touches no path outside the workspace. -/
def buildExecutable (goCacheRoot srcText outPath : String) (mode : OptMode) : IO Unit := do
  unless (← findOnPath "go") do
    IO.eprintln "go not found on PATH."
    IO.eprintln "Install it via 'mise install' (pinned in mise.toml) or https://go.dev/dl/"
    IO.Process.exit 1
  let readableSrc := outPath ++ ".go"
  IO.FS.writeFile readableSrc srcText
  let buildDir := System.FilePath.mk goCacheRoot / "go-build"
  IO.FS.createDirAll buildDir
  IO.FS.writeFile (buildDir / "go.mod") s!"module malgoprog\n\ngo {goDirective}\n"
  IO.FS.writeFile (buildDir / "main.go") srcText
  let absOut ← absolutise outPath
  let env : Array (String × Option String) :=
    #[ ("GOTOOLCHAIN", some "local"),
       ("GOCACHE", some ((System.FilePath.mk goCacheRoot / "go-cache").toString)),
       -- The module has no dependencies, so nothing is ever fetched; this
       -- only stops `go` from consulting a proxy if that ever changes.
       ("GOFLAGS", some "-mod=mod"),
       ("GOPROXY", some "off") ]
  let args := #["build", "-o", absOut] ++ optModeFlags mode ++ #["."]
  let out ← (IO.Process.output { cmd := "go", args, cwd := some buildDir, env }).toBaseIO
  match out with
  | .error e =>
    IO.eprintln s!"failed to run go: {e}"
    IO.Process.exit 1
  | .ok result =>
    if result.exitCode == 0 then
      pure ()
    else do
      IO.eprintln s!"go build failed (source kept at {readableSrc}):"
      IO.eprintln result.stderr
      IO.Process.exit 1

end Malgo.Backend.Go.Toolchain
