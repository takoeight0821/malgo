import Malgo
import Test.Fingerprint

/-! Port of `app/malgo/Main.hs`: the `malgo` CLI.

Haskell uses optparse-applicative; this is a hand-rolled argv parser (no
combinator library) with the same three subcommands, flags, and defaults.
Byte-parity with optparse's generated `--help` is explicitly not a goal.

Dispatch is stubbed for M1: the pipeline (`Malgo.Driver`) and the Zig
backend and the linter are not ported yet, so each `run*` arm prints a
clear message and exits 1. `runEval` for `--target eval` carries the single
integration seam (see the `TODO(M1 integration)` below). -/

open Malgo

namespace Malgo.Cli

/-- Zig build mode for `compile --opt` (the Zig backend and its real
`OptMode` are not ported yet; this local enum only records the choice). -/
inductive OptMode where
  | debug
  | releaseSafe
  | releaseFast
  deriving BEq, Repr

def OptMode.toString : OptMode → String
  | .debug => "debug"
  | .releaseSafe => "release-safe"
  | .releaseFast => "release-fast"

def parseTargetArg : String → Except String Target
  | "eval" => .ok .eval
  | "zig" => .ok .zig
  | t => .error s!"Unknown target: {t}"

def parseEvalModeArg : String → Except String EvalMode
  | "smallstep" => .ok .smallStep
  | "bigstep" => .ok .bigStep
  | m => .error s!"Unknown eval-mode: {m}"

def parseOptModeArg : String → Except String OptMode
  | "debug" => .ok .debug
  | "release-safe" => .ok .releaseSafe
  | "release-fast" => .ok .releaseFast
  | m => .error s!"Unknown opt mode: {m}"

def usage : String :=
  "malgo programming language\n\n" ++
  "Usage: malgo COMMAND\n\n" ++
  "Commands:\n" ++
  "  eval SOURCE [--no-opt] [--lambdalift] [--debug-mode]\n" ++
  "              [--target eval|zig] [--eval-mode smallstep|bigstep]\n" ++
  "              [--infer] [ARG...]\n" ++
  "  compile SOURCE [-o|--output OUT] [--opt debug|release-safe|release-fast]\n" ++
  "  lint SOURCE [--deny-warnings]\n" ++
  "  debug-trace SOURCE [-o|--output trace.html] [--infer] [--malgo2025]"

/-- Split `--name=value` into `(--name, some value)`; a plain token is
`(token, none)`. Mirrors optparse accepting both `--name value` and
`--name=value`. -/
def splitInline (a : String) : String × Option String :=
  match a.splitOn "=" with
  | k :: v :: vs => (k, some ("=".intercalate (v :: vs)))
  | _ => (a, none)

/-- Value for an option that takes one: the inline `=value`, else the next
argument. -/
def takeValue (name : String) (inline : Option String) (rest : List String) :
    Except String (String × List String) :=
  match inline with
  | some v => .ok (v, rest)
  | none => match rest with
    | v :: rest' => .ok (v, rest')
    | [] => .error s!"{name} requires a value"

def isHelp (a : String) : Bool := a == "--help" || a == "-h"

/-! ## `eval` -/

private structure EvalAcc where
  source : Option System.FilePath := none
  noOptimize : Bool := false
  lambdaLift : Bool := false
  debugMode : Bool := false
  target : Target := .eval
  evalMode : EvalMode := .smallStep
  useInfer : Bool := false
  /-- Program args, collected reversed. -/
  programArgs : List String := []

private def EvalAcc.addPositional (acc : EvalAcc) (a : String) : EvalAcc :=
  match acc.source with
  | none => { acc with source := some a }
  | some _ => { acc with programArgs := a :: acc.programArgs }

private partial def parseEval (args : List String) (acc : EvalAcc) (noMoreOpts : Bool) :
    Except String EvalAcc :=
  match args with
  | [] => .ok acc
  | a :: rest =>
    if noMoreOpts then
      parseEval rest (acc.addPositional a) true
    else if a == "--" then
      parseEval rest acc true
    else
      let (name, inline) := splitInline a
      if name == "--no-opt" then parseEval rest { acc with noOptimize := true } false
      else if name == "--lambdalift" then parseEval rest { acc with lambdaLift := true } false
      else if name == "--debug-mode" then parseEval rest { acc with debugMode := true } false
      else if name == "--infer" then parseEval rest { acc with useInfer := true } false
      else if name == "--target" then do
        let (v, rest') ← takeValue "--target" inline rest
        let t ← parseTargetArg v
        parseEval rest' { acc with target := t } false
      else if name == "--eval-mode" then do
        let (v, rest') ← takeValue "--eval-mode" inline rest
        let m ← parseEvalModeArg v
        parseEval rest' { acc with evalMode := m } false
      else if a.startsWith "-" && a != "-" then
        .error s!"unknown option: {a}"
      else
        parseEval rest (acc.addPositional a) false

/-! ## `compile` -/

private structure CompileAcc where
  source : Option System.FilePath := none
  outPath : Option System.FilePath := none
  optMode : OptMode := .debug

private partial def parseCompile (args : List String) (acc : CompileAcc) :
    Except String CompileAcc :=
  match args with
  | [] => .ok acc
  | a :: rest =>
    let (name, inline) := splitInline a
    if name == "--opt" then do
      let (v, rest') ← takeValue "--opt" inline rest
      let m ← parseOptModeArg v
      parseCompile rest' { acc with optMode := m }
    else if name == "-o" || name == "--output" then do
      let (v, rest') ← takeValue name inline rest
      parseCompile rest' { acc with outPath := some (System.FilePath.mk v) }
    else if a.startsWith "-" && a != "-" then
      .error s!"unknown option: {a}"
    else match acc.source with
      | none => parseCompile rest { acc with source := some (System.FilePath.mk a) }
      | some _ => .error s!"unexpected extra argument: {a}"

/-! ## `lint` -/

private structure LintAcc where
  source : Option System.FilePath := none
  denyWarnings : Bool := false

private partial def parseLint (args : List String) (acc : LintAcc) : Except String LintAcc :=
  match args with
  | [] => .ok acc
  | a :: rest =>
    if a == "--deny-warnings" then parseLint rest { acc with denyWarnings := true }
    else if a.startsWith "-" && a != "-" then .error s!"unknown option: {a}"
    else match acc.source with
      | none => parseLint rest { acc with source := some (System.FilePath.mk a) }
      | some _ => .error s!"unexpected extra argument: {a}"

/-! ## `debug-trace` (MET — M-exp-Tracer, port of `app/met`) -/

private structure DebugTraceAcc where
  source : Option System.FilePath := none
  outPath : System.FilePath := System.FilePath.mk "trace.html"
  useInfer : Bool := false
  malgo2025 : Bool := false

private partial def parseDebugTrace (args : List String) (acc : DebugTraceAcc) :
    Except String DebugTraceAcc :=
  match args with
  | [] => .ok acc
  | a :: rest =>
    let (name, inline) := splitInline a
    if name == "-o" || name == "--output" then do
      let (v, rest') ← takeValue name inline rest
      parseDebugTrace rest' { acc with outPath := System.FilePath.mk v }
    else if name == "--infer" then parseDebugTrace rest { acc with useInfer := true }
    else if name == "--malgo2025" then parseDebugTrace rest { acc with malgo2025 := true }
    else if a.startsWith "-" && a != "-" then .error s!"unknown option: {a}"
    else match acc.source with
      | none => parseDebugTrace rest { acc with source := some (System.FilePath.mk a) }
      | some _ => .error s!"unexpected extra argument: {a}"

/-! ## `dump` (hidden: cross-implementation IR-fingerprint parity tool) -/

inductive DumpStage where
  | flatFingerprint
  | joinFingerprint
  deriving BEq

private structure DumpAcc where
  source : Option System.FilePath := none
  stage : Option DumpStage := none

private partial def parseDump (args : List String) (acc : DumpAcc) : Except String DumpAcc :=
  match args with
  | [] => .ok acc
  | a :: rest =>
    let (name, inline) := splitInline a
    if name == "--stage" then do
      let (v, rest') ← takeValue "--stage" inline rest
      let stage ← match v with
        | "flat-fingerprint" => .ok .flatFingerprint
        | "join-fingerprint" => .ok .joinFingerprint
        | s => .error s!"Unknown stage: {s}"
      parseDump rest' { acc with stage := some stage }
    else if a.startsWith "-" && a != "-" then .error s!"unknown option: {a}"
    else match acc.source with
      | none => parseDump rest { acc with source := some (System.FilePath.mk a) }
      | some _ => .error s!"unexpected extra argument: {a}"

/-! ## Path resolution and dispatch -/

/-- Port of Haskell `makeAbsolute`: resolve to an absolute, normalized path.
`realPath` covers the common case (the source exists); the fallback keeps a
nonexistent path absolute against the cwd. -/
def makeAbsolute (p : System.FilePath) : IO System.FilePath := do
  try
    IO.FS.realPath p
  catch _ =>
    if p.isAbsolute then return p else return (← IO.currentDir) / p

def runEval (flag : Flag) (source : System.FilePath) : IO UInt32 := do
  match flag.target with
  | .eval =>
    try
      Malgo.Driver.compileAndEval flag source
    catch e =>
      IO.eprintln (toString e)
      return 1
  | .zig =>
    try
      Malgo.Driver.compileZig flag source
    catch e =>
      IO.eprintln (toString e)
      return 1

def toToolchainOpt : OptMode → Malgo.Backend.Zig.Toolchain.OptMode
  | .debug => .debug
  | .releaseSafe => .releaseSafe
  | .releaseFast => .releaseFast

def runCompile (source : System.FilePath) (outPath : Option System.FilePath)
    (optMode : OptMode) : IO UInt32 := do
  let out := outPath.getD (System.FilePath.mk ((source.fileStem).getD source.toString))
  -- `source` is already absolute (resolved in `main`); comparing the
  -- default output's absolute form catches `malgo compile hello` on an
  -- extension-less source in its own directory silently overwriting it.
  let outAbs ← makeAbsolute out
  if outAbs.toString == source.toString then
    IO.eprintln s!"malgo compile: refusing to overwrite the source file ({out})."
    IO.eprintln "Pass -o/--output to choose a different path."
    return 1
  let flag : Flag :=
    { noOptimize := false, lambdaLift := false, debugMode := false, testMode := false,
      target := .zig, evalMode := .smallStep, useInfer := false, programArgs := [] }
  try
    Malgo.Driver.compileToNativeExecutable flag source out (toToolchainOpt optMode)
  catch e =>
    IO.eprintln (toString e)
    return 1

/-- Port of the Haskell CLI's `Lint` arm: print every diagnostic to stderr,
one per line (`hPutDoc` + a line-terminating `hPutStrLn ""` — NOT a blank
separator line, just `prettyDiagnostic`'s own newline-free rendering
followed by a single newline), then exit 1 if any diagnostic is
`.error`-severity, or if `--deny-warnings` was passed and the diagnostic
list is non-empty. -/
def runLint (source : System.FilePath) (denyWarnings : Bool) : IO UInt32 := do
  let lintFlag : Flag :=
    { noOptimize := true, lambdaLift := false, debugMode := false, testMode := false,
      target := .eval, evalMode := .smallStep, useInfer := false, programArgs := [] }
  try
    let diags ← Malgo.MalgoM.run lintFlag {} (Malgo.Lint.lintFile source)
    for d in diags do
      IO.eprintln (Malgo.Doc.render (Malgo.Lint.prettyDiagnostic d))
    let hasError := diags.any (·.severity == .error)
    if hasError || (denyWarnings && !diags.isEmpty) then return 1
    return 0
  catch e =>
    IO.eprintln (toString e)
    return 1

/-- Port of `app/met` (MET — the M-exp-Tracer): trace `source` through the
whole pipeline once (`Malgo.Debug.Pipeline.runTrace`) and render every
stage/transition into one self-contained static HTML file at `outPath` —
see `Malgo.Debug.MetPage`'s module doc for why this replaces Haskell's
`met` web server rather than porting a server. -/
def runDebugTrace (source outPath : System.FilePath) (useInfer malgo2025 : Bool) : IO UInt32 := do
  try
    let stages ← Malgo.Debug.Pipeline.runTrace source useInfer malgo2025
    let html := Malgo.Debug.MetPage.renderPage source.toString stages
    IO.FS.writeFile outPath html
    IO.eprintln s!"MET: traced {source} ({stages.length} stages) -> {outPath}"
    return 0
  catch e =>
    IO.eprintln (toString e)
    return 1

/-- Hidden developer subcommand: lower a single module (unlinked) through
Parse → Rename → ToFun → ToCore → Flat → Join and print its canonical,
format-immune IR fingerprint. It exists because `scripts/lean-parity.sh`
diffed this against the Haskell implementation's own `dumpFingerprint`,
without depending on uniq-numbering or formatting parity; with that
implementation retired it survives as a debugging aid. -/
def runDump (source : System.FilePath) (stage : DumpStage) : IO UInt32 := do
  try
    let ws ← Malgo.Workspace.setup
    let cache : Malgo.Driver.InterfaceCache ← IO.mkRef {}
    let flag : Flag :=
      { noOptimize := false, lambdaLift := false, debugMode := false, testMode := false,
        target := .eval, evalMode := .smallStep, useInfer := false, programArgs := [] }
    let ir ← Malgo.MalgoM.run flag {} (Malgo.Driver.compileToJoin ws cache source)
    IO.println <|
      match stage with
      | .flatFingerprint => Malgo.Test.Fingerprint.fingerprintFlat ir.flat
      | .joinFingerprint => Malgo.Test.Fingerprint.fingerprintJoin ir.join
    return 0
  catch e =>
    IO.eprintln (toString e)
    return 1

/-- Flag record for `eval`, built from the parsed options. -/
private def evalFlag (acc : EvalAcc) : Flag :=
  { noOptimize := acc.noOptimize, lambdaLift := acc.lambdaLift,
    debugMode := acc.debugMode, testMode := false, target := acc.target,
    evalMode := acc.evalMode, useInfer := acc.useInfer,
    programArgs := acc.programArgs.reverse }

/-- Report a parse error with usage and exit 1. -/
private def parseError (msg : String) : IO UInt32 := do
  IO.eprintln s!"malgo: {msg}"
  IO.eprintln usage
  return 1

def run : List String → IO UInt32
  | [] => do IO.eprintln usage; return 1
  | cmd :: rest =>
    if isHelp cmd then do IO.println usage; return 0
    -- `--help` after `--` belongs to the evaluated program, not to malgo.
    else if (rest.takeWhile (· != "--")).any isHelp then do IO.println usage; return 0
    else match cmd with
      | "eval" =>
        match parseEval rest {} false with
        | .error e => parseError e
        | .ok acc => match acc.source with
          | none => parseError "eval: missing SOURCE"
          | some src => do
            let srcAbs ← makeAbsolute src
            runEval (evalFlag acc) srcAbs
      | "compile" =>
        match parseCompile rest {} with
        | .error e => parseError e
        | .ok acc => match acc.source with
          | none => parseError "compile: missing SOURCE"
          | some src => do
            let srcAbs ← makeAbsolute src
            runCompile srcAbs acc.outPath acc.optMode
      | "lint" =>
        match parseLint rest {} with
        | .error e => parseError e
        | .ok acc => match acc.source with
          | none => parseError "lint: missing SOURCE"
          | some src => do
            let srcAbs ← makeAbsolute src
            runLint srcAbs acc.denyWarnings
      | "dump" =>
        match parseDump rest {} with
        | .error e => parseError e
        | .ok acc => match acc.source, acc.stage with
          | none, _ => parseError "dump: missing SOURCE"
          | _, none => parseError "dump: missing --stage"
          | some src, some stage => do
            let srcAbs ← makeAbsolute src
            runDump srcAbs stage
      | "debug-trace" =>
        match parseDebugTrace rest {} with
        | .error e => parseError e
        | .ok acc => match acc.source with
          | none => parseError "debug-trace: missing SOURCE"
          | some src => do
            let srcAbs ← makeAbsolute src
            runDebugTrace srcAbs acc.outPath acc.useInfer acc.malgo2025
      | other => parseError s!"unknown command: {other}"

end Malgo.Cli

def main (args : List String) : IO UInt32 :=
  Malgo.Cli.run args
