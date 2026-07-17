import Malgo

/-! Port of `app/malgo/Main.hs`: the `malgo` CLI.

Haskell uses optparse-applicative; this is a hand-rolled argv parser (no
combinator library) with the same three subcommands, flags, and defaults.
Byte-parity with optparse's generated `--help` is explicitly not a goal.

Dispatch is stubbed for M1: the pipeline (`Malgo.Driver`) and the Scheme/Zig
backends and the linter are not ported yet, so each `run*` arm prints a
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
  | "scheme" => .ok .scheme
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
  "              [--target eval|scheme|zig] [--eval-mode smallstep|bigstep]\n" ++
  "              [--infer] [ARG...]\n" ++
  "  compile SOURCE [-o|--output OUT] [--opt debug|release-safe|release-fast]\n" ++
  "  lint SOURCE [--deny-warnings]"

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
  | .scheme =>
    IO.eprintln "malgo eval --target scheme: the Scheme backend is not yet ported."
    return 1
  | .zig =>
    IO.eprintln "malgo eval --target zig: the Zig backend is not yet ported."
    return 1

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
  IO.eprintln s!"malgo compile: the Zig backend is not yet ported (would emit {out}, opt {optMode.toString})."
  return 1

def runLint (source : System.FilePath) (_denyWarnings : Bool) : IO UInt32 := do
  IO.eprintln s!"malgo lint: the linter is not yet ported: {source}"
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
    else if rest.any isHelp then do IO.println usage; return 0
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
      | other => parseError s!"unknown command: {other}"

end Malgo.Cli

def main (args : List String) : IO UInt32 :=
  Malgo.Cli.run args
