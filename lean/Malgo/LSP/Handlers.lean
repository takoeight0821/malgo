import Malgo.LSP.Diagnostics
import Malgo.LSP.Server
import Malgo.Query
import Malgo.Query.Engine
import Malgo.Syntax

/-! Port of `malgo-lsp/src/Malgo/LSP/Handlers.hs`: LSP request and
notification handlers for the Malgo language server.

Only parse + rename ever run here (`lspFlag.useInfer = false`, matching
Haskell's own module doc: "parse + rename only, no codegen") — hover works
directly off the renamed AST, and diagnostics come only from parse/rename
failures (never from `Malgo.Lint`, which the Haskell LSP never calls
either — confirmed by grep against `malgo-lsp/`). -/

namespace Malgo.LSP

open Malgo
open Malgo.Syntax
open Malgo.LSP.Server

/-- Default compiler flags for LSP mode. -/
def lspFlag : Flag :=
  { noOptimize := false, lambdaLift := false, debugMode := false, testMode := false,
    target := .eval, evalMode := .smallStep, useInfer := false, programArgs := [] }

/-- Persistent state shared across all LSP requests. -/
structure LspState where
  db : Malgo.Query.QueryDB
  /-- Maps each open file's URI to its last successfully renamed module. -/
  renamedCache : IO.Ref (Std.TreeMap NormalizedUri (ModuleName × Module .rename))

/-- Run a query-based compilation, returning diagnostics on error. Mirrors
Haskell's `runLspCompile` — `MalgoM.runCatching` is the Lean equivalent of
Haskell's `runError @CompileError . ...` effect stack, preserving the
structured `CompileError` (not stringifying it) so `compileToDiagnostics`
can recover a real position. -/
def runLspCompile (action : MalgoM α) : IO (Except (List Diagnostic) α) := do
  match ← MalgoM.runCatching lspFlag {} action with
  | .error e => return .error (compileToDiagnostics e)
  | .ok a => return .ok a

/-- Check a file: register the source, run up to rename, publish
diagnostics. Stores the renamed module in the cache on success (deletes any
stale entry on failure). The module's identity is the absolute file path
itself (matching Haskell's `ModuleName (T.pack filePath)`), not a resolved
package-relative name. -/
def checkFile (state : LspState) (nuri : NormalizedUri) (filePath : String) (content : String) :
    IO (List Diagnostic) := do
  let modName := ModuleName.moduleName filePath
  IO.eprintln "checkFile: entering runLspCompile"
  let result ← runLspCompile do
    MalgoM.io (IO.eprintln "checkFile: Workspace.setup done, updateSource next")
    MalgoM.io (Malgo.Query.Engine.updateSource state.db modName (System.FilePath.mk filePath) content)
    MalgoM.io (IO.eprintln "checkFile: updateSource done")
    MalgoM.io (Malgo.Query.Engine.invalidateModule state.db modName)
    MalgoM.io (IO.eprintln "checkFile: invalidateModule done")
    let ws ← getWorkspace
    MalgoM.io (IO.eprintln "checkFile: getWorkspace done, fetchRenamedModule next")
    let r ← Malgo.Query.Engine.fetchRenamedModule ws state.db modName
    MalgoM.io (IO.eprintln "checkFile: fetchRenamedModule done")
    pure r
  IO.eprintln "checkFile: runLspCompile returned"
  match result with
  | .error diags =>
    state.renamedCache.modify (·.erase nuri)
    return diags
  | .ok (renamedMod, _rnState) =>
    state.renamedCache.modify (·.insert nuri (renamedMod.moduleName, renamedMod))
    return []

/-- Handle `textDocument/didOpen`. -/
def handleDidOpen (env : ServerEnv) (state : LspState) (uri : Uri) (content : String) : IO Unit := do
  let nuri := toNormalizedUri uri
  updateFileContent env nuri content
  match uriToFilePath uri with
  | none => logMessage env s!"Could not resolve path for URI: {uri.unUri}"
  | some filePath =>
    logMessage env s!"didOpen: {filePath}"
    let diags ← checkFile state nuri filePath content
    logMessage env s!"didOpen: checkFile returned {diags.length} diagnostics, publishing"
    publishDiagnostics env nuri diags
    logMessage env "didOpen: publishDiagnostics returned"

/-- Handle `textDocument/didChange`. Assumes full-document sync (only
`contentChanges[0].text` is ever read, matching Haskell). -/
def handleDidChange (env : ServerEnv) (state : LspState) (uri : Uri) (fullText : Option String) :
    IO Unit := do
  let nuri := toNormalizedUri uri
  match fullText with
  | some txt => updateFileContent env nuri txt
  | none => pure ()
  match uriToFilePath uri with
  | none => logMessage env s!"Could not resolve path for URI: {uri.unUri}"
  | some filePath =>
    match ← getFileContent env nuri with
    | none => logMessage env s!"No VFS entry for: {filePath}"
    | some content =>
      logMessage env s!"didChange: {filePath}"
      let diags ← checkFile state nuri filePath content
      publishDiagnostics env nuri diags

/-- Handle `textDocument/didClose`: no re-check, no diagnostics clear
(matching Haskell — the client is expected to have already cleared or
doesn't care once a document is closed). -/
def handleDidClose (env : ServerEnv) (state : LspState) (uri : Uri) : IO Unit := do
  let nuri := toNormalizedUri uri
  removeFileContent env nuri
  state.renamedCache.modify (·.erase nuri)

/-- Render hover markdown for an identifier. -/
def hoverText (id : Id) : String :=
  s!"**`{id.name}`** *(from {id.moduleName.toStr})*"

/-- Test whether an LSP position (0-indexed) falls within a Malgo `Range`
(1-indexed). -/
def posInRange (pos : LspPosition) (r : Range) : Bool :=
  let sl : Int := (r.start.line : Int) - 1
  let sc : Int := (r.start.column : Int) - 1
  let el : Int := (r.stop.line : Int) - 1
  let ec : Int := (r.stop.column : Int) - 1
  (pos.line > sl || (pos.line == sl && pos.character ≥ sc)) &&
    (pos.line < el || (pos.line == el && pos.character ≤ ec))

/-- A crude "how big is this range" measure, for picking the tightest
enclosing identifier at a cursor position — same formula as Haskell's
`rangeSize` (line-delta dominates, then column-delta). -/
def rangeSize (r : Range) : Int :=
  let sl : Int := r.start.line; let sc : Int := r.start.column
  let el : Int := r.stop.line; let ec : Int := r.stop.column
  (el - sl) * 10000 + (ec - sc)

/-- Every `(Range, Id)` occurrence in a renamed pattern. `.list`/`.boxed`
patterns cannot occur at the `.rename` phase (`XListP`/`XBoxedP` are
`Empty` there — Rename desugars both away), so those arms are `nomatch`,
mirroring `Malgo.Syntax`'s own `Expr.range`/`Pat.range`-style phase-specific
matches rather than Haskell's apparently-partial (but actually
phase-exhaustive) `collectPat`. -/
partial def collectPat : Pat .rename → List (Range × Id)
  | .var _ _ => []
  | .con conRange conId pats => (conRange, conId) :: pats.flatMap collectPat
  | .tuple _ pats => pats.flatMap collectPat
  | .record _ kps => kps.flatMap fun (_, p) => collectPat p
  | .list ext _ => nomatch ext
  | .unboxed _ _ => []
  | .boxed ext _ => nomatch ext

mutual

/-- Every sub-expression of a renamed `Clause`'s body/patterns. -/
partial def collectClause : Clause .rename → List (Range × Id)
  | .mk _ pats body => (pats.toList.flatMap collectPat) ++ collectExpr body

/-- Every `(Range, Id)` occurrence in a renamed statement. `.letPS`/`.withS`
cannot occur at `.rename` (desugared/simplified away by Rename — see
`collectPat`'s doc comment for the same phase-deletion story). -/
partial def collectStmt : Stmt .rename → List (Range × Id)
  | .letS _ _ e => collectExpr e
  | .letPS ext _ _ => nomatch ext
  | .withS ext _ _ => nomatch ext
  | .noBind _ e => collectExpr e

/-- Every `(Range, Id)` occurrence in a renamed expression. `.boxed`/`.list`
cannot occur at `.rename` (see `collectPat`'s doc comment). -/
partial def collectExpr : Expr .rename → List (Range × Id)
  | .var range id => [(range, id)]
  | .unboxed _ _ => []
  | .boxed ext _ => nomatch ext
  | .apply _ fn arg => collectExpr fn ++ collectExpr arg
  | .opApp (opRange, _, _) opId lhs rhs => (opRange, opId) :: collectExpr lhs ++ collectExpr rhs
  | .project _ e _ => collectExpr e
  | .fn _ clauses => clauses.toList.flatMap collectClause
  | .tuple _ es => es.flatMap collectExpr
  | .record _ kvs => kvs.flatMap fun (_, e) => collectExpr e
  | .list ext _ => nomatch ext
  | .ann _ e _ => collectExpr e
  | .seq _ stmts => stmts.toList.flatMap collectStmt
  | .parens _ e => collectExpr e
  | .codata _ clauses => clauses.flatMap fun (_, e) => collectExpr e
  | .label range labelId body => (range, labelId) :: collectExpr body
  | .goto _ value label => collectExpr value ++ collectExpr label

end

/-- Every `(Range, Id)` pair for identifiers referenced in the renamed
module. -/
def collectIds (m : Module .rename) : List (Range × Id) :=
  m.moduleDefinition.scDefs.flatMap fun grp => grp.flatMap fun (_, _, e) => collectExpr e

/-- Find the tightest-ranging identifier at the given LSP cursor position. -/
def findIdAtPos (pos : LspPosition) (m : Module .rename) : Option (Range × Id) :=
  match collectIds m |>.filter (fun (r, _) => posInRange pos r) with
  | [] => none
  | x :: xs => some (xs.foldl (fun best cur => if rangeSize cur.1 < rangeSize best.1 then cur else best) x)

/-- Handle `textDocument/hover`. Returns a JSON value for the response
(`null` when there's no cached module for the URI, or no identifier at that
position — matching Haskell, no fallback to an on-demand recompute). -/
def handleHover (state : LspState) (nuri : NormalizedUri) (pos : LspPosition) : IO Malgo.LSP.Json.JValue := do
  match (← state.renamedCache.get).get? nuri with
  | none => return .null
  | some (_, renamedMod) =>
    match findIdAtPos pos renamedMod with
    | none => return .null
    | some (r, id) => return encodeHover (hoverText id) (some (rangeToLsp r))

end Malgo.LSP
