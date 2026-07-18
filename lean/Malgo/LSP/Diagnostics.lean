import Malgo.LSP.Protocol
import Malgo.Monad

/-! Port of `malgo-lsp/src/Malgo/LSP/Diagnostics.hs`: conversion from Malgo
compiler errors to LSP diagnostics.

Haskell's `CompileError` is an existential wrapper over whatever exception
type a pass threw, so `compileToDiagnostics` uses `Data.Typeable.cast` to
try downcasting to a megaparsec `ParseErrorBundle` (producing one diagnostic
per error in the bundle, each at its own position) or a `RenameError`
(pattern-matching all 6 constructors to extract a `Range`), falling back to
a single zero-range diagnostic for anything else.

The Lean port's `CompileError` (`Malgo.Monad`) is not existential — every
pass already renders into a flat `{passName, message, range?}` record via
`Malgo.Pass.wrapError`, which stores `rangeOf err` directly (see
`Malgo.Rename.Pass`'s `wrapError "Rename" RenameError.render
RenameError.rangeOf`), so a *renamed* error already carries its real range
with no casting needed. The one gap: `Malgo.Query.Engine`/`Malgo.Driver`'s
own hand-written `parseError` helpers (not routed through `wrapError`) set
`range? := none` even though the underlying `Parser.PError` has `line`/`col`
fields — an existing, established simplification from M1 (parse-error
messages already embed `sourceName:line:col:` as text), consistently used
by every CLI entry point. Reusing that same convention here means parse
errors surface as a single zero-range diagnostic, exactly matching what
reusing the existing query-engine error path gives; this is a narrower
positioning gap than Haskell's per-error positioned parse diagnostics, but
consistent with (not a regression introduced by) the rest of the Lean
port's parse-error handling. -/

namespace Malgo.LSP

open Malgo

/-- Convert a megaparsec `SourcePos` (1-indexed)-equivalent to an LSP
`LspPosition` (0-indexed). Port of `srcPosToLsp`, specialized to Malgo's
own `SourcePos` (`Malgo.Prelude`), which already uses the same 1-indexed
`line`/`column` convention megaparsec does. -/
def srcPosToLsp (pos : SourcePos) : LspPosition :=
  { line := (pos.line : Int) - 1, character := (pos.column : Int) - 1 }

/-- Convert a Malgo `Range` to an LSP `LspRange`. -/
def rangeToLsp (r : Range) : LspRange :=
  { start := srcPosToLsp r.start, «end» := srcPosToLsp r.stop }

/-- Zero-width range at the start of the file (fallback position). -/
def zeroRange : LspRange :=
  { start := { line := 0, character := 0 }, «end» := { line := 0, character := 0 } }

/-- Convert a `CompileError` to a list of LSP diagnostics: one diagnostic,
positioned by `range?` when populated (renamed errors always have one),
falling back to `zeroRange` otherwise (parse errors — see the module doc). -/
def compileToDiagnostics (e : CompileError) : List Diagnostic :=
  let r := match e.range? with
    | some range => rangeToLsp range
    | none => zeroRange
  [mkDiagnostic r e.message]

end Malgo.LSP
