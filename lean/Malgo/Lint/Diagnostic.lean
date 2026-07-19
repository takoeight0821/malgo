import Malgo.Prelude
import Malgo.Doc

/-! Port of `src/Malgo/Lint/Diagnostic.hs`: diagnostics emitted by the Malgo
linter. -/

namespace Malgo.Lint

open Malgo Malgo.Doc

/-- How seriously to take a diagnostic. Only `error` makes `malgo lint` exit
nonzero; every v1 rule emits `warning`. -/
inductive Severity where
  | warning
  | error
  deriving BEq, Ord, Repr

instance : Pretty Severity where
  pretty
    | .warning => "warning"
    | .error => "error"

/-- A single lint finding pinned to a source range. -/
structure Diagnostic where
  ruleId : String
  severity : Severity
  range : Range
  message : String
  deriving BEq, Repr

instance : HasRange Diagnostic := ⟨(·.range)⟩

/-- Build a `warning` diagnostic at the range of any locatable node. -/
def warn [HasRange α] (ruleId : String) (node : α) (message : String) : Diagnostic :=
  { ruleId, severity := .warning, range := range node, message }

/-- Render a diagnostic as `<range>: <severity> [<ruleId>] <message>`,
matching the layout used by `warningOn`/`errorOn`. -/
def prettyDiagnostic (d : Diagnostic) : Doc :=
  fromString (pretty d.range)
    ++ atom ": "
    ++ atom (pretty d.severity)
    ++ atom " ["
    ++ fromString d.ruleId
    ++ atom "] "
    ++ fromString d.message

end Malgo.Lint
