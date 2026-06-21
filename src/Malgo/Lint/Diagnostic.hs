-- | Diagnostics emitted by the Malgo linter.
module Malgo.Lint.Diagnostic
  ( Severity (..),
    Diagnostic (..),
    warn,
    prettyDiagnostic,
  )
where

import Malgo.Prelude

-- | How seriously to take a diagnostic. Only 'Error' makes @malgo lint@ exit
-- nonzero; every v1 rule emits 'Warning'.
data Severity = Warning | Error
  deriving stock (Eq, Ord, Show)

instance Pretty Severity where
  pretty Warning = "warning"
  pretty Error = "error"

-- | A single lint finding pinned to a source range.
data Diagnostic = Diagnostic
  { ruleId :: Text,
    severity :: Severity,
    range :: Range,
    message :: Text
  }
  deriving stock (Eq, Show)

instance HasRange Diagnostic where
  range = (.range)

-- | Build a 'Warning' diagnostic at the range of any locatable node.
warn :: (HasRange a) => Text -> a -> Text -> Diagnostic
warn ruleId node message =
  Diagnostic {ruleId, severity = Warning, range = range node, message}

-- | Render a diagnostic as @\<range\>: \<severity\> [\<ruleId\>] \<message\>@,
-- matching the layout used by 'warningOn'/'errorOn'.
prettyDiagnostic :: Diagnostic -> Doc ann
prettyDiagnostic d =
  pretty d.range
    <> ": "
    <> pretty d.severity
    <> " ["
    <> pretty d.ruleId
    <> "] "
    <> pretty d.message
