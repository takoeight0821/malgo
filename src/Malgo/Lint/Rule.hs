-- | A lint rule: a named check over a module's declarations. Most rules are
-- built with 'exprRule' (inspect every sub-expression); rules needing custom
-- recursion (to avoid double-reporting overlapping nodes) supply 'run'
-- directly.
module Malgo.Lint.Rule
  ( Rule (..),
    exprRule,
  )
where

import Malgo.Lint.Diagnostic (Diagnostic)
import Malgo.Lint.Traversal (universeDecls)
import Malgo.Prelude
import Malgo.Syntax (Decl, Expr)

data Rule x = Rule
  { ruleId :: Text,
    run :: [Decl x] -> [Diagnostic]
  }

-- | A rule that inspects every sub-expression of every definition independently.
exprRule :: Text -> (Expr x -> [Diagnostic]) -> Rule x
exprRule ruleId check = Rule {ruleId, run = concatMap check . universeDecls}
