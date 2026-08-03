import Malgo.Lint.Diagnostic
import Malgo.Lint.Traversal
import Malgo.Syntax

/-! Port of `src/Malgo/Lint/Rule.hs`: a lint rule is a named check over a
module's declarations. -/

namespace Malgo.Lint

open Malgo.Syntax

structure Rule (p : Phase) where
  ruleId : String
  run : List (Decl p) → List Diagnostic

/-- A rule that inspects every sub-expression of every definition
independently. `check` receives the rule's own `ruleId`, so a `warn` call
inside it can't drift from the ID this rule was actually registered under. -/
def exprRule (ruleId : String) (check : String → Expr p → List Diagnostic) : Rule p :=
  { ruleId, run := fun decls => (universeDecls decls).flatMap (check ruleId) }

end Malgo.Lint
