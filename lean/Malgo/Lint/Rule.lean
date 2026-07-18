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
independently. -/
def exprRule (ruleId : String) (check : Expr p → List Diagnostic) : Rule p :=
  { ruleId, run := fun decls => (universeDecls decls).flatMap check }

end Malgo.Lint
