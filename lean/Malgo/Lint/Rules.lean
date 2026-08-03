import Malgo.Lint.Diagnostic
import Malgo.Lint.Rule
import Malgo.Lint.Traversal
import Malgo.Syntax

/-! Port of `src/Malgo/Lint/Rules.hs`: the default set of lint rules.

`if` and `case` are ordinary Prelude functions, so every rule is a pure
AST-shape match. Rules run on the parse-phase AST, where literals and the raw
`if`/`case` forms survive. -/

namespace Malgo.Lint

open Malgo Malgo.Syntax

/-! ## Shared matchers -/

/-- Strip a single-statement `seq` and `parens` down to the inner expression. -/
private partial def soleExpr : Expr .parse → Expr .parse
  | .seq _ ⟨.noBind _ e, []⟩ => soleExpr e
  | .parens _ e => soleExpr e
  | e => e

/-- The body of a thunk block `{ e }` (a zero-arg `fn` with a wildcard
clause). -/
private def thunkBody : Expr .parse → Option (Expr .parse)
  | .fn _ ⟨.mk _ ⟨.var _ "_", []⟩ body, []⟩ => some (soleExpr body)
  | _ => none

/-- Match `if c { t } { e }`, returning the condition and both branch bodies.
Matches the bare application only (no top-level `seq`/`parens` unwrap), so each
occurrence is reported once at its `apply` node. -/
private def matchIf : Expr .parse → Option (Expr .parse × Expr .parse × Expr .parse)
  | .apply _ (.apply _ (.apply _ (.var _ "if") c) tFn) eFn =>
    match thunkBody tFn, thunkBody eFn with
    | some t, some el => some (c, t, el)
    | _, _ => none
  | _ => none

/-- Match `case scrut { clauses }`, returning the scrutinee and the `fn` of
clauses (kept as an `Expr` so `freevars` can be applied directly). Matches the
bare application only; callers unwrap a wrapping `seq` themselves. -/
private def matchCase : Expr .parse → Option (Expr .parse × Expr .parse)
  | .apply _ (.apply _ (.var _ "case") scrut) (.fn ext cs) => some (scrut, .fn ext cs)
  | _ => none

/-! ## Rule: case-of-bound-arg -/

/-- Where a scrutinee variable is bound among a clause's head patterns. -/
private inductive BoundPosition where
  | lastParam
  | nestedBind
  | elsewhere

/-- Variables reachable through constructor/tuple/list nesting only —
substituting a pattern at these positions is a sound fold. Record-field binders
are excluded (folding would put a non-variable pattern in a record field, which
the self-hosted back end cannot match). -/
private partial def nestedFoldableVars : Pat .parse → Std.TreeSet String
  | .con _ _ ps => ps.foldl (fun acc p => acc.merge (nestedFoldableVars p)) {}
  | .tuple _ ps => ps.foldl (fun acc p => acc.merge (nestedFoldableVars p)) {}
  | .list _ ps => ps.foldl (fun acc p => acc.merge (nestedFoldableVars p)) {}
  | .var _ x => ({} : Std.TreeSet String).insert x
  | _ => {}

private def boundPosition (x : String) (pats : NEList (Pat .parse)) : BoundPosition :=
  let lastPat := pats.tail.getLastD pats.head
  let isLast := match lastPat with | .var _ x' => x == x' | _ => false
  if isLast then .lastParam
  else
    let topLevelVars := pats.toList.filterMap fun | .var _ v => some v | _ => none
    if topLevelVars.contains x then .elsewhere
    else
      let nested := pats.toList.foldl (fun acc p => acc.merge (nestedFoldableVars p))
        ({} : Std.TreeSet String)
      if nested.contains x then .nestedBind else .elsewhere

private def caseOfBoundArgCheck (id : String) : Expr .parse → List Diagnostic
  | .fn _ clauses => clauses.toList.flatMap fun c =>
    match c with
    | .mk _ pats body =>
      match matchCase (soleExpr body) with
      | some (scrut, fn) =>
        match soleExpr scrut with
        | .var _ x =>
          if fn.freevars.contains x then []
          else
            match boundPosition x pats with
            | .lastParam =>
              [warn id (range c)
                s!"`{x}` is bound only to be matched; drop `{x} -> case {x}` and match the argument directly."]
            | .nestedBind =>
              [warn id (range c)
                s!"`{x}` is bound by a pattern only to be matched; fold the `case` arms into the pattern that binds it."]
            | .elsewhere => []
        | _ => []
      | none => []
  | _ => []

private def caseOfBoundArg : Rule .parse := exprRule "case-of-bound-arg" caseOfBoundArgCheck

/-! ## Rule: single-branch-case -/

private def singleBranchCaseCheck (id : String) (e : Expr .parse) : List Diagnostic :=
  match matchCase e with
  | some (_, .fn _ ⟨.mk _ ⟨pat, []⟩ _, []⟩) =>
    match pat with
    | .var .. => [warn id (range e) "single-branch `case`; bind with `let` instead."]
    | _ => [warn id (range e) "single-branch `case`; consider a `let` if the match is irrefutable."]
  | _ => []

private def singleBranchCase : Rule .parse := exprRule "single-branch-case" singleBranchCaseCheck

/-! ## Rule: redundant-case-forward -/

/-- A branch that rebuilds its scrutinee unchanged, e.g. `Just v -> Just v`. -/
private def forwards : Clause .parse → Bool
  | .mk _ ⟨.con _ con [.var _ v], []⟩ body =>
    match soleExpr body with
    | .apply _ (.var _ con') (.var _ v') => con == con' && v == v'
    | _ => false
  | _ => false

private def redundantCaseForwardCheck (id : String) (e : Expr .parse) : List Diagnostic :=
  match matchCase e with
  | some (_, .fn _ clauses) =>
    if clauses.toList.any forwards then
      [warn id (range e)
        "a `case` branch rebuilds its scrutinee unchanged; consider an `orElse`/`maybe`-style combinator."]
    else []
  | _ => []

private def redundantCaseForward : Rule .parse :=
  exprRule "redundant-case-forward" redundantCaseForwardCheck

/-! ## Rule: if-returning-bool -/

private def asBool : Expr .parse → Option Bool
  | .var _ "True" => some true
  | .var _ "False" => some false
  | _ => none

private def ifReturningBoolCheck (id : String) (e : Expr .parse) : List Diagnostic :=
  match matchIf e with
  | some (_, t, el) =>
    match asBool t, asBool el with
    | some true, some false => [warn id (range e) "`if c { True } { False }` is just `c`."]
    | some false, some true => [warn id (range e) "`if c { False } { True }` is just `not c`."]
    | _, _ => []
  | none => []

private def ifReturningBool : Rule .parse := exprRule "if-returning-bool" ifReturningBoolCheck

/-! ## Rule: nested-if-equality-chain

A right-nested chain of `if (eqX s lit) { … } { … }` over the same scrutinee
should be a multi-clause function or a `case` over `s`. Uses custom recursion so
each maximal chain is reported once (at its head), not at every link. -/

private def eqFns : List String := ["eqString", "eqChar", "eqInt32", "eqInt64"]

private def isLiteral : Expr .parse → Bool
  | .boxed .. => true
  | .unboxed .. => true
  | _ => false

private def eqScrutLit (c : Expr .parse) : Option String :=
  match soleExpr c with
  | .apply _ (.apply _ (.var _ fn) (.var _ s)) lit =>
    if eqFns.contains fn && isLiteral (soleExpr lit) then some s else none
  | _ => none

private def scrutOf (e : Expr .parse) : String :=
  match matchIf e with
  | some (c, _, _) => (eqScrutLit c).getD "?"
  | none => "?"

/-- Walk the maximal same-scrutinee eq-literal chain from `e`, returning
(link count, then-branch bodies, final default body). -/
private partial def chainLinks (e : Expr .parse) (mscrut : Option String) :
    Nat × List (Expr .parse) × Expr .parse :=
  match matchIf e with
  | some (c, t, el) =>
    match eqScrutLit c with
    | some s =>
      if (match mscrut with | none => true | some s' => s' == s) then
        let (n, ts, dflt) := chainLinks (soleExpr el) (some s)
        (n + 1, t :: ts, dflt)
      else (0, [], e)
    | none => (0, [], e)
  | none => (0, [], e)

private partial def scan (id : String) (e : Expr .parse) : List Diagnostic :=
  let (n, thenBodies, dflt) := chainLinks e none
  if n >= 3 then
    warn id (range e)
        s!"nested `if` chain of depth {n} over `{scrutOf e}`; match it with multi-clause patterns or `case`."
      :: (thenBodies ++ [dflt]).flatMap (scan id)
  else
    (children e).flatMap (scan id)

private def nestedIfEqualityChain : Rule .parse :=
  let ruleId := "nested-if-equality-chain"
  { ruleId,
    run := fun decls =>
      (decls.filterMap fun | .scDef _ _ e => some e | _ => none).flatMap (scan ruleId) }

/-! ## The default rule set -/

def allRules : List (Rule .parse) :=
  [ caseOfBoundArg,
    singleBranchCase,
    redundantCaseForward,
    ifReturningBool,
    nestedIfEqualityChain ]

end Malgo.Lint
