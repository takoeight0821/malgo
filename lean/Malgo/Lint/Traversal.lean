import Malgo.Prelude
import Malgo.Syntax

/-! Port of `src/Malgo/Lint/Traversal.hs`: a `Plated`-style traversal over the
Malgo expression tree. The Haskell version is built on `microlens`; Lean has
no lens library, so `plate` becomes the plain recursive function it would
generate — `children` (extract the immediate sub-expressions) — from which
`universe` follows. -/

namespace Malgo.Lint

open Malgo Malgo.Syntax

private def stmtBody : Stmt p → Expr p
  | .letS _ _ e => e
  | .letPS _ _ e => e
  | .withS _ _ e => e
  | .noBind _ e => e

/-- The immediate sub-expressions of an expression — including those nested
inside clauses, statements and records (Haskell `toListOf plate`). -/
def children : Expr p → List (Expr p)
  | .apply _ a b => [a, b]
  | .opApp _ _ a b => [a, b]
  | .project _ e _ => [e]
  | .fn _ cs => cs.toList.map fun | .mk _ _ body => body
  | .tuple _ es => es
  | .record _ kvs => kvs.map (·.2)
  | .list _ es => es
  | .ann _ e _ => [e]
  | .seq _ ss => ss.toList.map stmtBody
  | .parens _ e => [e]
  | .codata _ cs => cs.map (·.2)
  | .label _ _ e => [e]
  | .goto _ a b => [a, b]
  | .var .. => []
  | .unboxed .. => []
  | .boxed .. => []

/-- An expression and all of its transitive sub-expressions (self first).
Named `«universe»` because `universe` is a reserved Lean keyword. -/
partial def «universe» (e : Expr p) : List (Expr p) :=
  e :: (children e).flatMap «universe»

/-- Every sub-expression of every top-level definition in a module. -/
def universeDecls (decls : List (Decl p)) : List (Expr p) :=
  (decls.filterMap fun | .scDef _ _ e => some e | _ => none).flatMap «universe»

end Malgo.Lint
