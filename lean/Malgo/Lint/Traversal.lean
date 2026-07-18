import Malgo.Prelude
import Malgo.Syntax

/-! Port of `src/Malgo/Lint/Traversal.hs`: a `Plated`-style traversal over the
Malgo expression tree. The Haskell version is built on `microlens`; Lean has
no lens library, so `plate` becomes the pair of plain recursive functions it
would generate — `children` (extract the immediate sub-expressions) and
`mapChildren` (rebuild with them replaced) — from which `universe`/`transform`
follow. -/

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

/-- Rebuild an expression with each immediate sub-expression mapped through
`g` (Haskell `over plate g`). -/
def mapChildren (g : Expr p → Expr p) : Expr p → Expr p
  | .apply x a b => .apply x (g a) (g b)
  | .opApp x op a b => .opApp x op (g a) (g b)
  | .project x e k => .project x (g e) k
  | .fn x cs => .fn x (cs.map fun | .mk cx ps body => .mk cx ps (g body))
  | .tuple x es => .tuple x (es.map g)
  | .record x kvs => .record x (kvs.map fun (k, v) => (k, g v))
  | .list x es => .list x (es.map g)
  | .ann x e t => .ann x (g e) t
  | .seq x ss => .seq x (ss.map fun
      | .letS sx n e => .letS sx n (g e)
      | .letPS sx p e => .letPS sx p (g e)
      | .withS sx n e => .withS sx n (g e)
      | .noBind sx e => .noBind sx (g e))
  | .parens x e => .parens x (g e)
  | .codata x cs => .codata x (cs.map fun (cp, e) => (cp, g e))
  | .label x n e => .label x n (g e)
  | .goto x a b => .goto x (g a) (g b)
  | e@(.var ..) => e
  | e@(.unboxed ..) => e
  | e@(.boxed ..) => e

/-- An expression and all of its transitive sub-expressions (self first).
Named `«universe»` because `universe` is a reserved Lean keyword. -/
partial def «universe» (e : Expr p) : List (Expr p) :=
  e :: (children e).flatMap «universe»

/-- Every sub-expression of every top-level definition in a module. -/
def universeDecls (decls : List (Decl p)) : List (Expr p) :=
  (decls.filterMap fun | .scDef _ _ e => some e | _ => none).flatMap «universe»

/-- Bottom-up rewrite of every sub-expression. Unused by the report-only v1
rules, but the natural basis for a future `--fix`. -/
partial def transform (f : Expr p → Expr p) (e : Expr p) : Expr p :=
  f (mapChildren (transform f) e)

/-- Source range of a clause. -/
def clauseRange (c : Clause p) : Range := c.range

end Malgo.Lint
