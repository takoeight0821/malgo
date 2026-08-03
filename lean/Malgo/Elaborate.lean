import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.Pass
import Malgo.Syntax
import Malgo.Syntax.Extension
import Malgo.Monad

/-! Port of `src/Malgo/Elaborate.hs`: the codata elaboration pass.

Transforms copattern (codata) definitions into records and lambdas before
ToFun, via a "classify by first accessor" algorithm:
  1. Flatten each copattern into a left-to-right list of accessors.
  2. All accessors consumed → pattern-matching case (`buildCase`).
  3. Leading field accessors → `Record` (`buildObject`).
  4. Leading apply accessors → `Fn` lambda (`buildLambda`).
  5. Mixed accessor kinds → error.

Elaborate is a `.rename → .rename` endo-pass (input and output are both
`BindGroup .rename`), unlike Rename which changes phase.

Effect mapping mirrors `Sequent/ToFun.lean`: Haskell `State Uniq` + `Reader
ModuleName` + `Error ElaborateError` becomes `ExceptT ElaborateError MalgoM`
with the `ModuleName` threaded explicitly (`newTemporalId` takes it as a
parameter). The entry `Elaborate.pass` wraps the error into `CompileError`
via `Malgo.wrapError`.

Parity: uniq assignment order is golden-observable, so every traversal is
mechanically left-to-right, matching Haskell `traverse`/`<$>`/`<*>`. The two
Haskell `error "..."` internal-invariant cases become `.internalError`
throws, mirroring how `ToFun.lean` handles its own impossible cases. -/

namespace Malgo.Elaborate

open Malgo
open Malgo.Syntax

/-- `partial def`s below return `ExceptT ElaborateError MalgoM α`, inhabited
via the monad's error branch; this needs `CompileError` inhabited so instance
search can build that default. -/
instance : Inhabited CompileError := ⟨{ passName := "", message := "" }⟩

inductive ElaborateError where
  | mixedAccessors (range : Range)
  | emptyCopattern (range : Range)
  | internalError (range : Range) (msg : String)
  deriving Repr

def ElaborateError.render : ElaborateError → String
  | .mixedAccessors _ => "mixed field and call accessors in codata"
  | .emptyCopattern _ => "empty copattern"
  | .internalError _ msg => msg

def ElaborateError.rangeOf : ElaborateError → Option Range
  | .mixedAccessors range => some range
  | .emptyCopattern range => some range
  | .internalError range _ => some range

abbrev ElaborateM := ExceptT ElaborateError MalgoM

/-- An accessor extracted from a flattened copattern. -/
inductive Accessor where
  | field (range : Range) (name : String)
  | apply (range : Range) (pat : Pat .rename)

/-- A clause for the copattern elaboration algorithm: the remaining accessors
to process, patterns accumulated from stripped apply accessors, and the body. -/
structure ElabClause where
  accessors : List Accessor
  accPatterns : List (Pat .rename)
  body : Expr .rename

abbrev Scrutinees := List Id

/-- Flatten a `CoPat` tree into a list of accessors (left to right). -/
partial def flattenCoPat : CoPat .rename → List Accessor
  | .hole _ => []
  | .apply pos copat pat => flattenCoPat copat ++ [.apply pos pat]
  | .project pos copat field => flattenCoPat copat ++ [.field pos field]

def firstIsField (c : ElabClause) : Bool :=
  match c.accessors with | .field _ _ :: _ => true | _ => false

def firstIsApply (c : ElabClause) : Bool :=
  match c.accessors with | .apply _ _ :: _ => true | _ => false

/-- Insert a clause into the field-grouped list, appending within a group and
preserving first-appearance order (port of Haskell `insertGrouped`). -/
def insertGrouped (field : String) (c : ElabClause) :
    List (String × List ElabClause) → List (String × List ElabClause)
  | [] => [(field, [c])]
  | (f, cs) :: rest =>
    if f == field then (f, cs ++ [c]) :: rest
    else (f, cs) :: insertGrouped field c rest

/-- Group clauses by the field name of their first accessor, preserving order
(port of Haskell `groupByField`, made monadic to throw the impossible-case). -/
def groupByField (range : Range) (clauses : List ElabClause) :
    ElaborateM (List (String × List ElabClause)) :=
  clauses.foldlM (init := []) fun acc c =>
    match c.accessors with
    | .field _ field :: rest => pure (insertGrouped field ⟨rest, c.accPatterns, c.body⟩ acc)
    | _ => throw (.internalError range "groupByField: expected field accessor")

mutual

/-- Recursively elaborate all `Codata` expressions within an expression. -/
partial def elabExpr (mn : ModuleName) : Expr .rename → ElaborateM (Expr .rename)
  | .var pos name => pure (.var pos name)
  | .unboxed pos lit => pure (.unboxed pos lit)
  | .apply pos e1 e2 => do
    let e1' ← elabExpr mn e1
    let e2' ← elabExpr mn e2
    pure (.apply pos e1' e2')
  | .opApp ext op e1 e2 => do
    let e1' ← elabExpr mn e1
    let e2' ← elabExpr mn e2
    pure (.opApp ext op e1' e2')
  | .project pos e field => do
    let e' ← elabExpr mn e
    pure (.project pos e' field)
  | .fn pos clauses => do
    pure (.fn pos (← NEList.mapM (elabClause mn) clauses))
  | .tuple pos es => do
    let es' ← es.mapM (elabExpr mn)
    pure (.tuple pos es')
  | .record pos kvs => do
    let kvs' ← kvs.mapM (fun (k, v) => do let v' ← elabExpr mn v; pure (k, v'))
    pure (.record pos kvs')
  | .ann pos e t => do
    let e' ← elabExpr mn e
    pure (.ann pos e' t)
  | .seq pos stmts => do
    pure (.seq pos (← NEList.mapM (elabStmt mn) stmts))
  | .parens pos e => do
    let e' ← elabExpr mn e
    pure (.parens pos e')
  | .label pos name body => do
    let body' ← elabExpr mn body
    pure (.label pos name body')
  | .goto pos value label => do
    let value' ← elabExpr mn value
    let label' ← elabExpr mn label
    pure (.goto pos value' label')
  | .codata pos coclauses => elabCodata mn pos coclauses
  | .boxed ext _ => nomatch ext
  | .list ext _ => nomatch ext

partial def elabClause (mn : ModuleName) : Clause .rename → ElaborateM (Clause .rename)
  | .mk pos pats expr => do
    let expr' ← elabExpr mn expr
    pure (.mk pos pats expr')

partial def elabStmt (mn : ModuleName) : Stmt .rename → ElaborateM (Stmt .rename)
  | .letS pos v e => do
    let e' ← elabExpr mn e
    pure (.letS pos v e')
  | .noBind pos e => do
    let e' ← elabExpr mn e
    pure (.noBind pos e')
  | .letPS ext _ _ => nomatch ext
  | .withS ext _ _ => nomatch ext

/-- Entry point: elaborate a `Codata` expression. -/
partial def elabCodata (mn : ModuleName) (pos : Range)
    (coclauses : List (CoClause .rename)) : ElaborateM (Expr .rename) :=
  let clauses := coclauses.map fun (copat, body) => ⟨flattenCoPat copat, [], body⟩
  buildExpr mn [] pos clauses

/-- Recursively build an expression from elaboration clauses, dispatching on
the first accessor kind (port of Haskell `buildExpr`). -/
partial def buildExpr (mn : ModuleName) (scrutinees : Scrutinees) (pos : Range)
    (clauses : List ElabClause) : ElaborateM (Expr .rename) :=
  if clauses.all (fun c => c.accessors.isEmpty) then buildCase mn scrutinees pos clauses
  else if clauses.all firstIsField then buildObject mn scrutinees pos clauses
  else if clauses.all firstIsApply then buildLambda mn scrutinees pos clauses
  else throw (.mixedAccessors pos)

/-- Build a pattern-matching expression for the base case (all accessors
consumed): `(\pats -> body) scrutinee` for each clause. -/
partial def buildCase (mn : ModuleName) (scrutinees : Scrutinees) (pos : Range)
    (clauses : List ElabClause) : ElaborateM (Expr .rename) :=
  match scrutinees, clauses with
  | [], [c] => elabExpr mn c.body
  | [], _ => throw (.emptyCopattern pos)
  | _, _ => do
    let fnClauses ← clauses.mapM (fun c => do
      let body' ← elabExpr mn c.body
      let pat := match c.accPatterns with
        | [p] => p
        | ps => Pat.tuple pos ps
      pure (Clause.mk pos ⟨pat, []⟩ body'))
    let scrutineeExpr :=
      match scrutinees with
      | [s] => Expr.var pos s
      | ss => Expr.tuple pos (ss.map (Expr.var pos))
    match fnClauses with
    | [] => throw (.emptyCopattern pos)
    | c :: cs => pure (Expr.apply pos (Expr.fn pos ⟨c, cs⟩) scrutineeExpr)

/-- Build a lambda for apply accessors: strip the first apply accessor from
each clause, generate a fresh parameter, and recursively build the body. -/
partial def buildLambda (mn : ModuleName) (scrutinees : Scrutinees) (pos : Range)
    (clauses : List ElabClause) : ElaborateM (Expr .rename) := do
  let param ← newTemporalId mn "elab"
  let clauses' ← clauses.mapM (fun c =>
    match c.accessors with
    | .apply _ pat :: rest => pure (ElabClause.mk rest (c.accPatterns ++ [pat]) c.body)
    | _ => throw (.internalError pos "buildLambda: expected apply accessor"))
  let body ← buildExpr mn (scrutinees ++ [param]) pos clauses'
  pure (Expr.fn pos ⟨Clause.mk pos ⟨Pat.var pos param, []⟩ body, []⟩)

/-- Build a record for field accessors: group by field name and recursively
build each field's expression. -/
partial def buildObject (mn : ModuleName) (scrutinees : Scrutinees) (pos : Range)
    (clauses : List ElabClause) : ElaborateM (Expr .rename) := do
  let grouped ← groupByField pos clauses
  let fields ← grouped.mapM (fun (field, cs) => do
    let e ← buildExpr mn scrutinees pos cs
    pure (field, e))
  pure (Expr.record pos fields)

end

def elabScDef (mn : ModuleName) : ScDef .rename → ElaborateM (ScDef .rename)
  | (pos, name, expr) => do
    let expr' ← elabExpr mn expr
    pure (pos, name, expr')

def elaborate (mn : ModuleName) (bg : BindGroup .rename) : ElaborateM (BindGroup .rename) := do
  let scDefs' ← bg.scDefs.mapM (fun group => group.mapM (elabScDef mn))
  pure { bg with scDefs := scDefs' }

/-- Pass entry: elaborate a renamed bind group, wrapping any `ElaborateError`
into the uniform `CompileError`. -/
def pass (moduleName : ModuleName) (bg : BindGroup .rename) : MalgoM (BindGroup .rename) :=
  wrapError "Elaborate" ElaborateError.render ElaborateError.rangeOf (elaborate moduleName bg)

end Malgo.Elaborate
