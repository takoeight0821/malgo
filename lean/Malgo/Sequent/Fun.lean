import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.SExpr

/-! Port of `src/Malgo/Sequent/Fun.hs`: the Fun IR — a functional IR close
to the AST (`Program`/`Expr`/`Branch`/`Pattern` plus `Tag`/`Literal`).

Naming deviations forced by Lean keywords: the `Expr.Let` constructor is
`«let»`. Constructor names are otherwise the Haskell names in lowerCamel
(`Tag.Tuple`/`Tag.Tag` → `tuple`/`tag`, `Literal.String` → `string`).

The Haskell `Object`/`Expand` fields are `Map Text _`; a `Std.TreeMap`
cannot nest inside a recursive inductive, so they become
`List (String × _)` maintained in ascending key order. Dumps sort with
`sortAssocAscending` to reproduce `Data.Map`'s ascending `toList` exactly
(golden files compare these byte-for-byte).

`Binary` is skipped for now. -- TODO(M2): ToJson/FromJson -/

namespace Malgo.Sequent.Fun

abbrev Name := Malgo.Id

/-- Tag distinguishes different structures. Haskell's `Tag` type has a
constructor also named `Tag`; renamed to `tag` here. -/
inductive Tag where
  | tuple
  | tag (name : String)
  deriving BEq, Repr

instance : ToSExpr Tag where
  toSExpr
    | .tuple => .atom (.symbol "tuple")
    | .tag t => toSExpr t

inductive Literal where
  | int32 (n : Int32)
  | int64 (n : Int64)
  | float (n : Float32)
  | double (n : Float)
  | char (c : Char)
  | string (s : String)
  deriving BEq, Repr

instance : ToSExpr Literal where
  toSExpr
    | .int32 n => .atom (.int n.toInt (some "i32"))
    | .int64 n => .atom (.int n.toInt (some "i64"))
    | .float n => .atom (.float n)
    | .double n => .atom (.double n)
    | .char c => .atom (.char c)
    | .string s => .atom (.str s)

inductive Pattern where
  | pvar (range : Range) (name : Name)
  | pliteral (range : Range) (lit : Literal)
  | destruct (range : Range) (tag : Tag) (pats : List Pattern)
  | expand (range : Range) (fields : List (String × Pattern))
  deriving Repr

def Pattern.range : Pattern → Range
  | .pvar r _ => r
  | .pliteral r _ => r
  | .destruct r _ _ => r
  | .expand r _ => r

instance : HasRange Pattern := ⟨Pattern.range⟩

private def sym (s : String) : SExpr := .atom (.symbol s)

partial def Pattern.toSExpr : Pattern → SExpr
  | .pvar _ name => Malgo.toSExpr name
  | .pliteral _ lit => Malgo.toSExpr lit
  | .destruct _ tag patterns => .list [Malgo.toSExpr tag, .list (patterns.map Pattern.toSExpr)]
  | .expand _ fields =>
    .list [sym "expand",
      .list ((sortAssocAscending fields).map fun (k, v) => .list [sym k, v.toSExpr])]

instance : ToSExpr Pattern := ⟨Pattern.toSExpr⟩

mutual

inductive Expr where
  | var (range : Range) (name : Name)
  | literal (range : Range) (lit : Literal)
  | construct (range : Range) (tag : Tag) (arguments : List Expr)
  | «let» (range : Range) (name : Name) (value : Expr) (body : Expr)
  | lambda (range : Range) (parameters : List Name) (body : Expr)
  | object (range : Range) (fields : List (String × Expr))
  | apply (range : Range) (callee : Expr) (arguments : List Expr)
  | project (range : Range) (callee : Expr) (field : String)
  | primitive (range : Range) (operator : String) (arguments : List Expr)
  | select (range : Range) (scrutinee : Expr) (branches : List Branch)
  | invoke (range : Range) (name : Name)
  | fix (range : Range) (name : Name) (body : Expr)

inductive Branch where
  | branch (range : Range) (pattern : Pattern) (body : Expr)

end

def Expr.range : Expr → Range
  | .var r _ => r
  | .literal r _ => r
  | .construct r _ _ => r
  | .«let» r _ _ _ => r
  | .lambda r _ _ => r
  | .object r _ => r
  | .apply r _ _ => r
  | .project r _ _ => r
  | .primitive r _ _ => r
  | .select r _ _ => r
  | .invoke r _ => r
  | .fix r _ _ => r

instance : HasRange Expr := ⟨Expr.range⟩

def Branch.range : Branch → Range
  | .branch r _ _ => r

instance : HasRange Branch := ⟨Branch.range⟩

mutual

partial def Expr.toSExpr : Expr → SExpr
  | .var _ name => Malgo.toSExpr name
  | .literal _ literal => Malgo.toSExpr literal
  | .construct _ tag arguments =>
    .list [Malgo.toSExpr tag, .list (arguments.map Expr.toSExpr)]
  | .«let» _ name value body =>
    .list [sym "let", Malgo.toSExpr name, value.toSExpr, body.toSExpr]
  | .lambda _ parameters body =>
    .list [sym "lambda", .list (parameters.map Malgo.toSExpr), body.toSExpr]
  | .object _ fields =>
    .list [sym "object",
      .list ((sortAssocAscending fields).map fun (k, v) => .list [sym k, v.toSExpr])]
  | .apply _ callee arguments =>
    .list [sym "apply", callee.toSExpr, .list (arguments.map Expr.toSExpr)]
  | .project _ callee field =>
    .list [sym "project", callee.toSExpr, Malgo.toSExpr field]
  | .primitive _ operator arguments =>
    .list [sym "primitive", Malgo.toSExpr operator, .list (arguments.map Expr.toSExpr)]
  | .select _ scrutinees branches =>
    .list [sym "select", scrutinees.toSExpr, .list (branches.map Branch.toSExpr)]
  | .invoke _ name => .list [sym "invoke", Malgo.toSExpr name]
  | .fix _ name body => .list [sym "fix", Malgo.toSExpr name, body.toSExpr]

partial def Branch.toSExpr : Branch → SExpr
  | .branch _ pattern body => .list [Malgo.toSExpr pattern, body.toSExpr]

end

instance : ToSExpr Expr := ⟨Expr.toSExpr⟩
instance : ToSExpr Branch := ⟨Branch.toSExpr⟩

structure Program where
  definitions : List (Range × Name × Expr)
  dependencies : List ModuleName

instance : ToSExpr Program where
  toSExpr p :=
    .list
      [ sym "program",
        .list (p.definitions.map fun (_, name, body) => .list [Malgo.toSExpr name, body.toSExpr]),
        .list (p.dependencies.map Malgo.toSExpr) ]

private def r0 : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
private def idInt32 : Name := { name := "Int32#", moduleName := .moduleName "t", sort := .external }

#guard Malgo.sShow Tag.tuple == "tuple"
#guard Malgo.sShow (Tag.tag "Cons") == "Cons"
#guard Malgo.sShow (Literal.int32 1) == "1_i32"
#guard Malgo.sShow (Literal.int64 (-5)) == "-5_i64"
-- Fragment lifted from .golden/Malgo.Sequent.ToFun/ZeroArgs.
#guard Malgo.sShow (Expr.apply r0 (Expr.invoke r0 idInt32) [Expr.literal r0 (.int32 1)])
  == "(apply (invoke Int32#) (1_i32))"
-- Ascending-key ordering of object fields (b before r).
#guard Malgo.sShow (Expr.object r0 [("return", Expr.invoke r0 idInt32), ("bind", Expr.invoke r0 idInt32)])
  == "(object ((bind (invoke Int32#)) (return (invoke Int32#))))"

end Malgo.Sequent.Fun
