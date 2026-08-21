import Std.Data.TreeMap
import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.Monad
import Malgo.Pass
import Malgo.Syntax
import Malgo.Syntax.Extension
import Malgo.Sequent.Fun

/-! Port of `src/Malgo/Sequent/ToFun.hs`: lowers a renamed `BindGroup` into
the Fun IR (`Program`). Clause/copattern desugaring goes through
`build`/`classify`; data constructors compile to curried lambdas ending in a
`Construct`; foreigns become curried `Primitive`s; `Label`/`Goto` become
`Fix`/`Apply`.

Effect mapping: Haskell `State Uniq` + `Reader ModuleName` + `Error
ToFunError` becomes `ExceptT ToFunError MalgoM`, with the current
`ModuleName` threaded explicitly (`newTemporalId`/`newInternalId` take it as
a parameter). The entry `ToFun.pass` wraps the error into `CompileError` via
`Malgo.wrapError`.

Parity: uniq assignment order is golden-observable, so every traversal is
mechanically left-to-right, matching Haskell `traverse`/`replicateM`. -/

namespace Malgo.Sequent.ToFun

open Malgo
open Malgo.Syntax
open Malgo.Sequent.Fun (Name)

/-- `partial def`s below return `ExceptT ToFunError MalgoM α`, inhabited via
the monad's error branch; this makes `CompileError` inhabited so instance
search can build that default. `Inhabited Fun.Pattern` supports the pure
`partial def fromPattern` (its map recursion is not structurally
recognized). -/
instance : Inhabited CompileError := ⟨{ passName := "", message := "" }⟩
instance : Inhabited Range := ⟨{ start := SourcePos.initial "", stop := SourcePos.initial "" }⟩
instance : Inhabited Fun.Pattern := ⟨.expand default []⟩

inductive ToFunError where
  | emptyCoClauses (range : Range)
  | mismatchCopatterns (range : Range)
  | internalError (range : Range) (msg : String)
  deriving Repr

def ToFunError.render : ToFunError → String
  | .emptyCoClauses range => s!"{pretty range}: empty coclauses"
  | .mismatchCopatterns range => s!"{pretty range}: mismatch copatterns"
  | .internalError range msg => s!"{pretty range}: {msg}"

def ToFunError.range? : ToFunError → Option Range
  | .emptyCoClauses range => some range
  | .mismatchCopatterns range => some range
  | .internalError range _ => some range

abbrev ToFunM := ExceptT ToFunError MalgoM

/-- Split a copattern into its constituent parts, mirroring Haskell's
`CoPat'`. -/
inductive CoPat' where
  | applyP (range : Range) (pat : Pat .rename)
  | projectP (range : Range) (field : String)

/-- Haskell `CoClause' es`: a partially-desugared coclause. `body` is a
deferred `ToFunM` action (a `fromExpr` thunk); constructing it mints no
uniqs — the effects run only when `build`/`buildCase` executes it. -/
structure CoClause' where
  copats : List CoPat'
  pats : List Fun.Pattern
  body : ToFunM Fun.Expr

inductive CoClauseKind where
  | case | field | function | mismatch

/-! ## Pure helpers -/

def fromLiteral : Literal .unboxed → Fun.Literal
  | .int32 n => .int32 n
  | .int64 n => .int64 n
  | .float n => .float n
  | .double n => .double n
  | .char c => .char c
  | .str s => .string s

/-- `recordFields` maps a record-shaped constructor's tag name (see
`recordCtorFields`) to its declared field names, in the order
`fromConstructor` projects them in. A `.con` pattern whose sole argument is
a record pattern (`Ctor { .field -> pat, ... }`, #422) looks its tag up
here: a hit reorders the pattern's fields to that declared order and
destructures positionally; a miss — an ordinary constructor like `Just`
applied to a record *value* rather than a record-*shaped* one — destructures
the nested `expand` as a single sub-pattern. `recordCtorFields`'s own
doc comment covers the case where a miss instead means "record-shaped but
declared in another module." A field the record pattern omits is simply
dropped rather than padded with a wildcard — the resulting arity mismatch
makes `matchMany` reject the clause, the same "list every field, `_` for
don't-care" discipline positional constructor patterns already require
elsewhere in this language. -/
partial def fromPattern (recordFields : Std.TreeMap String (List String)) :
    Pat .rename → Fun.Pattern
  | .var range name => .pvar range name
  | .con range tag [.record _ fields] =>
    match recordFields.get? tag.name with
    | some declared =>
      let positional := declared.filterMap fun field =>
        (fields.find? (fun kv => kv.1 == field)).map (fun kv => fromPattern recordFields kv.2)
      .destruct range (.tag tag.name) positional
    | none =>
      .destruct range (.tag tag.name) [fromPattern recordFields (.record range fields)]
  | .con range tag pats => .destruct range (.tag tag.name) (pats.map (fromPattern recordFields))
  | .tuple range pats => .destruct range .tuple (pats.map (fromPattern recordFields))
  | .record range fields =>
    .expand range (sortAssocAscending (fields.map fun (k, p) => (k, fromPattern recordFields p)))
  | .unboxed range lit => .pliteral range (fromLiteral lit)
  | .list ext _ => nomatch ext
  | .boxed ext _ => nomatch ext

partial def makeCoPatList : CoPat .rename → List CoPat'
  | .hole _ => []
  | .apply x copat arg => makeCoPatList copat ++ [.applyP x arg]
  | .project x copat field => makeCoPatList copat ++ [.projectP x field]

def classify (clauses : List CoClause') : CoClauseKind :=
  if clauses.any (fun c => c.copats.isEmpty) then .case
  else if clauses.all (fun c => match c.copats with | .projectP _ _ :: _ => true | _ => false) then .field
  else if clauses.all (fun c => match c.copats with | .applyP _ _ :: _ => true | _ => false) then .function
  else .mismatch

/-- Group object clauses by projected field, appending within a group and
keeping keys ascending — reproduces Haskell `Map.unionsWith (<>)`. -/
def insertGrouped (field : String) (c : CoClause') :
    List (String × List CoClause') → List (String × List CoClause')
  | [] => [(field, [c])]
  | (k, cs) :: rest =>
    match compare field k with
    | .lt => (field, [c]) :: (k, cs) :: rest
    | .eq => (k, cs ++ [c]) :: rest
    | .gt => (k, cs) :: insertGrouped field c rest

/-- Count leading arrows in a foreign's type (its arity). -/
def countArrows : Ty .rename → Nat
  | .arr _ _ cod => 1 + countArrows cod
  | _ => 0

/-! ## Constructors, data defs, foreigns -/

/-- #422: the shape a tagged-record merge applies to — exactly one *closed*
record-typed parameter (`data B = B { a: Int32, b: Int32 }`). An open row
(`{a: Int32 | r}`, a `some` row tail) has no closed field set to project or
key a table by, so it isn't this shape; neither is a type-variable-typed
parameter (`data Maybe a = Just a`) — `Just { .val -> v, .rest -> r }`
(used throughout `Json.mlg`/`Lexer.mlg`/`Parser.mlg`) applies an ordinary
constructor to a record *value*, not a record-*shaped* one. -/
def closedRecordFields? : List (Ty .rename) → Option (List (String × Ty .rename))
  | [Ty.record _ fields none] => some fields
  | _ => none

/-- A record-shaped constructor's closure projects each declared field out
of the record argument it receives, building the tag and fields into one
merged value (`Construct B [v, w]`), instead of wrapping the whole record
as one nested positional slot (`Construct B [record]`). Every other
constructor keeps its plain curried-lambda shape. -/
def fromConstructor (mn : ModuleName) :
    Range × Name × List (Ty .rename) → ToFunM (Range × Name × Fun.Expr)
  | (range, name, parameters) =>
    match closedRecordFields? parameters with
    | some fields => do
      let p ← newTemporalId mn "constructor"
      let body := Fun.Expr.construct range (.tag name.name)
        (fields.map fun (field, _) => Fun.Expr.project range (Fun.Expr.var range p) field)
      pure (range, name, Fun.Expr.lambda range [p] body)
    | none => do
      let arity := parameters.length
      let params ← (List.range arity).mapM (fun _ => newTemporalId mn "constructor")
      let body := Fun.Expr.construct range (.tag name.name) (params.map (Fun.Expr.var range))
      let lambda := params.foldr (fun p acc => Fun.Expr.lambda range [p] acc) body
      pure (range, name, lambda)

def fromDataDef (mn : ModuleName) :
    DataDef .rename → ToFunM (List (Range × Name × Fun.Expr))
  | (_, _, _, constructors) => constructors.mapM (fromConstructor mn)

/-- The tag→declared-field-order table `fromPattern` consults to recognize a
#422 record-shaped constructor pattern (`Ctor { .field -> pat, ... }`) and
flatten it to match `fromConstructor`'s merged representation.

Built from this module's own `dataDefs` only — a constructor's `Ty.record`
parameter type is known at the point it's declared, not at every site that
pattern-matches it, and this pass never sees another module's `dataDefs`.
This is a real gap, not a graceful fallback: `fromConstructor` merges a
record-shaped constructor's tag and fields unconditionally, wherever it's
declared, so *every* value of that constructor is merged regardless of
which module constructs it — but a `Ctor { .field -> pat }` pattern for a
constructor declared in a *different* module misses this table and falls
back to `fromPattern`'s nested `destruct [expand ...]` reading, which no
longer matches any value of that constructor. The clause compiles and
type-checks; it just never fires. Construction has no such gap (calling a
constructor's closure works identically from any module) — only pattern
flattening needs a lookup this table can't yet provide across modules. -/
def recordCtorFields (dataDefs : List (DataDef .rename)) : Std.TreeMap String (List String) :=
  dataDefs.foldl (init := ({} : Std.TreeMap String (List String))) fun acc (_, _, _, cons) =>
    cons.foldl (init := acc) fun acc (_, name, parameters) =>
      match closedRecordFields? parameters with
      | some fields => acc.insert name.name (fields.map Prod.fst)
      | none => acc

def fromForeign (mn : ModuleName) :
    Foreign .rename → ToFunM (Range × Name × Fun.Expr)
  | ((range, _), name, typ) =>
    match typ with
    | .arr _ _ _ => do
      let arity := countArrows typ
      let params ← (List.range arity).mapM (fun _ => newTemporalId mn "primitive")
      let body := Fun.Expr.primitive range name.name (params.map (Fun.Expr.var range))
      let prim := params.foldr (fun p acc => Fun.Expr.lambda range [p] acc) body
      pure (range, name, prim)
    | _ => throw (.internalError range "invalid foreign type")

/-! ## Expression lowering (mutual, `partial`) -/

mutual

partial def fromExpr (mn : ModuleName) (recordFields : Std.TreeMap String (List String)) :
    Expr .rename → ToFunM Fun.Expr
  | .var range name =>
    if name.isExternal then pure (.invoke range name) else pure (.var range name)
  | .unboxed range lit => pure (.literal range (fromLiteral lit))
  | .apply range f x => do
    let f' ← fromExpr mn recordFields f
    let x' ← fromExpr mn recordFields x
    pure (.apply range f' [x'])
  | .opApp ext op x y => do
    let range := ext.1
    let f := if op.isExternal then Fun.Expr.invoke range op else Fun.Expr.var range op
    let x' ← fromExpr mn recordFields x
    let y' ← fromExpr mn recordFields y
    pure (.apply range (.apply range f [x']) [y'])
  | .project range expr field => do
    let expr' ← fromExpr mn recordFields expr
    pure (.project range expr' field)
  | .fn range clauses => do
    let numPats := match clauses.head with | .mk _ pats _ => pats.length
    let parameters ← (List.range numPats).mapM (fun _ => newTemporalId mn "param")
    let body ← fromClauses mn recordFields range parameters clauses
    pure (parameters.foldr (fun p acc => Fun.Expr.lambda range [p] acc) body)
  | .tuple range exprs => do
    let exprs' ← exprs.mapM (fromExpr mn recordFields)
    pure (.construct range .tuple exprs')
  | .record range fields => do
    let fields' ← fields.mapM (fun (k, e) => do let e' ← fromExpr mn recordFields e; pure (k, e'))
    pure (.object range (sortAssocAscending fields'))
  | .ann _ expr _ => fromExpr mn recordFields expr
  | .seq _ stmts => fromStmts mn recordFields stmts
  | .parens _ expr => fromExpr mn recordFields expr
  | .codata range coclauses => fromCoClauses mn recordFields range coclauses
  | .label range name body => do
    let body' ← fromExpr mn recordFields body
    pure (.fix range name body')
  | .goto range value label => do
    let value' ← fromExpr mn recordFields value
    let label' ← fromExpr mn recordFields label
    pure (.apply range label' [value'])
  | .boxed ext _ => nomatch ext
  | .list ext _ => nomatch ext

partial def fromStmts (mn : ModuleName) (recordFields : Std.TreeMap String (List String)) :
    NEList (Stmt .rename) → ToFunM Fun.Expr
  | ⟨.noBind _ expr, []⟩ => fromExpr mn recordFields expr
  | ⟨.noBind range value, stmt :: stmts⟩ => do
    let tmp ← newTemporalId mn "tmp"
    let value' ← fromExpr mn recordFields value
    let expr ← fromStmts mn recordFields ⟨stmt, stmts⟩
    pure (.apply range (.lambda range [tmp] expr) [value'])
  | ⟨.letS range name value, stmt :: stmts⟩ => do
    let value' ← fromExpr mn recordFields value
    let expr ← fromStmts mn recordFields ⟨stmt, stmts⟩
    pure (.«let» range name value' expr)
  | ⟨.letS _ _ value, []⟩ => fromExpr mn recordFields value
  | ⟨.letPS ext _ _, _⟩ => nomatch ext
  | ⟨.withS ext _ _, _⟩ => nomatch ext

partial def fromClauses (mn : ModuleName) (recordFields : Std.TreeMap String (List String))
    (range : Range) :
    List Name → NEList (Clause .rename) → ToFunM Fun.Expr
  | [parameter], clauses => do
    let bs ← clauses.toList.mapM (fromClause mn recordFields)
    pure (.select range (.var range parameter) bs)
  | parameters, clauses => do
    let bs ← clauses.toList.mapM (fromClause mn recordFields)
    pure (.select range (.construct range .tuple (parameters.map (Fun.Expr.var range))) bs)

partial def fromClause (mn : ModuleName) (recordFields : Std.TreeMap String (List String)) :
    Clause .rename → ToFunM Fun.Branch
  | .mk range ⟨pat, []⟩ body => do
    let b ← fromExpr mn recordFields body
    pure (.branch range (fromPattern recordFields pat) b)
  | .mk range pats body => do
    let b ← fromExpr mn recordFields body
    pure (.branch range (.destruct range .tuple (pats.toList.map (fromPattern recordFields))) b)

partial def fromCoClauses (mn : ModuleName) (recordFields : Std.TreeMap String (List String))
    (range : Range) :
    List (CoClause .rename) → ToFunM Fun.Expr
  | coclauses =>
    let coclauses' := coclauses.map fun (copat, body) =>
      CoClause'.mk (makeCoPatList copat) [] (fromExpr mn recordFields body)
    build mn recordFields [] range coclauses'

partial def build (mn : ModuleName) (recordFields : Std.TreeMap String (List String))
    (scrutinees : List Name) (range : Range)
    (clauses : List CoClause') : ToFunM Fun.Expr :=
  match classify clauses with
  | .case => buildCase mn recordFields scrutinees range clauses
  | .field => buildObject mn recordFields scrutinees range clauses
  | .function => buildLambda mn recordFields scrutinees range clauses
  | .mismatch => throw (.mismatchCopatterns range)

partial def buildCase (mn : ModuleName) (recordFields : Std.TreeMap String (List String))
    (scrutinees : List Name) (range : Range)
    (clauses : List CoClause') : ToFunM Fun.Expr :=
  match scrutinees, clauses with
  | [], c :: _ => c.body
  | [], [] => throw (.emptyCoClauses range)
  | _, _ => do
    let (noCoPats, rest) := clauses.partition (fun c => c.copats.isEmpty)
    let branches ← noCoPats.mapM (fun c => do
      let b ← c.body
      pure (Fun.Branch.branch range (.destruct range .tuple c.pats) b))
    let restBody ← build mn recordFields scrutinees range rest
    let anyPatterns ← scrutinees.mapM (fun _ => do
      let n ← newTemporalId mn "_"
      pure (Fun.Pattern.pvar range n))
    let restBranch := Fun.Branch.branch range (.destruct range .tuple anyPatterns) restBody
    pure (.select range (.construct range .tuple (scrutinees.map (Fun.Expr.var range)))
      (branches ++ [restBranch]))

partial def buildLambda (mn : ModuleName) (recordFields : Std.TreeMap String (List String))
    (scrutinees : List Name) (range : Range)
    (clauses : List CoClause') : ToFunM Fun.Expr := do
  let clauses' ← clauses.mapM (fun c =>
    match c.copats with
    | .applyP _ pat :: rest =>
      pure (CoClause'.mk rest (c.pats ++ [fromPattern recordFields pat]) c.body)
    | _ => throw (.internalError range "invalid function clauses"))
  let param ← newTemporalId mn "param"
  let body ← build mn recordFields (scrutinees ++ [param]) range clauses'
  pure (.lambda range [param] body)

partial def buildObject (mn : ModuleName) (recordFields : Std.TreeMap String (List String))
    (scrutinees : List Name) (range : Range)
    (clauses : List CoClause') : ToFunM Fun.Expr := do
  let grouped ← clauses.foldlM (init := ([] : List (String × List CoClause'))) (fun acc c =>
    match c.copats with
    | .projectP _ field :: rest => pure (insertGrouped field (CoClause'.mk rest c.pats c.body) acc)
    | _ => throw (.internalError range "invalid object clauses"))
  let fields ← grouped.mapM (fun (k, cs) => do
    let e ← build mn recordFields scrutinees range cs
    pure (k, e))
  pure (.object range fields)

end

/-! ## Entry point -/

def fromScDef (mn : ModuleName) (recordFields : Std.TreeMap String (List String)) :
    ScDef .rename → ToFunM (Range × Name × Fun.Expr)
  | (range, name, expr) => do
    let e ← fromExpr mn recordFields expr
    pure (range, name, e)

def toFun (mn : ModuleName) (bg : BindGroup .rename) : ToFunM Fun.Program := do
  let recordFields := recordCtorFields bg.dataDefs
  let scGroups ← bg.scDefs.mapM (fun group => group.mapM (fromScDef mn recordFields))
  let scDefs := scGroups.flatten
  let dataGroups ← bg.dataDefs.mapM (fromDataDef mn)
  let dataDefs := dataGroups.flatten
  let foreigns ← bg.foreigns.mapM (fromForeign mn)
  let dependencies := bg.imports.map (fun imp => imp.2.1)
  pure { definitions := scDefs ++ dataDefs ++ foreigns, dependencies }

/-- Pass entry: lower a renamed bind group into the Fun IR, wrapping any
`ToFunError` into the uniform `CompileError`. -/
def pass (mn : ModuleName) (bg : BindGroup .rename) : MalgoM Fun.Program :=
  wrapError "ToFun" ToFunError.render ToFunError.range? (toFun mn bg)

/-! #422 regression tests: `Pair`'s fields are declared out of alphabetical
order (z before a) on purpose, so a `recordCtorFields`/`fromPattern`
implementation that resorted them (alphabetically, or by pattern-source
order) rather than keeping the declared order would still pass every
type-checked `.mlg` golden — record syntax hides positional order from any
program that only ever addresses fields by name — but would fail here. -/
section Test

private def r0 : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
private def extId (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }

private def pairDataDef : DataDef .rename :=
  (r0, extId "Pair", [],
    [(r0, extId "Pair",
      [Ty.record r0 [("z", Ty.var r0 (extId "Int32")), ("a", Ty.var r0 (extId "Int32"))] none])])

#guard (recordCtorFields [pairDataDef]).get? "Pair" == some ["z", "a"]

/-- The pattern lists `a` before `z` — the reverse of `Pair`'s declared
order — to confirm `fromPattern` reorders by declaration, not by how the
source happens to write the fields. -/
private def pairPat : Pat .rename :=
  .con r0 (extId "Pair") [.record r0 [("a", .var r0 (extId "av")), ("z", .var r0 (extId "zv"))]]

#guard Malgo.sShow (fromPattern (recordCtorFields [pairDataDef]) pairPat) ==
  Malgo.sShow (Fun.Pattern.destruct r0 (Fun.Tag.tag "Pair")
    [Fun.Pattern.pvar r0 (extId "zv"), Fun.Pattern.pvar r0 (extId "av")])

end Test

end Malgo.Sequent.ToFun
