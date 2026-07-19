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

partial def fromPattern : Pat .rename → Fun.Pattern
  | .var range name => .pvar range name
  | .con range tag pats => .destruct range (.tag tag.name) (pats.map fromPattern)
  | .tuple range pats => .destruct range .tuple (pats.map fromPattern)
  | .record range fields =>
    .expand range (sortAssocAscending (fields.map fun (k, p) => (k, fromPattern p)))
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

def fromConstructor (mn : ModuleName) :
    Range × Name × List (Ty .rename) → ToFunM (Range × Name × Fun.Expr)
  | (range, name, parameters) => do
    let arity := parameters.length
    let params ← (List.range arity).mapM (fun _ => newTemporalId mn "constructor")
    let body := Fun.Expr.construct range (.tag name.name) (params.map (Fun.Expr.var range))
    let lambda := params.foldr (fun p acc => Fun.Expr.lambda range [p] acc) body
    pure (range, name, lambda)

def fromDataDef (mn : ModuleName) :
    DataDef .rename → ToFunM (List (Range × Name × Fun.Expr))
  | (_, _, _, constructors) => constructors.mapM (fromConstructor mn)

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

partial def fromExpr (mn : ModuleName) : Expr .rename → ToFunM Fun.Expr
  | .var range name =>
    if name.isExternal then pure (.invoke range name) else pure (.var range name)
  | .unboxed range lit => pure (.literal range (fromLiteral lit))
  | .apply range f x => do
    let f' ← fromExpr mn f
    let x' ← fromExpr mn x
    pure (.apply range f' [x'])
  | .opApp ext op x y => do
    let range := ext.1
    let f := if op.isExternal then Fun.Expr.invoke range op else Fun.Expr.var range op
    let x' ← fromExpr mn x
    let y' ← fromExpr mn y
    pure (.apply range (.apply range f [x']) [y'])
  | .project range expr field => do
    let expr' ← fromExpr mn expr
    pure (.project range expr' field)
  | .fn range clauses => do
    let numPats := match clauses.head with | .mk _ pats _ => pats.length
    let parameters ← (List.range numPats).mapM (fun _ => newTemporalId mn "param")
    let body ← fromClauses mn range parameters clauses
    pure (parameters.foldr (fun p acc => Fun.Expr.lambda range [p] acc) body)
  | .tuple range exprs => do
    let exprs' ← exprs.mapM (fromExpr mn)
    pure (.construct range .tuple exprs')
  | .record range fields => do
    let fields' ← fields.mapM (fun (k, e) => do let e' ← fromExpr mn e; pure (k, e'))
    pure (.object range (sortAssocAscending fields'))
  | .ann _ expr _ => fromExpr mn expr
  | .seq _ stmts => fromStmts mn stmts
  | .parens _ expr => fromExpr mn expr
  | .codata range coclauses => fromCoClauses mn range coclauses
  | .label range name body => do
    let body' ← fromExpr mn body
    pure (.fix range name body')
  | .goto range value label => do
    let value' ← fromExpr mn value
    let label' ← fromExpr mn label
    pure (.apply range label' [value'])
  | .boxed ext _ => nomatch ext
  | .list ext _ => nomatch ext

partial def fromStmts (mn : ModuleName) : NEList (Stmt .rename) → ToFunM Fun.Expr
  | ⟨.noBind _ expr, []⟩ => fromExpr mn expr
  | ⟨.noBind range value, stmt :: stmts⟩ => do
    let tmp ← newTemporalId mn "tmp"
    let value' ← fromExpr mn value
    let expr ← fromStmts mn ⟨stmt, stmts⟩
    pure (.apply range (.lambda range [tmp] expr) [value'])
  | ⟨.letS range name value, stmt :: stmts⟩ => do
    let value' ← fromExpr mn value
    let expr ← fromStmts mn ⟨stmt, stmts⟩
    pure (.«let» range name value' expr)
  | ⟨.letS _ _ value, []⟩ => fromExpr mn value
  | ⟨.letPS ext _ _, _⟩ => nomatch ext
  | ⟨.withS ext _ _, _⟩ => nomatch ext

partial def fromClauses (mn : ModuleName) (range : Range) :
    List Name → NEList (Clause .rename) → ToFunM Fun.Expr
  | [parameter], clauses => do
    let bs ← clauses.toList.mapM (fromClause mn)
    pure (.select range (.var range parameter) bs)
  | parameters, clauses => do
    let bs ← clauses.toList.mapM (fromClause mn)
    pure (.select range (.construct range .tuple (parameters.map (Fun.Expr.var range))) bs)

partial def fromClause (mn : ModuleName) : Clause .rename → ToFunM Fun.Branch
  | .mk range ⟨pat, []⟩ body => do
    let b ← fromExpr mn body
    pure (.branch range (fromPattern pat) b)
  | .mk range pats body => do
    let b ← fromExpr mn body
    pure (.branch range (.destruct range .tuple (pats.toList.map fromPattern)) b)

partial def fromCoClauses (mn : ModuleName) (range : Range) :
    List (CoClause .rename) → ToFunM Fun.Expr
  | coclauses =>
    let coclauses' := coclauses.map fun (copat, body) =>
      CoClause'.mk (makeCoPatList copat) [] (fromExpr mn body)
    build mn [] range coclauses'

partial def build (mn : ModuleName) (scrutinees : List Name) (range : Range)
    (clauses : List CoClause') : ToFunM Fun.Expr :=
  match classify clauses with
  | .case => buildCase mn scrutinees range clauses
  | .field => buildObject mn scrutinees range clauses
  | .function => buildLambda mn scrutinees range clauses
  | .mismatch => throw (.mismatchCopatterns range)

partial def buildCase (mn : ModuleName) (scrutinees : List Name) (range : Range)
    (clauses : List CoClause') : ToFunM Fun.Expr :=
  match scrutinees, clauses with
  | [], c :: _ => c.body
  | [], [] => throw (.emptyCoClauses range)
  | _, _ => do
    let (noCoPats, rest) := clauses.partition (fun c => c.copats.isEmpty)
    let branches ← noCoPats.mapM (fun c => do
      let b ← c.body
      pure (Fun.Branch.branch range (.destruct range .tuple c.pats) b))
    let restBody ← build mn scrutinees range rest
    let anyPatterns ← scrutinees.mapM (fun _ => do
      let n ← newTemporalId mn "_"
      pure (Fun.Pattern.pvar range n))
    let restBranch := Fun.Branch.branch range (.destruct range .tuple anyPatterns) restBody
    pure (.select range (.construct range .tuple (scrutinees.map (Fun.Expr.var range)))
      (branches ++ [restBranch]))

partial def buildLambda (mn : ModuleName) (scrutinees : List Name) (range : Range)
    (clauses : List CoClause') : ToFunM Fun.Expr := do
  let clauses' ← clauses.mapM (fun c =>
    match c.copats with
    | .applyP _ pat :: rest => pure (CoClause'.mk rest (c.pats ++ [fromPattern pat]) c.body)
    | _ => throw (.internalError range "invalid function clauses"))
  let param ← newTemporalId mn "param"
  let body ← build mn (scrutinees ++ [param]) range clauses'
  pure (.lambda range [param] body)

partial def buildObject (mn : ModuleName) (scrutinees : List Name) (range : Range)
    (clauses : List CoClause') : ToFunM Fun.Expr := do
  let grouped ← clauses.foldlM (init := ([] : List (String × List CoClause'))) (fun acc c =>
    match c.copats with
    | .projectP _ field :: rest => pure (insertGrouped field (CoClause'.mk rest c.pats c.body) acc)
    | _ => throw (.internalError range "invalid object clauses"))
  let fields ← grouped.mapM (fun (k, cs) => do
    let e ← build mn scrutinees range cs
    pure (k, e))
  pure (.object range fields)

end

/-! ## Entry point -/

def fromScDef (mn : ModuleName) :
    ScDef .rename → ToFunM (Range × Name × Fun.Expr)
  | (range, name, expr) => do
    let e ← fromExpr mn expr
    pure (range, name, e)

def toFun (mn : ModuleName) (bg : BindGroup .rename) : ToFunM Fun.Program := do
  let scGroups ← bg.scDefs.mapM (fun group => group.mapM (fromScDef mn))
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

end Malgo.Sequent.ToFun
