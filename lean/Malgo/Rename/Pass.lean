import Std.Data.TreeMap
import Std.Data.TreeSet
import Malgo.Id
import Malgo.Monad
import Malgo.Module
import Malgo.Pass
import Malgo.Syntax
import Malgo.Syntax.Extension
import Malgo.Rename.RnEnv
import Malgo.Rename.RnState

/-! Port of `src/Malgo/Rename/Pass.hs`: name resolution plus the small
desugarings (list literals, boxed literals, `let`/`with` in sequences,
`OpApp` fixity resolution).

Monad-stack mapping from the Haskell effect stack:
- `Reader ModuleName`, `Reader RnEnv` → bundled into `RnCtx` under a single
  `ReaderT`; `local` only ever rewrites the `env` field.
- `State RnState` → `StateT RnState`.
- `Error RenameError` → `ExceptT RenameError`, wrapped into `CompileError`
  by `Malgo.wrapError` at the entry point.
- `State Uniq`, `IOE`, `Reader Flag`, `Workspace` → the underlying `MalgoM`.
- `QueryDB`/`loadInterface` → a `loadInterface` callback in `RnCtx`, since
  `Malgo.Interface`/`QueryDB` do not exist in Lean yet; the driver wires it.

Parity note: this pass cannot be golden-tested yet (the Lean parser is a
stub), so name/uniq parity with the Haskell renamer is by construction.
Uniq-order-critical sites carry a comment. Also note `Char.isUpper` is
ASCII-only in Lean vs Haskell's Unicode `Data.Char.isUpper`; identical for
ASCII identifiers. -/

namespace Malgo.Rename

open Malgo Malgo.Syntax

/-- Reader context: the resolution environment (rewritten by `local`), the
current module name, and the import-loading callback. -/
structure RnCtx where
  env : RnEnv
  moduleName : ModuleName
  loadInterface : ModuleName → MalgoM Interface

/-- The renamer monad. Must be an `abbrev` so the transformer instances
(MonadReader/State/Except), the auto-lift chain from `MalgoM`, and the
`Inhabited` trick all resolve through the unfolded stack. -/
abbrev RnM := ReaderT RnCtx (StateT RnState (ExceptT RenameError MalgoM))

instance {α} : Inhabited (ExceptT RenameError MalgoM α) := ⟨throw (default : RenameError)⟩
instance {α} : Inhabited (RnM α) := ⟨throw (default : RenameError)⟩

/-- `local (f)` from the Haskell `Reader RnEnv`. -/
def localEnv (f : RnEnv → RnEnv) (x : RnM α) : RnM α :=
  withReader (fun c => { c with env := f c.env }) x

/-- `throwError`-style abort mirroring Haskell `errorOn`
(see the `RenameError.other` note in `RnEnv.lean`). -/
def errorOn (pos : Range) (msg : String) : RnM α :=
  throw (.other pos msg)

/-- Lift a pure `Except RenameError` result (from `RnEnv`'s lookups). -/
def liftE (e : Except RenameError α) : RnM α :=
  match e with
  | .ok a => pure a
  | .error err => throw err

/-- Port of `resolveName`: a fresh internal (local) name. Consumes a uniq. -/
def resolveName (name : String) : RnM Id := do
  newInternalId (← read).moduleName name

/-- Port of `resolveGlobalName`: an external (top-level) name (no uniq). -/
def resolveGlobalName (name : String) : RnM Id := do
  return newExternalId (← read).moduleName name

def lookupVarName (pos : Range) (name : String) : RnM Id := do
  liftE (lookupVar (← read).env pos name)

def lookupTypeName (pos : Range) (name : String) : RnM Id := do
  liftE (lookupType (← read).env pos name)

def lookupQualifiedVarName (pos : Range) (modName : ModuleName) (name : String) : RnM Id := do
  liftE (lookupQualifiedVar (← read).env pos modName name)

/-- Renamed identifier corresponding to a boxed literal (`Int32#`, …). -/
def lookupBox (pos : Range) : Literal .boxed → RnM Id
  | .int32 _ => lookupVarName pos "Int32#"
  | .int64 _ => lookupVarName pos "Int64#"
  | .float _ => lookupVarName pos "Float#"
  | .double _ => lookupVarName pos "Double#"
  | .char _ => lookupVarName pos "Char#"
  | .str _ => lookupVarName pos "String#"

/-! ## Pure helpers -/

private def startsUpper (s : String) : Bool :=
  match s.toList with
  | c :: _ => c.isUpper
  | [] => false

private def hasDuplicates [BEq α] : List α → Bool
  | [] => false
  | x :: rest => rest.contains x || hasDuplicates rest

/-- Inhabited instances needed for the `partial` pure helpers below. The
`Pat`/`CoPat` witnesses avoid `XId`, so no `Inhabited Id` is required. -/
instance : Inhabited SourcePos := ⟨SourcePos.initial ""⟩
instance : Inhabited Range := ⟨{ start := default, stop := default }⟩
instance {p} : Inhabited (Pat p) := ⟨.unboxed default (.int32 0)⟩
instance {p} : Inhabited (CoPat p) := ⟨.hole default⟩

/-- Termination helper: a pair's second component is strictly smaller than
any list it's a member of — used to justify recursing into the value half
of a `(String × _)` field below (the record/variant fields the parser
attaches names to). -/
private theorem sizeOf_snd_lt_of_mem {α β : Type} [SizeOf α] [SizeOf β] {p : α × β}
    {l : List (α × β)} (h : p ∈ l) : sizeOf p.snd < sizeOf l := by
  have h1 : sizeOf p.snd < sizeOf p := by
    cases p with
    | mk a b => simp; omega
  exact Nat.lt_trans h1 (List.sizeOf_lt_of_mem h)

/-- Free type variables of a parse-phase type (lowercase-initial `TyVar`s).
Returned as a `TreeSet` so `.toList` is ascending, matching Haskell's
`Set.toList` — the order in which they receive uniqs. -/
def getTyVars : Ty .parse → Std.TreeSet String
  | .app _ t ts => ts.foldl (fun acc x => acc.merge (getTyVars x)) (getTyVars t)
  | .var _ v => if startsUpper v then {} else ({} : Std.TreeSet String).insert v
  | .con e _ => nomatch e
  | .arr _ t1 t2 => (getTyVars t1).merge (getTyVars t2)
  | .tuple _ ts => ts.foldl (fun acc x => acc.merge (getTyVars x)) {}
  | .record _ kvs rowTail =>
    let base := kvs.foldl (fun acc kv => acc.merge (getTyVars kv.2)) {}
    match rowTail with
    | some r => base.merge (getTyVars r)
    | none => base
  | .block _ t => getTyVars t
  | .bottom _ => {}
  | .tilde _ t => getTyVars t
  | .variant _ cases rowTail =>
    let base := cases.foldl (fun acc kts => acc.merge (kts.2.foldl (fun a x => a.merge (getTyVars x)) {})) {}
    match rowTail with
    | some r => base.merge (getTyVars r)
    | none => base
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))
    | (rename_i h
       exact Nat.lt_of_lt_of_le (sizeOf_snd_lt_of_mem h) (by omega))
    | (rename_i h2 h1
       exact Nat.lt_of_lt_of_le
         (Nat.lt_trans (List.sizeOf_lt_of_mem h1) (sizeOf_snd_lt_of_mem h2)) (by omega))

/-- Rewrite uppercase-initial `VarP` to nullary `ConP` (the parser cannot
tell them apart). Pure; mirrors Haskell `resolveConP`. -/
def resolveConP : Pat .parse → Pat .parse
  | .var pos name => if startsUpper name then .con pos name [] else .var pos name
  | .con pos name params => .con pos name (params.map resolveConP)
  | .tuple pos params => .tuple pos (params.map resolveConP)
  | .record pos kvs => .record pos (kvs.map (fun kv => (kv.1, resolveConP kv.2)))
  | .list pos params => .list pos (params.map resolveConP)
  | .unboxed pos x => .unboxed pos x
  | .boxed pos x => .boxed pos x
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))
    | (rename_i h
       exact Nat.lt_of_lt_of_le (sizeOf_snd_lt_of_mem h) (by omega))

/-- Variables a pattern binds, left-to-right (the uniq-assignment order). -/
def patVars : Pat .parse → List String
  | .var _ x => [x]
  | .con _ _ xs => xs.flatMap patVars
  | .tuple _ xs => xs.flatMap patVars
  | .record _ kvs => kvs.flatMap (fun kv => patVars kv.2)
  | .list _ xs => xs.flatMap patVars
  | .unboxed _ _ => []
  | .boxed _ _ => []
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))
    | (rename_i h
       exact Nat.lt_of_lt_of_le (sizeOf_snd_lt_of_mem h) (by omega))

def resolveCoConP : CoPat .parse → CoPat .parse
  | .hole x => .hole x
  | .apply x cp pat => .apply x (resolveCoConP cp) (resolveConP pat)
  | .project x cp field => .project x (resolveCoConP cp) field

def coPatVars : CoPat .parse → List String
  | .hole _ => []
  | .apply _ cp pat => coPatVars cp ++ patVars pat
  | .project _ cp _ => coPatVars cp

/-- Desugar a list literal `[a, b]` into `Cons a (Cons b Nil)`. -/
def buildListApply (pos : Range) (nilName consName : Id) : List (Expr .rename) → Expr .rename
  | [] => .var pos nilName
  | x :: xs => .apply pos (.apply pos (.var pos consName) x) (buildListApply pos nilName consName xs)

/-- Desugar a list pattern `[a, b]` into `Cons a (Cons b Nil)`. -/
def buildListP (pos : Range) (nilName consName : Id) : List (Pat .rename) → Pat .rename
  | [] => .con pos nilName []
  | x :: xs => .con pos consName [x, buildListP pos nilName consName xs]

/-- `(nofix_error, associate_right)` from Haskell `compareFixity`. -/
def compareFixity (f1 f2 : Assoc × Int) : Bool × Bool :=
  let (assoc1, prec1) := f1
  let (assoc2, prec2) := f2
  match compare prec1 prec2 with
  | .gt => (false, false)  -- left-associate
  | .lt => (false, true)   -- right-associate
  | .eq =>
    match assoc1, assoc2 with
    | .rightA, .rightA => (false, true)
    | .leftA, .leftA => (false, false)
    | _, _ => (true, false)

/-- `OpApp` recombination: the parser emits everything left-associative;
this rewrites to the declared associativity/precedence. Pure control-flow,
so it lives in `RnM` only for `errorOn`. -/
partial def mkOpApp (pos2 : Range) (fix2 : Assoc × Int) (op2 : Id)
    (lhs rhs : Expr .rename) : RnM (Expr .rename) := do
  match lhs with
  | .opApp ext op1 e11 e12 =>
    let (pos1, fix1) := ext
    let (nofix_error, associate_right) := compareFixity fix1 fix2
    if nofix_error then
      errorOn pos1 s!"Precedence parsing error: cannot mix '{pretty op1}' [{pretty fix1.1}{fix1.2}] and '{pretty op2}' [{pretty fix2.1}{fix2.2}] in the same infix expression"
    else if associate_right then
      let e' ← mkOpApp pos2 fix2 op2 e12 rhs
      pure (.opApp (pos1, fix1) op1 e11 e')
    else
      pure (.opApp (pos2, fix2) op2 lhs rhs)
  | _ => pure (.opApp (pos2, fix2) op2 lhs rhs)

/-- Rename a type. Uses only lookups (no uniqs). -/
partial def rnType : Ty .parse → RnM (Ty .rename)
  | .app pos t ts => do
    let t' ← rnType t
    let ts' ← ts.mapM rnType
    pure (.app pos t' ts')
  | .var pos x => do
    let x' ← lookupTypeName pos x
    if startsUpper x then pure (.con pos x') else pure (.var pos x')
  | .con e _ => nomatch e
  | .arr pos t1 t2 => do
    let t1' ← rnType t1
    let t2' ← rnType t2
    pure (.arr pos t1' t2')
  | .tuple pos ts => do pure (.tuple pos (← ts.mapM rnType))
  | .record pos kts rowTail => do
    let kts' ← kts.mapM (fun (k, v) => do pure (k, ← rnType v))
    let rowTail' ← rowTail.mapM rnType
    pure (.record pos kts' rowTail')
  | .block pos t => do pure (.arr pos (.tuple pos []) (← rnType t))
  | .bottom pos => pure (.bottom pos)
  | .tilde pos t => do pure (.tilde pos (← rnType t))
  | .variant pos cases rowTail => do
    let cases' ← cases.mapM (fun (k, ts) => do pure (k, ← ts.mapM rnType))
    let rowTail' ← rowTail.mapM rnType
    pure (.variant pos cases' rowTail')

/-! ## Export registration -/

/-- Record an identifier as exported when it is external and belongs to the
current module. Order is reverse-registration (unobserved by goldens). -/
def registerExportedIdent (ident : Id) : RnM Unit := do
  let currentModule := (← read).moduleName
  if ident.isExternal && ident.moduleName == currentModule then
    modify fun s => { s with exportedIdentifiers := ident.name :: s.exportedIdentifiers }

def registerExportedTypeIdent (ident : Id) : RnM Unit := do
  let currentModule := (← read).moduleName
  if ident.isExternal && ident.moduleName == currentModule then
    modify fun s => { s with exportedTypeIdentifiers := ident.name :: s.exportedTypeIdentifiers }

/-! ## Top-level environment -/

/-- Visibility of an import binding: `Selected` names present in the import
list are implicit, others are module-qualified; `As` forces qualified;
`All` is implicit. Port of Haskell `resolveImport`. -/
def resolveImport (modName : ModuleName) (importList : ImportList) :
    String × Id → String × Resolved
  | (psId, rnId) =>
    match importList with
    | .all => (psId, ⟨.implicit, rnId⟩)
    | .selected implicits =>
      if implicits.contains psId then (psId, ⟨.implicit, rnId⟩)
      else (psId, ⟨.explicit modName, rnId⟩)
    | .«as» modNameAs => (psId, ⟨.explicit modNameAs, rnId⟩)

/-- Only an existing *implicit* (same-module) binding counts as a real
duplicate; `Explicit` imports must not block a same-named local definition. -/
private def hasImplicitBinding (name : String) (m : Std.TreeMap String (List Resolved)) : Bool :=
  (m.get? name |>.getD []).any (fun q => q.visibility == .implicit)

/-- Build the top-level `RnEnv` extending `env0`. Mirrors Haskell
`genToplevelEnv` (`execState env (traverse aux ds)`): pure external names
(no uniqs), duplicate detection, and import wiring via `loadInterface`. -/
partial def genToplevelEnv (loadInterface : ModuleName → MalgoM Interface) (modName : ModuleName)
    (ds : List (Decl .parse)) (env0 : RnEnv) : ExceptT RenameError MalgoM RnEnv := do
  let aux (env : RnEnv) : Decl .parse → ExceptT RenameError MalgoM RnEnv := fun d => do
    match d with
    | .scDef pos x _ => do
      if hasImplicitBinding x env.resolvedVarIdentMap then throw (.duplicateName pos x)
      pure (insertVarIdent [(x, ⟨.implicit, newExternalId modName x⟩)] env)
    | .scSig .. => pure env
    | .dataDef pos x _ cs => do
      if hasImplicitBinding x env.resolvedTypeIdentMap then throw (.duplicateName pos x)
      let conNames := cs.map (·.2.1)
      let conConflicts := conNames.filter (fun c => hasImplicitBinding c env.resolvedVarIdentMap)
      if !conConflicts.isEmpty then throw (.duplicateNames pos conConflicts)
      let x' := newExternalId modName x
      let xs' := conNames.map (newExternalId modName ·)
      let env := insertVarIdent (conNames.zip (xs'.map (fun i => (⟨.implicit, i⟩ : Resolved)))) env
      let env := addConstructors xs' env
      pure (insertTypeIdent [(x, ⟨.implicit, x'⟩)] env)
    | .typeSynonym pos x _ _ => do
      if hasImplicitBinding x env.resolvedTypeIdentMap then throw (.duplicateName pos x)
      pure (insertTypeIdent [(x, ⟨.implicit, newExternalId modName x⟩)] env)
    | .foreign pos x _ => do
      if hasImplicitBinding x env.resolvedVarIdentMap then throw (.duplicateName pos x)
      pure (insertVarIdent [(x, ⟨.implicit, newExternalId modName x⟩)] env)
    | .«import» _ modName' importList => do
      let interface ← loadInterface modName'
      let varAssoc := interface.exportedIdentList.map (fun n => (n, externalFromInterface interface n))
      let typeAssoc := interface.exportedTypeIdentList.map (fun n => (n, externalFromInterface interface n))
      let env := insertVarIdent (varAssoc.map (resolveImport modName' importList)) env
      let env := insertTypeIdent (typeAssoc.map (resolveImport modName' importList)) env
      match importList with
      | .«as» moduleName => pure { env with moduleNames := env.moduleNames.insert moduleName }
      | _ => pure env
    | .«infix» .. => pure env
  ds.foldlM aux env0

/-- Convert `infix` declarations to a `Map`. Runs under the top-level env
(so `lookupVarName` resolves the operators); left-biased on conflicts,
matching Haskell `foldMapM` + `Map.singleton`. -/
def infixDecls (ds : List (Decl .parse)) : RnM (Std.TreeMap Id (Assoc × Int)) :=
  ds.foldlM (init := {}) fun acc d =>
    match d with
    | .«infix» pos assoc order name => do
      let name' ← lookupVarName pos name
      pure (if acc.contains name' then acc else acc.insert name' (assoc, order))
    | _ => pure acc

/-! ## The renamer proper -/

/-- Monadic map over `NEList`, head first (mirrors `traverse` on a
`NonEmpty`). -/
private def neMapM [Monad m] (f : α → m β) (xs : NEList α) : m (NEList β) := do
  let h ← f xs.head
  let t ← xs.tail.mapM f
  pure ⟨h, t⟩

mutual

/-- Rename an expression; also performs `OpApp` recombination. -/
partial def rnExpr : Expr .parse → RnM (Expr .rename)
  | .var range name => do pure (.var range (← lookupVarName range name))
  | .unboxed pos val => pure (.unboxed pos val)
  | .boxed pos val => do
    let f ← lookupBox pos val
    pure (.apply pos (.var pos f) (.unboxed pos val.toUnboxed))
  | .apply pos e1 e2 => do
    let e1' ← rnExpr e1  -- uniq order: fn before arg
    let e2' ← rnExpr e2
    pure (.apply pos e1' e2')
  | .opApp pos op e1 e2 => do
    let op' ← lookupVarName pos op
    let e1' ← rnExpr e1
    let e2' ← rnExpr e2
    match (← get).infixInfo.get? op' with
    | some fixity => mkOpApp pos fixity op' e1' e2'
    | none => errorOn pos s!"No infix declaration: '{op}'"
  | .project range expr field => do
    match expr with
    | .var vRange name =>
      let mods := (← read).env.moduleNames
      if mods.contains (.moduleName name) then
        pure (.var range (← lookupQualifiedVarName range (.moduleName name) field))
      else
        pure (.project range (← rnExpr (.var vRange name)) field)
    | _ => pure (.project range (← rnExpr expr) field)
  | .fn pos cs => do pure (.fn pos (← neMapM rnClause cs))
  | .tuple pos es => do pure (.tuple pos (← es.mapM rnExpr))
  | .record pos kvs => do pure (.record pos (← kvs.mapM (fun (k, v) => do pure (k, ← rnExpr v))))
  | .list pos es => do
    let nilName ← lookupVarName pos "Nil"
    let consName ← lookupVarName pos "Cons"
    let es' ← es.mapM rnExpr
    pure (buildListApply pos nilName consName es')
  | .ann pos e t => do
    let e' ← rnExpr e
    let t' ← rnType t
    pure (.ann pos e' t')
  | .seq pos ss => do pure (.seq pos (← rnStmts ss))
  | .parens pos e => do pure (.parens pos (← rnExpr e))
  | .codata pos clauses => do pure (.codata pos (← clauses.mapM rnCoClause))
  | .label pos name body => do
    let name' ← resolveName name  -- uniq: label binder
    localEnv (insertVarIdent [(name, ⟨.implicit, name'⟩)]) do
      pure (.label pos name' (← rnExpr body))
  | .goto pos value label => do
    let value' ← rnExpr value
    let label' ← rnExpr label
    pure (.goto pos value' label')

/-- Rename a pattern (no uniqs — binders are resolved by the caller). -/
partial def rnPat : Pat .parse → RnM (Pat .rename)
  | .var pos x => do pure (.var pos (← lookupVarName pos x))
  | .con pos x xs => do
    let x' ← lookupVarName pos x
    let xs' ← xs.mapM rnPat
    pure (.con pos x' xs')
  | .tuple pos xs => do pure (.tuple pos (← xs.mapM rnPat))
  | .record pos kvs => do pure (.record pos (← kvs.mapM (fun (k, p) => do pure (k, ← rnPat p))))
  | .list pos xs => do
    let nilName ← lookupVarName pos "Nil"
    let consName ← lookupVarName pos "Cons"
    let xs' ← xs.mapM rnPat
    pure (buildListP pos nilName consName xs')
  | .unboxed pos x => pure (.unboxed pos x)
  | .boxed pos x => do pure (.con pos (← lookupBox pos x) [.unboxed pos x.toUnboxed])

partial def rnCoPat : CoPat .parse → RnM (CoPat .rename)
  | .hole x => pure (.hole x)
  | .apply x cp pat => do
    let cp' ← rnCoPat cp
    let pat' ← rnPat pat
    pure (.apply x cp' pat')
  | .project x cp field => do pure (.project x (← rnCoPat cp) field)

/-- Rename a clause: resolve constructor-looking patterns, bind the pattern
variables (fresh uniqs, left-to-right), then rename the patterns and body. -/
partial def rnClause : Clause .parse → RnM (Clause .rename)
  | .mk pos ps e => do
    let ps := ps.map resolveConP
    let vars := ps.toList.flatMap patVars
    if hasDuplicates (vars.filter (· != "_")) then
      errorOn pos "Same variables occurs in a pattern"
    let resolved ← vars.mapM resolveName  -- uniq: pattern binders in bind order
    let vm := vars.zip (resolved.map (fun i => (⟨.implicit, i⟩ : Resolved)))
    localEnv (insertVarIdent vm) do
      let ps' ← neMapM rnPat ps  -- patterns before body (body may consume uniqs)
      pure (.mk pos ps' (← rnExpr e))

partial def rnCoClause : CoClause .parse → RnM (CoClause .rename)
  | (copat, expr) => do
    let copat := resolveCoConP copat
    let vars := coPatVars copat
    if hasDuplicates (vars.filter (· != "_")) then
      errorOn copat.range "Same variables occurs in a pattern"
    let resolved ← vars.mapM resolveName
    let vm := vars.zip (resolved.map (fun i => (⟨.implicit, i⟩ : Resolved)))
    localEnv (insertVarIdent vm) do
      let copat' ← rnCoPat copat
      let expr' ← rnExpr expr
      pure (copat', expr')

/-- Rename statements in `{}`, desugaring pattern-`let` and `with`. -/
partial def rnStmts : NEList (Stmt .parse) → RnM (NEList (Stmt .rename))
  | ⟨.noBind x e, []⟩ => do pure ⟨.noBind x (← rnExpr e), []⟩
  | ⟨.letS x v e, []⟩ => do
    let e' ← rnExpr e  -- uniq: bound expr before the binder
    let v' ← resolveName v
    pure ⟨.letS x v' e', []⟩
  | ⟨.letPS x _ _, []⟩ =>
    errorOn x "`let` binding a pattern cannot appear in the last line of the sequence expression."
  | ⟨.withS x _ _, []⟩ =>
    errorOn x "`with` statement cannnot appear in the last line of the sequence expression."
  | ⟨.noBind x e, s :: ss⟩ => do
    let e' ← rnExpr e
    let rest ← rnStmts ⟨s, ss⟩
    pure ⟨.noBind x e', rest.toList⟩
  | ⟨.letS x v e, s :: ss⟩ => do
    let e' ← rnExpr e
    let v' ← resolveName v
    localEnv (insertVarIdent [(v, ⟨.implicit, v'⟩)]) do
      let rest ← rnStmts ⟨s, ss⟩
      pure ⟨.letS x v' e', rest.toList⟩
  | ⟨.letPS x pat e, s :: ss⟩ => do
    -- desugar `let pat = e; rest` to `{ pat -> rest } e`; e renamed first
    let e' ← rnExpr e
    let k ← rnExpr (.fn x (NEList.singleton (Clause.mk x (NEList.singleton pat) (.seq x ⟨s, ss⟩))))
    pure ⟨.noBind x (.apply x k e'), []⟩
  | ⟨.withS x (some v) e, s :: ss⟩ => do
    let e' ← rnExpr e
    let k ← rnExpr (.fn x (NEList.singleton (Clause.mk x (NEList.singleton (.var x v)) (.seq x ⟨s, ss⟩))))
    pure ⟨.noBind x (.apply x e' k), []⟩
  | ⟨.withS x none e, s :: ss⟩ => do
    let e' ← rnExpr e
    let k ← rnExpr (.fn x (NEList.singleton (Clause.mk x (NEList.singleton (.var x "_")) (.seq x ⟨s, ss⟩))))
    pure ⟨.noBind x (.apply x e' k), []⟩

/-- Rename a top-level declaration. The top-level binder is assumed already
registered in the env by `genToplevelEnv`; infix decls already in state. -/
partial def rnDecl : Decl .parse → RnM (Decl .rename)
  | .scDef pos name expr => do
    let resolvedName ← lookupVarName pos name
    registerExportedIdent resolvedName
    pure (.scDef pos resolvedName (← rnExpr expr))
  | .scSig pos name typ => do
    let tyVars := (getTyVars typ).toList
    let tyVars' ← tyVars.mapM resolveName  -- uniq: type variables, ascending
    let resolvedName ← lookupVarName pos name
    registerExportedIdent resolvedName
    localEnv (insertTypeIdent (tyVars.zip (tyVars'.map (fun i => (⟨.implicit, i⟩ : Resolved))))) do
      pure (.scSig pos resolvedName (← rnType typ))
  | .dataDef pos name params cs => do
    let params' ← params.mapM (fun p => resolveName p.2)  -- uniq: type params
    let resolvedName ← lookupTypeName pos name
    registerExportedTypeIdent resolvedName
    let paramsR := (params.zip params').map (fun (rp, p') => (rp.1, p'))
    localEnv (insertTypeIdent ((params.map (·.2)).zip (params'.map (fun i => (⟨.implicit, i⟩ : Resolved))))) do
      let cs' ← cs.mapM (fun (crange, cname, types) => do
        let resolvedCName ← lookupVarName pos cname
        registerExportedIdent resolvedCName
        let types' ← types.mapM rnType
        pure (crange, resolvedCName, types'))
      pure (.dataDef pos resolvedName paramsR cs')
  | .typeSynonym pos name params typ => do
    let params' ← params.mapM resolveName  -- uniq: type params
    let resolvedName ← lookupTypeName pos name
    registerExportedTypeIdent resolvedName
    localEnv (insertTypeIdent (params.zip (params'.map (fun i => (⟨.implicit, i⟩ : Resolved))))) do
      pure (.typeSynonym pos resolvedName params' (← rnType typ))
  | .«infix» pos assoc prec name => do
    let resolvedName ← lookupVarName pos name
    registerExportedIdent resolvedName
    pure (.«infix» pos assoc prec resolvedName)
  | .foreign pos name typ => do
    let tyVars := (getTyVars typ).toList
    let tyVars' ← tyVars.mapM resolveName  -- uniq: type variables, ascending
    let resolvedName ← lookupVarName pos name
    registerExportedIdent resolvedName
    localEnv (insertTypeIdent (tyVars.zip (tyVars'.map (fun i => (⟨.implicit, i⟩ : Resolved))))) do
      pure (.foreign (pos, name) resolvedName (← rnType typ))
  | .«import» pos modName importList => do
    let ctx ← read
    let interface ← ctx.loadInterface modName
    modify fun s =>
      let mappedInfix := interface.infixInfo.foldl (fun acc k v =>
        let key := externalFromInterface interface k
        if acc.contains key then acc else acc.insert key v) s.infixInfo
      { s with
        infixInfo := mappedInfix,
        dependencies := (s.dependencies.insert modName).merge interface.dependencies }
    pure (.«import» pos modName importList)

end

/-- Rename all top-level declarations. Builds the top-level env, seeds the
state with resolved infix info, then renames each decl in order. -/
partial def rnDecls (ds : List (Decl .parse)) : RnM (List (Decl .rename)) := do
  let ctx ← read
  let rnEnv ← genToplevelEnv ctx.loadInterface ctx.moduleName ds ctx.env
  localEnv (fun _ => rnEnv) do
    let inf ← infixDecls ds
    set ({ infixInfo := inf : RnState })
    ds.mapM rnDecl

/-- Entry point mirroring Haskell `RenamePass`'s `runPassImpl`. `loadInterface`
is a callback because `Malgo.Interface`/`QueryDB` are not ported yet; the
driver supplies it. Errors are wrapped into `CompileError`. -/
def pass (loadInterface : ModuleName → MalgoM Interface)
    (input : Module .parse × RnEnv) : MalgoM (Module .rename × RnState) := do
  let (m, builtinEnv) := input
  let modName := m.moduleName
  let ds := m.moduleDefinition.decls
  let ctx : RnCtx := { env := builtinEnv, moduleName := modName, loadInterface }
  let act : ExceptT RenameError MalgoM (List (Decl .rename) × RnState) :=
    (((rnDecls ds).run ctx).run ({} : RnState))
  let (ds', rnState) ← Malgo.wrapError "Rename" RenameError.render RenameError.rangeOf act
  return ({ moduleName := modName, moduleDefinition := makeBindGroup ds' }, rnState)

end Malgo.Rename
