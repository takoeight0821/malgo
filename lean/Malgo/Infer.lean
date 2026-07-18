import Std.Data.TreeMap
import Std.Data.TreeSet
import Malgo.Prelude
import Malgo.Id
import Malgo.Monad
import Malgo.Pass
import Malgo.Module
import Malgo.Syntax
import Malgo.Syntax.Extension
import Malgo.Infer.Constraint
import Malgo.Infer.Unify

/-! Port of `src/Malgo/Infer.hs`: constraint-based Hindley-Milner inference
with level-based let-polymorphism and row polymorphism.

`InferPass` (Haskell) takes `(TyEnv, BindGroup .rename)` and returns
`(BindGroup .rename, TyEnv)` — the bind group passes through unchanged; only
the `TyEnv` is new. Ported as `Infer.pass`. -/

namespace Malgo.Infer

open Malgo Malgo.Syntax

/-- Type environment mapping identifiers to schemes. -/
abbrev TyEnv := Std.TreeMap Id Scheme

/-- Left-biased union (Haskell `Map <>`): `a`'s entries win on collision. -/
private def unionEnv (a b : TyEnv) : TyEnv :=
  b.foldl (fun m k v => if m.contains k then m else m.insert k v) a

private def insertAll (env : TyEnv) (xs : List (Id × Scheme)) : TyEnv :=
  xs.foldl (fun m (k, v) => m.insert k v) env

/-- Build the synonym environment from a bind group. -/
def buildSynEnv (bg : BindGroup .rename) : SynEnv :=
  bg.typeSynonyms.foldl (fun m (_, name, params, body) => m.insert name (params, body)) {}

-- Convert a surface type to an internal type, expanding synonyms found in
-- the ambient `SynEnv`. Recursive expansion is detected via a visited set;
-- partial application of a synonym is rejected.
mutual

partial def expandType (visited : Std.TreeSet Id) : Malgo.Syntax.Ty .rename → InferM Ty
  | .var _ name => pure (.tVar name 0)
  | .con pos name => do
    match (← read).synEnv.get? name with
    | some (params, body) =>
      if !params.isEmpty then throw (.synonymArityMismatch pos name params.length 0)
      else if visited.contains name then throw (.cyclicSynonym pos name)
      else expandType (visited.insert name) body
    | none => pure (.tCon name.name)
  | .arr _ arg ret => do pure (.tArr (← expandType visited arg) (← expandType visited ret))
  | .app pos f args => do
    match f with
    | .con _ name =>
      match (← read).synEnv.get? name with
      | some (params, body) =>
        if params.length != args.length then
          throw (.synonymArityMismatch pos name params.length args.length)
        else do
          let argTys ← args.mapM (expandType visited)
          expandSynonymApp pos visited name params argTys body
      | none => do
        let f' ← expandType visited f
        let argTys ← args.mapM (expandType visited)
        pure (argTys.foldl .tApp f')
    | _ => do
      let f' ← expandType visited f
      let argTys ← args.mapM (expandType visited)
      pure (argTys.foldl .tApp f')
  | .tuple _ ts => do pure (.tTuple (← ts.mapM (expandType visited)))
  | .record _ fields rowTail => do
    let fields' ← fields.mapM (fun (n, t) => do pure (n, ← expandType visited t))
    let rowTail' ← rowTail.mapM (expandType visited)
    pure (.tRecord fields' rowTail')
  | .block ext _ => nomatch ext
  | .bottom _ => pure .tBottom
  | .tilde _ t => expandType visited t
  | .variant _ cases rowTail => do
    let cases' ← cases.mapM (fun (n, ts) => do pure (n, ← ts.mapM (expandType visited)))
    let rowTail' ← rowTail.mapM (expandType visited)
    pure (.tVariant cases' rowTail')

/-- Expand a parameterised synonym application by substituting arg types
into the body's free type variables. -/
partial def expandSynonymApp (pos : Range) (visited : Std.TreeSet Id) (name : Id)
    (params : List Id) (argTys : List Ty) (body : Malgo.Syntax.Ty .rename) : InferM Ty := do
  if visited.contains name then throw (.cyclicSynonym pos name)
  let bodyTy ← expandType (visited.insert name) body
  let subst : Subst := (params.zip argTys).foldl (fun m (p, a) => m.insert p a) {}
  pure (applySubst subst bodyTy)

end

def surfaceTypeToTy (ty : Malgo.Syntax.Ty .rename) : InferM Ty := expandType {} ty

/-- Build type environment from type signatures. -/
def buildSigEnv (bg : BindGroup .rename) : InferM TyEnv := do
  let entries ← bg.scSigs.mapM fun (_, name, ty) => do
    let inferTy ← surfaceTypeToTy ty
    pure (name, ({ vars := (freeVars inferTy).toList, ty := inferTy } : Scheme))
  pure (insertAll {} entries)

/-- Build type environment from data definitions (constructors). -/
def buildDataEnv (bg : BindGroup .rename) : InferM TyEnv := do
  let entries ← bg.dataDefs.mapM fun (_, typeName, params, cons) => do
    let paramTys := params.map (fun (_, p) => Ty.tVar p 0)
    let resultTy := paramTys.foldl .tApp (.tCon typeName.name)
    cons.mapM fun (_, conName, argTypes) => do
      let argTys ← argTypes.mapM surfaceTypeToTy
      let conTy := argTys.foldr .tArr resultTy
      pure (conName, ({ vars := (freeVars conTy).toList, ty := conTy } : Scheme))
  pure (insertAll {} entries.flatten)

/-- Build type environment from foreign declarations. -/
def buildForeignEnv (bg : BindGroup .rename) : InferM TyEnv := do
  let entries ← bg.foreigns.mapM fun (_, name, ty) => do
    let inferTy ← surfaceTypeToTy ty
    pure (name, ({ vars := (freeVars inferTy).toList, ty := inferTy } : Scheme))
  pure (insertAll {} entries)

def inferLiteral : Literal .unboxed → Ty
  | .int32 _ => tyInt32
  | .int64 _ => tyInt64
  | .float _ => tyFloat
  | .double _ => tyDouble
  | .char _ => tyChar
  | .str _ => tyString

mutual

/-- Infer the type of an expression. -/
partial def inferExpr (env : TyEnv) : Expr .rename → InferM Ty
  | .var pos name =>
    match env.get? name with
    | some sc => instantiate sc
    | none => throw (.unboundVariable pos name)
  | .unboxed _ lit => pure (inferLiteral lit)
  | .boxed ext _ => nomatch ext
  | .apply pos f arg => do
    let fTy ← inferExpr env f
    let argTy ← inferExpr env arg
    let retTy ← freshTyVar
    addConstraint (.cUnify pos fTy (.tArr argTy retTy))
    pure retTy
  | .opApp ext op lhs rhs => do
    let pos := ext.1
    let opTy ← match env.get? op with
      | some sc => instantiate sc
      | none => throw (.unboundVariable pos op)
    let lhsTy ← inferExpr env lhs
    let rhsTy ← inferExpr env rhs
    let retTy ← freshTyVar
    let midTy ← freshTyVar
    addConstraint (.cUnify pos opTy (.tArr lhsTy midTy))
    addConstraint (.cUnify pos midTy (.tArr rhsTy retTy))
    pure retTy
  | .project pos expr field => do
    let exprTy ← inferExpr env expr
    let fieldTy ← freshTyVar
    let rowTail ← freshTyVar
    addConstraint (.cUnify pos exprTy (.tRecord [(field, fieldTy)] (some rowTail)))
    pure fieldTy
  | .fn _ clauses => do
    let ty ← inferClause env clauses.head
    for clause in clauses.tail do
      let clauseTy ← inferClause env clause
      addConstraint (.cUnify dummyRange ty clauseTy)
    pure ty
  | .tuple _ exprs => do
    let tys ← exprs.mapM (inferExpr env)
    pure (.tTuple tys)
  | .record _ fields => do
    let fieldTys ← fields.mapM (fun (name, expr) => do pure (name, ← inferExpr env expr))
    pure (.tRecord fieldTys none)
  | .list ext _ => nomatch ext
  | .ann pos expr ty => do
    let exprTy ← inferExpr env expr
    let annTy ← surfaceTypeToTy ty
    addConstraint (.cUnify pos exprTy annTy)
    pure annTy
  | .seq _ stmts => inferStmts env stmts.toList
  | .parens _ expr => inferExpr env expr
  | .codata _ clauses => do
    let resultTy ← freshTyVar
    for (copat, expr) in clauses do
      inferCoClause env copat expr resultTy
    pure resultTy
  | .label pos name body => do
    let resultTy ← freshTyVar
    let labelEnv := env.insert name { vars := [], ty := resultTy }
    let bodyTy ← inferExpr labelEnv body
    addConstraint (.cUnify pos resultTy bodyTy)
    pure resultTy
  | .goto _ value label => do
    let _ ← inferExpr env value
    let _ ← inferExpr env label
    pure .tBottom

/-- Infer the type of a clause (pattern-matching branch). -/
partial def inferClause (env : TyEnv) : Clause .rename → InferM Ty
  | .mk _ pats body => do
    let results ← pats.toList.mapM (inferPat env)
    let bindings := results.flatMap (·.1)
    let patTys := results.map (·.2)
    let localEnv := insertAll env bindings
    let bodyTy ← inferExpr localEnv body
    pure (patTys.foldr .tArr bodyTy)

/-- Infer the type of a pattern, returning bindings and the pattern type. -/
partial def inferPat (env : TyEnv) : Pat .rename → InferM (List (Id × Scheme) × Ty)
  | .var _ name => do
    let ty ← freshTyVar
    pure ([(name, { vars := [], ty })], ty)
  | .con pos conName pats => do
    match env.get? conName with
    | some sc => do
      let conTy ← instantiate sc
      let results ← pats.mapM (inferPat env)
      let bindings := results.flatMap (·.1)
      let patTys := results.map (·.2)
      let resultTy ← freshTyVar
      let expectedTy := patTys.foldr .tArr resultTy
      addConstraint (.cUnify pos conTy expectedTy)
      pure (bindings, resultTy)
    | none => throw (.unboundVariable pos conName)
  | .tuple _ pats => do
    let results ← pats.mapM (inferPat env)
    pure (results.flatMap (·.1), .tTuple (results.map (·.2)))
  | .record pos fields => do
    let results ← fields.mapM fun (name, pat) => do
      let (b, ty) ← inferPat env pat
      pure (b, (name, ty))
    let bindings := results.flatMap (·.1)
    let fieldTys := results.map (·.2)
    let resultTy ← freshTyVar
    addConstraint (.cUnify pos resultTy (.tRecord fieldTys none))
    pure (bindings, resultTy)
  | .list ext _ => nomatch ext
  | .unboxed _ lit => pure ([], inferLiteral lit)
  | .boxed ext _ => nomatch ext

/-- Infer a coclause (copattern matching). -/
partial def inferCoClause (env : TyEnv) : CoPat .rename → Expr .rename → Ty → InferM Unit
  | .hole _, body, resultTy => do
    let bodyTy ← inferExpr env body
    addConstraint (.cUnify dummyRange resultTy bodyTy)
  | .apply pos copat pat, body, resultTy => do
    let (bindings, argTy) ← inferPat env pat
    let retTy ← freshTyVar
    inferCoClause (insertAll env bindings) copat body retTy
    addConstraint (.cUnify pos resultTy (.tArr argTy retTy))
  | .project pos copat field, body, resultTy => do
    let fieldTy ← freshTyVar
    inferCoClause env copat body fieldTy
    let rowTail ← freshTyVar
    addConstraint (.cUnify pos resultTy (.tRecord [(field, fieldTy)] (some rowTail)))

/-- Infer the type of a sequence of statements. -/
partial def inferStmts (env : TyEnv) : List (Stmt .rename) → InferM Ty
  | [] => pure tyUnit
  | [.noBind _ expr] => inferExpr env expr
  | .letS _ name expr :: rest => do
    enterLevel
    let exprTy ← inferExpr env expr
    let _ ← solveConstraints
    exitLevel
    let st ← get
    let exprTy' := applySubst st.solvedSubst exprTy
    let scheme := generalize st.currentLevel exprTy'
    inferStmts (env.insert name scheme) rest
  | .letPS ext _ _ :: _ => nomatch ext
  | .withS ext _ _ :: _ => nomatch ext
  | .noBind _ expr :: rest => do
    let _ ← inferExpr env expr
    inferStmts env rest

end

/-- Infer a mutually recursive group of definitions. -/
def inferScGroup (env : TyEnv) (defs : List (ScDef .rename)) : InferM TyEnv := do
  enterLevel
  let freshVars ← defs.mapM fun (_, name, _) => do pure (name, ← freshTyVar)
  let localEnv := freshVars.foldl (fun e (name, ty) => e.insert name { vars := [], ty }) env
  for ((pos, _, expr), (_, expectedTy)) in defs.zip freshVars do
    let actualTy ← inferExpr localEnv expr
    addConstraint (.cUnify pos expectedTy actualTy)
  let _ ← solveConstraints
  exitLevel
  let st ← get
  let finalEnv := freshVars.foldl (fun e (name, ty) =>
    let ty' := applySubst st.solvedSubst ty
    let scheme := generalize st.currentLevel ty'
    e.insert name scheme) env
  pure finalEnv

/-- Infer types for an entire bind group. -/
def inferBindGroup (importedEnv : TyEnv) (bg : BindGroup .rename) : InferM TyEnv := do
  let sigEnv ← buildSigEnv bg
  let dataEnv ← buildDataEnv bg
  let foreignEnv ← buildForeignEnv bg
  let env0 := unionEnv (unionEnv (unionEnv importedEnv sigEnv) dataEnv) foreignEnv
  let env ← bg.scDefs.foldlM inferScGroup env0
  let _ ← solveConstraints
  pure env

/-- Entry point: type-check a renamed module. The bind group passes through
unchanged; only the returned `TyEnv` is new. -/
def pass (moduleName : ModuleName) (importedEnv : TyEnv) (bg : BindGroup .rename) :
    MalgoM (BindGroup .rename × TyEnv) := do
  let ctx : InferCtx := { moduleName, synEnv := buildSynEnv bg }
  let act : ExceptT InferError MalgoM TyEnv :=
    (((inferBindGroup importedEnv bg).run ctx).run' initGenState)
  let finalEnv ← Malgo.wrapError "Infer" InferError.render InferError.rangeOf act
  pure (bg, finalEnv)

end Malgo.Infer
