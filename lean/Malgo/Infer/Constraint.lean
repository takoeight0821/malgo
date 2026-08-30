import Std.Data.TreeMap
import Std.Data.TreeSet
import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.Monad
import Malgo.Syntax

/-! Port of `src/Malgo/Infer/Constraint.hs`: the inference-internal type
representation, substitutions, constraints, generation state, and the
inference monad.

Name-clash note: the surface AST type is `Malgo.Syntax.Ty`; the
inference-internal type here is `Malgo.Infer.Ty` (different namespace).
Binders (`tForall`/`tMu`) are nameless — bound occurrences are `tBound`
with a de Bruijn *index* (0 = innermost binder), so alpha-equivalent types
are structurally equal under `BEq`/`Ord`.

Monad design (Haskell's `Reader ModuleName`/`State GenState`/`State Uniq`/
`Reader SynEnv`/`Error InferError` effects): `InferCtx` bundles the fixed
`moduleName` and the read-only `synEnv` under a single `ReaderT`; `GenState`
is a `StateT`; `InferError` an `ExceptT`; `State Uniq`/IO come from the
underlying `MalgoM`. -/

namespace Malgo.Infer

open Malgo Malgo.Syntax

/-- Level for let-polymorphism. Higher = deeper let-nesting. -/
abbrev Level := Int

/-- Inference-internal type. -/
inductive Ty where
  | tVar (id : Id) (level : Level)
  | tBound (idx : Nat)
  | tCon (name : String)
  | tArr (dom cod : Ty)
  | tApp (fn arg : Ty)
  | tTuple (tys : List Ty)
  | tRecord (fields : List (String × Ty)) (rowTail : Option Ty)
  | tVariant (cases : List (String × List Ty)) (rowTail : Option Ty)
  | tBottom
  | tForall (body : Ty)
  | tMu (body : Ty)
  deriving BEq, Repr

instance : Inhabited Ty := ⟨.tBottom⟩

def tyCtorIdx : Ty → Nat
  | .tVar .. => 0
  | .tBound .. => 1
  | .tCon .. => 2
  | .tArr .. => 3
  | .tApp .. => 4
  | .tTuple .. => 5
  | .tRecord .. => 6
  | .tVariant .. => 7
  | .tBottom => 8
  | .tForall .. => 9
  | .tMu .. => 10

-- Hand-written total order (Lean's `deriving Ord` does not recurse into the
-- `List (String × Ty)` / `List (String × List Ty)` fields). Structural
-- comparison; alpha-equivalent nameless binders compare equal.
mutual
partial def cmpTy : Ty → Ty → Ordering
  | .tVar a la, .tVar b lb => (compare a b).then (compare la lb)
  | .tBound a, .tBound b => compare a b
  | .tCon a, .tCon b => compare a b
  | .tArr a1 b1, .tArr a2 b2 => (cmpTy a1 a2).then (cmpTy b1 b2)
  | .tApp a1 b1, .tApp a2 b2 => (cmpTy a1 a2).then (cmpTy b1 b2)
  | .tTuple a, .tTuple b => cmpTyList a b
  | .tRecord fa ra, .tRecord fb rb => (cmpFields fa fb).then (cmpTyOpt ra rb)
  | .tVariant ca ra, .tVariant cb rb => (cmpCases ca cb).then (cmpTyOpt ra rb)
  | .tBottom, .tBottom => .eq
  | .tForall a, .tForall b => cmpTy a b
  | .tMu a, .tMu b => cmpTy a b
  | a, b => compare (tyCtorIdx a) (tyCtorIdx b)
partial def cmpTyList : List Ty → List Ty → Ordering
  | [], [] => .eq
  | [], _ => .lt
  | _, [] => .gt
  | x :: xs, y :: ys => (cmpTy x y).then (cmpTyList xs ys)
partial def cmpTyOpt : Option Ty → Option Ty → Ordering
  | none, none => .eq
  | none, _ => .lt
  | _, none => .gt
  | some x, some y => cmpTy x y
partial def cmpFields : List (String × Ty) → List (String × Ty) → Ordering
  | [], [] => .eq
  | [], _ => .lt
  | _, [] => .gt
  | (k1, v1) :: xs, (k2, v2) :: ys => ((compare k1 k2).then (cmpTy v1 v2)).then (cmpFields xs ys)
partial def cmpCases : List (String × List Ty) → List (String × List Ty) → Ordering
  | [], [] => .eq
  | [], _ => .lt
  | _, [] => .gt
  | (k1, v1) :: xs, (k2, v2) :: ys => ((compare k1 k2).then (cmpTyList v1 v2)).then (cmpCases xs ys)
end

instance : Ord Ty := ⟨cmpTy⟩
instance : Ord (Ty × Ty) := lexOrd

private def boundVarName (n : Nat) : String :=
  if n < 26 then String.singleton (Char.ofNat (('a'.toNat) + n))
  else s!"t{n - 26}"

/-- Pretty-print a `Ty` given the innermost-first names of enclosing
binders (`env[0]` names `tBound 0`). -/
partial def prettyTyWith (env : List String) : Ty → String
  | .tVar name _ => pretty name
  | .tBound i => if h : i < env.length then env[i] else s!"?{i}"
  | .tCon name => name
  | .tArr arg ret => s!"({prettyTyWith env arg} -> {prettyTyWith env ret})"
  | .tApp f a => s!"({prettyTyWith env f} {prettyTyWith env a})"
  | .tTuple ts => s!"({String.intercalate ", " (ts.map (prettyTyWith env))})"
  | .tRecord fields rowTail =>
    let body := String.intercalate ", " (fields.map (fun (k, v) => s!"{k} : {prettyTyWith env v}"))
    let tail := match rowTail with | some r => s!" | {prettyTyWith env r}" | none => ""
    s!"\{{body}{tail}}"
  | .tVariant cases rowTail =>
    let body := String.intercalate " | "
      (cases.map (fun (c, ts) => s!"{c} {String.intercalate " " (ts.map (prettyTyWith env))}"))
    let tail := match rowTail with | some r => s!" | {prettyTyWith env r}" | none => ""
    s!"[{body}{tail}]"
  | .tBottom => "_|_"
  | .tForall body =>
    let n := boundVarName env.length
    s!"(forall {n}. {prettyTyWith (n :: env) body})"
  | .tMu body =>
    let n := boundVarName env.length
    s!"(mu {n}. {prettyTyWith (n :: env) body})"

instance : Pretty Ty := ⟨prettyTyWith []⟩

/-- Type scheme for let-polymorphism. -/
structure Scheme where
  vars : List Id
  ty : Ty
  deriving BEq, Repr

/-- Type substitution keyed by `Id`. -/
abbrev Subst := Std.TreeMap Id Ty

/-- Apply a substitution to a type. Only `tVar` (metavariables) are
substituted; `tBound` (de Bruijn) are never touched. Chases through the
substitution, so it is not structurally recursive. -/
partial def applySubst (subst : Subst) : Ty → Ty
  | .tVar name lvl => match subst.get? name with
    | some ty' => applySubst subst ty'
    | none => .tVar name lvl
  | .tBound i => .tBound i
  | .tCon c => .tCon c
  | .tArr a b => .tArr (applySubst subst a) (applySubst subst b)
  | .tApp f a => .tApp (applySubst subst f) (applySubst subst a)
  | .tTuple ts => .tTuple (ts.map (applySubst subst))
  | .tRecord fields rowTail =>
    .tRecord (fields.map (fun (k, v) => (k, applySubst subst v))) (rowTail.map (applySubst subst))
  | .tVariant cases rowTail =>
    .tVariant (cases.map (fun (c, ts) => (c, ts.map (applySubst subst)))) (rowTail.map (applySubst subst))
  | .tBottom => .tBottom
  | .tForall t => .tForall (applySubst subst t)
  | .tMu t => .tMu (applySubst subst t)

/-- Free metavariables in a type. `tBound` are not free. -/
partial def freeVars : Ty → Std.TreeSet Id
  | .tVar name _ => ({} : Std.TreeSet Id).insert name
  | .tBound _ => {}
  | .tCon _ => {}
  | .tArr a b => (freeVars a).merge (freeVars b)
  | .tApp f a => (freeVars f).merge (freeVars a)
  | .tTuple ts => ts.foldl (fun acc t => acc.merge (freeVars t)) {}
  | .tRecord fields rowTail =>
    let base := fields.foldl (fun acc (_, v) => acc.merge (freeVars v)) {}
    match rowTail with | some r => base.merge (freeVars r) | none => base
  | .tVariant cases rowTail =>
    let base := cases.foldl (fun acc (_, ts) => acc.merge (ts.foldl (fun a t => a.merge (freeVars t)) {})) {}
    match rowTail with | some r => base.merge (freeVars r) | none => base
  | .tBottom => {}
  | .tForall t => freeVars t
  | .tMu t => freeVars t

/-- Occurs check: does metavariable `name` occur free in a type? -/
partial def occursIn (name : Id) : Ty → Bool
  | .tVar v _ => name == v
  | .tBound _ => false
  | .tCon _ => false
  | .tArr a b => occursIn name a || occursIn name b
  | .tApp f a => occursIn name f || occursIn name a
  | .tTuple ts => ts.any (occursIn name)
  | .tRecord fields rowTail =>
    fields.any (fun (_, v) => occursIn name v) || (rowTail.map (occursIn name)).getD false
  | .tVariant cases rowTail =>
    cases.any (fun (_, ts) => ts.any (occursIn name)) || (rowTail.map (occursIn name)).getD false
  | .tBottom => false
  | .tForall t => occursIn name t
  | .tMu t => occursIn name t

/-- `nub` preserving first-occurrence order. -/
private def nub [BEq α] (xs : List α) : List α :=
  (xs.foldl (fun acc x => if acc.contains x then acc else acc ++ [x]) [])

private partial def collectVars : Ty → List Ty
  | .tVar id l => [.tVar id l]
  | .tBound _ => []
  | .tCon _ => []
  | .tArr a b => collectVars a ++ collectVars b
  | .tApp f a => collectVars f ++ collectVars a
  | .tTuple ts => ts.flatMap collectVars
  | .tRecord fields rowTail =>
    fields.flatMap (fun (_, v) => collectVars v) ++ (rowTail.map collectVars).getD []
  | .tVariant cases rowTail =>
    cases.flatMap (fun (_, ts) => ts.flatMap collectVars) ++ (rowTail.map collectVars).getD []
  | .tBottom => []
  | .tForall t => collectVars t
  | .tMu t => collectVars t

/-- Generalize a type over free metavariables with level > `lvl`. -/
def generalize (lvl : Level) (ty : Ty) : Scheme :=
  let vars := (collectVars ty).filterMap fun
    | .tVar v l => if l > lvl then some v else none
    | _ => none
  { vars := nub vars, ty }

/-- Type constraints generated during inference. -/
inductive TyConstraint where
  | cUnify (range : Range) (t1 t2 : Ty)
  | cBottomProp (sources : List Ty) (target : Ty)
  deriving Repr

/-- State for constraint generation. -/
structure GenState where
  constraints : List TyConstraint := []
  currentLevel : Level := 0
  solvedSubst : Subst := {}

def initGenState : GenState := {}

/-- Inference error type. -/
inductive InferError where
  | unificationError (range : Range) (expected actual : Ty) (msg : String)
  | unboundVariable (range : Range) (name : Id)
  | occursCheckError (range : Range) (varName : Id) (ty : Ty)
  | notImplemented (range : Range) (feature : String)
  | cyclicSynonym (range : Range) (name : Id)
  | synonymArityMismatch (range : Range) (name : Id) (expected got : Nat)

/-- A dummy range for compiler-generated constraints. -/
def dummyRange : Range :=
  let pos : SourcePos := { sourceName := "<infer>", line := 1, column := 1 }
  { start := pos, stop := pos }

-- `.notImplemented` exists only to give `InferError` this `Inhabited`
-- witness; it is never thrown by real inference code (see
-- `inferErrorCoverage` in lean/Test/Main.lean).
instance : Inhabited InferError := ⟨.notImplemented dummyRange "uninhabited"⟩

def InferError.render : InferError → String
  | .unificationError _ expected actual msg =>
    s!"Type error: {msg}\n  Expected: {pretty expected}\n  Actual: {pretty actual}"
  | .unboundVariable _ name => s!"Unbound variable: {pretty name}"
  | .occursCheckError _ varName ty =>
    s!"Occurs check failed: type variable '{pretty varName}' occurs in {pretty ty}"
  | .notImplemented _ feature => s!"Type inference not yet implemented for: {feature}"
  | .cyclicSynonym _ name => s!"Cyclic type synonym: {pretty name}"
  | .synonymArityMismatch _ name expected got =>
    s!"Type synonym '{pretty name}' expects {expected} argument(s) but got {got}"

def InferError.rangeOf : InferError → Option Range
  | .unificationError r .. => some r
  | .unboundVariable r _ => some r
  | .occursCheckError r .. => some r
  | .notImplemented r _ => some r
  | .cyclicSynonym r _ => some r
  | .synonymArityMismatch r .. => some r

/-- Type-synonym environment: name ↦ (params, unexpanded body). -/
abbrev SynEnv := Std.TreeMap Id (List Id × Malgo.Syntax.Ty .rename)

/-- Reader context: the fixed module name (for fresh `Id`s) and the
read-only synonym environment. -/
structure InferCtx where
  moduleName : ModuleName
  synEnv : SynEnv := {}

/-- The inference monad. `abbrev` so transformer instances and the
auto-lift chain from `MalgoM` resolve through the unfolded stack. -/
abbrev InferM := ReaderT InferCtx (StateT GenState (ExceptT InferError MalgoM))

instance {α} : Inhabited (ExceptT InferError MalgoM α) := ⟨throw (default : InferError)⟩
instance {α} : Inhabited (InferM α) := ⟨throw (default : InferError)⟩

/-- Fresh type variable at the current level (globally unique `Id`). -/
def freshTyVar : InferM Ty := do
  let lvl := (← get).currentLevel
  let freshId ← newTemporalId (← read).moduleName "_t"
  pure (.tVar freshId lvl)

def addConstraint (c : TyConstraint) : InferM Unit :=
  modify fun s => { s with constraints := c :: s.constraints }

def enterLevel : InferM Unit :=
  modify fun s => { s with currentLevel := s.currentLevel + 1 }

def exitLevel : InferM Unit :=
  modify fun s => { s with currentLevel := s.currentLevel - 1 }

def currentSubst : InferM Subst :=
  return (← get).solvedSubst

/-- Instantiate a scheme with fresh type variables. -/
def instantiate (sc : Scheme) : InferM Ty := do
  let freshVars ← sc.vars.mapM (fun v => do pure (v, ← freshTyVar))
  let subst : Subst := freshVars.foldl (fun m (v, t) => m.insert v t) {}
  pure (applySubst subst sc.ty)

/-- Standard types. -/
def tyInt32 : Ty := .tCon "Int32#"
def tyInt64 : Ty := .tCon "Int64#"
def tyFloat : Ty := .tCon "Float#"
def tyDouble : Ty := .tCon "Double#"
def tyChar : Ty := .tCon "Char#"
def tyString : Ty := .tCon "String#"
def tyUnit : Ty := .tTuple []

end Malgo.Infer
