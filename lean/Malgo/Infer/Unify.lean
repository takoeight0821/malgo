import Std.Data.TreeMap
import Std.Data.TreeSet
import Malgo.Infer.Constraint

/-! Port of `src/Malgo/Infer/Unify.hs`: unification with row polymorphism
and equi-recursive (`tMu`) types, plus constraint solving.

The Haskell `evalState (Set.empty) $ unifyTypes` seen-set (cycle detection)
becomes a `StateT (Std.TreeSet (Ty × Ty))` layer (`UnifyM`) wrapping
`InferM`; `unify`/`solveConstraints` run it with a fresh empty set. -/

namespace Malgo.Infer

open Malgo

/-- Substitute the bound variable at de Bruijn index `target` with `repl`
throughout `ty`. No shifting needed: replacements are either closed
(`tMu` bodies) or fresh metavariables. -/
partial def substBound (target : Nat) (repl : Ty) : Ty → Ty
  | .tBound i => if i == target then repl else .tBound i
  | .tVar v l => .tVar v l
  | .tCon c => .tCon c
  | .tBottom => .tBottom
  | .tArr a b => .tArr (substBound target repl a) (substBound target repl b)
  | .tApp f a => .tApp (substBound target repl f) (substBound target repl a)
  | .tTuple ts => .tTuple (ts.map (substBound target repl))
  | .tRecord fs row => .tRecord (fs.map (fun (k, v) => (k, substBound target repl v))) (row.map (substBound target repl))
  | .tVariant cs row => .tVariant (cs.map (fun (k, vs) => (k, vs.map (substBound target repl)))) (row.map (substBound target repl))
  | .tForall body => .tForall (substBound (target + 1) repl body)
  | .tMu body => .tMu (substBound (target + 1) repl body)

private partial def abstractVarGo (x : Id) (depth : Nat) : Ty → Ty
  | .tVar v l => if v == x then .tBound depth else .tVar v l
  | .tBound i => .tBound i
  | .tCon c => .tCon c
  | .tBottom => .tBottom
  | .tArr a b => .tArr (abstractVarGo x depth a) (abstractVarGo x depth b)
  | .tApp f a => .tApp (abstractVarGo x depth f) (abstractVarGo x depth a)
  | .tTuple ts => .tTuple (ts.map (abstractVarGo x depth))
  | .tRecord fs row => .tRecord (fs.map (fun (k, v) => (k, abstractVarGo x depth v))) (row.map (abstractVarGo x depth))
  | .tVariant cs row => .tVariant (cs.map (fun (k, vs) => (k, vs.map (abstractVarGo x depth)))) (row.map (abstractVarGo x depth))
  | .tForall body => .tForall (abstractVarGo x (depth + 1) body)
  | .tMu body => .tMu (abstractVarGo x (depth + 1) body)

/-- Abstract free metavariable `x` out of `ty`, replacing each free
occurrence with `tBound depth` (counting binders crossed). Result is the
body of a new `tMu` closing over `x`. -/
partial def abstractVar (x : Id) : Ty → Ty := abstractVarGo x 0

/-- Compose two substitutions: `s2` after `s1`. After applying `s2` to each
value of `s1`, a key may appear free in its own value; normalize such
self-references into `tMu` via `abstractVar`. The mapped `s1` entries win
over `s2` on key collision (Haskell `<>` is left-biased). -/
def composeSubst (s2 s1 : Subst) : Subst :=
  s1.foldl (init := s2) fun acc k v =>
    let v' := applySubst s2 v
    let v'' :=
      if occursIn k v' then
        match v' with
        | .tMu body => .tMu (abstractVar k body)
        | _ => .tMu (abstractVar k v')
      else v'
    acc.insert k v''

/-- Commit a computed substitution to the global inference state. -/
def commitSubst (s : Subst) : InferM Unit :=
  modify fun st => { st with solvedSubst := composeSubst s st.solvedSubst }

private def isBottom : Ty → Bool
  | .tBottom => true
  | _ => false

private def lookupField (name : String) (fields : List (String × Ty)) : Ty :=
  match fields.find? (·.1 == name) with
  | some (_, t) => t
  | none => panic! s!"lookupField: field not found: {name}"

private def lookupCon (name : String) (cases : List (String × List Ty)) : List Ty :=
  match cases.find? (·.1 == name) with
  | some (_, ts) => ts
  | none => panic! s!"lookupCon: constructor not found: {name}"

/-- Unification monad: the cycle-detection seen-set over `InferM`. -/
abbrev UnifyM := StateT (Std.TreeSet (Ty × Ty)) InferM

instance {α} : Inhabited (UnifyM α) := ⟨throw (default : InferError)⟩

mutual

partial def unifyTypes (pos : Range) (t1 t2 : Ty) : UnifyM Subst := do
  if t1 == t2 then pure {}
  else
    let seen ← get
    if seen.contains (t1, t2) || seen.contains (t2, t1) then pure {}
    else do
      modify (·.insert (t1, t2))
      unifyInternal pos t1 t2

partial def unifyInternal (pos : Range) : Ty → Ty → UnifyM Subst
  | .tBottom, _ => pure {}
  | _, .tBottom => pure {}
  | .tCon c1, .tCon c2 =>
    if c1 == c2 then pure {}
    else throw (.unificationError pos (.tCon c1) (.tCon c2) s!"Cannot unify type constructors '{c1}' and '{c2}'")
  | .tVar x _, .tVar y ly =>
    if x == y then pure {}
    else pure (({} : Subst).insert x (.tVar y ly))
  | .tVar x _, t =>
    if occursIn x t then pure (({} : Subst).insert x (.tMu (abstractVar x t)))
    else pure (({} : Subst).insert x t)
  | t, .tVar x l => unifyTypes pos (.tVar x l) t
  | .tMu body, t2 => unifyTypes pos (substBound 0 (.tMu body) body) t2
  | t1, .tMu body => unifyTypes pos t1 (substBound 0 (.tMu body) body)
  | .tArr a1 b1, .tArr a2 b2 => do
    let s1 ← unifyTypes pos a1 a2
    let s2 ← unifyTypes pos (applySubst s1 b1) (applySubst s1 b2)
    pure (composeSubst s2 s1)
  | .tApp f1 a1, .tApp f2 a2 => do
    let s1 ← unifyTypes pos f1 f2
    let s2 ← unifyTypes pos (applySubst s1 a1) (applySubst s1 a2)
    pure (composeSubst s2 s1)
  | .tTuple ts1, .tTuple ts2 =>
    if ts1.length == ts2.length then unifyList pos ts1 ts2
    else throw (.unificationError pos (.tTuple ts1) (.tTuple ts2) "Tuple lengths differ")
  | .tRecord fs1 r1, .tRecord fs2 r2 => unifyRecords pos fs1 r1 fs2 r2
  | .tVariant cs1 r1, .tVariant cs2 r2 => unifyVariants pos cs1 r1 cs2 r2
  | .tForall body, t2 => do
    let fresh ← freshTyVar
    unifyTypes pos (substBound 0 fresh body) t2
  | t1, .tForall body => do
    let fresh ← freshTyVar
    unifyTypes pos t1 (substBound 0 fresh body)
  | t1, t2 => throw (.unificationError pos t1 t2 "Cannot unify types")

partial def unifyList (pos : Range) : List Ty → List Ty → UnifyM Subst
  | [], [] => pure {}
  | t1 :: ts1, t2 :: ts2 => do
    let s1 ← unifyTypes pos t1 t2
    let s2 ← unifyList pos (ts1.map (applySubst s1)) (ts2.map (applySubst s1))
    pure (composeSubst s2 s1)
  | _, _ => pure {}

partial def unifyRecords (pos : Range) (fs1 : List (String × Ty)) (r1 : Option Ty)
    (fs2 : List (String × Ty)) (r2 : Option Ty) : UnifyM Subst := do
  let names1 := fs1.map (·.1)
  let names2 := fs2.map (·.1)
  let commonNames := names1.filter (fun n => names2.contains n)
  let only1 := fs1.filter (fun (n, _) => !names2.contains n)
  let only2 := fs2.filter (fun (n, _) => !names1.contains n)
  let commonSubst ← commonNames.foldlM (init := ({} : Subst)) fun s name => do
    let t1 := applySubst s (lookupField name fs1)
    let t2 := applySubst s (lookupField name fs2)
    let s' ← unifyTypes pos t1 t2
    pure (composeSubst s' s)
  match r1, r2 with
  | none, none =>
    if only1.isEmpty && only2.isEmpty then pure commonSubst
    else throw (.unificationError pos (.tRecord fs1 r1) (.tRecord fs2 r2) "Record field mismatch")
  | some row1, none =>
    if only1.isEmpty then do
      let only2' := only2.map (fun (n, t) => (n, applySubst commonSubst t))
      let s ← unifyTypes pos (applySubst commonSubst row1) (.tRecord only2' none)
      pure (composeSubst s commonSubst)
    else throw (.unificationError pos (.tRecord fs1 r1) (.tRecord fs2 r2) "Record field mismatch: left has extra fields")
  | none, some row2 =>
    if only2.isEmpty then do
      let only1' := only1.map (fun (n, t) => (n, applySubst commonSubst t))
      let s ← unifyTypes pos (applySubst commonSubst row2) (.tRecord only1' none)
      pure (composeSubst s commonSubst)
    else throw (.unificationError pos (.tRecord fs1 r1) (.tRecord fs2 r2) "Record field mismatch: right has extra fields")
  | some row1, some row2 => do
    let freshRow ← freshTyVar
    let only2' := only2.map (fun (n, t) => (n, applySubst commonSubst t))
    let only1' := only1.map (fun (n, t) => (n, applySubst commonSubst t))
    let s1 ← unifyTypes pos (applySubst commonSubst row1) (.tRecord only2' (some freshRow))
    let s2 ← unifyTypes pos (applySubst (composeSubst s1 commonSubst) row2) (applySubst s1 (.tRecord only1' (some freshRow)))
    pure (composeSubst s2 (composeSubst s1 commonSubst))

partial def unifyVariants (pos : Range) (cs1 : List (String × List Ty)) (r1 : Option Ty)
    (cs2 : List (String × List Ty)) (r2 : Option Ty) : UnifyM Subst := do
  let names1 := cs1.map (·.1)
  let names2 := cs2.map (·.1)
  let commonNames := names1.filter (fun n => names2.contains n)
  let only1 := cs1.filter (fun (n, _) => !names2.contains n)
  let only2 := cs2.filter (fun (n, _) => !names1.contains n)
  let commonSubst ← commonNames.foldlM (init := ({} : Subst)) fun s name => do
    let tys1 := lookupCon name cs1
    let tys2 := lookupCon name cs2
    if tys1.length != tys2.length then
      throw (.unificationError pos (.tVariant cs1 r1) (.tVariant cs2 r2) s!"Constructor '{name}' has different arities")
    else do
      let s' ← unifyList pos (tys1.map (applySubst s)) (tys2.map (applySubst s))
      pure (composeSubst s' s)
  match r1, r2 with
  | none, none =>
    if only1.isEmpty && only2.isEmpty then pure commonSubst
    else throw (.unificationError pos (.tVariant cs1 r1) (.tVariant cs2 r2) "Variant mismatch")
  | some row1, none =>
    if only1.isEmpty then do
      let s ← unifyTypes pos (applySubst commonSubst row1) (.tVariant only2 none)
      pure (composeSubst s commonSubst)
    else throw (.unificationError pos (.tVariant cs1 r1) (.tVariant cs2 r2) "Variant mismatch: left has extra constructors")
  | none, some row2 =>
    if only2.isEmpty then do
      let s ← unifyTypes pos (applySubst commonSubst row2) (.tVariant only1 none)
      pure (composeSubst s commonSubst)
    else throw (.unificationError pos (.tVariant cs1 r1) (.tVariant cs2 r2) "Variant mismatch: right has extra constructors")
  | some row1, some row2 => do
    let freshRow ← freshTyVar
    let s1 ← unifyTypes pos (applySubst commonSubst row1) (.tVariant only2 (some freshRow))
    let s2 ← unifyTypes pos (applySubst (composeSubst s1 commonSubst) row2) (applySubst s1 (.tVariant only1 (some freshRow)))
    pure (composeSubst s2 (composeSubst s1 commonSubst))

end

/-- Unify two types, committing the result to the global state. -/
def unify (pos : Range) (t1 t2 : Ty) : InferM Subst := do
  let subst ← currentSubst
  let t1' := applySubst subst t1
  let t2' := applySubst subst t2
  let s ← (unifyTypes pos t1' t2').run' {}
  commitSubst s
  pure s

private def solveOne (baseSubst : Subst) (acc : Subst) : TyConstraint → UnifyM Subst
  | .cUnify pos t1 t2 => do
    let subst := composeSubst acc baseSubst
    let t1' := applySubst subst t1
    let t2' := applySubst subst t2
    let s ← unifyTypes pos t1' t2'
    pure (composeSubst s acc)
  | .cBottomProp sources target => do
    let subst := composeSubst acc baseSubst
    let sources' := sources.map (applySubst subst)
    let target' := applySubst subst target
    if sources'.any isBottom then
      match target' with
      | .tVar _ _ => do
        let s ← unifyTypes dummyRange target' .tBottom
        pure (composeSubst s acc)
      | _ => pure acc
    else pure acc

/-- Solve all accumulated constraints, committing the result. -/
def solveConstraints : InferM Subst := do
  let st ← get
  let cs := st.constraints.reverse
  let baseSubst := st.solvedSubst
  let s ← (cs.foldlM (solveOne baseSubst) ({} : Subst)).run' {}
  commitSubst s
  modify fun st' => { st' with constraints := [] }
  currentSubst

end Malgo.Infer
