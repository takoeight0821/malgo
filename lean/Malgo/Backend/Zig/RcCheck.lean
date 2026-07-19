import Std.Data.TreeMap
import Std.Data.TreeSet
import Malgo.Backend.Zig.Ir

/-! Port of `src/Malgo/Backend/Zig/RcCheck.hs`: a linearity checker over the
RC-annotated `Ir.Program` produced by `Perceus.perceusProgram`. It symbolically
executes each function, counting each variable's owned references along every
control-flow path, reporting any path where a reference is consumed twice, used
after being consumed, or never consumed.

The model is Perceus's local ownership discipline, deliberately: a borrowed
alias (`ReadPath`/`ReadCapture` result) is accessible only while its root still
holds a reference in this scope (or the alias was itself promoted by a `Dup`).
Panic paths (`Terminator.panic`, `Expr.panicExpr`) terminate the process and
are exempt from the consumed-exactly-once obligation. -/

namespace Malgo.Backend.Zig.RcCheck

open Malgo.Sequent.Fun (Name)
open Malgo.Backend.Zig.Ir

inductive RcViolation where
  /-- A variable was read or consumed on a path where its reference had
  already been moved away (or its borrow root died). -/
  | useAfterConsume (fn x : Name)
  /-- Owned references still held when a consuming terminator fired. -/
  | unconsumedAtExit (fn : Name) (xs : List Name)
  /-- `Dup` of a variable that is no longer accessible. -/
  | dupOfDead (fn x : Name)
  /-- `Drop` of a variable holding no reference. -/
  | dropOfDead (fn x : Name)
  /-- `MkStructReuse` referenced a reuse token not currently held (never
  produced by a `DropReuse`, or already consumed). -/
  | tokenUnavailable (fn tok : Name)
  /-- Reuse tokens (`DropReuse`) still held when a consuming terminator fired —
  every token must be consumed by exactly one `MkStructReuse` before its
  statement list ends. -/
  | tokenUnconsumed (fn : Name) (toks : List Name)
  deriving BEq, Repr

structure St where
  counts : Std.TreeMap Name Int := {}
  aliasRoot : Std.TreeMap Name Name := {}
  tokens : Std.TreeSet Name := {}

def St.ownedCount (st : St) (x : Name) : Int :=
  st.counts.getD x 0

/-- Accessible = holds a reference itself, or is a borrowed alias whose root
(transitively) still does. -/
partial def accessible (st : St) (x : Name) : Bool :=
  st.ownedCount x ≥ 1 ||
    (match st.aliasRoot.get? x with
     | some root => accessible st root
     | none => false)

/-- Decrement an owned reference count, or report that none was held. Shared by
`consume`, the `Drop` case, and the `DropReuse` case. -/
def decrementOwned (st : St) (x : Name) : Bool × St :=
  if st.ownedCount x ≥ 1 then
    (true, { st with counts := st.counts.insert x (st.ownedCount x - 1) })
  else
    (false, st)

def useBorrowed (fname : Name) (st : St) (x : Name) : List RcViolation :=
  if accessible st x then [] else [.useAfterConsume fname x]

def consume (fname : Name) (st : St) (x : Name) : List RcViolation × St :=
  let (ok, st') := decrementOwned st x
  ((if ok then [] else [.useAfterConsume fname x]), st')

def consumeMany (fname : Name) (st : St) (xs : List Name) : List RcViolation × St :=
  xs.foldl (fun (acc : List RcViolation × St) x =>
    let (vs, st') := consume fname acc.2 x
    (acc.1 ++ vs, st')) ([], st)

def bind (st : St) (x : Name) (n : Int) : St :=
  { st with counts := st.counts.insert x n }

def bindAlias (st : St) (x root : Name) : St :=
  { bind st x 0 with aliasRoot := st.aliasRoot.insert x root }

mutual

partial def goBlock (fname : Name) (st : St) : Block → List RcViolation
  | .mk stmts term => goStmts fname st stmts term

partial def goStmts (fname : Name) (st : St) : List Stmt → Terminator → List RcViolation
  | [], term => goTerm fname st term
  | .dup x :: rest, term =>
    let st' := { st with counts := st.counts.insert x (st.ownedCount x + 1) }
    (if accessible st x then [] else [.dupOfDead fname x]) ++ goStmts fname st' rest term
  | .drop x :: rest, term =>
    let (ok, st') := decrementOwned st x
    (if ok then [] else [.dropOfDead fname x]) ++ goStmts fname st' rest term
  | .dropReuse tok x _ :: rest, term =>
    let (ok, st') := decrementOwned st x
    let st'' := { st' with tokens := st'.tokens.insert tok }
    (if ok then [] else [.dropOfDead fname x]) ++ goStmts fname st'' rest term
  | .let x e :: rest, term =>
    let consuming := fun (ops : List Name) =>
      let (vs, st') := consumeMany fname st ops
      vs ++ goStmts fname (bind st' x 1) rest term
    match e with
    -- noreturn: the path exits the process here; no obligations.
    | .panicExpr _ => []
    | .readPath p => useBorrowed fname st p.root' ++ goStmts fname (bindAlias st x p.root') rest term
    | .readCapture self _ => useBorrowed fname st self ++ goStmts fname (bindAlias st x self) rest term
    | .lit _ => goStmts fname (bind st x 1) rest term
    | .prim _ ops => ops.flatMap (useBorrowed fname st) ++ goStmts fname (bind st x 1) rest term
    | .mkStruct _ ops => consuming ops
    | .mkClosure _ ops => consuming ops
    | .mkRecord _ ops => consuming ops
    | .force v _ => consuming [v]
    | .mkStructReuse tok _ ops =>
      let tokViolation := if st.tokens.contains tok then [] else [.tokenUnavailable fname tok]
      let st1 := { st with tokens := st.tokens.erase tok }
      let (vs, st2) := consumeMany fname st1 ops
      tokViolation ++ vs ++ goStmts fname (bind st2 x 1) rest term

partial def goTerm (fname : Name) (st : St) : Terminator → List RcViolation
  | .«if» guard t e =>
    (freeVarsGuard guard).toList.flatMap (useBorrowed fname st) ++ goBlock fname st t ++ goBlock fname st e
  | .panic _ => []
  | term =>
    let (vs, st') := consumeMany fname st (termOperands term)
    let leftover := st'.counts.toList.filterMap (fun (y, n) => if n > 0 then some y else none)
    let leftoverTokens := st'.tokens.toList
    vs
      ++ (if leftover.isEmpty then [] else [.unconsumedAtExit fname leftover])
      ++ (if leftoverTokens.isEmpty then [] else [.tokenUnconsumed fname leftoverTokens])

end

def checkFunc (fn : Func) : List RcViolation :=
  let initial : List (Name × Int) :=
    fn.params.map (fun p => (p, 1)) ++
      (if fn.kind != .topLevelFn then [(fn.selfVar, 1)] else [])
  let st0 : St := { counts := initial.foldl (fun m (k, v) => m.insert k v) {} }
  goBlock fn.name st0 fn.body

def checkProgram (program : Program) : Except (List RcViolation) Unit :=
  match program.funcs.flatMap checkFunc with
  | [] => .ok ()
  | violations => .error violations

/-! Sanity checks over hand-built fixtures. -/

private def n (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }

/-- A top-level function that consumes its single param exactly once. -/
private def wellFormed : Func :=
  { range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
    name := n "f", kind := .topLevelFn, selfVar := n "self"
    params := [n "x"]
    body := .mk [] (.«return» (n "x")) }

/-- Same, but drops `x` and then also returns it — a double consume. -/
private def doubleConsume : Func :=
  { range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
    name := n "g", kind := .topLevelFn, selfVar := n "self"
    params := [n "x"]
    body := .mk [.drop (n "x")] (.«return» (n "x")) }

#guard (match checkProgram { funcs := [wellFormed], entry := none } with | .ok () => true | _ => false)
#guard checkFunc doubleConsume == [.useAfterConsume (n "g") (n "x")]

end Malgo.Backend.Zig.RcCheck
