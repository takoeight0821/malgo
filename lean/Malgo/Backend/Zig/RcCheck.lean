import Std.Data.TreeMap
import Std.Data.TreeSet
import Malgo.Backend.Zig.Ir
import Malgo.Backend.Zig.Stage

/-! Port of `src/Malgo/Backend/Zig/RcCheck.hs`: a linearity checker over the
RC-annotated `Ir.Program` produced by `Reuse.reuseProgram` (the last stage
before this one — it also checks the reuse-token discipline `Reuse` adds,
not just Perceus's `Dup`/`Drop`). It symbolically
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

def checkProgram (staged : Staged .reuse) : Except (List RcViolation) Unit :=
  match staged.program.funcs.flatMap checkFunc with
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

#guard (match checkProgram (⟨{ funcs := [wellFormed], entry := none }⟩ : Staged .reuse) with
  | .ok () => true | _ => false)
#guard checkFunc doubleConsume == [.useAfterConsume (n "g") (n "x")]

/-! ## Reuse-token linearity (port of `ReuseSpec.hs`'s `rcCheckSpec`)

A reuse token must be produced by a `dropReuse` and consumed by exactly one
`mkStructReuse` before the block's terminator. Both failure directions are
pinned, since a checker that accepts everything would pass the positive case
alone. -/

private def rtok : Name := n "reuse_0"

private def reuseFn (body : Block) : Func :=
  { range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
    name := n "r", kind := .topLevelFn, selfVar := n "self"
    params := [n "a", n "b", n "k"], body }

-- Well-formed: token produced, then consumed by a same-arity rebuild.
#guard (match checkProgram (⟨{ funcs := [reuseFn
    (.mk [.dropReuse rtok (n "a") 1, .let (n "s") (.mkStructReuse rtok .tuple [n "b"])]
      (.applyCo (n "k") (n "s")))], entry := none }⟩ : Staged .reuse) with
  | .ok () => true | _ => false)

-- Consuming a token that was never produced.
#guard (checkFunc (reuseFn
    (.mk [.let (n "s") (.mkStructReuse rtok .tuple [n "a"])] (.applyCo (n "k") (n "s"))))
  |>.any (fun v => match v with | .tokenUnavailable _ t => t == rtok | _ => false))

-- Producing a token and reaching the terminator without consuming it.
#guard (checkFunc (reuseFn (.mk [.dropReuse rtok (n "a") 1] (.applyCo (n "k") (n "k"))))
  |>.any (fun v => match v with | .tokenUnconsumed _ ts => ts.contains rtok | _ => false))

/-! ## Linearity violations (port of `PerceusSpec.hs`'s `rcCheckSpec`)

A checker that accepts everything would satisfy the positive cases and the
corpus oracle alike, so each violation kind gets a fixture that must trip it. -/

private def cnm (s : String) : Name :=
  { name := s, moduleName := .moduleName "RcTest", sort := .external }

private def cFn (kind : FuncKind) (params : List Name) (body : Block) : Func :=
  { range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
    name := cnm "c", kind, selfVar := cnm "self", params, body }

-- Consuming the same reference twice in one call.
#guard (checkFunc (cFn .topLevelFn [cnm "a", cnm "k"]
    (.mk [] (.callClosure (cnm "k") [cnm "a", cnm "a"])))
  |>.any (fun v => match v with | .useAfterConsume _ x => x == cnm "a" | _ => false))

-- An owned parameter that reaches the terminator unconsumed (a leak).
#guard (checkFunc (cFn .topLevelFn [cnm "a", cnm "k"]
    (.mk [.let (cnm "l") (.lit (.int32 1))] (.applyCo (cnm "k") (cnm "l"))))
  |>.any (fun v => match v with | .unconsumedAtExit _ xs => xs.contains (cnm "a") | _ => false))

-- Touching a borrowed alias after its root's reference was moved away.
#guard (checkFunc (cFn .topLevelFn [cnm "s", cnm "k"]
    (.mk [.let (cnm "h") (.readPath (.root (cnm "s"))), .drop (cnm "s"), .dup (cnm "h")]
      (.applyCo (cnm "k") (cnm "h"))))
  |>.any (fun v => match v with | .dupOfDead _ x => x == cnm "h" | _ => false))

end Malgo.Backend.Zig.RcCheck
