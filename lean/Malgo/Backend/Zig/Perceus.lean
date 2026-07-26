import Std.Data.TreeMap
import Malgo.Backend.Zig.Ir
import Malgo.Prelude
import Malgo.Sequent.Fun

/-! Port of `src/Malgo/Backend/Zig/Perceus.hs`: the Perceus reference-
counting pass (PLDI 2021 "Counting Immutable Beans"). A pure
`Ir.Program → Ir.Program` rewrite inserting `Dup`/`Drop` so that, on every
non-panic control-flow path, every owned reference is consumed exactly
once.

Ownership discipline (matching `runtime/zig/runtime.zig`):
* A function owns its parameters, and — for `closureFn`/`fieldFn` — its
  `self` closure object. Captures are borrowed reads out of `self`
  (`ReadCapture`) promoted to owned by a `Dup` if live; `self` is dropped
  as soon as it is dead.
* `MkStruct`/`MkClosure`/`MkRecord` move one reference per operand;
  `Force` moves one reference of the record; every operand of a consuming
  terminator moves into the call.
* `Prim` operands, `ReadPath`/`ReadCapture` sources and guard tests only
  borrow.

At every insertion point all `Dup`s precede any `Drop` of the same
statement position (garbage-free ordering). -/

namespace Malgo.Backend.Zig.Perceus

open Malgo.Sequent.Fun (Name)
open Malgo.Backend.Zig.Ir

instance : Inhabited Terminator := ⟨.panic ""⟩
instance : Inhabited Block := ⟨.mk [] (.panic "")⟩

/-- Group operands by name with multiplicity, ascending by name
(reproducing `Data.Map.toList`'s order so the emitted `Dup`s are in the
same order as the Haskell pass). -/
def countOps (ops : List Name) : List (Name × Nat) :=
  let m := ops.foldl (init := (∅ : Std.TreeMap Name Nat)) fun acc o =>
    acc.insert o ((acc.getD o 0) + 1)
  m.toList

mutual

/-- Insert RC operations into a block, given the set Δ of owned variables
in scope. Invariant: each Δ variable holds exactly one owned reference. -/
partial def insertBlock (delta : Std.TreeSet Name) : Block → Block
  | .mk stmts term =>
      let (stmts', term') := goStmts delta (suffixFreeVars stmts term) stmts term
      .mk stmts' term'

partial def goStmts (delta : Std.TreeSet Name) (lives : List (Std.TreeSet Name))
    (stmts : List Stmt) (term : Terminator) : List Stmt × Terminator :=
  match stmts, term with
  -- A bare panic block is RC-exempt (the process exits): no drops before it.
  | [], .panic msg => ([], .panic msg)
  | _, _ =>
      match lives with
      | live :: _ =>
          -- (D) Eagerly drop every owned variable dead from here on.
          let deadNow := (delta.filter (fun x => !live.contains x)).toList
          let (rest, term') := goLive (delta.filter (fun x => live.contains x)) lives stmts term
          (deadNow.map Stmt.drop ++ rest, term')
      | [] => panic! "Malgo.Backend.Zig.Perceus: liveSets shorter than stmts (invariant violation)"

partial def goLive (delta : Std.TreeSet Name) (lives : List (Std.TreeSet Name))
    (stmts : List Stmt) (term : Terminator) : List Stmt × Terminator :=
  match stmts with
  | [] => insertTerminator delta term
  | stmt :: rest =>
      match lives with
      | _ :: liveAfter :: lives' =>
          let lives'' := liveAfter :: lives'
          match stmt with
          | .dup _ => panic! "Malgo.Backend.Zig.Perceus: input already contains Dup"
          | .drop _ => panic! "Malgo.Backend.Zig.Perceus: input already contains Drop"
          | .dropReuse .. =>
              panic! "Malgo.Backend.Zig.Perceus: input already contains DropReuse (Reuse runs after Perceus)"
          | .let x e =>
              -- A borrowed read creates no reference: promote to owned with
              -- a Dup when live, elide entirely when dead.
              let borrowedLet : Unit → List Stmt × Terminator := fun _ =>
                if liveAfter.contains x then
                  let (rest', term') := goStmts (delta.insert x) lives'' rest term
                  (stmt :: Stmt.dup x :: rest', term')
                else goStmts delta lives'' rest term
              -- (Let) The expression moves one reference per operand
              -- occurrence; an operand still live afterwards keeps its own
              -- reference (needs one Dup per occurrence), else one fewer.
              let owningLet : List Name → List Stmt × Terminator := fun ops =>
                let counts := countOps ops
                let dupsFor := fun (yn : Name × Nat) =>
                  if !delta.contains yn.1 then
                    panic! "Malgo.Backend.Zig.Perceus: consuming an unowned variable"
                  else if liveAfter.contains yn.1 then List.replicate yn.2 (Stmt.dup yn.1)
                  else List.replicate (yn.2 - 1) (Stmt.dup yn.1)
                let consumedAway := counts.filterMap
                  (fun (y, _) => if !liveAfter.contains y then some y else none)
                let deltaMinus := delta.filter (fun z => !consumedAway.contains z)
                let delta' := deltaMinus.insert x
                let (rest', term') := goStmts delta' lives'' rest term
                ((counts.map dupsFor).flatten ++ (stmt :: rest'), term')
              match e with
              -- noreturn: the rest of the block is unreachable, leave it untouched.
              | .panicExpr _ => (stmt :: rest, term)
              | .readPath _ => borrowedLet ()
              | .readCapture _ _ => borrowedLet ()
              | .lit _ => owningLet []
              | .prim "reuseHint" ops => owningLet ops
              | .prim _ _ => owningLet []
              | .mkStruct _ ops => owningLet ops
              | .mkClosure _ ops => owningLet ops
              | .mkRecord _ ops => owningLet ops
              | .force v _ => owningLet [v]
              | .mkStructReuse .. =>
                  panic! "Malgo.Backend.Zig.Perceus: input already contains MkStructReuse (Reuse runs after Perceus)"
      | _ => panic! "Malgo.Backend.Zig.Perceus: liveSets shorter than stmts (invariant violation)"

partial def insertTerminator (delta : Std.TreeSet Name) : Terminator → List Stmt × Terminator
  -- (S) The guard only borrows; each branch drops (via goStmts's (D) rule)
  -- whatever is live only in the other branch.
  | .«if» guard t e => ([], .«if» guard (insertBlock delta t) (insertBlock delta e))
  | .panic msg => ([], .panic msg)
  -- (T) Consuming terminators: after the (D) rule, Δ is exactly the set of
  -- operands, each holding one reference; an operand occurring n times
  -- needs n − 1 extra references.
  | term =>
      let counts := countOps (termOperands term)
      let dups := (counts.map (fun (y, n) => List.replicate (n - 1) (Stmt.dup y))).flatten
      (dups, term)

end

partial def perceusFunc (fn : Func) : Func :=
  let delta0 :=
    let base := Std.TreeSet.ofList fn.params
    if fn.kind == .topLevelFn then base else base.insert fn.selfVar
  { fn with body := insertBlock delta0 fn.body }

def perceusProgram (program : Program) : Program :=
  { program with funcs := program.funcs.map perceusFunc }

private def nm (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }
private def r0 : Range := { start := SourcePos.initial "", stop := SourcePos.initial "" }
private def mkFn (params : List Name) (body : Block) : Func :=
  { range := r0, name := nm "f", kind := .topLevelFn, selfVar := nm "self", params, body }

-- `a` used twice in one struct gets exactly one `Dup a` (its own owned
-- reference moves into the last occurrence; the extra occurrence needs a dup).
#guard (perceusFunc (mkFn [nm "a"]
    (.mk [ .let (nm "y") (.mkStruct (.tag "P") [nm "a", nm "a"]) ] (.«return» (nm "y"))))).body ==
  Block.mk [ .dup (nm "a"), .let (nm "y") (.mkStruct (.tag "P") [nm "a", nm "a"]) ] (.«return» (nm "y"))

-- A parameter `b` never used again is eagerly dropped; the operand `a`
-- consumed by the struct needs no extra dup (its single reference moves).
#guard (perceusFunc (mkFn [nm "a", nm "b"]
    (.mk [ .let (nm "y") (.mkStruct (.tag "P") [nm "a"]) ] (.«return» (nm "y"))))).body ==
  Block.mk [ .drop (nm "b"), .let (nm "y") (.mkStruct (.tag "P") [nm "a"]) ] (.«return» (nm "y"))

-- A variable consumed by a (moving) terminator needs no extra drop.
#guard (perceusFunc (mkFn [nm "a"] (.mk [] (.«return» (nm "a"))))).body ==
  Block.mk [] (.«return» (nm "a"))

/-! ## Exact-placement checks (port of `PerceusSpec.hs`'s `placementSpec`)

The pass is pure, so these are `#guard`s. Corpus-wide linearity is covered by
the oracle in `lean/Test/Main.lean` (`ZigCorpus`), which runs the whole
pipeline over every testcase and hands the result to `RcCheck`; these pin the
individual insertion *rules* the oracle can only observe in aggregate. -/

private def pnm (s : String) (uniq : Nat) : Name :=
  { name := s, moduleName := .moduleName "PerceusTest", sort := .temporal uniq }

private def pA : Name := pnm "a" 10
private def pB : Name := pnm "b" 11
private def pK : Name := pnm "k" 12
private def pS : Name := pnm "s" 13
private def pH : Name := pnm "h" 14
private def pL : Name := pnm "l" 15
private def pF1 : Name := pnm "f1" 16
private def pF2 : Name := pnm "f2" 17
private def pC : Name := pnm "c" 18
private def pSelf : Name := pnm "self" 1

private def pFn (kind : FuncKind) (params : List Name) (body : Block) : Func :=
  { range := r0, name := pnm "fn" 0, kind, selfVar := pSelf, params, body }

private def pBody (kind : FuncKind) (params : List Name) (body : Block) : Block :=
  (perceusFunc (pFn kind params body)).body

-- An unused parameter is dropped at entry rather than leaked.
#guard pBody .topLevelFn [pA, pK] (.mk [.let pL (.lit (.int32 1))] (.applyCo pK pL))
  == .mk [.drop pA, .let pL (.lit (.int32 1))] (.applyCo pK pL)

-- Two occurrences in one construction: the variable's own reference moves
-- into the last, so exactly one dup covers the extra.
#guard pBody .topLevelFn [pA, pK] (.mk [.let pS (.mkStruct .tuple [pA, pA])] (.applyCo pK pS))
  == .mk [.dup pA, .let pS (.mkStruct .tuple [pA, pA])] (.applyCo pK pS)

-- Consumed by the struct AND still live at the terminator: one dup.
#guard pBody .topLevelFn [pA, pK] (.mk [.let pS (.mkStruct .tuple [pA])] (.callClosure pK [pA, pS]))
  == .mk [.dup pA, .let pS (.mkStruct .tuple [pA])] (.callClosure pK [pA, pS])

-- Each branch drops what only the other branch consumes.
#guard pBody .topLevelFn [pA, pB, pK]
    (.mk [] (.«if» (.isZero pA) (.mk [] (.applyCo pK pA)) (.mk [] (.applyCo pK pB))))
  == .mk [] (.«if» (.isZero pA)
      (.mk [.drop pB] (.applyCo pK pA)) (.mk [.drop pA] (.applyCo pK pB)))

-- A borrowed read that stays live is promoted to owned with a dup, and the
-- now-dead root is dropped.
#guard pBody .topLevelFn [pS, pK] (.mk [.let pH (.readPath (.field (.root pS) 0))] (.applyCo pK pH))
  == .mk [.let pH (.readPath (.field (.root pS) 0)), .dup pH, .drop pS] (.applyCo pK pH)

-- A borrowed read nobody uses is elided outright.
#guard pBody .topLevelFn [pS, pK] (.mk [.let pH (.readPath (.field (.root pS) 0))] (.applyCo pK pS))
  == .mk [] (.applyCo pK pS)

-- The closure protocol: dup the captures still needed, then drop self.
#guard pBody .closureFn [pK] (.mk [.let pC (.readCapture pSelf 0)] (.applyCo pK pC))
  == .mk [.let pC (.readCapture pSelf 0), .dup pC, .drop pSelf] (.applyCo pK pC)

-- Record fields are call-by-name, so forcing twice consumes two references.
#guard pBody .topLevelFn [pS, pK]
    (.mk [.let pF1 (.force pS "a"), .let pF2 (.force pS "b")] (.callClosure pK [pF1, pF2]))
  == .mk [.dup pS, .let pF1 (.force pS "a"), .let pF2 (.force pS "b")]
      (.callClosure pK [pF1, pF2])

-- Finish returns one value; everything else must be released first.
#guard pBody .topLevelFn [pA, pB] (.mk [] (.«return» pB)) == .mk [.drop pA] (.«return» pB)

-- A bare panic arm is RC-exempt, so nothing is inserted anywhere.
#guard pBody .topLevelFn [pA, pK]
    (.mk [] (.«if» (.isZero pA) (.mk [] (.applyCo pK pA)) (.mk [] (.panic "no matching branch"))))
  == .mk [] (.«if» (.isZero pA) (.mk [] (.applyCo pK pA)) (.mk [] (.panic "no matching branch")))

end Malgo.Backend.Zig.Perceus
