import Malgo.Backend.Zig.Ir
import Malgo.Backend.Zig.Stage
import Malgo.Prelude
import Malgo.Sequent.Fun
import Malgo.Monad
import Malgo.Id
import Malgo.Module

/-! Port of `src/Malgo/Backend/Zig/Reuse.hs`: Koka-style reuse-token
insertion (FBIP). Runs AFTER Perceus (needs its `Drop` placement) and
BEFORE `RcCheck`.

Within a single straight-line statement list, pairs the nearest preceding
`Drop` with a later `MkStruct`, LIFO, rewriting both to
`DropReuse`/`MkStructReuse`. A `Let hint (Prim "reuseHint" [x])`
immediately followed by `Drop hint` is recognized specially: both are
dropped and `x` (the matched, about-to-be-discarded scrutinee) — not the
immediately-dead `hint` — is offered up for pairing.

A `Drop` that finds no partner in its own list is **sunk into both arms** of
an `if` terminator rather than emitted before it (#354). An interpreter's
`eval` is almost entirely "match the node, then rebuild in each arm", so
the rebuild is systematically in a different block from the drop, and
pairing that never crossed a branch could not see it at all.

Sinking, rather than hoisting a `DropReuse` above the `if`, is what keeps
this cheap: the drop lands as an ordinary `Drop` at the head of each arm and
each arm's own pass pairs it locally, so the token is created and consumed
inside one statement list — the shape `RcCheck` already verifies. Hoisting
would instead need a new IR statement to release a token on whichever path
did not reach a `MkStructReuse`, plus a branch-linearity rule in `RcCheck`.

It is RC-neutral: the drop appears on every path exactly once, so no
reference is lost or double-released — which is also why a drop is sunk into
*both* arms or neither, never into one arm plus before the `if`. Perceus
only placed the drop here because the value is dead from this point on, so no
arm can read it.

The subtlety is *where* an unpaired sunk drop lands, and getting it wrong
makes this a pessimization rather than a no-op. See `reuseBlockWithSunk`:
releasing a container also releases its children, so a drop before the `if`
is what makes those children uniquely referenced in time for the arm's own
reconstruction to recycle one. Unpaired sunk drops therefore go to the arm's
*head*, preserving that release order, not appended after the arm's body.

Sinking is screened on an arm having a top-level `MkStruct`
(`rebuildsAtTopLevel`) purely to bound duplication: a lowered `Select` is
nested `if`s, so an unscreened sink would copy one statement into every leaf.

Effectful: `newTemporalId` needs the fresh-name supply (`getUniq`) and the
module name, so this pass runs in `MalgoM` (mirroring the Haskell
`State Uniq + Reader ModuleName`). -/

namespace Malgo.Backend.Zig.Reuse

open Malgo.Sequent.Fun (Name)
open Malgo.Backend.Zig.Ir

private def isDrop : Stmt → Bool
  | .drop _ => true
  | _ => false

/-- Pairs each `Drop` with the nearest LATER `MkStruct` in this same
statement list, LIFO (the most recently seen still-unpaired drop wins). A
`PanicExpr` is a barrier: any pending drop cannot reach a pairing partner
past it and is flushed unchanged. Unpaired drops (no later MkStruct at
all) are left as plain `Drop`s.

`pending`: drops seen so far, not yet paired, most-recent-first. -/
partial def pairGo (mn : ModuleName) (pending : List Name) : List Stmt → MalgoM (List Stmt × List Name)
  | [] => pure ([], pending)
  | .let x (.panicExpr what) :: rest => do
      let (rest', leftover) ← pairGo mn [] rest
      pure ((pending.reverse).map Stmt.drop ++ (.let x (.panicExpr what) :: rest'), leftover)
  -- ReuseSpecialize inserts `reuseHint scrutinee` right before a
  -- reconstruction so `scrutinee` (not the immediately-unused `hint`) is
  -- offered up for reuse. Perceus treats reuseHint as consuming
  -- `scrutinee`, so its reference is only ever released by THIS rewrite —
  -- failing to recognize the pattern would leak `scrutinee`. `hint` is
  -- always newly dead right after this Let, but not necessarily first in
  -- the contiguous run of eager drops (Set.toList's alphabetical order),
  -- so scan the whole run for `hint`; other drops found in the run are
  -- flushed immediately.
  | .let hint (.prim "reuseHint" [scrutinee]) :: rest =>
      let drops := rest.takeWhile isDrop
      let rest' := rest.dropWhile isDrop
      let dropNames := drops.filterMap (fun s => match s with | .drop x => some x | _ => none)
      if dropNames.contains hint then do
        let otherDrops := (dropNames.filter (· != hint)).map Stmt.drop
        let (tail, leftover) ← pairGo mn [scrutinee] rest'
        pure ((pending.reverse).map Stmt.drop ++ otherDrops ++ tail, leftover)
      else do
        let (tail, leftover) ← pairGo mn pending rest
        pure (.let hint (.prim "reuseHint" [scrutinee]) :: tail, leftover)
  | .drop x :: rest => pairGo mn (x :: pending) rest
  | .let x (.mkStruct tag ops) :: rest =>
      match pending with
      | dropped :: restPending => do
          let tok ← newTemporalId mn "reuse"
          let (rest', leftover) ← pairGo mn restPending rest
          pure (.dropReuse tok dropped ops.length :: .let x (.mkStructReuse tok tag ops) :: rest', leftover)
      | [] => do
          let (tail, leftover) ← pairGo mn pending rest
          pure (.let x (.mkStruct tag ops) :: tail, leftover)
  | stmt :: rest => do
      let (tail, leftover) ← pairGo mn pending rest
      pure (stmt :: tail, leftover)

/-- Whether this block's own statement list rebuilds anything, i.e. whether
offering it a sunk `Drop` has any chance of paying off. Only a cheap
syntactic screen: it bounds how far a drop is duplicated, since a lowered
`Select` is nested `if`s and an unconditional sink would copy one statement
into every leaf. -/
private def rebuildsAtTopLevel : Block → Bool
  | .mk stmts _ => stmts.any fun
      | .let _ (.mkStruct _ _) => true
      | _ => false

/-- Pairs a block, optionally with drops sunk in from an enclosing `if`
(#354).

`sunk` drops are offered to this block's pairing as if they had been written
at its head. Whichever of them still fail to pair are emitted at the head --
**not** appended with this block's own leftovers, which is the whole reason
this is a separate function.

That placement is what makes sinking safe to do speculatively. Releasing a
container also releases its children, so a drop that happens before the `if`
is what makes those children uniquely referenced in time for the arm's own
reconstruction to recycle one. Moving such a drop to the *end* of an arm
leaves them at refcount 2 past that point and converts hits into misses:
measured on `fib`, an earlier version that appended unpaired sunk drops cost
986 pairings on the shallow tier and 121,392 on the deep one, exactly
matching the rise in `total_allocs`. Emitting at the head keeps the original
release order (only the branch test intervenes, and a guard merely borrows),
so a sink either pairs and wins, or is a no-op. -/
partial def reuseBlockWithSunk (mn : ModuleName) (sunk : List Name) : Block → MalgoM Block
  | .mk stmts terminator => do
      let (stmts', leftover) ← pairGo mn [] ((sunk.reverse).map Stmt.drop ++ stmts)
      let sunkUnpaired := leftover.filter (fun n => sunk.contains n)
      let ownUnpaired := leftover.filter (fun n => !sunk.contains n)
      let head := (sunkUnpaired.reverse).map Stmt.drop
      match terminator with
      | .«if» guard t e =>
          -- A drop must happen exactly once on every path, so it is sunk into
          -- both arms or neither -- never into one arm plus before the `if`.
          if !ownUnpaired.isEmpty && (rebuildsAtTopLevel t || rebuildsAtTopLevel e) then do
            let t' ← reuseBlockWithSunk mn ownUnpaired t
            let e' ← reuseBlockWithSunk mn ownUnpaired e
            pure (.mk (head ++ stmts') (.«if» guard t' e'))
          else do
            let t' ← reuseBlockWithSunk mn [] t
            let e' ← reuseBlockWithSunk mn [] e
            pure (.mk (head ++ stmts' ++ (ownUnpaired.reverse).map Stmt.drop) (.«if» guard t' e'))
      | term => pure (.mk (head ++ stmts' ++ (ownUnpaired.reverse).map Stmt.drop) term)

def reuseBlock (mn : ModuleName) : Block → MalgoM Block :=
  reuseBlockWithSunk mn []

def reuseFunc (mn : ModuleName) (fn : Func) : MalgoM Func := do
  let body ← reuseBlock mn fn.body
  pure { fn with body }

def reuseProgram (mn : ModuleName) (staged : Staged .perceus) : MalgoM (Staged .reuse) := do
  let program := staged.program
  let funcs ← program.funcs.mapM (reuseFunc mn)
  pure ⟨{ program with funcs }⟩

end Malgo.Backend.Zig.Reuse
