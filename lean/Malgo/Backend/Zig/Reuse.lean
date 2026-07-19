import Malgo.Backend.Zig.Ir
import Malgo.Prelude
import Malgo.Sequent.Fun
import Malgo.Monad
import Malgo.Id
import Malgo.Module

/-! Port of `src/Malgo/Backend/Zig/Reuse.hs`: Koka-style reuse-token
insertion (FBIP). Runs AFTER Perceus (needs its `Drop` placement) and
BEFORE `RcCheck`.

Within a single straight-line statement list (never crossing a `TIf`
branch), pairs the nearest preceding `Drop` with a later `MkStruct`, LIFO,
rewriting both to `DropReuse`/`MkStructReuse`. A `Let hint (Prim
"reuseHint" [x])` immediately followed by `Drop hint` is recognized
specially: both are dropped and `x` (the matched, about-to-be-discarded
scrutinee) — not the immediately-dead `hint` — is offered up for pairing.

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
partial def pairGo (mn : ModuleName) (pending : List Name) : List Stmt → MalgoM (List Stmt)
  | [] => pure ((pending.reverse).map Stmt.drop)
  | .let x (.panicExpr what) :: rest => do
      let rest' ← pairGo mn [] rest
      pure ((pending.reverse).map Stmt.drop ++ (.let x (.panicExpr what) :: rest'))
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
        let tail ← pairGo mn [scrutinee] rest'
        pure ((pending.reverse).map Stmt.drop ++ otherDrops ++ tail)
      else do
        let tail ← pairGo mn pending rest
        pure (.let hint (.prim "reuseHint" [scrutinee]) :: tail)
  | .drop x :: rest => pairGo mn (x :: pending) rest
  | .let x (.mkStruct tag ops) :: rest =>
      match pending with
      | dropped :: restPending => do
          let tok ← newTemporalId mn "reuse"
          let rest' ← pairGo mn restPending rest
          pure (.dropReuse tok dropped ops.length :: .let x (.mkStructReuse tok tag ops) :: rest')
      | [] => do
          let tail ← pairGo mn pending rest
          pure (.let x (.mkStruct tag ops) :: tail)
  | stmt :: rest => do
      let tail ← pairGo mn pending rest
      pure (stmt :: tail)

def pairStmts (mn : ModuleName) (stmts : List Stmt) : MalgoM (List Stmt) :=
  pairGo mn [] stmts

partial def reuseBlock (mn : ModuleName) : Block → MalgoM Block
  | .mk stmts terminator => do
      let stmts' ← pairStmts mn stmts
      let terminator' ← match terminator with
        | .«if» guard t e => do
            let t' ← reuseBlock mn t
            let e' ← reuseBlock mn e
            pure (.«if» guard t' e')
        | term => pure term
      pure (.mk stmts' terminator')

def reuseFunc (mn : ModuleName) (fn : Func) : MalgoM Func := do
  let body ← reuseBlock mn fn.body
  pure { fn with body }

def reuseProgram (mn : ModuleName) (program : Program) : MalgoM Program := do
  let funcs ← program.funcs.mapM (reuseFunc mn)
  pure { program with funcs }

end Malgo.Backend.Zig.Reuse
