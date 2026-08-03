import Malgo.Backend.Zig.Ir

/-! The Zig backend's post-Join pipeline runs ClosureConv → Peephole →
Perceus → Reuse → RcCheck/Emit, all over the same `Ir.Program`/`Func`/
`Block`/`Stmt` representation. Before this file, nothing but the order of
five function calls in `Backend/Zig.lean` enforced that sequence: a pass
run on the wrong stage's input either hit a partial, inconsistent set of
`panic!` guards (`Peephole`/`Perceus`) or, worse, produced a well-formed
`Ir.Program` with silently corrupted reference counts (`Peephole` running
after `Perceus` would rewire reads around a `MkStruct` whose RC accounting
Perceus already computed, with nothing about the resulting AST looking
wrong).

`Staged` is a phantom-tagged wrapper around `Ir.Program`, not a
re-indexing of `Ir.Stmt`/`Ir.Expr`'s own constructors (that would force
duplicating each stage's constructor list, the same tripling this
pipeline's Full/Flat/Join IRs already pay elsewhere). Each pass's
top-level `*Program` entry point unwraps its required stage on the way in
and rewraps the stage it produces on the way out; every internal
recursive helper is untouched, still operating on plain `Func`/`Block`. -/

namespace Malgo.Backend.Zig

inductive ZigStage where
  | closureConv
  | peephole
  | perceus
  | reuse
  deriving BEq, Repr

structure Staged (s : ZigStage) where
  program : Ir.Program

instance : BEq (Staged s) := ⟨fun a b => a.program == b.program⟩

end Malgo.Backend.Zig
