import Malgo.Monad
import Malgo.Module
import Malgo.Sequent.Core.Join
import Malgo.Backend.Zig.Ir
import Malgo.Backend.Zig.Stage
import Malgo.Backend.Zig.ClosureConv
import Malgo.Backend.Zig.Peephole
import Malgo.Backend.Zig.Perceus
import Malgo.Backend.Zig.Reuse
import Malgo.Backend.Zig.RcCheck
import Malgo.Backend.Zig.Emit

/-! Port of `src/Malgo/Backend/Zig.hs`: the top-level orchestration of the Zig
backend's post-Join lowering pipeline.

Haskell threads the stages through the `Pass`/`Effects` type-class machinery
(`ZigPass`); this codebase has no `Pass` typeclass (see `Malgo.Pass.wrapError`),
so `runZigStages` is a plain `MalgoM` function and the `ZigPass.runPassImpl`
logic becomes `compileToZigText`: run the stages, run the linearity check
(`RcCheck.checkProgram`), and on success emit the final Zig text; a non-linear
result becomes a `CompileError` instead of a use-after-free in the binary. -/

namespace Malgo.Backend.Zig

open Malgo

/-- Every intermediate stage of the Zig backend's post-Join lowering pipeline,
in this fixed order (mirrors Haskell `ZigStages`). Kept as a record so a future
debug tracer / linearity-corpus test can inspect each stage without re-running
the pipeline. -/
structure ZigStages where
  closureConv : Staged .closureConv
  peephole : Staged .peephole
  perceus : Staged .perceus
  reuse : Staged .reuse

def runZigStages (mn : ModuleName) (program : Malgo.Sequent.Core.Join.Program) : MalgoM ZigStages := do
  let ir ← ClosureConv.convertProgram mn program
  let peepholed := Peephole.peepholeProgram ir
  let perceused := Perceus.perceusProgram peepholed
  let reused ← Reuse.reuseProgram mn perceused
  pure { closureConv := ir, peephole := peepholed, perceus := perceused, reuse := reused }

/-- Run the whole Zig lowering pipeline and produce the final Zig source text.
Mirrors `ZigPass.runPassImpl`: the linearity check is pure and fast relative to
the rest of the pipeline, so running it unconditionally turns any Perceus/Reuse
bug into a compile-time error instead of a use-after-free in the produced
binary. -/
def compileToZigText (mn : ModuleName) (program : Malgo.Sequent.Core.Join.Program) : MalgoM String := do
  let stages ← runZigStages mn program
  match RcCheck.checkProgram stages.reuse with
  | .ok () => pure (Emit.emitProgram mn stages.reuse)
  | .error violations =>
    throw { passName := "Zig",
            message := s!"Perceus produced a non-linear program: {repr violations}" }

end Malgo.Backend.Zig
