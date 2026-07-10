-- | ZigPass translates Join IR to Zig source code, mirroring
-- 'Malgo.Backend.Scheme.SchemePass'\'s shape exactly.
module Malgo.Backend.Zig
  ( ZigPass (..),
    ZigStages (..),
    runZigStages,
  )
where

import Control.Exception (Exception (..))
import Effectful
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Malgo.Backend.Zig.ClosureConv (convertProgram)
import Malgo.Backend.Zig.Emit (emitProgram)
import Malgo.Backend.Zig.Ir qualified as Ir
import Malgo.Backend.Zig.Peephole (peepholeProgram)
import Malgo.Backend.Zig.Perceus (perceusProgram)
import Malgo.Backend.Zig.RcCheck (checkProgram)
import Malgo.Backend.Zig.Reuse (reuseProgram)
import Malgo.Module (ModuleName)
import Malgo.Pass
import Malgo.Prelude
import Malgo.Sequent.Core.Join qualified as Join

data ZigPass = ZigPass

instance Pass ZigPass where
  type Input ZigPass = Join.Program
  type Output ZigPass = Text
  type ErrorType ZigPass = ZigError
  type Effects ZigPass es = (Reader ModuleName :> es, State Uniq :> es)

  runPassImpl _ program = do
    stages <- runZigStages program
    -- The linearity check is pure and fast relative to the rest of the
    -- pipeline; running it unconditionally turns any Perceus bug into a
    -- compile-time error instead of a use-after-free in the produced
    -- binary. It also verifies reuse-token linearity, so a Reuse bug is
    -- caught the same way.
    case checkProgram stages.reuse of
      Right () -> emitProgram stages.reuse
      Left violations ->
        throwError (ZigError $ "Perceus produced a non-linear program: " <> convertString (show violations))

data ZigError = ZigError Text
  deriving stock (Show)

instance Exception ZigError where
  displayException (ZigError msg) = "Zig backend error: " <> convertString msg

-- | Every intermediate stage of the Zig backend's post-Join lowering
-- pipeline, in this fixed order. A single definition shared by production
-- ('ZigPass' above), the MET debug tracer ('Malgo.Debug.Pipeline'), and the
-- golden-corpus linearity test ('Malgo.Backend.Zig.PerceusSpec') so the
-- three can never drift out of sync.
data ZigStages = ZigStages
  { closureConv :: Ir.Program,
    peephole :: Ir.Program,
    perceus :: Ir.Program,
    reuse :: Ir.Program
  }

runZigStages :: (State Uniq :> es, Reader ModuleName :> es) => Join.Program -> Eff es ZigStages
runZigStages program = do
  ir <- convertProgram program
  let peepholed = peepholeProgram ir
      perceused = perceusProgram peepholed
  reused <- reuseProgram perceused
  pure ZigStages {closureConv = ir, peephole = peepholed, perceus = perceused, reuse = reused}
