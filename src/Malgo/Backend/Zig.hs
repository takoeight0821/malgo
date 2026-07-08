-- | ZigPass translates Join IR to Zig source code, mirroring
-- 'Malgo.Backend.Scheme.SchemePass'\'s shape exactly.
module Malgo.Backend.Zig
  ( ZigPass (..),
  )
where

import Control.Exception (Exception (..))
import Effectful
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Malgo.Backend.Zig.ClosureConv (convertProgram)
import Malgo.Backend.Zig.Emit (emitProgram)
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
    ir <- perceusProgram . peepholeProgram <$> convertProgram program
    ir <- reuseProgram ir
    -- The linearity check is pure and fast relative to the rest of the
    -- pipeline; running it unconditionally turns any Perceus bug into a
    -- compile-time error instead of a use-after-free in the produced
    -- binary. It also verifies reuse-token linearity, so a Reuse bug is
    -- caught the same way.
    case checkProgram ir of
      Right () -> emitProgram ir
      Left violations ->
        throwError (ZigError $ "Perceus produced a non-linear program: " <> convertString (show violations))

data ZigError = ZigError Text
  deriving stock (Show)

instance Exception ZigError where
  displayException (ZigError msg) = "Zig backend error: " <> convertString msg
