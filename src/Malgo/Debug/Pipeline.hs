-- | Drives a single @.mlg@ file through the whole compilation pipeline —
-- Parse through the Zig backend's Reuse pass — capturing every stage's
-- Malgo-ish rendered text. Used by the MET (M-exp-Tracer) debug tool
-- ("app/met") and by its golden tests.
--
-- Mirrors the exact pass sequence 'Malgo.Query.Engine' 's @LinkedProgram@
-- handler and 'Malgo.Backend.Zig' 's @ZigPass@ run in production, just
-- without discarding the intermediate values. Only the target module's own
-- definitions are traced (no Builtin\/Prelude linking): linking in every
-- dependency's definitions would bury the interesting diff in Prelude
-- noise, and each pass rewrites definitions independently anyway.
module Malgo.Debug.Pipeline
  ( Stage (..),
    runTrace,
  )
where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Effectful.Reader.Static (runReader)
import Malgo.Backend.Zig.ClosureConv (convertProgram)
import Malgo.Backend.Zig.Peephole (peepholeProgram)
import Malgo.Backend.Zig.Perceus (perceusProgram)
import Malgo.Backend.Zig.RcCheck (checkProgram)
import Malgo.Backend.Zig.Reuse (reuseProgram)
import Malgo.Debug.PrettyIR
import Malgo.Elaborate (ElaboratePass (..))
import Malgo.Features (Feature (..), FeatureFlags (..))
import Malgo.Infer (InferPass (..), TyEnv)
import Malgo.Module
import Malgo.Monad
import Malgo.Parser (ParserPass (..))
import Malgo.Pass (runCompileError, runPass)
import Malgo.Prelude
import Malgo.Query.Database (newDatabase)
import Malgo.Query.Engine (runQueryDB)
import Malgo.Rename
import Malgo.Sequent.Core.Flat (flatProgram)
import Malgo.Sequent.Core.Join (joinProgram)
import Malgo.Sequent.ReuseSpecialize (specializeProgram)
import Malgo.Sequent.SaturateCtor (saturateProgram)
import Malgo.Sequent.ToCore (toCore)
import Malgo.Sequent.ToFun (ToFunPass (..))
import Malgo.Syntax (Module (..))

-- | One pipeline stage: its label and its Malgo-ish rendered text.
data Stage = Stage {name :: Text, rendered :: Text}

-- | Run @srcPath@ through the pipeline, rendering every stage.
-- @useInfer@\/@malgo2025@ mirror the @malgo eval@ flags of the same name.
runTrace :: FilePath -> Bool -> Bool -> IO [Stage]
runTrace srcPath useInfer malgo2025 = do
  src <- BS.readFile srcPath
  db <- newDatabase
  runMalgoMWith flag (FeatureFlags features) $ runCompileError $ runQueryDB db do
    srcModulePath <- parseArtifactPathFromPwd srcPath
    save srcModulePath ".mlg" src
    parsed <- runPass ParserPass (srcPath, convertString src)
    rnEnv <- genBuiltinRnEnv
    (Module modName renamedBindGroup, _) <- runPass RenamePass (parsed, rnEnv)
    runReader modName do
      elaborated <-
        if malgo2025
          then runPass ElaboratePass renamedBindGroup
          else pure renamedBindGroup
      bindGroup <-
        if useInfer
          then fst <$> runPass InferPass (Map.empty :: TyEnv, elaborated)
          else pure elaborated
      fun <- runPass ToFunPass bindGroup
      -- Saturated-constructor inlining and reuse-hint insertion now run
      -- first thing inside toCore (shared by every backend), not as
      -- Join-IR-local Zig passes; traced here at the Fun IR level, where
      -- the rewrites actually happen. `toCore fun` reapplies both
      -- internally (idempotent, cheap), so `core` reflects the real
      -- post-specialization pipeline regardless.
      let saturatedFun = saturateProgram fun
      specializedFun <- specializeProgram saturatedFun
      core <- toCore fun
      flat <- flatProgram core
      joined <- joinProgram flat
      ir <- convertProgram joined
      let peepholed = peepholeProgram ir
          perceused = perceusProgram peepholed
      reused <- reuseProgram perceused
      let reuseNote = case checkProgram reused of
            Right () -> ""
            Left violations ->
              "-- RC CHECK VIOLATIONS:\n"
                <> foldMap (\v -> "--   " <> convertString (show v) <> "\n") violations
      pure
        $ [ Stage "Parse" (renderParsedModule parsed),
            Stage "Rename" (renderBindGroup renamedBindGroup)
          ]
        <> [Stage "Elaborate" (renderBindGroup elaborated) | malgo2025]
        <> [ Stage "ToFun" (renderFun fun),
             Stage "SaturateCtor" (renderFun saturatedFun),
             Stage "ReuseSpecialize" (renderFun specializedFun),
             Stage "ToCore" (renderCoreFull core),
             Stage "Flat" (renderFlat flat),
             Stage "Join" (renderJoin joined),
             Stage "ClosureConv" (renderZigIr ir),
             Stage "Peephole" (renderZigIr peepholed),
             Stage "Perceus" (renderZigIr perceused),
             Stage "Reuse" (reuseNote <> renderZigIr reused)
           ]
  where
    flag =
      Flag
        { noOptimize = False,
          lambdaLift = False,
          debugMode = False,
          testMode = True,
          target = TargetEval,
          evalMode = EvalSmallStep,
          useInfer,
          programArgs = []
        }
    features = if malgo2025 then Set.singleton Malgo2025 else Set.empty
