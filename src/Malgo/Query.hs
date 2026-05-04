module Malgo.Query
  ( Query (..),
    QueryDB (..),
    fetch,
    loadInterface,
    updateSource,
    invalidateModule,
  )
where

import Data.Text.Lazy qualified as TL
import Effectful
import Effectful.Dispatch.Dynamic
import Malgo.Infer (TyEnv)
import Malgo.Interface (Interface)
import Malgo.Module
import Malgo.Prelude
import Malgo.Rename.RnState (RnState)
import Malgo.Sequent.Core.Join qualified as Join
import Malgo.Syntax
import Malgo.Syntax.Extension

-- | GADT representing all compiler queries.
data Query a where
  ParsedModule :: ModuleName -> Query (Module (Malgo Parse))
  RenamedModule :: ModuleName -> Query (Module (Malgo Rename), RnState)
  -- | Type environment exported by a module after type inference.
  -- The returned 'TyEnv' contains entries for explicit signatures, foreign
  -- imports, data constructors, and inferred bare 'def' bindings — i.e. all
  -- names this module contributes — but excludes entries inherited from its
  -- own dependencies.
  InferredModule :: ModuleName -> Query TyEnv
  LinkedProgram :: ModuleName -> Query Join.Program
  ModuleInterface :: ModuleName -> Query Interface

-- | QueryDB effect for demand-driven compilation.
data QueryDB :: Effect where
  Fetch :: Query a -> QueryDB m a
  UpdateSource :: ModuleName -> FilePath -> TL.Text -> QueryDB m ()
  InvalidateModule :: ModuleName -> QueryDB m ()

type instance DispatchOf QueryDB = Dynamic

-- | Fetch a query result, computing and caching it on first access.
fetch :: (QueryDB :> es) => Query a -> Eff es a
fetch query = send (Fetch query)

-- | Fetch the 'Interface' for a module via the query cache.
-- Must be called inside 'runQueryDB'.
loadInterface :: (HasCallStack) => (QueryDB :> es) => ModuleName -> Eff es Interface
loadInterface modName = fetch (ModuleInterface modName)

-- | Register an in-memory source for a module (used by LSP).
updateSource :: (QueryDB :> es) => ModuleName -> FilePath -> TL.Text -> Eff es ()
updateSource modName srcPath text = send (UpdateSource modName srcPath text)

-- | Invalidate all cached results for a module (used by LSP on file change).
invalidateModule :: (QueryDB :> es) => ModuleName -> Eff es ()
invalidateModule modName = send (InvalidateModule modName)
