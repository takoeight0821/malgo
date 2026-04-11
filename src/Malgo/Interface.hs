{-# LANGUAGE UndecidableInstances #-}

module Malgo.Interface
  ( Interface (..),
    buildInterface,
    loadInterfaceFromDisk,
    externalFromInterface,
    exportedIdentList,
    exportedTypeIdentList,
  )
where

import Data.Binary (Binary)
import Data.Map.Strict qualified as Map
import Effectful (Eff, IOE, (:>))
import GHC.Records (HasField)
import Malgo.Id
import Malgo.Module
import Malgo.Prelude
import Malgo.Syntax.Extension
import Prettyprinter (viaShow)

data Interface = Interface
  { moduleName :: ModuleName,
    -- | Used in Rename
    infixInfo :: Map PsId (Assoc, Int),
    -- | Used in Rename
    dependencies :: Set ModuleName,
    -- | Used in Rename - List of exported identifiers collected from Rename pass
    exportedIdentList :: [PsId],
    -- | Used in Rename - List of exported type identifiers collected from Rename pass
    exportedTypeIdentList :: [PsId]
  }
  deriving stock (Show, Generic)

instance Binary Interface

instance Pretty Interface where
  pretty = viaShow

externalFromInterface :: Interface -> PsId -> RnId
externalFromInterface Interface {moduleName} psId =
  Id {name = psId, sort = External, moduleName}

buildInterface ::
  ( HasField "dependencies" rnState (Set ModuleName),
    HasField "infixInfo" rnState (Map Id (Assoc, Int)),
    HasField "exportedIdentifiers" rnState [PsId],
    HasField "exportedTypeIdentifiers" rnState [PsId]
  ) =>
  ModuleName ->
  rnState ->
  Interface
buildInterface moduleName rnState =
  Interface
    { moduleName,
      infixInfo = Map.mapKeys (\id -> id.name) rnState.infixInfo,
      dependencies = rnState.dependencies,
      exportedIdentList = rnState.exportedIdentifiers,
      exportedTypeIdentList = rnState.exportedTypeIdentifiers
    }

exportedIdentList :: (HasField "exportedIdentList" r [k]) => r -> [k]
exportedIdentList inf = inf.exportedIdentList

exportedTypeIdentList ::
  (HasField "exportedTypeIdentList" inf [k]) =>
  inf ->
  [k]
exportedTypeIdentList inf = inf.exportedTypeIdentList

-- | Load the interface for a module directly from disk, bypassing any query cache.
-- Use 'loadInterface' (from 'Malgo.Query.Engine') in code that runs inside 'runQueryDB'.
loadInterfaceFromDisk ::
  (HasCallStack) =>
  (IOE :> es, Workspace :> es) =>
  ModuleName ->
  Eff es Interface
loadInterfaceFromDisk modName = do
  modPath <- getModulePath modName
  ViaBinary interface <- load modPath ".mlgi"
  pure interface
