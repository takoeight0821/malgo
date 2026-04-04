module Malgo.Query.Database
  ( Database (..),
    newDatabase,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as TL
import Malgo.Interface (Interface)
import Malgo.Module
import Malgo.Prelude
import Malgo.Rename.RnState (RnState)
import Malgo.Sequent.Core.Join qualified as Join
import Malgo.Syntax
import Malgo.Syntax.Extension

-- | Per-query-kind IORef caches and in-memory source registry.
data Database = Database
  { cacheParsedModule :: IORef (Map ModuleName (Module (Malgo Parse))),
    cacheRenamedModule :: IORef (Map ModuleName (Module (Malgo Rename), RnState)),
    cacheLinkedProgram :: IORef (Map ModuleName Join.Program),
    cacheModuleInterface :: IORef (Map ModuleName Interface),
    -- | In-memory source registry; populated by 'updateSource' (e.g. from LSP).
    sourceMap :: IORef (Map ModuleName (FilePath, TL.Text))
  }

-- | Create a fresh empty Database.
newDatabase :: IO Database
newDatabase = do
  cacheParsedModule <- newIORef Map.empty
  cacheRenamedModule <- newIORef Map.empty
  cacheLinkedProgram <- newIORef Map.empty
  cacheModuleInterface <- newIORef Map.empty
  sourceMap <- newIORef Map.empty
  pure Database {..}
