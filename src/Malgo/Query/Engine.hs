module Malgo.Query.Engine
  ( runQueryDB,
  )
where

import Control.Exception (SomeException, try)
import Data.Binary (Binary, decode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy qualified as TL
import Data.Traversable (for)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.State.Static.Local (State)
import Malgo.Elaborate (ElaboratePass (..))
import Malgo.Features
import Malgo.Infer (InferPass (..))
import Malgo.Interface
import Malgo.Module
import Malgo.Parser (ParserPass (..))
import Malgo.Pass (CompileError, Pass (..))
import Malgo.Path (replaceExtension, toFilePath)
import Malgo.Prelude
import Malgo.Query
import Malgo.Query.Database
import Malgo.Rename
import Malgo.Sequent.Core.Flat (FlatPass (..))
import Malgo.Sequent.Core.Join (JoinPass (..))
import Malgo.Sequent.Core.Join qualified as Join
import Malgo.Sequent.ToCore (ToCorePass (..))
import Malgo.Sequent.ToFun (ToFunPass (..))
import Malgo.Syntax qualified as Syntax
import System.Directory (doesFileExist)

-- | Run the QueryDB effect using a 'Database' for caching.
-- The caller is responsible for providing all required effects in the stack.
runQueryDB ::
  ( Reader Flag :> es,
    State Uniq :> es,
    IOE :> es,
    Workspace :> es,
    Features :> es,
    Error CompileError :> es
  ) =>
  Database ->
  Eff (QueryDB : es) a ->
  Eff es a
runQueryDB db = interpret_ \case
  Fetch query -> handleFetch db query
  UpdateSource modName srcPath text ->
    liftIO $ modifyIORef db.sourceMap $ Map.insert modName (srcPath, text)
  InvalidateModule modName -> liftIO do
    modifyIORef db.cacheParsedModule $ Map.delete modName
    modifyIORef db.cacheRenamedModule $ Map.delete modName
    modifyIORef db.cacheLinkedProgram $ Map.delete modName
    modifyIORef db.cacheModuleInterface $ Map.delete modName

-- | Core query handler; called recursively for sub-queries.
handleFetch ::
  ( Reader Flag :> es,
    State Uniq :> es,
    IOE :> es,
    Workspace :> es,
    Features :> es,
    Error CompileError :> es
  ) =>
  Database ->
  Query a ->
  Eff es a
handleFetch db = \case
  ParsedModule modName -> do
    cache <- liftIO $ readIORef db.cacheParsedModule
    case Map.lookup modName cache of
      Just result -> pure result
      Nothing -> do
        (srcPath, text) <- fetchSource db modName
        result <- runPass ParserPass (srcPath, text)
        liftIO $ modifyIORef db.cacheParsedModule $ Map.insert modName result
        pure result
  RenamedModule modName -> do
    cache <- liftIO $ readIORef db.cacheRenamedModule
    case Map.lookup modName cache of
      Just result -> pure result
      Nothing -> do
        parsedAst <- handleFetch db (ParsedModule modName)
        rnEnv <- genBuiltinRnEnv
        -- Re-inject QueryDB so RenamePass can call loadInterface for imports.
        result@(_, rnState) <- runQueryDB db $ runPass RenamePass (parsedAst, rnEnv)
        liftIO $ modifyIORef db.cacheRenamedModule $ Map.insert modName result
        -- Build and persist the interface (best-effort: skip save if path is unavailable,
        -- e.g. for in-memory LSP sources whose paths lie outside the workspace root).
        let inf = buildInterface modName rnState
        liftIO $ modifyIORef db.cacheModuleInterface $ Map.insert modName inf
        mSrcPath <- tryGetModulePath modName
        case mSrcPath of
          Just srcPath -> save srcPath ".mlgi" (ViaBinary inf)
          Nothing -> pure ()
        pure result
  ModuleInterface modName -> do
    cache <- liftIO $ readIORef db.cacheModuleInterface
    case Map.lookup modName cache of
      Just result -> pure result
      Nothing -> do
        -- Try loading from a pre-built .mlgi file first.
        mInf <- tryLoadInterfaceFromDisk modName
        case mInf of
          Just inf -> do
            liftIO $ modifyIORef db.cacheModuleInterface $ Map.insert modName inf
            pure inf
          Nothing -> do
            -- No pre-built artifact: compile from scratch.
            (_, rnState) <- handleFetch db (RenamedModule modName)
            pure $ buildInterface modName rnState
  LinkedProgram modName -> do
    cache <- liftIO $ readIORef db.cacheLinkedProgram
    case Map.lookup modName cache of
      Just result -> pure result
      Nothing -> do
        (renamedAst, rnState) <- handleFetch db (RenamedModule modName)
        srcPath <- getModulePath modName
        flags <- ask @Flag
        malgo2025 <- isMalgo2025Enabled
        program <- runReader modName do
          bindGroup <-
            if malgo2025
              then runPass ElaboratePass renamedAst.moduleDefinition
              else pure renamedAst.moduleDefinition
          bindGroup' <-
            if flags.useInfer
              then runPass InferPass bindGroup
              else pure bindGroup
          runPass ToFunPass bindGroup'
            >>= runPass ToCorePass
            >>= runPass FlatPass
            >>= runPass JoinPass
        save srcPath ".sqt" (ViaBinary program)
        linked <- linkDeps rnState.dependencies program
        liftIO $ modifyIORef db.cacheLinkedProgram $ Map.insert modName linked
        pure linked

-- | Read source text for a module: from in-memory registry first, then disk.
fetchSource :: (IOE :> es, Workspace :> es) => Database -> ModuleName -> Eff es (FilePath, TL.Text)
fetchSource db modName = do
  srcMap <- liftIO $ readIORef db.sourceMap
  case Map.lookup modName srcMap of
    Just result -> pure result
    Nothing -> do
      modPath <- getModulePath modName
      content <- liftIO $ BS.readFile $ toFilePath modPath.originPath
      pure (modPath.rawPath, convertString content)

-- | Try to load a pre-built '.mlgi' interface from disk.  Returns 'Nothing' when
-- the artifact does not yet exist so the caller can fall back to compilation.
tryLoadInterfaceFromDisk :: (IOE :> es, Workspace :> es) => ModuleName -> Eff es (Maybe Interface)
tryLoadInterfaceFromDisk modName = do
  mPath <- tryGetModulePath modName
  case mPath of
    Nothing -> pure Nothing
    Just modPath -> do
      targetPath <- replaceExtension ".mlgi" modPath.targetPath
      exists <- liftIO $ doesFileExist $ toFilePath targetPath
      if exists
        then do
          content <- liftIO $ BS.readFile $ toFilePath targetPath
          pure $ Just $ decode $ BSL.fromStrict content
        else pure Nothing

-- | Try to get the ArtifactPath for a module, returning 'Nothing' if not found.
tryGetModulePath :: (IOE :> es) => ModuleName -> Eff es (Maybe ArtifactPath)
tryGetModulePath modName = do
  result <- liftIO $ try @SomeException $ do
    runEff $ runWorkspaceOnPwd do
      getModulePath modName
  pure $ either (const Nothing) Just result

-- | Load dependency programs from disk and merge into a single linked program.
linkDeps :: (Workspace :> es, IOE :> es) => Set ModuleName -> Join.Program -> Eff es Join.Program
linkDeps dependencies program = do
  deps <- for (Set.toList dependencies) \dep -> do
    path <- getModulePath dep
    ViaBinary x <- load path ".sqt"
    pure x
  pure
    $ Join.Program
      { definitions =
          program.definitions
            <> concatMap (\Join.Program {definitions} -> definitions) deps,
        dependencies = []
      }
