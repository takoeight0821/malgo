module Malgo.Query.Engine
  ( runQueryDB,

    -- * Test-only helpers
    reverseDepClosure,
  )
where

import Control.Exception (try)
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
import Malgo.Infer (InferPass (..), TyEnv)
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
  InvalidateModule modName -> liftIO $ invalidateWithRdeps db modName

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
        -- Use the parsed module's own moduleName so that External Ids in the
        -- interface match Ids generated when the module is compiled directly,
        -- regardless of which alias (e.g. ModuleName "Builtin" vs Artifact path)
        -- was used as the query key.
        let inf = buildInterface parsedAst.moduleName rnState
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
  InferredModule modName -> do
    cache <- liftIO $ readIORef db.cacheInferredModule
    case Map.lookup modName cache of
      Just result -> pure result
      Nothing -> do
        (renamedAst, rnState) <- handleFetch db (RenamedModule modName)
        depsEnv <- buildDepsEnv db rnState.dependencies
        malgo2025 <- isMalgo2025Enabled
        finalEnv <- runReader modName do
          bindGroup <-
            if malgo2025
              then runPass ElaboratePass renamedAst.moduleDefinition
              else pure renamedAst.moduleDefinition
          (_, env) <- runPass InferPass (depsEnv, bindGroup)
          pure env
        -- Export only the entries this module contributes — names inherited
        -- from its dependencies are left to the caller's own dep walk.
        let exported = Map.difference finalEnv depsEnv
        liftIO $ modifyIORef db.cacheInferredModule $ Map.insert modName exported
        pure exported
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
              then do
                importedEnv <- buildDepsEnv db rnState.dependencies
                (bg, _) <- runPass InferPass (importedEnv, bindGroup)
                pure bg
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
      pure (toFilePath modPath.originPath, convertString content)

-- | Try to load a pre-built '.mlgi' interface from disk.  Returns 'Nothing' when
-- the artifact does not yet exist so the caller can fall back to compilation.
tryLoadInterfaceFromDisk :: (IOE :> es) => ModuleName -> Eff es (Maybe Interface)
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
-- Only swallows 'WorkspaceError' (i.e. 'ModuleNotFound'); other IO/programming
-- errors propagate so they remain visible.
tryGetModulePath :: (IOE :> es) => ModuleName -> Eff es (Maybe ArtifactPath)
tryGetModulePath modName = do
  result <- liftIO $ try @WorkspaceError $ do
    runEff $ runWorkspaceOnPwd do
      getModulePath modName
  pure $ either (const Nothing) Just result

-- | Invalidate 'modName' and every cached module that transitively depends
-- on it. Reverse-dep tracking is reconstructed from the cached
-- 'cacheRenamedModule' (rnState.dependencies). Without this cascade,
-- an edit to module @A@ would leave stale 'cacheInferredModule[B]' /
-- 'cacheLinkedProgram[B]' entries for any importer @B@, since their
-- values were computed against the previous version of @A@.
invalidateWithRdeps :: Database -> ModuleName -> IO ()
invalidateWithRdeps db modName = do
  renamed <- readIORef db.cacheRenamedModule
  let depsOf = Map.map (\(_, st) -> st.dependencies) renamed
      victims = Set.insert modName (reverseDepClosure depsOf modName)
  for_ (Set.toList victims) \m -> do
    modifyIORef db.cacheParsedModule $ Map.delete m
    modifyIORef db.cacheRenamedModule $ Map.delete m
    modifyIORef db.cacheInferredModule $ Map.delete m
    modifyIORef db.cacheLinkedProgram $ Map.delete m
    modifyIORef db.cacheModuleInterface $ Map.delete m

-- | Compute the transitive set of modules that depend on 'target', using
-- a forward dep-edge map @M -> M's deps@. Excludes 'target' itself.
-- Pure so it can be unit-tested without a populated 'Database'.
reverseDepClosure ::
  Map ModuleName (Set ModuleName) ->
  ModuleName ->
  Set ModuleName
reverseDepClosure depsOf target = go Set.empty (Set.singleton target)
  where
    go acc frontier
      | Set.null frontier = acc
      | otherwise =
          let next =
                Set.fromList
                  [ m
                  | (m, ds) <- Map.toList depsOf,
                    m /= target,
                    not (m `Set.member` acc),
                    not (Set.null (Set.intersection ds frontier))
                  ]
           in go (acc <> next) next

-- | Build a TyEnv by unioning each dependency's exported 'TyEnv'.
-- Each 'InferredModule' result already covers explicit signatures, foreign
-- imports, data constructors, *and* inferred bare 'def' bindings, so the
-- caller does not need to walk the renamed AST itself.
--
-- Two dependencies must not export the same 'Id' — 'Id' carries its
-- defining 'ModuleName', so a collision indicates an upstream invariant
-- violation (for example, a renamer bug producing non-unique Ids, or a
-- future re-export feature without a deduplication strategy). Surfacing
-- the collision loudly is far better than silently dropping one of the
-- two entries via 'Map's left-biased 'Semigroup'.
buildDepsEnv ::
  ( Reader Flag :> es,
    State Uniq :> es,
    IOE :> es,
    Workspace :> es,
    Features :> es,
    Error CompileError :> es
  ) =>
  Database ->
  Set ModuleName ->
  Eff es TyEnv
buildDepsEnv db deps =
  foldlM
    ( \acc dep -> do
        depEnv <- handleFetch db (InferredModule dep)
        let collisions = Map.intersectionWith (,) acc depEnv
        unless (Map.null collisions)
          $ error
          $ "Malgo.Query.Engine.buildDepsEnv: dependency "
          <> show dep
          <> " redefines names already exported by an earlier dep: "
          <> show (Map.keys collisions)
        pure (acc <> depEnv)
    )
    Map.empty
    (Set.toList deps)

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
