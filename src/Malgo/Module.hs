{-# LANGUAGE DeriveAnyClass #-}

module Malgo.Module
  ( ModuleName (..),
    HasModuleName,
    Workspace,
    getWorkspace,
    getWorkspaceAbs,
    registerModule,
    getModulePath,
    runWorkspaceOnPwd,
    ArtifactPath (..),
    WorkspaceError (..),
    Resource (..),
    parseArtifactPath,
    parseArtifactPathFromPwd,
    ViaBinary (..),
    ViaShow (..),
    moduleNameToString,
    moduleNameDigest,
    Pragma (..),
    insertPragmas,
  )
where

import Control.Monad.Catch
import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Data
import Data.Hashable (Hashable (..))
import Data.Map qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static (prettyCallStack)
import GHC.Records
import GHC.Stack (callStack)
import Malgo.Path
import Malgo.Prelude
import Malgo.SExpr (ToSExpr (..))
import Malgo.SExpr qualified as S
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findFile, getCurrentDirectory, listDirectory, makeAbsolute, removeFile, renameFile)
import System.Environment (lookupEnv)
import System.FilePath (makeRelative, takeDirectory, takeFileName)
import System.FilePath qualified as F
import System.FilePath qualified as FP
import System.IO (hClose, openBinaryTempFile)
import Text.Megaparsec.Pos (initialPos)

data ModuleName
  = ModuleName Text
  | Artifact ArtifactPath
  deriving stock (Eq, Show, Ord, Generic, Data)
  deriving anyclass (Hashable, Binary)

instance HasRange ModuleName where
  range (ModuleName raw) = Range (initialPos $ convertString raw) (initialPos $ convertString raw)
  range (Artifact path) = Range (initialPos $ toFilePath path.relPath) (initialPos $ toFilePath path.relPath)

instance ToSExpr ModuleName where
  toSExpr (ModuleName raw) = S.A $ S.Symbol raw
  toSExpr (Artifact path) = S.A $ S.String $ convertString $ toFilePath path.relPath

instance Pretty ModuleName where
  pretty (ModuleName raw) = pretty raw
  pretty (Artifact path) = pretty $ toFilePath path.relPath

moduleNameToString :: (ConvertibleStrings FilePath b, ConvertibleStrings Text b) => ModuleName -> b
moduleNameToString (ModuleName raw) = convertString raw
moduleNameToString (Artifact path) = convertString $ toFilePath path.relPath

-- | Convert ModuleName to a digest string.
-- This is used for debugging.
moduleNameDigest :: (ConvertibleStrings Text b, ConvertibleStrings FilePath b) => ModuleName -> b
moduleNameDigest (ModuleName raw) = convertString raw
moduleNameDigest (Artifact path) = convertString $ toFilePath $ case splitExtension $ filename path.relPath of
  Just (name, _) -> name
  Nothing -> filename path.relPath

type HasModuleName r = HasField "moduleName" r ModuleName

instance HasField "moduleName" ModuleName ModuleName where
  getField = identity

data WorkspaceHolder = WorkspaceHolder
  { getWorkspace :: FilePath,
    modulePathMap :: IORef (Map ModuleName ArtifactPath)
  }

data Workspace :: Effect

type instance DispatchOf Workspace = Static WithSideEffects

newtype instance StaticRep Workspace = Workspace WorkspaceHolder

{-# WARNING getWorkspace "This function is unsafe and should not be used in production code." #-}
getWorkspace :: (Workspace :> es) => Eff es FilePath
getWorkspace = do
  Workspace handler <- getStaticRep
  pure handler.getWorkspace

runWorkspaceOnPwd :: (IOE :> es) => Eff (Workspace : es) a -> Eff es a
runWorkspaceOnPwd action = do
  pwd <- liftIO getCurrentDirectory
  -- The work-dir name is overridable so the Haskell and Lean toolchains can
  -- coexist in one checkout (the Lean port defaults to ".malgo-work-lean").
  workDirName <- liftIO $ fromMaybe ".malgo-work" <$> lookupEnv "MALGO_WORK_DIR"
  liftIO $ createDirectoryIfMissing True $ pwd F.</> workDirName
  workspaceDir <- liftIO $ makeAbsolute $ pwd F.</> workDirName
  modulePathMap <- newIORef mempty
  evalStaticRep (Workspace $ WorkspaceHolder workspaceDir modulePathMap) action

getWorkspaceAbs :: (Workspace :> es) => Eff es (Path Abs Dir)
getWorkspaceAbs = do
  workspace <- getWorkspace
  parseAbsDir workspace

registerModule :: (Workspace :> es, IOE :> es) => ModuleName -> ArtifactPath -> Eff es ()
registerModule moduleName path = do
  Workspace WorkspaceHolder {modulePathMap} <- getStaticRep
  modifyIORef modulePathMap $ Map.insert moduleName path

getModulePath :: (HasCallStack) => (Workspace :> es, IOE :> es) => ModuleName -> Eff es ArtifactPath
getModulePath moduleName = do
  Workspace WorkspaceHolder {modulePathMap} <- getStaticRep
  modulePathMap' <- readIORef modulePathMap
  case Map.lookup moduleName modulePathMap' of
    Just path -> pure path
    Nothing -> searchAndRegister moduleName

searchAndRegister :: (HasCallStack) => (Workspace :> es, IOE :> es) => ModuleName -> Eff es ArtifactPath
searchAndRegister (ModuleName moduleName) = do
  let fileName = convertString moduleName <> ".mlg"
  -- Find fileName in workspace
  workspace <- getWorkspaceAbs
  file <- search [toFilePath workspace] fileName
  let relPath = makeRelative (toFilePath workspace) file
  path <- parseArtifactPathFromPwd relPath
  registerModule (ModuleName moduleName) path
  pure path
  where
    search [] _ = throwM $ ModuleNotFound $ ModuleName moduleName
    search dirs fileName = do
      mfile <- liftIO $ findFile dirs fileName
      case mfile of
        Just file -> pure file
        Nothing -> do
          subDirs <- liftIO $ traverse listSubDirectories dirs
          search (concat subDirs) fileName
searchAndRegister (Artifact path) = pure path

listSubDirectories :: FilePath -> IO [FilePath]
listSubDirectories dir = do
  entries <- listDirectory dir
  filterM doesDirectoryExist $ map (dir FP.</>) entries

data WorkspaceError where
  ModuleNotFound :: (HasCallStack) => ModuleName -> WorkspaceError

instance Show WorkspaceError where
  show = displayException

instance Exception WorkspaceError where
  displayException (ModuleNotFound moduleName) =
    "Module not found: " <> moduleNameToString moduleName <> "\n" <> prettyCallStack callStack

data ArtifactPath = ArtifactPath
  { rawPath :: FilePath,
    originPath :: Path Abs File,
    relPath :: Path Rel File,
    targetPath :: Path Abs File
  }
  deriving stock (Generic, Data)
  deriving anyclass (Binary)

-- | Equality based on relPath so that paths to the same file via different
-- traversal routes (e.g. "./runtime/..." vs "../../../runtime/...") compare equal.
instance Eq ArtifactPath where
  a == b = a.relPath == b.relPath

instance Ord ArtifactPath where
  compare a b = compare a.relPath b.relPath

instance Hashable ArtifactPath where
  hash a = hash (toFilePath a.relPath)
  hashWithSalt s a = hashWithSalt s (toFilePath a.relPath)

instance Show ArtifactPath where
  -- Do not show rawPath, originPath, targetPath.
  -- Because they include absolute path, which is not portable and may leak information.
  showsPrec d (ArtifactPath {relPath}) = showParen (d > 10) $ showString "ArtifactPath " . showsPrec 11 (toFilePath relPath)

instance Pretty ArtifactPath where
  pretty path = pretty $ toFilePath path.relPath

parseArtifactPath :: (IOE :> es, Workspace :> es) => ArtifactPath -> FilePath -> Eff es ArtifactPath
parseArtifactPath from path = do
  basePath <- liftIO $ makeAbsolute $ toFilePath $ parent from.originPath
  rawPath <- liftIO $ canonicalizePath (basePath F.</> path)
  originPath <- parseAbsFile rawPath

  workspace <- getWorkspaceAbs
  let originBasePath = parent workspace
  relPath <- stripProperPrefix originBasePath originPath

  let targetPath = workspace </> relPath
  pure $ ArtifactPath {rawPath = path, originPath, relPath, targetPath}

-- | Resolve a path string relative to the directory containing the workspace
-- (i.e. the user's "pwd" in the project root).
parseArtifactPathFromPwd :: (IOE :> es, Workspace :> es) => FilePath -> Eff es ArtifactPath
parseArtifactPathFromPwd path = do
  workspace <- getWorkspaceAbs
  let pwd = parent workspace
  basePath <- liftIO $ makeAbsolute $ toFilePath pwd
  rawPath <- liftIO $ canonicalizePath (basePath F.</> path)
  originPath <- parseAbsFile rawPath
  relPath <- stripProperPrefix pwd originPath
  let targetPath = workspace </> relPath
  pure $ ArtifactPath {rawPath = path, originPath, relPath, targetPath}

class Resource a where
  toByteString :: a -> ByteString
  fromByteString :: ByteString -> a
  load :: (IOE :> es) => ArtifactPath -> String -> Eff es a
  load ArtifactPath {originPath, targetPath} ext = do
    targetPath <- replaceExtension ext targetPath
    exists <- liftIO $ doesFileExist $ toFilePath targetPath
    if exists
      then do
        content <- liftIO $ BS.readFile $ toFilePath targetPath
        pure $ fromByteString content
      else do
        originPath <- replaceExtension ext originPath
        content <- liftIO $ BS.readFile $ toFilePath originPath
        liftIO $ createDirectoryIfMissing True $ toFilePath $ parent targetPath
        liftIO $ BS.writeFile (toFilePath targetPath) content
        pure $ fromByteString content
  save :: (IOE :> es) => ArtifactPath -> String -> a -> Eff es ()
  save ArtifactPath {targetPath} ext content = do
    targetPath <- replaceExtension ext targetPath
    let path = toFilePath targetPath
    liftIO $ createDirectoryIfMissing True $ toFilePath $ parent targetPath
    -- Write atomically via a temp file + rename, so concurrent writers
    -- (multiple test specs racing on a shared interface like
    -- runtime/malgo/Builtin.mlgi) cannot trip 'ResourceBusy' on
    -- 'BS.writeFile'. POSIX/NTFS rename overwrites atomically; the
    -- last writer wins, which is fine because the cached payload is
    -- a deterministic function of the source.
    liftIO $ atomicWriteByteString path $ toByteString content

-- | Write 'bytes' to 'path' such that concurrent callers cannot observe
-- a half-written file or trip 'ResourceBusy' on the destination handle.
-- Implementation: open a uniquely-named temp file in the same directory
-- (so the subsequent 'renameFile' is an atomic rename within one
-- filesystem), write the bytes, close the handle, then rename over
-- 'path'. On rename failure, the temp file is best-effort cleaned up.
atomicWriteByteString :: FilePath -> ByteString -> IO ()
atomicWriteByteString path bytes = do
  let dir = takeDirectory path
      template = takeFileName path <> ".tmp"
  (tmpPath, hdl) <- openBinaryTempFile dir template
  let cleanup = handle (\(_ :: SomeException) -> pure ()) (removeFile tmpPath)
  ( do
      BS.hPut hdl bytes
      hClose hdl
      renameFile tmpPath path
    )
    `onException` cleanup

newtype ViaBinary a = ViaBinary a
  deriving newtype (Binary)

instance (Binary a) => Resource (ViaBinary a) where
  toByteString (ViaBinary a) = BSL.toStrict $ Binary.encode a
  fromByteString bs = Binary.decode (BSL.fromStrict bs)

instance Resource ByteString where
  toByteString = identity
  fromByteString = identity

newtype ViaShow a = ViaShow a
  deriving newtype (Pretty)

instance (Show a) => Resource (ViaShow a) where
  toByteString (ViaShow a) = convertString $ show a
  fromByteString = error "fromByteString: ViaShow cannot be deserialized"

newtype Pragma = Pragma (Map ModuleName [Text])
  deriving stock (Eq, Show, Generic, Data)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (Hashable, Binary)

insertPragmas :: ModuleName -> [Text] -> Pragma -> Pragma
insertPragmas path value (Pragma map) = Pragma $ Map.insertWith (<>) path value map
