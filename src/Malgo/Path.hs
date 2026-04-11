{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lightweight typed path library, replacing the @path@ package.
-- Provides phantom-typed wrappers around 'FilePath' for type-safe
-- path manipulation without heavy dependencies.
module Malgo.Path
  ( Abs,
    Rel,
    File,
    Dir,
    Path,
    toFilePath,
    parseAbsFile,
    parseAbsDir,
    mkRelFile,
    parent,
    (</>),
    replaceExtension,
    splitExtension,
    filename,
    stripProperPrefix,
  )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Binary (Binary)
import Data.Data (Data)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import System.FilePath qualified as FP
import Prelude

-- | Phantom type tag for absolute paths.
data Abs

-- | Phantom type tag for relative paths.
data Rel

-- | Phantom type tag for files.
data File

-- | Phantom type tag for directories.
data Dir

-- | A typed path. @b@ is 'Abs' or 'Rel', @t@ is 'File' or 'Dir'.
newtype Path b t = Path FilePath
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving newtype (Hashable)
  deriving anyclass (Binary)

-- | Extract the underlying 'FilePath'.
toFilePath :: Path b t -> FilePath
toFilePath (Path fp) = fp

-- | Parse an absolute file path. Throws if not absolute.
parseAbsFile :: (MonadThrow m) => FilePath -> m (Path Abs File)
parseAbsFile fp
  | FP.isAbsolute fp = pure (Path fp)
  | otherwise = throwM (PathException $ "Expected absolute file path: " <> fp)

-- | Parse an absolute directory path. Throws if not absolute.
-- Directory paths are stored WITHOUT trailing separator internally.
parseAbsDir :: (MonadThrow m) => FilePath -> m (Path Abs Dir)
parseAbsDir fp
  | FP.isAbsolute fp = pure (Path (FP.dropTrailingPathSeparator fp))
  | otherwise = throwM (PathException $ "Expected absolute directory path: " <> fp)

-- | Create a relative file path (no validation).
mkRelFile :: FilePath -> Path Rel File
mkRelFile = Path

-- | Get the parent directory of a path.
parent :: Path b t -> Path b Dir
parent (Path fp) = Path (FP.takeDirectory fp)

-- | Combine a directory path with a relative path.
(</>) :: Path b Dir -> Path Rel t -> Path b t
Path dir </> Path rel = Path (dir FP.</> rel)

infixr 5 </>

-- | Replace the extension of a file path.
replaceExtension :: (MonadThrow m) => String -> Path b File -> m (Path b File)
replaceExtension ext (Path fp) = pure (Path (FP.replaceExtension fp ext))

-- | Split the extension from a file path.
splitExtension :: Path b File -> Maybe (Path b File, String)
splitExtension (Path fp) =
  let (base, ext) = FP.splitExtension fp
   in if null ext then Nothing else Just (Path base, ext)

-- | Get the filename component of a path.
filename :: Path b File -> Path Rel File
filename (Path fp) = Path (FP.takeFileName fp)

-- | Strip a directory prefix from a path, yielding a relative path.
-- Throws if the prefix doesn't match.
stripProperPrefix :: (MonadThrow m) => Path b Dir -> Path b t -> m (Path Rel t)
stripProperPrefix (Path dir) (Path fp) =
  let rel = FP.makeRelative (FP.addTrailingPathSeparator dir) fp
   in if rel == fp
        then throwM (PathException $ "Path " <> fp <> " is not inside " <> dir)
        else pure (Path rel)

-- | Exception type for path operations.
newtype PathException = PathException String
  deriving stock (Show)

instance Exception PathException
