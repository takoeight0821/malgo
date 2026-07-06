-- | Invokes the system @zig@ toolchain to turn generated Zig source into a
-- native executable, for the @malgo compile@ subcommand.
module Malgo.Backend.Zig.Toolchain
  ( OptMode (..),
    parseOptMode,
    buildExecutable,
  )
where

import Data.Text qualified as T
import Malgo.Prelude
import System.Directory (findExecutable)
import System.Exit (ExitCode (..), exitFailure)
import System.Process (readProcessWithExitCode)

data OptMode = Debug | ReleaseSafe | ReleaseFast

parseOptMode :: String -> Either String OptMode
parseOptMode "debug" = Right Debug
parseOptMode "release-safe" = Right ReleaseSafe
parseOptMode "release-fast" = Right ReleaseFast
parseOptMode m = Left $ "Unknown --opt mode: " <> m

optModeFlag :: OptMode -> String
optModeFlag Debug = "Debug"
optModeFlag ReleaseSafe = "ReleaseSafe"
optModeFlag ReleaseFast = "ReleaseFast"

-- | Compile a @.zig@ source file to a native executable at @outPath@,
-- using a per-invocation cache directory under @.malgo-work@ so repeated
-- builds do not touch any path outside the workspace.
buildExecutable :: FilePath -> FilePath -> FilePath -> OptMode -> IO ()
buildExecutable zigCacheRoot srcPath outPath mode = do
  mzig <- findExecutable "zig"
  zig <- case mzig of
    Just z -> pure z
    Nothing -> do
      hPutStrLn stderr "zig not found on PATH."
      hPutStrLn stderr "Install it via 'mise install' (pinned in mise.toml) or https://ziglang.org/download/"
      exitFailure
  let args =
        [ "build-exe",
          srcPath,
          "-femit-bin=" <> outPath,
          "--cache-dir",
          zigCacheRoot <> "/zig-cache",
          "--global-cache-dir",
          zigCacheRoot <> "/zig-global-cache",
          "-O",
          optModeFlag mode
        ]
  (exitCode, _stdout, stderrOutput) <- readProcessWithExitCode zig args ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      hPutStrLn stderr $ "zig build-exe failed (source kept at " <> srcPath <> "):"
      hPutStrLn stderr $ T.unpack (convertString stderrOutput)
      exitFailure
