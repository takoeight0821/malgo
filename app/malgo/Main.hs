-- For use of 'undefined'
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Malgo.Backend.Zig.Toolchain qualified as Zig
import Malgo.Driver qualified as Driver
import Malgo.Lint (lintFile)
import Malgo.Lint.Diagnostic (Diagnostic (..), Severity (Error), prettyDiagnostic)
import Malgo.Monad (runMalgoM)
import Malgo.Monad qualified as Flag
import Malgo.Prelude
import Options.Applicative
import Prettyprinter.Render.Text (hPutDoc)
import System.Directory (makeAbsolute)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName)

data EvalOpt = EvalOpt
  { srcPath :: FilePath,
    noOptimize :: Bool,
    lambdaLift :: Bool,
    debugMode :: Bool,
    target :: Target,
    evalMode :: EvalMode,
    useInfer :: Bool,
    programArgs :: [String]
  }

data LintOpt = LintOpt
  { srcPath :: FilePath,
    denyWarnings :: Bool
  }

data CompileOpt = CompileOpt
  { srcPath :: FilePath,
    outPath :: Maybe FilePath,
    optMode :: Zig.OptMode
  }

data DumpOpt = DumpOpt
  { srcPath :: FilePath,
    stage :: Driver.DumpStage
  }

main :: IO ()
main = do
  command <- parseCommand
  case command of
    Eval opt ->
      Driver.compile opt.srcPath
        & runMalgoM
          Flag
            { Flag.noOptimize = opt.noOptimize,
              Flag.lambdaLift = opt.lambdaLift,
              Flag.debugMode = opt.debugMode,
              Flag.testMode = False,
              Flag.target = opt.target,
              Flag.evalMode = opt.evalMode,
              Flag.useInfer = opt.useInfer,
              Flag.programArgs = opt.programArgs
            }
    Compile opt -> do
      let outPath = fromMaybe (takeBaseName opt.srcPath) opt.outPath
      -- opt.srcPath is already absolute (parseCommand resolves it), so
      -- comparing it against the default output path's own absolute form
      -- catches e.g. `malgo compile hello` on an extension-less source
      -- file run from its own directory, where the default output name
      -- (the source's base name) would otherwise silently overwrite it.
      outPathAbs <- makeAbsolute outPath
      when (outPathAbs == opt.srcPath) do
        hPutStrLn stderr $ "malgo compile: refusing to overwrite the source file (" <> outPath <> ")."
        hPutStrLn stderr "Pass -o/--output to choose a different path."
        exitFailure
      Driver.compileToExecutable opt.srcPath outPath opt.optMode
        & runMalgoM compileFlag
    Lint opt -> do
      diags <- lintFile opt.srcPath & runMalgoM lintFlag
      for_ diags \d -> do
        hPutDoc stderr (prettyDiagnostic d)
        hPutStrLn stderr ""
      let hasError = any (\Diagnostic {severity} -> severity == Error) diags
      when (hasError || (opt.denyWarnings && not (null diags))) exitFailure
    Dump opt ->
      Driver.dumpFingerprint opt.stage opt.srcPath
        & runMalgoM dumpFlag
  where
    -- Mirror TestUtils.flag: the fingerprint goldens were produced under
    -- testMode with no elaboration, and 'dumpFingerprint' must reproduce
    -- them for cross-implementation parity.
    dumpFlag =
      Flag
        { Flag.noOptimize = False,
          Flag.lambdaLift = False,
          Flag.debugMode = False,
          Flag.testMode = True,
          Flag.target = TargetEval,
          Flag.evalMode = EvalSmallStep,
          Flag.useInfer = False,
          Flag.programArgs = []
        }
    lintFlag =
      Flag
        { Flag.noOptimize = True,
          Flag.lambdaLift = False,
          Flag.debugMode = False,
          Flag.testMode = False,
          Flag.target = TargetEval,
          Flag.evalMode = EvalSmallStep,
          Flag.useInfer = False,
          Flag.programArgs = []
        }
    compileFlag =
      Flag
        { Flag.noOptimize = False,
          Flag.lambdaLift = False,
          Flag.debugMode = False,
          Flag.testMode = False,
          Flag.target = TargetZig,
          Flag.evalMode = EvalSmallStep,
          Flag.useInfer = False,
          Flag.programArgs = []
        }

targetOpt :: Parser Target
targetOpt =
  option
    (eitherReader parseTarget)
    ( long "target"
        <> value TargetEval
        <> help "Compilation target: eval (default), scheme, or zig"
    )
  where
    parseTarget "eval" = Right TargetEval
    parseTarget "scheme" = Right TargetScheme
    parseTarget "zig" = Right TargetZig
    parseTarget t = Left $ "Unknown target: " <> t

optModeOpt :: Parser Zig.OptMode
optModeOpt =
  option
    (eitherReader Zig.parseOptMode)
    ( long "opt"
        <> value Zig.Debug
        <> help "Zig build mode: debug (default), release-safe, or release-fast"
    )

evalModeOpt :: Parser EvalMode
evalModeOpt =
  option
    (eitherReader parseEvalMode)
    ( long "eval-mode"
        <> value EvalSmallStep
        <> help "Evaluation mode: smallstep (default) or bigstep"
    )
  where
    parseEvalMode "smallstep" = Right EvalSmallStep
    parseEvalMode "bigstep" = Right EvalBigStep
    parseEvalMode m = Left $ "Unknown eval-mode: " <> m

evalOpt :: Parser EvalOpt
evalOpt =
  ( EvalOpt
      <$> strArgument (metavar "SOURCE" <> help "Source file (relative path)" <> action "file")
      <*> switch (long "no-opt")
      <*> switch (long "lambdalift")
      <*> switch (long "debug-mode")
      <*> targetOpt
      <*> evalModeOpt
      <*> switch (long "infer" <> help "Run type inference pass")
      <*> many (strArgument (metavar "ARG" <> help "Argument visible to the evaluated program"))
  )
    <**> helper

lintOpt :: Parser LintOpt
lintOpt =
  ( LintOpt
      <$> strArgument (metavar "SOURCE" <> help "Source file (relative path)" <> action "file")
      <*> switch (long "deny-warnings" <> help "Exit with a nonzero status if any diagnostic is reported")
  )
    <**> helper

compileOpt :: Parser CompileOpt
compileOpt =
  ( CompileOpt
      <$> strArgument (metavar "SOURCE" <> help "Source file (relative path)" <> action "file")
      <*> optional (strOption (long "output" <> short 'o' <> metavar "OUT" <> help "Output executable path (default: SOURCE's base name)"))
      <*> optModeOpt
  )
    <**> helper

dumpStageOpt :: Parser Driver.DumpStage
dumpStageOpt =
  option
    (eitherReader parseStage)
    (long "stage" <> metavar "STAGE" <> help "flat-fingerprint or join-fingerprint")
  where
    parseStage "flat-fingerprint" = Right Driver.FlatFingerprint
    parseStage "join-fingerprint" = Right Driver.JoinFingerprint
    parseStage s = Left $ "Unknown dump stage: " <> s

dumpOpt :: Parser DumpOpt
dumpOpt =
  ( DumpOpt
      <$> strArgument (metavar "SOURCE" <> help "Source file (relative path)" <> action "file")
      <*> dumpStageOpt
  )
    <**> helper

data Command
  = Eval EvalOpt
  | Compile CompileOpt
  | Lint LintOpt
  | Dump DumpOpt

parseCommand :: IO Command
parseCommand = do
  command <-
    execParser
      ( info (subparser (eval <> compile <> lint <> dump) <**> helper)
          $ fullDesc
          <> header "malgo programming language"
      )
  case command of
    Eval opt -> do
      srcPath <- makeAbsolute opt.srcPath
      pure $ Eval opt {srcPath = srcPath}
    Compile opt -> do
      srcPath <- makeAbsolute opt.srcPath
      pure $ Compile opt {srcPath = srcPath}
    Lint opt -> do
      srcPath <- makeAbsolute opt.srcPath
      pure $ Lint opt {srcPath = srcPath}
    Dump opt -> do
      srcPath <- makeAbsolute opt.srcPath
      pure $ Dump opt {srcPath = srcPath}
  where
    eval =
      command "eval"
        $ info (Eval <$> evalOpt)
        $ fullDesc
        <> progDesc "Evaluate a malgo program"
    compile =
      command "compile"
        $ info (Compile <$> compileOpt)
        $ fullDesc
        <> progDesc "Compile a malgo program to a native executable via the Zig backend"
    lint =
      command "lint"
        $ info (Lint <$> lintOpt)
        $ fullDesc
        <> progDesc "Lint a malgo program for stylistic issues"
    -- Hidden developer subcommand: print a format-immune IR fingerprint for
    -- cross-implementation (Haskell vs Lean) parity checking.
    dump =
      command "dump"
        $ info (Dump <$> dumpOpt)
        $ fullDesc
        <> progDesc "Dump a format-immune IR fingerprint (internal parity tool)"
