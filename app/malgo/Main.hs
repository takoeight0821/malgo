-- For use of 'undefined'
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

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
    Lint opt -> do
      diags <- lintFile opt.srcPath & runMalgoM lintFlag
      for_ diags \d -> do
        hPutDoc stderr (prettyDiagnostic d)
        hPutStrLn stderr ""
      let hasError = any (\Diagnostic {severity} -> severity == Error) diags
      when (hasError || (opt.denyWarnings && not (null diags))) exitFailure
  where
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

targetOpt :: Parser Target
targetOpt =
  option
    (eitherReader parseTarget)
    ( long "target"
        <> value TargetEval
        <> help "Compilation target: eval (default) or scheme"
    )
  where
    parseTarget "eval" = Right TargetEval
    parseTarget "scheme" = Right TargetScheme
    parseTarget t = Left $ "Unknown target: " <> t

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

data Command
  = Eval EvalOpt
  | Lint LintOpt

parseCommand :: IO Command
parseCommand = do
  command <-
    execParser
      ( info (subparser (eval <> lint) <**> helper)
          $ fullDesc
          <> header "malgo programming language"
      )
  case command of
    Eval opt -> do
      srcPath <- makeAbsolute opt.srcPath
      pure $ Eval opt {srcPath = srcPath}
    Lint opt -> do
      srcPath <- makeAbsolute opt.srcPath
      pure $ Lint opt {srcPath = srcPath}
  where
    eval =
      command "eval"
        $ info (Eval <$> evalOpt)
        $ fullDesc
        <> progDesc "Evaluate a malgo program"
    lint =
      command "lint"
        $ info (Lint <$> lintOpt)
        $ fullDesc
        <> progDesc "Lint a malgo program for stylistic issues"
