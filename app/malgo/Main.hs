-- For use of 'undefined'
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Malgo.Driver qualified as Driver
import Malgo.Monad (runMalgoM)
import Malgo.Monad qualified as Flag
import Malgo.Prelude
import Options.Applicative
import System.Directory (makeAbsolute)

data EvalOpt = EvalOpt
  { srcPath :: FilePath,
    noOptimize :: Bool,
    lambdaLift :: Bool,
    debugMode :: Bool,
    target :: Target,
    evalMode :: EvalMode,
    useInfer :: Bool
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
              Flag.useInfer = opt.useInfer
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
  )
    <**> helper

newtype Command
  = Eval EvalOpt

parseCommand :: IO Command
parseCommand = do
  command <-
    execParser
      ( info (subparser eval <**> helper)
          $ fullDesc
          <> header "malgo programming language"
      )
  case command of
    Eval opt -> do
      srcPath <- makeAbsolute opt.srcPath
      pure $ Eval opt {srcPath = srcPath}
  where
    eval =
      command "eval"
        $ info (Eval <$> evalOpt)
        $ fullDesc
        <> progDesc "Evaluate a malgo program"
