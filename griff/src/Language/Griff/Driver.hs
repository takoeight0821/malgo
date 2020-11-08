{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Driver (compile) where

import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Koriel.Core.CodeGen (codeGen)
import Koriel.Core.Core
import Koriel.Core.Flat (flat)
import Koriel.Core.LambdaLift (lambdalift)
import Koriel.Core.Lint (lintProgram, runLint)
import Koriel.Core.Optimize (optimizeProgram)
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty
import qualified LLVM.AST as L
import LLVM.Pretty (ppllvm)
import Language.Griff.Desugar (desugar)
import Language.Griff.Option
import Language.Griff.Parser (pTopLevel)
import Language.Griff.Rename (rename)
import Language.Griff.RnEnv
  ( genRnEnv,
    genRnState,
  )
import qualified Language.Griff.TcEnv as T
import Language.Griff.Typing (typeCheck)
import System.IO
  ( hPrint,
    hPutStrLn,
    stderr,
  )
import Text.Megaparsec
  ( errorBundlePretty,
    parse,
  )
import qualified Text.PrettyPrint.HughesPJ as P

compile :: Opt -> IO ()
compile opt = do
  src <- T.readFile (srcName opt)
  (packageName, ds) <- case parse pTopLevel (srcName opt) src of
    Right ds -> pure ds
    Left err -> error $ errorBundlePretty err
  when (dumpParsed opt) $ do
    hPutStrLn stderr "=== PARSE ==="
    hPrint stderr $ "package" <+> P.text packageName
    hPrint stderr $ P.sep $ P.punctuate ";" $ map pPrint ds
  void $
    runUniqT ?? UniqSupply 0 $ do
      rnState <- genRnState packageName
      rnEnv <- genRnEnv
      ds' <- rename rnState rnEnv ds
      when (dumpRenamed opt) $
        liftIO $ do
          hPutStrLn stderr "=== RENAME ==="
          hPrint stderr $ P.sep $ P.punctuate ";" $ map pPrint ds'
      (bg, tcEnv) <- typeCheck rnEnv ds'
      when (dumpTyped opt) $
        liftIO $ do
          hPutStrLn stderr "=== TYPE CHECK ==="
          hPrint stderr $ pPrint $ Map.toList $ view T.varEnv tcEnv
          hPrint stderr $ Map.toList $ view T.typeEnv tcEnv
          hPrint stderr $ pPrint $ Map.toList $ view T.tyConEnv tcEnv
          hPrint stderr $ pPrint bg
      core <- desugar tcEnv bg
      when (dumpDesugar opt) $
        liftIO $ do
          hPutStrLn stderr "=== DESUGAR ==="
          hPrint stderr . pPrint =<< appProgram (pure . flat) core
      runLint $ lintProgram core
      coreOpt <- if noOptimize opt then appProgram (pure . flat) core else optimizeProgram (inlineSize opt) core
      when (dumpDesugar opt && not (noOptimize opt)) $
        liftIO $ do
          hPutStrLn stderr "=== OPTIMIZE ==="
          hPrint stderr $ pPrint coreOpt
      runLint $ lintProgram coreOpt
      coreProg <-
        if noOptimize opt
          then lambdalift coreOpt
          else
            optimizeProgram (inlineSize opt)
              =<< lambdalift coreOpt
      when (dumpDesugar opt) $
        liftIO $ do
          hPutStrLn stderr "=== LAMBDALIFT ==="
          hPrint stderr $ pPrint coreProg
      llvmir <- codeGen coreProg
      liftIO $
        TL.writeFile (dstName opt) $
          ppllvm
            L.defaultModule
              { L.moduleName = fromString $ srcName opt,
                L.moduleSourceFileName = fromString $ srcName opt,
                L.moduleDefinitions = llvmir
              }
