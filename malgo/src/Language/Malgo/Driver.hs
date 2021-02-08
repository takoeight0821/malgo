{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Driver (compile) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Koriel.Core.CodeGen (codeGen)
import Koriel.Core.Flat (flat)
import Koriel.Core.LambdaLift (lambdalift)
import Koriel.Core.Lint (lintProgram, runLint)
import Koriel.Core.Optimize (optimizeProgram)
import Koriel.Core.Syntax
import Koriel.MonadUniq
import Koriel.Pretty
import qualified LLVM.AST as L
import LLVM.Context (withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)
import LLVM.Pretty (ppllvm)
import Language.Malgo.Desugar.Pass (desugar)
import Language.Malgo.Interface (buildInterface, loadInterface, storeInterface)
import Language.Malgo.Parser (parseMalgo)
import Language.Malgo.Prelude
import Language.Malgo.Rename.Pass (rename)
import qualified Language.Malgo.Rename.RnEnv as RnEnv
import qualified Language.Malgo.Syntax as Syntax
import Language.Malgo.TypeCheck.Pass (typeCheck)
import System.FilePath.Lens (extension)
import System.IO
  ( hPrint,
    hPutStrLn,
    stderr,
  )
import Text.Megaparsec
  ( errorBundlePretty,
  )

-- |
-- dumpHoge系のフラグによるダンプ出力を行うコンビネータ
--
-- 引数 m のアクションの返り値をpPrintしてstderrに吐く
withDump ::
  (MonadIO m, Pretty a) =>
  -- | dumpHoge系のフラグの値
  Bool ->
  String ->
  m a ->
  m a
withDump isDump label m = do
  result <- m
  when isDump $ liftIO do
    hPutStrLn stderr label
    hPrint stderr $ pPrint result
  pure result

-- | .mlgから.llへのコンパイル
compile :: Opt -> IO ()
compile opt = do
  src <- T.readFile (srcName opt)
  parsedAst <- case parseMalgo (srcName opt) src of
    Right x -> pure x
    Left err -> error $ errorBundlePretty err
  when (dumpParsed opt) $ do
    hPutStrLn stderr "=== PARSE ==="
    hPrint stderr $ pPrint parsedAst
  void $
    runReaderT ?? opt $
      unMalgoM $
        runUniqT ?? UniqSupply 0 $ do
          rnEnv <- RnEnv.genBuiltinRnEnv
          (renamedAst, rnState) <- withDump (dumpRenamed opt) "=== RENAME ===" $ rename rnEnv parsedAst
          (typedAst, tcEnv) <- withDump (dumpTyped opt) "=== TYPE CHECK ===" $ typeCheck rnEnv renamedAst
          (dsEnv, core) <- withDump (dumpDesugar opt) "=== DESUGAR ===" $ desugar tcEnv typedAst
          let inf = buildInterface rnState dsEnv
          storeInterface inf
          when (debugMode opt) $ do
            inf <- loadInterface (Syntax._moduleName typedAst)
            liftIO $ do
              hPutStrLn stderr "=== INTERFACE ==="
              hPutStrLn stderr $ renderStyle (style {lineLength = 120}) $ pPrint inf
          runLint $ lintProgram core
          coreOpt <- if noOptimize opt then pure core else optimizeProgram (inlineSize opt) core
          when (dumpDesugar opt && not (noOptimize opt)) $
            liftIO $ do
              hPutStrLn stderr "=== OPTIMIZE ==="
              hPrint stderr $ pPrint $ over appProgram flat coreOpt
          runLint $ lintProgram coreOpt
          coreLL <- if noLambdaLift opt then pure coreOpt else lambdalift coreOpt
          when (dumpDesugar opt && not (noLambdaLift opt)) $
            liftIO $ do
              hPutStrLn stderr "=== LAMBDALIFT ==="
              hPrint stderr $ pPrint $ over appProgram flat coreLL
          coreLLOpt <- if noOptimize opt then pure coreLL else optimizeProgram (inlineSize opt) coreLL
          when (dumpDesugar opt && not (noLambdaLift opt) && not (noOptimize opt)) $
            liftIO $ do
              hPutStrLn stderr "=== LAMBDALIFT OPTIMIZE ==="
              hPrint stderr $ pPrint $ over appProgram flat coreLLOpt

          when (genCoreJSON opt) $ storeCoreJSON coreLLOpt

          llvmir <- codeGen coreLLOpt

          let llvmModule =
                L.defaultModule
                  { L.moduleName = fromString $ srcName opt,
                    L.moduleSourceFileName = fromString $ srcName opt,
                    L.moduleDefinitions = llvmir
                  }
          if viaBinding opt
            then liftIO $
              withContext $ \ctx ->
                BS.writeFile (dstName opt) =<< withModuleFromAST ctx llvmModule moduleLLVMAssembly
            else
              liftIO $
                TL.writeFile (dstName opt) $
                  ppllvm llvmModule

storeCoreJSON :: (MonadMalgo m, MonadIO m, ToJSON a, FromJSON a) => a -> m ()
storeCoreJSON core = do
  opt <- getOpt
  let json = encode core
  let decoded = eitherDecode json
  case decoded of
    Left err -> error err
    Right x -> (x `asTypeOf` core) `seq` pure ()
  liftIO $ BL.writeFile (dstName opt & extension .~ ".json") json
