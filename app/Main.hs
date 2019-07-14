{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Data.Map              as Map
import           Data.Outputable
import qualified Data.Text.Lazy.IO     as T
import           Language.Malgo.Driver
import qualified Language.Malgo.Lexer  as Lexer
import           Language.Malgo.Monad
import qualified Language.Malgo.Parser as Parser
import           Language.Malgo.Pretty
import           LLVM.Pretty
import           Universum

main :: IO ()
main = do
  opt <- parseOpt

  let file = srcName opt
  tokens <- Lexer.lexing () (toString file) =<< readFile file
  let parser = Parser.parseExpr
  let ast = case parser <$> tokens of
              Left x  -> error $ show x
              Right x -> x

  u <- newIORef 0
  ll <- compile file ast (UniqSupply u) opt

  T.writeFile (dstName opt) (ppllvm ll)
