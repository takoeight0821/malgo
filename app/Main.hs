module Main where

import           Control.Monad.State
import qualified Language.Malgo.Parser as Parser
import qualified Language.Malgo.Syntax as Syntax
import qualified Language.Malgo.Typing as Typing
import           System.Environment    (getArgs)
import qualified Text.Parsec.String    as P

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  result <- P.parseFromFile Parser.parseToplevel file
  case result of
    Left err  -> print err
    Right ast -> do print ast
                    print $ runStateT (sequence (map Typing.typeofDecl ast)) Typing.initEnv
