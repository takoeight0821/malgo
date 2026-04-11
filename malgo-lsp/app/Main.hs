module Main (main) where

import Malgo.LSP (runLSP)
import Malgo.Prelude
import System.Exit (ExitCode (..), exitWith)

main :: IO ()
main = do
  code <- runLSP
  case code of
    0 -> pure ()
    c -> exitWith (ExitFailure c)
