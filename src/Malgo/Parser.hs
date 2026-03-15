{-# LANGUAGE NoMonomorphismRestriction #-}

module Malgo.Parser (parse, ParserPass (..)) where

import Data.Text.Lazy qualified as TL
import Effectful
import Effectful.Error.Static (throwError)
import Malgo.Features
import Malgo.Module (Workspace)
import Malgo.Parser.Wrapper (parseWithWrapper)
import Malgo.Pass
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec (ParseErrorBundle)

data ParserPass = ParserPass

instance Pass ParserPass where
  type Input ParserPass = (FilePath, TL.Text)
  type Output ParserPass = Module (Malgo Parse)
  type ErrorType ParserPass = ParseErrorBundle TL.Text Void
  type Effects ParserPass es = (IOE :> es, Workspace :> es, Features :> es)

  runPassImpl _ (srcPath, text) = do
    result <- parse srcPath text
    case result of
      Left err -> throwError err
      Right result -> pure result

-- | parse a module using the unified parser entrypoint with pragma feature extraction
parse :: (IOE :> es, Workspace :> es, Features :> es) => FilePath -> TL.Text -> Eff es (Either (ParseErrorBundle TL.Text Void) (Module (Malgo Parse)))
parse = parseWithWrapper
