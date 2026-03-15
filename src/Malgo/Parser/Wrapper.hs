module Malgo.Parser.Wrapper (parseWithWrapper) where

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effectful (Eff, IOE, type (:>))
import Malgo.Features (Features, addFeatures, parseFeatures)
import Malgo.Module (Workspace)
import Malgo.Parser.CStyle (parseCStyle)
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec (ParseErrorBundle)

-- | parseWithWrapper extracts pragma features and parses with the unified parser entrypoint.
parseWithWrapper :: (IOE :> es, Workspace :> es, Features :> es) => FilePath -> TL.Text -> Eff es (Either (ParseErrorBundle TL.Text Void) (Module (Malgo Parse)))
parseWithWrapper srcPath text = do
  let pragmas = extractPragmas text
  let knownPragmas = filterKnownPragmas pragmas
  let features = parseFeatures knownPragmas
  addFeatures features
  parseCStyle srcPath text

-- | Filter out unknown pragmas to avoid parse errors
filterKnownPragmas :: [Text] -> [Text]
filterKnownPragmas = filter isKnownPragma
  where
    isKnownPragma pragma =
      pragma
        == "c-style-apply"
        || pragma
        == "malgo-2025"
        || "experimental-"
        `T.isPrefixOf` pragma

-- | Extract pragmas from a module.
-- Returns the list of pragmas.
extractPragmas :: TL.Text -> [Text]
extractPragmas = go [] . TL.lines
  where
    go pragmas [] = map convertString $ reverse pragmas
    go pragmas (l : ls)
      | "#" `TL.isPrefixOf` l = go (TL.drop 1 l : pragmas) ls
      | otherwise = go pragmas ls
