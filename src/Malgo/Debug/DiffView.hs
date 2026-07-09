-- | Line- and word-level diffing between two rendered IR stages, for the
-- MET (M-exp-Tracer) debug tool ("app/met") and its golden tests.
--
-- Diffing runs in two passes, both via 'Data.Algorithm.Diff' (no external
-- @diff@ process — the library already gives a generic list-diff, so it is
-- reused unchanged at the token level): first line-by-line, then, for a
-- same-position "replace" (a run of removed lines immediately followed by
-- a run of added lines — the shape every changed statement in this
-- renderer takes), word-by-word within each paired line. A single renamed
-- identifier no longer paints its whole line red\/green; only the token
-- that actually changed does.
module Malgo.Debug.DiffView
  ( LineTag (..),
    Span (..),
    DiffLine (..),
    unifiedDiff,
    SideBySideRow (..),
    sideBySide,
    renderUnifiedDiff,
  )
where

import Data.Algorithm.Diff (PolyDiff (..), getGroupedDiff)
import Data.Text qualified as T
import Malgo.Prelude hiding (First)

data LineTag = Unchanged | Added | Removed
  deriving stock (Eq, Show)

-- | One word-level run within a line, tagged with how it differs from the
-- other side. A line with no intra-line match at all (a pure insertion\/
-- deletion) is just a single span covering the whole line.
data Span = Span {tag :: LineTag, text :: Text}
  deriving stock (Eq, Show)

data DiffLine = DiffLine {lineTag :: LineTag, spans :: [Span]}
  deriving stock (Eq, Show)

-- | A git-style +\/- listing between two texts, line by line, with
-- word-level spans inside each changed line.
unifiedDiff :: Text -> Text -> [DiffLine]
unifiedDiff before after = [DiffLine tag spans | (tag, spans) <- pairedLineDiff before after]

-- | Render a unified diff as plain text, one line per 'DiffLine', prefixed
-- with @" "@\/@"+"@\/@"-"@ (the format golden tests pin). Word-level spans
-- are flattened back into the whole line here — the highlighting is a
-- presentation detail for the HTML views, not part of the pinned text.
renderUnifiedDiff :: Text -> Text -> Text
renderUnifiedDiff before after = T.unlines (map renderLine (unifiedDiff before after))
  where
    renderLine (DiffLine tag spans) = prefix tag <> T.concat (map (.text) spans)
    prefix Unchanged = "  "
    prefix Added = "+ "
    prefix Removed = "- "

data SideBySideRow = SideBySideRow
  { leftTag :: Maybe LineTag,
    leftSpans :: Maybe [Span],
    rightTag :: Maybe LineTag,
    rightSpans :: Maybe [Span]
  }
  deriving stock (Eq, Show)

-- | Pairs lines for a left\/right panel view: unchanged lines line up on
-- both sides, removed lines appear only on the left, added lines only on
-- the right. A same-position replace pairs its removed\/added lines onto
-- one row each, word-diffed against each other.
sideBySide :: Text -> Text -> [SideBySideRow]
sideBySide before after = go (getGroupedDiff (T.lines before) (T.lines after))
  where
    go [] = []
    go (Both bs _ : rest) =
      [SideBySideRow (Just Unchanged) (Just [Span Unchanged l]) (Just Unchanged) (Just [Span Unchanged l]) | l <- bs] <> go rest
    go (First bs : Second as : rest) = pairRows bs as <> go rest
    go (First bs : rest) = [SideBySideRow (Just Removed) (Just [Span Removed l]) Nothing Nothing | l <- bs] <> go rest
    go (Second as : rest) = [SideBySideRow Nothing Nothing (Just Added) (Just [Span Added l]) | l <- as] <> go rest

    pairRows bs as =
      let n = min (length bs) (length as)
          (bHead, bTail) = splitAt n bs
          (aHead, aTail) = splitAt n as
       in zipWith rowFor bHead aHead
            <> [SideBySideRow (Just Removed) (Just [Span Removed l]) Nothing Nothing | l <- bTail]
            <> [SideBySideRow Nothing Nothing (Just Added) (Just [Span Added l]) | l <- aTail]

    rowFor b a =
      let (bSpans, aSpans) = diffWords b a
       in SideBySideRow (Just Removed) (Just bSpans) (Just Added) (Just aSpans)

-- | Shared by 'unifiedDiff': groups line-diff blocks, pairing up a
-- same-position replace (a 'First' run immediately followed by a
-- 'Second' run) into word-diffed line pairs instead of whole-line
-- remove+add. Pairing is positional (i-th removed line against i-th
-- added line) — simple, and correct for this renderer's output, where a
-- statement rewrite keeps the surrounding line count stable far more
-- often than not. Leftover lines when the two runs differ in length fall
-- back to plain whole-line spans.
pairedLineDiff :: Text -> Text -> [(LineTag, [Span])]
pairedLineDiff before after = go (getGroupedDiff (T.lines before) (T.lines after))
  where
    go [] = []
    go (Both bs _ : rest) = [(Unchanged, [Span Unchanged l]) | l <- bs] <> go rest
    go (First bs : Second as : rest) = pairLines bs as <> go rest
    go (First bs : rest) = [(Removed, [Span Removed l]) | l <- bs] <> go rest
    go (Second as : rest) = [(Added, [Span Added l]) | l <- as] <> go rest

    pairLines bs as =
      let n = min (length bs) (length as)
          (bHead, bTail) = splitAt n bs
          (aHead, aTail) = splitAt n as
       in concat (zipWith wordPair bHead aHead)
            <> [(Removed, [Span Removed l]) | l <- bTail]
            <> [(Added, [Span Added l]) | l <- aTail]

    wordPair b a =
      let (bSpans, aSpans) = diffWords b a
       in [(Removed, bSpans), (Added, aSpans)]

-- | Word-level diff between two single lines, returning the annotated
-- spans for the "before" and "after" side respectively. Consecutive
-- matching\/differing tokens are merged into one span each (that's what
-- 'getGroupedDiff' already does at the token level), so e.g. renaming
-- @outer$31@ to @outer$40@ highlights just the number, not the whole line.
diffWords :: Text -> Text -> ([Span], [Span])
diffWords before after = (concatMap toBefore grouped, concatMap toAfter grouped)
  where
    grouped = getGroupedDiff (tokenize before) (tokenize after)
    toBefore = \case
      Both ts _ -> [Span Unchanged (T.concat ts)]
      First ts -> [Span Removed (T.concat ts)]
      Second _ -> []
    toAfter = \case
      Both _ ts -> [Span Unchanged (T.concat ts)]
      First _ -> []
      Second ts -> [Span Added (T.concat ts)]

-- | Splits a line into maximal runs of identifier characters, whitespace,
-- and punctuation, e.g. @"outer$31 ~ apply$53"@ becomes
-- @["outer$31", " ", "~", " ", "apply$53"]@. Concatenating the tokens
-- reconstructs the original line exactly, so this is safe to diff and
-- rejoin without corrupting content.
tokenize :: Text -> [Text]
tokenize = T.groupBy (\a b -> classify a == classify b)

data CharClass = SpaceC | IdentC | PunctC
  deriving stock (Eq)

classify :: Char -> CharClass
classify c
  | isSpace c = SpaceC
  | isAlphaNum c || c `elem` ("_$#'" :: String) = IdentC
  | otherwise = PunctC
