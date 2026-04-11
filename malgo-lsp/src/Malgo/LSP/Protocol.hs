{-# LANGUAGE OverloadedStrings #-}

-- | Minimal LSP protocol types, replacing lsp-types.
-- Only the subset actually used by malgo-lsp.
module Malgo.LSP.Protocol
  ( Uri (..),
    NormalizedUri (..),
    toNormalizedUri,
    fromNormalizedUri,
    uriToFilePath,
    LspPosition (..),
    LspRange (..),
    Diagnostic (..),
    mkDiagnostic,
    encodeDiagnostic,
    encodeHover,
  )
where

import Data.Char (chr, digitToInt, isHexDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Malgo.LSP.Json (JValue (..), jObject, (.=))
import Prelude

-- * URI types

-- | An LSP document URI (e.g., @file:///path/to/file.mlg@).
newtype Uri = Uri {unUri :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- | A normalized URI, suitable for use as a Map key.
newtype NormalizedUri = NormalizedUri {unNormalizedUri :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- | Normalize a URI (lowercase the scheme and host).
toNormalizedUri :: Uri -> NormalizedUri
toNormalizedUri (Uri u) = NormalizedUri (T.toLower u)

-- | Convert back to a Uri.
fromNormalizedUri :: NormalizedUri -> Uri
fromNormalizedUri (NormalizedUri u) = Uri u

-- | Extract a file path from a file:// URI.
-- Returns 'Nothing' for non-file URIs.
uriToFilePath :: Uri -> Maybe FilePath
uriToFilePath (Uri u)
  | Just path <- T.stripPrefix "file://" u = Just (T.unpack (percentDecode path))
  | otherwise = Nothing

-- | Decode percent-encoded characters in a URI path.
percentDecode :: Text -> Text
percentDecode t = case T.uncons t of
  Nothing -> T.empty
  Just ('%', rest)
    | Just (a, rest') <- T.uncons rest,
      Just (b, rest'') <- T.uncons rest',
      isHexDigit a,
      isHexDigit b ->
        T.cons (chr (digitToInt a * 16 + digitToInt b)) (percentDecode rest'')
  Just (c, rest) -> T.cons c (percentDecode rest)

-- * Position and Range

-- | An LSP position (0-indexed line and character).
data LspPosition = LspPosition
  { _line :: Int,
    _character :: Int
  }
  deriving stock (Show, Eq)

-- | An LSP range (start and end positions).
data LspRange = LspRange
  { _start :: LspPosition,
    _end :: LspPosition
  }
  deriving stock (Show, Eq)

-- * Diagnostic

-- | An LSP diagnostic message.
data Diagnostic = Diagnostic
  { _range :: LspRange,
    _message :: Text,
    _source :: Maybe Text
  }
  deriving stock (Show, Eq)

-- | Create an error-severity diagnostic.
mkDiagnostic :: LspRange -> Text -> Diagnostic
mkDiagnostic r msg =
  Diagnostic
    { _range = r,
      _message = msg,
      _source = Just "malgo"
    }

-- * JSON encoding

encodePosition :: LspPosition -> JValue
encodePosition p =
  jObject
    [ "line" .= JNumber (fromIntegral p._line),
      "character" .= JNumber (fromIntegral p._character)
    ]

encodeRange :: LspRange -> JValue
encodeRange r =
  jObject
    [ "start" .= encodePosition r._start,
      "end" .= encodePosition r._end
    ]

-- | Encode a Diagnostic to JSON.
encodeDiagnostic :: Diagnostic -> JValue
encodeDiagnostic d =
  jObject
    [ "range" .= encodeRange d._range,
      "severity" .= JNumber 1,
      "source" .= maybe JNull JString d._source,
      "message" .= JString d._message
    ]

-- | Encode a hover response (markdown content with optional range) to JSON.
encodeHover :: Text -> Maybe LspRange -> JValue
encodeHover content mRange =
  jObject $
    [ "contents"
        .= jObject
          [ "kind" .= JString "markdown",
            "value" .= JString content
          ]
    ]
      ++ maybe [] (\r -> ["range" .= encodeRange r]) mRange
