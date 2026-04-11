{-# LANGUAGE OverloadedStrings #-}

-- | Minimal JSON value type with encoder and decoder.
-- Replaces aeson for lightweight JSON-RPC handling.
module Malgo.LSP.Json
  ( JValue (..),
    encodeJson,
    decodeJson,
    jObject,
    (.=),
    jLookup,
    jLookupMaybe,
    jText,
    jInt,
    jBool,
    jArray,
    jFields,
  )
where

import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr, digitToInt, isDigit, isHexDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Prelude

-- | Minimal JSON value representation.
data JValue
  = JNull
  | JBool Bool
  | JNumber Double
  | JString Text
  | JArray [JValue]
  | JObject [(Text, JValue)]
  deriving stock (Show, Eq)

-- | Build a JSON object from key-value pairs.
jObject :: [(Text, JValue)] -> JValue
jObject = JObject

-- | Create a key-value pair for a JSON object.
(.=) :: Text -> JValue -> (Text, JValue)
(.=) = (,)

infixr 8 .=

-- | Look up a required field in a JSON object.
jLookup :: Text -> JValue -> Maybe JValue
jLookup key (JObject kvs) = lookup key kvs
jLookup _ _ = Nothing

-- | Look up an optional field in a JSON object (returns JNull if missing).
jLookupMaybe :: Text -> JValue -> Maybe JValue
jLookupMaybe key (JObject kvs) = Just $ maybe JNull id (lookup key kvs)
jLookupMaybe _ _ = Nothing

-- | Extract a Text from a JString.
jText :: JValue -> Maybe Text
jText (JString t) = Just t
jText _ = Nothing

-- | Extract an Int from a JNumber.
jInt :: JValue -> Maybe Int
jInt (JNumber n) = Just (round n)
jInt _ = Nothing

-- | Extract a Bool from a JBool.
jBool :: JValue -> Maybe Bool
jBool (JBool b) = Just b
jBool _ = Nothing

-- | Extract a list from a JArray.
jArray :: JValue -> Maybe [JValue]
jArray (JArray xs) = Just xs
jArray _ = Nothing

-- | Extract fields from a JObject.
jFields :: JValue -> Maybe [(Text, JValue)]
jFields (JObject kvs) = Just kvs
jFields _ = Nothing

-- * Encoder

-- | Encode a JValue to a lazy ByteString.
encodeJson :: JValue -> BL.ByteString
encodeJson = B.toLazyByteString . buildValue

buildValue :: JValue -> B.Builder
buildValue JNull = "null"
buildValue (JBool True) = "true"
buildValue (JBool False) = "false"
buildValue (JNumber n)
  | isInfinite n || isNaN n = "null"
  | n == fromIntegral (round n :: Int) = B.intDec (round n)
  | otherwise = B.doubleDec n
buildValue (JString t) = buildString t
buildValue (JArray xs) =
  B.char7 '[' <> mconcat (intersperse (B.char7 ',') (map buildValue xs)) <> B.char7 ']'
buildValue (JObject kvs) =
  B.char7 '{' <> mconcat (intersperse (B.char7 ',') (map buildKV kvs)) <> B.char7 '}'
  where
    buildKV (k, v) = buildString k <> B.char7 ':' <> buildValue v

buildString :: Text -> B.Builder
buildString t = B.char7 '"' <> T.foldl' (\acc c -> acc <> escapeChar c) mempty t <> B.char7 '"'

escapeChar :: Char -> B.Builder
escapeChar '"' = "\\\""
escapeChar '\\' = "\\\\"
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar c
  | c < ' ' = "\\u" <> B.word16HexFixed (fromIntegral (fromEnum c))
  | otherwise = B.charUtf8 c

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x : xs) = x : sep : intersperse sep xs

-- * Decoder

-- | Decode a strict ByteString to a JValue.
decodeJson :: BS.ByteString -> Maybe JValue
decodeJson bs = case parseValue (BS.unpack bs) of
  Just (v, rest) | all (`elem` (" \t\n\r" :: String)) rest -> Just v
  _ -> Nothing

parseValue :: String -> Maybe (JValue, String)
parseValue s = case dropWhile (`elem` (" \t\n\r" :: String)) s of
  'n' : 'u' : 'l' : 'l' : rest -> Just (JNull, rest)
  't' : 'r' : 'u' : 'e' : rest -> Just (JBool True, rest)
  'f' : 'a' : 'l' : 's' : 'e' : rest -> Just (JBool False, rest)
  '"' : rest -> parseString rest
  '[' : rest -> parseArray rest
  '{' : rest -> parseObject rest
  s'@(c : _) | c == '-' || isDigit c -> parseNumber s'
  _ -> Nothing

parseString :: String -> Maybe (JValue, String)
parseString = go []
  where
    go acc ('"' : rest) = Just (JString (T.pack (reverse acc)), rest)
    go acc ('\\' : '"' : rest) = go ('"' : acc) rest
    go acc ('\\' : '\\' : rest) = go ('\\' : acc) rest
    go acc ('\\' : '/' : rest) = go ('/' : acc) rest
    go acc ('\\' : 'n' : rest) = go ('\n' : acc) rest
    go acc ('\\' : 'r' : rest) = go ('\r' : acc) rest
    go acc ('\\' : 't' : rest) = go ('\t' : acc) rest
    go acc ('\\' : 'b' : rest) = go ('\b' : acc) rest
    go acc ('\\' : 'f' : rest) = go ('\f' : acc) rest
    go acc ('\\' : 'u' : a : b : c : d : rest)
      | all isHexDigit [a, b, c, d] =
          let code = digitToInt a * 4096 + digitToInt b * 256 + digitToInt c * 16 + digitToInt d
           in go (chr code : acc) rest
    go acc (c : rest) = go (c : acc) rest
    go _ [] = Nothing

parseNumber :: String -> Maybe (JValue, String)
parseNumber s =
  let (numStr, rest) = span (\c -> isDigit c || c == '.' || c == 'e' || c == 'E' || c == '+' || c == '-') s
   in case reads numStr :: [(Double, String)] of
        [(n, "")] -> Just (JNumber n, rest)
        _ -> Nothing

parseArray :: String -> Maybe (JValue, String)
parseArray s = case dropWhile (`elem` (" \t\n\r" :: String)) s of
  ']' : rest -> Just (JArray [], rest)
  _ -> go [] s
  where
    go acc s' = case parseValue s' of
      Just (v, rest) -> case dropWhile (`elem` (" \t\n\r" :: String)) rest of
        ',' : rest' -> go (v : acc) rest'
        ']' : rest' -> Just (JArray (reverse (v : acc)), rest')
        _ -> Nothing
      Nothing -> Nothing

parseObject :: String -> Maybe (JValue, String)
parseObject s = case dropWhile (`elem` (" \t\n\r" :: String)) s of
  '}' : rest -> Just (JObject [], rest)
  _ -> go [] s
  where
    go acc s' = case parseKey (dropWhile (`elem` (" \t\n\r" :: String)) s') of
      Just (key, rest) -> case dropWhile (`elem` (" \t\n\r" :: String)) rest of
        ':' : rest' -> case parseValue rest' of
          Just (v, rest'') -> case dropWhile (`elem` (" \t\n\r" :: String)) rest'' of
            ',' : rest''' -> go ((key, v) : acc) rest'''
            '}' : rest''' -> Just (JObject (reverse ((key, v) : acc)), rest''')
            _ -> Nothing
          Nothing -> Nothing
        _ -> Nothing
      Nothing -> Nothing

    parseKey ('"' : rest) = case parseString rest of
      Just (JString k, rest') -> Just (k, rest')
      _ -> Nothing
    parseKey _ = Nothing
