{-# LANGUAGE OverloadedStrings #-}

-- | Minimal JSON-RPC 2.0 message framing over stdio.
module Malgo.LSP.Server.JsonRpc
  ( JsonRpcMessage (..),
    readMessage,
    sendResponse,
    sendNotification,
  )
where

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Malgo.LSP.Json (JValue (..), decodeJson, encodeJson, jObject, (.=))
import System.IO (Handle, hFlush, hSetBinaryMode)
import Prelude

-- | An incoming JSON-RPC message (request or notification).
data JsonRpcMessage = JsonRpcMessage
  { id :: Maybe JValue,
    method :: Text,
    params :: Maybe JValue
  }

-- | Parse a JsonRpcMessage from a JValue.
parseMessage :: JValue -> Maybe JsonRpcMessage
parseMessage (JObject kvs) = do
  methodVal <- lookup "method" kvs
  m <- case methodVal of
    JString t -> Just t
    _ -> Nothing
  let mid = lookup "id" kvs
      mparams = lookup "params" kvs
  Just JsonRpcMessage {id = mid, method = m, params = mparams}
parseMessage _ = Nothing

-- | Read one Content-Length framed JSON-RPC message from a handle.
-- Returns 'Nothing' on EOF.
readMessage :: Handle -> IO (Maybe JsonRpcMessage)
readMessage h = do
  hSetBinaryMode h True
  mLen <- readContentLength h
  case mLen of
    Nothing -> pure Nothing
    Just len -> do
      body <- BS.hGet h len
      if BS.null body
        then pure Nothing
        else pure $ decodeJson body >>= parseMessage

-- | Parse the Content-Length header from the input.
readContentLength :: Handle -> IO (Maybe Int)
readContentLength h = go
  where
    go = do
      line <- BS.hGetLine h
      let stripped = stripCR line
      if BS.null stripped
        then go
        else case BS.stripPrefix "Content-Length: " stripped of
          Just rest -> do
            let len = read (T.unpack (decodeUtf8 rest))
            skipUntilBlank h
            pure (Just len)
          Nothing -> go

    stripCR bs
      | BS.null bs = bs
      | BS.last bs == '\r' = BS.init bs
      | otherwise = bs

    decodeUtf8 = T.pack . map (toEnum . fromEnum) . BS.unpack

-- | Skip lines until we hit a blank line (the header/body separator).
skipUntilBlank :: Handle -> IO ()
skipUntilBlank h = do
  line <- BS.hGetLine h
  let stripped = stripCR line
  if BS.null stripped
    then pure ()
    else skipUntilBlank h
  where
    stripCR bs
      | BS.null bs = bs
      | BS.last bs == '\r' = BS.init bs
      | otherwise = bs

-- | Send a JSON-RPC response (for a request with an id).
sendResponse :: Handle -> JValue -> JValue -> IO ()
sendResponse h reqId result =
  sendMessage h $
    jObject
      [ "jsonrpc" .= JString "2.0",
        "id" .= reqId,
        "result" .= result
      ]

-- | Send a JSON-RPC notification (server -> client, no id).
sendNotification :: Handle -> Text -> JValue -> IO ()
sendNotification h method_ params_ =
  sendMessage h $
    jObject
      [ "jsonrpc" .= JString "2.0",
        "method" .= JString method_,
        "params" .= params_
      ]

-- | Encode a JSON value with Content-Length framing and write to handle.
sendMessage :: Handle -> JValue -> IO ()
sendMessage h val = do
  let body = encodeJson val
      len = BL.length body
      header = "Content-Length: " <> BL.pack (map (fromIntegral . fromEnum) (show len)) <> "\r\n\r\n"
  BL.hPut h header
  BL.hPut h body
  hFlush h
