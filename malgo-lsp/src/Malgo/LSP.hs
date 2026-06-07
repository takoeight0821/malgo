{-# LANGUAGE OverloadedStrings #-}

-- | LSP server entry point for Malgo.
module Malgo.LSP (runLSP) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Malgo.LSP.Handlers (LspState (..), handleDidChange, handleDidClose, handleDidOpen, handleHover)
import Malgo.LSP.Json (JValue (..), jLookup, jText)
import Malgo.LSP.Protocol (LspPosition (..), Uri (..), toNormalizedUri)
import Malgo.LSP.Server (ServerEnv (..), logMessage, runServer)
import Malgo.LSP.Server.JsonRpc (JsonRpcMessage (..), sendResponse)
import Malgo.Prelude
import Malgo.Query.Database (newDatabase)

-- | Default compiler flags for LSP mode (parse + rename only, no codegen).
lspFlag :: Flag
lspFlag =
  Flag
    { noOptimize = False,
      lambdaLift = False,
      debugMode = False,
      testMode = False,
      target = TargetEval,
      evalMode = EvalSmallStep,
      useInfer = False,
      programArgs = []
    }

-- | Run the Malgo LSP server over stdin/stdout.
runLSP :: IO Int
runLSP = do
  db <- newDatabase
  renamedCache <- newIORef Map.empty
  let state = LspState {db, flag = lspFlag, renamedCache}
  runServer (dispatch state)

-- | Dispatch an incoming JSON-RPC message to the appropriate handler.
dispatch :: LspState -> ServerEnv -> JsonRpcMessage -> IO ()
dispatch state env msg = case msg.method of
  "textDocument/didOpen" -> case parseDidOpen msg of
    Nothing -> logMessage env "Failed to parse didOpen params"
    Just (uri, content) -> handleDidOpen env state uri content
  "textDocument/didChange" -> case parseDidChange msg of
    Nothing -> logMessage env "Failed to parse didChange params"
    Just (uri, mText) -> handleDidChange env state uri mText
  "textDocument/didClose" -> case parseDidCloseUri msg of
    Nothing -> logMessage env "Failed to parse didClose params"
    Just uri -> handleDidClose env state uri
  "textDocument/hover" -> case parseHover msg of
    Nothing -> case msg.id of
      Just reqId -> sendResponse env.envStdout reqId JNull
      Nothing -> pure ()
    Just (uri, pos) -> do
      let nuri = toNormalizedUri uri
      result <- handleHover state nuri pos
      case msg.id of
        Just reqId -> sendResponse env.envStdout reqId result
        Nothing -> pure ()
  _ -> logMessage env ("Unknown method: " <> msg.method)

-- | Parse didOpen params: extract URI and text content.
parseDidOpen :: JsonRpcMessage -> Maybe (Uri, T.Text)
parseDidOpen msg = do
  params_ <- msg.params
  td <- jLookup "textDocument" params_
  uriText <- jLookup "uri" td >>= jText
  text <- jLookup "text" td >>= jText
  pure (Uri uriText, text)

-- | Parse didChange params: extract URI and optional full text.
parseDidChange :: JsonRpcMessage -> Maybe (Uri, Maybe T.Text)
parseDidChange msg = do
  params_ <- msg.params
  td <- jLookup "textDocument" params_
  uriText <- jLookup "uri" td >>= jText
  let mText = do
        changes <- jLookup "contentChanges" params_
        arr <- case changes of
          JArray xs -> Just xs
          _ -> Nothing
        case arr of
          (first : _) -> jLookup "text" first >>= jText
          _ -> Nothing
  pure (Uri uriText, mText)

-- | Parse didClose params: extract URI.
parseDidCloseUri :: JsonRpcMessage -> Maybe Uri
parseDidCloseUri msg = do
  params_ <- msg.params
  td <- jLookup "textDocument" params_
  uriText <- jLookup "uri" td >>= jText
  pure (Uri uriText)

-- | Parse hover params: extract URI and position.
parseHover :: JsonRpcMessage -> Maybe (Uri, LspPosition)
parseHover msg = do
  params_ <- msg.params
  td <- jLookup "textDocument" params_
  uriText <- jLookup "uri" td >>= jText
  posVal <- jLookup "position" params_
  line <- jLookup "line" posVal >>= jInt
  char <- jLookup "character" posVal >>= jInt
  pure (Uri uriText, LspPosition line char)
  where
    jInt (JNumber n) = Just (round n)
    jInt _ = Nothing
