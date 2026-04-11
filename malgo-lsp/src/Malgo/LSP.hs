{-# LANGUAGE OverloadedStrings #-}

-- | LSP server entry point for Malgo.
module Malgo.LSP (runLSP) where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import Malgo.LSP.Handlers (LspState (..), handleDidChange, handleDidClose, handleDidOpen, handleHover)
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
      useInfer = False
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
      Just reqId -> sendResponse env.envStdout reqId Aeson.Null
      Nothing -> pure ()
    Just (uri, pos) -> do
      let nuri = LSP.toNormalizedUri uri
      result <- handleHover state nuri pos
      case msg.id of
        Just reqId -> sendResponse env.envStdout reqId result
        Nothing -> pure ()
  _ -> logMessage env ("Unknown method: " <> msg.method)

-- | Parse didOpen params: extract URI and text content.
parseDidOpen :: JsonRpcMessage -> Maybe (LSP.Uri, T.Text)
parseDidOpen msg = msg.params >>= Aeson.parseMaybe parser
  where
    parser = Aeson.withObject "params" $ \o -> do
      td <- o .: "textDocument"
      Aeson.withObject "textDocument" (\td' -> (,) <$> td' .: "uri" <*> td' .: "text") td

-- | Parse didChange params: extract URI and optional full text.
parseDidChange :: JsonRpcMessage -> Maybe (LSP.Uri, Maybe T.Text)
parseDidChange msg = msg.params >>= Aeson.parseMaybe parser
  where
    parser = Aeson.withObject "params" $ \o -> do
      td <- o .: "textDocument"
      uri <- Aeson.withObject "textDocument" (\td' -> td' .: "uri") td
      changes <- o .: "contentChanges"
      let mText = case changes of
            Aeson.Array arr
              | (first : _) <- toList arr ->
                  Aeson.parseMaybe (\v -> Aeson.withObject "change" (\c -> c .: "text") v) first
            _ -> Nothing
      pure (uri, mText)

-- | Parse didClose params: extract URI.
parseDidCloseUri :: JsonRpcMessage -> Maybe LSP.Uri
parseDidCloseUri msg = msg.params >>= Aeson.parseMaybe parser
  where
    parser = Aeson.withObject "params" $ \o -> do
      td <- o .: "textDocument"
      Aeson.withObject "textDocument" (\td' -> td' .: "uri") td

-- | Parse hover params: extract URI and position.
parseHover :: JsonRpcMessage -> Maybe (LSP.Uri, LSP.Position)
parseHover msg = msg.params >>= Aeson.parseMaybe parser
  where
    parser = Aeson.withObject "params" $ \o -> do
      td <- o .: "textDocument"
      uri <- Aeson.withObject "textDocument" (\td' -> td' .: "uri") td
      pos <- o .: "position"
      pure (uri, pos)
