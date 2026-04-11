{-# LANGUAGE OverloadedStrings #-}

-- | Lightweight LSP server loop over stdin/stdout.
module Malgo.LSP.Server
  ( ServerEnv (..),
    runServer,
    publishDiagnostics,
    getFileContent,
    updateFileContent,
    removeFileContent,
    logMessage,
  )
where

import Data.Aeson (Value (..), object, (.=))
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import Malgo.LSP.Server.JsonRpc
import System.IO (Handle, hPutStrLn, stderr, stdin, stdout)
import Prelude

-- | The server environment, accessible to all handlers.
data ServerEnv = ServerEnv
  { envStdout :: Handle,
    envVfs :: IORef (Map LSP.NormalizedUri Text),
    envLogger :: Text -> IO ()
  }

-- | Run the LSP server. Returns an exit code.
runServer :: (ServerEnv -> JsonRpcMessage -> IO ()) -> IO Int
runServer dispatch = do
  vfs <- newIORef Map.empty
  let logger msg = hPutStrLn stderr (T.unpack msg)
      env = ServerEnv {envStdout = stdout, envVfs = vfs, envLogger = logger}
  serverLoop env dispatch
  pure 0

-- | Main message dispatch loop.
serverLoop :: ServerEnv -> (ServerEnv -> JsonRpcMessage -> IO ()) -> IO ()
serverLoop env dispatch = go False
  where
    go shutdown = do
      mMsg <- readMessage stdin
      case mMsg of
        Nothing -> pure ()
        Just msg -> case msg.method of
          "initialize" -> do
            handleInitialize env msg
            go shutdown
          "initialized" -> go shutdown
          "shutdown" -> do
            case msg.id of
              Just reqId -> sendResponse env.envStdout reqId Null
              Nothing -> pure ()
            go True
          "exit" -> pure ()
          _ ->
            if shutdown
              then pure ()
              else do
                dispatch env msg
                go shutdown

-- | Respond to the initialize request with server capabilities.
handleInitialize :: ServerEnv -> JsonRpcMessage -> IO ()
handleInitialize env msg = do
  let capabilities =
        object
          [ "capabilities"
              .= object
                [ "textDocumentSync"
                    .= object
                      [ "openClose" .= True,
                        "change" .= (1 :: Int),
                        "willSave" .= False,
                        "willSaveWaitUntil" .= False,
                        "save" .= object ["includeText" .= False]
                      ],
                  "hoverProvider" .= True
                ]
          ]
  case msg.id of
    Just reqId -> sendResponse env.envStdout reqId capabilities
    Nothing -> pure ()

-- | Send textDocument/publishDiagnostics notification.
publishDiagnostics :: ServerEnv -> LSP.NormalizedUri -> [LSP.Diagnostic] -> IO ()
publishDiagnostics env nuri diags =
  sendNotification env.envStdout "textDocument/publishDiagnostics" $
    object
      [ "uri" .= LSP.fromNormalizedUri nuri,
        "diagnostics" .= diags
      ]

-- | Get file content from the VFS.
getFileContent :: ServerEnv -> LSP.NormalizedUri -> IO (Maybe Text)
getFileContent env nuri = Map.lookup nuri <$> readIORef env.envVfs

-- | Store or update file content in the VFS.
updateFileContent :: ServerEnv -> LSP.NormalizedUri -> Text -> IO ()
updateFileContent env nuri content =
  modifyIORef' env.envVfs (Map.insert nuri content)

-- | Remove file content from the VFS.
removeFileContent :: ServerEnv -> LSP.NormalizedUri -> IO ()
removeFileContent env nuri =
  modifyIORef' env.envVfs (Map.delete nuri)

-- | Log a message to stderr.
logMessage :: ServerEnv -> Text -> IO ()
logMessage env = env.envLogger
