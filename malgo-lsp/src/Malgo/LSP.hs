{-# LANGUAGE OverloadedStrings #-}

-- | LSP server entry point for Malgo.
module Malgo.LSP (runLSP) where

import Colog.Core (LogAction, WithSeverity (..), cmap, hoistLogAction, logStringStderr)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server
import Malgo.LSP.Handlers (LspState (..), lspHandlers)
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

-- | Text document sync options: full-document updates.
syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { LSP._openClose = Just True,
      LSP._change = Just LSP.TextDocumentSyncKind_Full,
      LSP._willSave = Just False,
      LSP._willSaveWaitUntil = Just False,
      LSP._save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
    }

-- | Run the Malgo LSP server over stdin/stdout.
runLSP :: IO Int
runLSP = do
  db <- newDatabase
  renamedCache <- newIORef Map.empty
  let state = LspState {db, flag = lspFlag, renamedCache}

  -- Server infrastructure loggers (LspServerLog type)
  let ioLogger :: LogAction IO (WithSeverity LspServerLog)
      ioLogger = cmap show logStringStderr

      lspLogger :: LogAction (LspM LspState) (WithSeverity LspServerLog)
      lspLogger = hoistLogAction liftIO ioLogger

  -- Application-level logger for user-facing diagnostics (Text type)
  let appLogger :: LogAction (LspM LspState) (WithSeverity T.Text)
      appLogger = defaultClientLogger

  runServerWithHandles
    ioLogger
    lspLogger
    stdin
    stdout
    ServerDefinition
      { defaultConfig = state,
        configSection = "malgo",
        parseConfig = \oldCfg _ -> Right oldCfg,
        onConfigChange = const $ pure (),
        doInitialize = \env _req -> pure (Right env),
        staticHandlers = \_caps -> lspHandlers appLogger state,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions {optTextDocumentSync = Just syncOptions}
      }
