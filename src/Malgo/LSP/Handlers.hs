{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | LSP request and notification handlers for the Malgo language server.
module Malgo.LSP.Handlers
  ( LspState (..),
    lspHandlers,
  )
where

import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Control.Lens hiding (Iso)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error, runError)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local (State, evalState)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)
import Malgo.Features (Features, runFeatures)
import Malgo.Id (Id (..))
import Malgo.LSP.Diagnostics (compileToDiagnostics, rangeToLsp)
import Malgo.Module (ModuleName (..), Pragma, Workspace, moduleNameToString, runWorkspaceOnPwd)
import Malgo.Pass (CompileError)
import Malgo.Prelude
import Malgo.Query
import Malgo.Query.Database (Database)
import Malgo.Query.Engine (runQueryDB)
import Malgo.Syntax qualified as Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec.Pos (sourceColumn, sourceLine, unPos)

-- | Persistent state shared across all LSP requests.
data LspState = LspState
  { db :: Database,
    flag :: Flag,
    -- | Maps each open file's URI to its last successfully renamed module.
    renamedCache :: IORef (Map LSP.NormalizedUri (ModuleName, Syntax.Module (Malgo Rename)))
  }

-- | Run a query-based compilation, returning diagnostics on error.
runLspCompile ::
  LspState ->
  Eff
    '[ QueryDB,
       State Pragma,
       Features,
       State Uniq,
       Reader Flag,
       Error CompileError,
       Workspace,
       IOE
     ]
    a ->
  IO (Either [LSP.Diagnostic] a)
runLspCompile LspState {db, flag} action = do
  result <-
    runEff $ runWorkspaceOnPwd do
      fmap (either (Left . snd) Right)
        . runError @CompileError
        . runReader flag
        . evalState (Uniq 0)
        . runFeatures mempty
        . evalState @Pragma mempty
        $ runQueryDB db action
  pure $ case result of
    Left err -> Left (compileToDiagnostics err)
    Right x -> Right x

-- | Check a file: register the source, run up to rename, publish diagnostics.
-- Stores the renamed module in the cache on success.
checkFile ::
  LspState ->
  LSP.NormalizedUri ->
  FilePath ->
  TL.Text ->
  LspM LspState [LSP.Diagnostic]
checkFile state nuri filePath content = do
  let modName = ModuleName (T.pack filePath)
  result <- liftIO $ runLspCompile state do
    updateSource modName filePath content
    invalidateModule modName
    (renamedMod, _rnState) <- fetch (RenamedModule modName)
    pure (modName, renamedMod)
  case result of
    Left diags -> do
      liftIO $ modifyIORef state.renamedCache $ Map.delete nuri
      pure diags
    Right (mn, renamedMod) -> do
      liftIO $ modifyIORef state.renamedCache $ Map.insert nuri (mn, renamedMod)
      pure []

-- | All LSP handlers.
lspHandlers ::
  (m ~ LspM LspState) =>
  LogAction m (WithSeverity T.Text) ->
  LspState ->
  Handlers m
lspHandlers logger state =
  mconcat
    [ notificationHandler LSP.SMethod_TextDocumentDidOpen $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument
            uri = doc ^. LSP.uri
            nuri = LSP.toNormalizedUri uri
            content = TL.fromStrict $ doc ^. LSP.text
        case LSP.uriToFilePath uri of
          Nothing ->
            logger <& ("Could not resolve path for URI: " <> T.pack (show uri)) `WithSeverity` Warning
          Just filePath -> do
            logger <& ("didOpen: " <> T.pack filePath) `WithSeverity` Info
            diags <- checkFile state nuri filePath content
            publishDiagnostics 100 nuri Nothing (partitionBySource diags),
      notificationHandler LSP.SMethod_TextDocumentDidChange $ \msg -> do
        let uri = msg ^. LSP.params . LSP.textDocument . LSP.uri
            nuri = LSP.toNormalizedUri uri
        case LSP.uriToFilePath uri of
          Nothing ->
            logger <& ("Could not resolve path for URI: " <> T.pack (show uri)) `WithSeverity` Warning
          Just filePath -> do
            mVf <- getVirtualFile nuri
            case mVf of
              Nothing ->
                logger <& ("No VFS entry for: " <> T.pack filePath) `WithSeverity` Warning
              Just vf -> do
                let content = TL.fromStrict $ virtualFileText vf
                logger <& ("didChange: " <> T.pack filePath) `WithSeverity` Info
                diags <- checkFile state nuri filePath content
                publishDiagnostics 100 nuri Nothing (partitionBySource diags),
      requestHandler LSP.SMethod_TextDocumentHover $ \req responder -> do
        let pos = req ^. LSP.params . LSP.position
            nuri = LSP.toNormalizedUri $ req ^. LSP.params . LSP.textDocument . LSP.uri
        cache <- liftIO $ readIORef state.renamedCache
        responder $ Right $ case Map.lookup nuri cache of
          Nothing -> LSP.InR LSP.Null
          Just (_, renamedMod) ->
            case findIdAtPos pos renamedMod of
              Nothing -> LSP.InR LSP.Null
              Just (r, idInfo) ->
                LSP.InL
                  $ LSP.Hover
                    { _contents =
                        LSP.InL
                          $ LSP.MarkupContent
                            LSP.MarkupKind_Markdown
                            (hoverText idInfo),
                      _range = Just (rangeToLsp r)
                    }
    ]

-- | Render hover markdown for an identifier.
hoverText :: Id -> T.Text
hoverText Id {name = n, moduleName = mn} =
  "**`" <> n <> "`**" <> " *(from " <> moduleNameToString mn <> ")*"

-- | Find the tightest-ranging identifier at the given LSP cursor position.
findIdAtPos :: LSP.Position -> Syntax.Module (Malgo Rename) -> Maybe (Range, Id)
findIdAtPos pos mod_ =
  case filter (posInRange pos . fst) (collectIds mod_) of
    [] -> Nothing
    xs -> Just $ minimumBy comparingRangeSize xs
  where
    comparingRangeSize (r1, _) (r2, _) = rangeSize r1 `compare` rangeSize r2
    rangeSize r =
      let sl = unPos (sourceLine r._start)
          sc = unPos (sourceColumn r._start)
          el = unPos (sourceLine r._end)
          ec = unPos (sourceColumn r._end)
       in (el - sl) * 10000 + (ec - sc)

-- | Test whether an LSP position (0-indexed) falls within a Malgo Range (1-indexed).
posInRange :: LSP.Position -> Range -> Bool
posInRange (LSP.Position line char) r =
  let sl = fromIntegral (unPos (sourceLine r._start)) - 1
      sc = fromIntegral (unPos (sourceColumn r._start)) - 1
      el = fromIntegral (unPos (sourceLine r._end)) - 1
      ec = fromIntegral (unPos (sourceColumn r._end)) - 1
   in (line > sl || (line == sl && char >= sc))
        && (line < el || (line == el && char <= ec))

-- | Collect all (Range, Id) pairs for identifiers referenced in the renamed module.
collectIds :: Syntax.Module (Malgo Rename) -> [(Range, Id)]
collectIds mod_ = collectBindGroup mod_.moduleDefinition

collectBindGroup :: Syntax.BindGroup (Malgo Rename) -> [(Range, Id)]
collectBindGroup bg = concatMap (concatMap collectScDef) bg._scDefs

collectScDef :: Syntax.ScDef (Malgo Rename) -> [(Range, Id)]
collectScDef (_, _, expr) = collectExpr expr

collectExpr :: Syntax.Expr (Malgo Rename) -> [(Range, Id)]
collectExpr = \case
  Syntax.Var range id_ -> [(range, id_)]
  Syntax.Unboxed {} -> []
  Syntax.Apply _ e1 e2 -> collectExpr e1 <> collectExpr e2
  Syntax.OpApp (opRange, _) opId e1 e2 ->
    (opRange, opId) : collectExpr e1 <> collectExpr e2
  Syntax.Project _ e _ -> collectExpr e
  Syntax.Fn _ clauses -> concatMap collectClause (toList clauses)
  Syntax.Tuple _ es -> concatMap collectExpr es
  Syntax.Record _ kvs -> concatMap (collectExpr . snd) kvs
  Syntax.Ann _ e _ -> collectExpr e
  Syntax.Seq _ stmts -> concatMap collectStmt (toList stmts)
  Syntax.Parens _ e -> collectExpr e
  Syntax.Codata _ clauses -> concatMap (collectExpr . snd) clauses
  Syntax.Label range labelId body -> (range, labelId) : collectExpr body
  Syntax.Goto _ val label -> collectExpr val <> collectExpr label

collectClause :: Syntax.Clause (Malgo Rename) -> [(Range, Id)]
collectClause (Syntax.Clause _ pats body) =
  concatMap collectPat (toList pats) <> collectExpr body

collectPat :: Syntax.Pat (Malgo Rename) -> [(Range, Id)]
collectPat = \case
  Syntax.VarP {} -> []
  Syntax.ConP conRange conId pats -> (conRange, conId) : concatMap collectPat pats
  Syntax.TupleP _ pats -> concatMap collectPat pats
  Syntax.RecordP _ kps -> concatMap (collectPat . snd) kps
  Syntax.UnboxedP {} -> []

collectStmt :: Syntax.Stmt (Malgo Rename) -> [(Range, Id)]
collectStmt = \case
  Syntax.Let _ _ e -> collectExpr e
  Syntax.NoBind _ e -> collectExpr e
