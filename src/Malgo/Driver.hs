-- | Malgo.Driver is the entry point of `malgo to-ll`.
module Malgo.Driver (compile, compileFromAST, withDump) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Reader.Static
import Effectful.State.Static.Local (State)
import Malgo.Backend.Scheme (SchemePass (..))
import Malgo.Features
import Malgo.Module
import Malgo.Parser (ParserPass (..))
import Malgo.Pass (CompileError, Pass (..), runCompileError)
import Malgo.Prelude
import Malgo.Query
import Malgo.Query.Database
import Malgo.Query.Engine
import Malgo.Sequent.BigStepEval (BigStepEvalPass (..))
import Malgo.Sequent.Eval (EvalPass (..), Handlers (..))
import Malgo.Syntax qualified as Syntax
import Malgo.Syntax.Extension
import System.IO (hPutChar)
import System.IO qualified as IO

-- | `withDump` is the wrapper for check `dump` flag and output dump if that flag is `True`.
withDump ::
  (MonadIO m, Pretty a) =>
  -- | `dump` flag.
  Bool ->
  -- | Header of dump.
  String ->
  -- | The pass (e.g. `Malgo.Rename.Pass.rename rnEnv parsedAst`)
  m a ->
  m a
withDump isDump label m = do
  result <- m
  when isDump do
    hPutStrLn stderr label
    hPrint stderr $ pretty result
  pure result

-- | Compile the parsed AST.
compileFromAST ::
  ( Reader Flag :> es,
    Error CompileError :> es,
    IOE :> es,
    State Uniq :> es,
    Workspace :> es,
    Features :> es
  ) =>
  ArtifactPath ->
  Syntax.Module (Malgo Parse) ->
  Eff es ()
compileFromAST srcPath parsedAst = do
  flags <- ask @Flag
  let modName = parsedAst.moduleName
  registerModule modName srcPath
  db <- liftIO newDatabase
  -- Pre-populate the parse cache so the query engine reuses this result.
  liftIO $ modifyIORef db.cacheParsedModule $ Map.insert modName parsedAst
  core <- runQueryDB db $ fetch (LinkedProgram modName)
  case flags.target of
    TargetScheme -> do
      schemeCode <- runPass SchemePass core
      liftIO $ putStr $ convertString schemeCode
    TargetEval -> do
      let stdin = fmap Just getChar `catch` \(_ :: IOException) -> pure Nothing
      let stdout = putChar
      let stderr = hPutChar IO.stderr
      case flags.evalMode of
        EvalBigStep -> runPass BigStepEvalPass (modName, Handlers {..}, core)
        EvalSmallStep -> runPass EvalPass (modName, Handlers {..}, core)

-- | Read the source file and parse it, then compile.
compile ::
  ( Reader Flag :> es,
    IOE :> es,
    State Uniq :> es,
    Workspace :> es,
    Features :> es
  ) =>
  FilePath ->
  Eff es ()
compile srcPath = do
  flags <- ask @Flag
  pwd <- pwdPath
  srcModulePath <- parseArtifactPath pwd srcPath
  src <- liftIO $ BS.readFile srcPath
  save srcModulePath ".mlg" src
  runCompileError do
    parsedAst <- runPass ParserPass (srcPath, convertString @BS.ByteString src)
    when flags.debugMode do
      hPutStrLn stderr "=== PARSE ==="
      hPrint stderr $ pretty parsedAst
    runReader parsedAst.moduleName
      $ compileFromAST srcModulePath parsedAst
