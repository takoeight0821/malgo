-- | Malgo.Driver is the entry point of `malgo to-ll`.
module Malgo.Driver (compile, compileFromAST, compileToExecutable, withDump) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Reader.Static
import Effectful.State.Static.Local (State)
import Malgo.Backend.Scheme (SchemePass (..))
import Malgo.Backend.Zig (ZigPass (..))
import Malgo.Backend.Zig.Toolchain qualified as Zig
import Malgo.Features
import Malgo.Module
import Malgo.Parser (ParserPass (..))
import Malgo.Pass (CompileError, Pass (..), runCompileError)
import Malgo.Prelude
import Malgo.Query
import Malgo.Query.Database
import Malgo.Query.Engine
import Malgo.Sequent.BigStepEval (BigStepEvalPass (..))
import Malgo.Sequent.Core.Join qualified as Join
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
  core <- fetchLinkedCore srcPath parsedAst
  case flags.target of
    TargetScheme -> do
      schemeCode <- runReader modName $ runPass SchemePass core
      liftIO $ putStr $ convertString schemeCode
    TargetZig -> do
      zigCode <- runReader modName $ runPass ZigPass core
      liftIO $ putStr $ convertString zigCode
    TargetEval -> do
      let stdin = fmap Just getChar `catch` \(_ :: IOException) -> pure Nothing
      let stdout = putChar
      let stderr = hPutChar IO.stderr
      let arguments = flags.programArgs
      case flags.evalMode of
        EvalBigStep -> runPass BigStepEvalPass (modName, Handlers {..}, core)
        EvalSmallStep -> runPass EvalPass (modName, Handlers {..}, core)

-- | Register the module and run the query pipeline through linking --
-- shared by every 'compileFromAST' target and by 'compileToExecutable'.
fetchLinkedCore ::
  ( Reader Flag :> es,
    Error CompileError :> es,
    IOE :> es,
    State Uniq :> es,
    Workspace :> es,
    Features :> es
  ) =>
  ArtifactPath ->
  Syntax.Module (Malgo Parse) ->
  Eff es Join.Program
fetchLinkedCore srcPath parsedAst = do
  let modName = parsedAst.moduleName
  registerModule modName srcPath
  db <- liftIO newDatabase
  -- Pre-populate the parse cache so the query engine reuses this result.
  liftIO $ modifyIORef db.cacheParsedModule $ Map.insert modName parsedAst
  runQueryDB db $ fetch (LinkedProgram modName)

-- | Compile a source file to Zig, then invoke the @zig@ toolchain to
-- produce a native executable at @outPath@. Used by the @malgo compile@
-- subcommand (as opposed to @malgo eval --target zig@, which just prints
-- generated Zig source to stdout via 'compileFromAST').
compileToExecutable ::
  ( Reader Flag :> es,
    IOE :> es,
    State Uniq :> es,
    Workspace :> es,
    Features :> es
  ) =>
  FilePath ->
  FilePath ->
  Zig.OptMode ->
  Eff es ()
compileToExecutable srcPath outPath optMode = do
  srcModulePath <- parseArtifactPathFromPwd srcPath
  src <- liftIO $ BS.readFile srcPath
  save srcModulePath ".mlg" src
  zigText <- runCompileError do
    parsedAst <- runPass ParserPass (srcPath, convertString @BS.ByteString src)
    core <- fetchLinkedCore srcModulePath parsedAst
    runReader parsedAst.moduleName $ runPass ZigPass core
  save srcModulePath ".zig" (convertString @Text @BS.ByteString zigText)
  workspace <- getWorkspace
  let zigPath = outPath <> ".zig"
  liftIO $ BS.writeFile zigPath (convertString zigText)
  liftIO $ Zig.buildExecutable workspace zigPath outPath optMode

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
  srcModulePath <- parseArtifactPathFromPwd srcPath
  src <- liftIO $ BS.readFile srcPath
  save srcModulePath ".mlg" src
  runCompileError do
    parsedAst <- runPass ParserPass (srcPath, convertString @BS.ByteString src)
    when flags.debugMode do
      hPutStrLn stderr "=== PARSE ==="
      hPrint stderr $ pretty parsedAst
    runReader parsedAst.moduleName
      $ compileFromAST srcModulePath parsedAst
