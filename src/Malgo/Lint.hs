-- | The Malgo linter: parses a source file and reports stylistic diagnostics.
-- Advisory only — it never rewrites code.
module Malgo.Lint
  ( lintParsed,
    lintFile,
  )
where

import Data.ByteString qualified as BS
import Effectful
import Effectful.Error.Static (runError)
import Malgo.Features (Features)
import Malgo.Lint.Diagnostic (Diagnostic)
import Malgo.Lint.Rule (Rule (..))
import Malgo.Lint.Rules (allRules)
import Malgo.Module (Workspace, parseArtifactPathFromPwd, save)
import Malgo.Parser (ParserPass (..))
import Malgo.Pass (CompileError, runPass)
import Malgo.Prelude
import Malgo.Syntax (Module (..), ParsedDefinitions (..))
import Malgo.Syntax.Extension (Malgo, MalgoPhase (Parse))

-- | Run the given rules over a parsed module, sorted by source position.
lintParsed :: [Rule (Malgo Parse)] -> Module (Malgo Parse) -> [Diagnostic]
lintParsed rules m =
  let ParsedDefinitions decls = m.moduleDefinition
   in sortWith range $ concatMap (\r -> r.run decls) rules

-- | Read, parse, and lint a source file with the default rule set. Mirrors the
-- parse stage of 'Malgo.Driver.compile' and stops there. Files that fail to
-- parse are skipped (with a note on stderr): parseability is enforced by the
-- build/eval jobs, not the linter.
lintFile ::
  ( IOE :> es,
    Workspace :> es,
    Features :> es
  ) =>
  FilePath ->
  Eff es [Diagnostic]
lintFile srcPath = do
  srcModulePath <- parseArtifactPathFromPwd srcPath
  src <- liftIO $ BS.readFile srcPath
  save srcModulePath ".mlg" src
  result <- runError @CompileError $ runPass ParserPass (srcPath, convertString @BS.ByteString src)
  case result of
    Left _ -> do
      hPutStrLn stderr $ "lint: skipping " <> srcPath <> " (parse error)"
      pure []
    Right parsed -> pure (lintParsed allRules parsed)
