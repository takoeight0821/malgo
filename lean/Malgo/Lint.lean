import Malgo.Monad
import Malgo.Module
import Malgo.Parser
import Malgo.Lint.Diagnostic
import Malgo.Lint.Rule
import Malgo.Lint.Rules

/-! Port of `src/Malgo/Lint.hs`: the Malgo linter parses a source file and
reports stylistic diagnostics. Advisory only — it never rewrites code. -/

namespace Malgo.Lint

open Malgo Malgo.Syntax

/-- Run the given rules over a parsed module, sorted by source position. -/
def lintParsed (rules : List (Rule .parse)) (m : Module .parse) : List Diagnostic :=
  let decls := m.moduleDefinition.decls
  (rules.flatMap fun r => r.run decls).mergeSort fun a b => compare a.range b.range != .gt

/-- Read, parse, and lint a source file with the default rule set. Mirrors the
parse stage of `Malgo.Driver.compile` and stops there. Files that fail to parse
are skipped (with a note on stderr): parseability is enforced by the build/eval
jobs, not the linter. -/
def lintFile (srcPath : System.FilePath) : MalgoM (List Diagnostic) := do
  let ws ← getWorkspace
  let srcModulePath ← ws.parseArtifactPathFromPwd srcPath
  let src ← IO.FS.readBinFile srcPath
  Resource.save srcModulePath ".mlg" src
  let text ← IO.FS.readFile srcPath
  match ← Malgo.Parser.pass ws srcPath text with
  | (.error _, _) =>
    IO.eprintln s!"lint: skipping {srcPath} (parse error)"
    pure []
  | (.ok parsed, _) => pure (lintParsed allRules parsed)

end Malgo.Lint
