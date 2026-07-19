import Malgo.Parser.CStyle
import Malgo.Module
import Malgo.Features

/-! Port of `src/Malgo/Parser/Wrapper.hs` (pragma extraction) and the
`ParserPass` entry in `src/Malgo/Parser.hs`.

The Haskell entry runs the parser inside the effect stack so pragma
features can be added and path imports resolved via IO mid-parse. The Lean
parser is pure, so `pass` does the IO around it: extract pragmas, build the
feature flags, run the pure parser, then resolve the `rawPath` module and
import names to `.artifact` against the workspace. -/

namespace Malgo.Parser

open Malgo
open Malgo.Syntax

/-- Port of `Wrapper.extractPragmas`: lines starting with `#`, with the
`#` dropped. -/
def extractPragmas (text : String) : List String :=
  (text.splitOn "\n").filterMap fun l =>
    if l.startsWith "#" then some (toString (l.drop 1)) else none

/-- Port of `Wrapper.filterKnownPragmas`. -/
def filterKnownPragmas (pragmas : List String) : List String :=
  pragmas.filter fun p =>
    p == "c-style-apply" || p == "malgo-2025" || p.startsWith "experimental-"

/-- Resolve a `rawPath` import against the module that mentions it. Other
module-name forms pass through unchanged. -/
private def resolveImportName (ws : Workspace) (from_ : ArtifactPath) :
    ModuleName → IO ModuleName
  | .rawPath path => .artifact <$> ws.parseArtifactPath from_ path
  | m => pure m

/-- Resolve `rawPath` names inside a declaration: the imported module and,
for an aliased import, the import list's module name. -/
private def resolveDecl (ws : Workspace) (from_ : ArtifactPath) :
    Decl .parse → IO (Decl .parse)
  | .«import» ext moduleName importList => do
    let moduleName ← resolveImportName ws from_ moduleName
    let importList ← match importList with
      | .«as» m => ImportList.«as» <$> resolveImportName ws from_ m
      | other => pure other
    return .«import» ext moduleName importList
  | d => pure d

/-- Resolve the module's own `rawPath` name, then every declaration's
`rawPath` names, against the workspace. -/
def resolveModule (ws : Workspace) (m : Module .parse) : IO (Module .parse) := do
  let moduleArtifact ← ws.parseArtifactPathFromPwd m.moduleName.toStr
  let decls ← m.moduleDefinition.decls.mapM (resolveDecl ws moduleArtifact)
  return { moduleName := .artifact moduleArtifact,
           moduleDefinition := (⟨decls⟩ : ParsedDefinitions .parse) }

/-- Parse a module: extract pragma features, run the pure C-style parser,
and resolve path/module names. Returns the parsed+resolved module (or the
parse error) alongside the module's feature flags. -/
def pass (ws : Workspace) (srcPath : System.FilePath) (text : String) :
    IO (Except PError (Module .parse) × FeatureFlags) := do
  let flags := (parseFeatures (filterKnownPragmas (extractPragmas text))).toOption.getD .empty
  match parseCStyle srcPath.toString text with
  | .error e => return (.error e, flags)
  | .ok m =>
    let resolved ← resolveModule ws m
    return (.ok resolved, flags)

end Malgo.Parser
