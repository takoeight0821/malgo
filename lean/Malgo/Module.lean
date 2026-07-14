import Std.Data.TreeMap
import Malgo.Prelude
import Malgo.Path
import Malgo.SExpr

/-! Port of `src/Malgo/Module.hs`: module names, the workspace mirror, and
the `Resource` artifact-serialization class.

Differences from Haskell (both per plan):
- the workspace directory honors `MALGO_WORK_DIR` and defaults to
  `.malgo-work-lean`, so both toolchains can share one checkout;
- `Resource` artifacts are not wire-compatible with Haskell's `binary`.
-/

namespace Malgo

structure ArtifactPath where
  rawPath : System.FilePath
  originPath : Path .abs .file
  relPath : Path .rel .file
  targetPath : Path .abs .file
  deriving Repr

namespace ArtifactPath

/-- Equality/ordering on `relPath` only, so paths reached via different
traversal routes compare equal (mirrors the Haskell instances). -/
instance : BEq ArtifactPath := ⟨fun a b => a.relPath == b.relPath⟩

instance : Ord ArtifactPath := ⟨fun a b => compare a.relPath b.relPath⟩

end ArtifactPath

inductive ModuleName where
  | moduleName (raw : String)
  | artifact (path : ArtifactPath)
  deriving BEq, Ord, Repr

namespace ModuleName

def toStr : ModuleName → String
  | .moduleName raw => raw
  | .artifact path => path.relPath.toFilePath.toString

/-- Basename of the module without extension; used in `Id` dumps. -/
def digest : ModuleName → String
  | .moduleName raw => raw
  | .artifact path =>
    match path.relPath.filename.splitExtension with
    | some (name, _) => name.toFilePath.toString
    | none => path.relPath.filename.toFilePath.toString

instance : Pretty ModuleName := ⟨toStr⟩

instance : ToSExpr ModuleName where
  toSExpr
    | .moduleName raw => .atom (.symbol raw)
    | .artifact path => .atom (.str path.relPath.toFilePath.toString)

instance : HasRange ModuleName where
  range m :=
    let pos := SourcePos.initial m.toStr
    { start := pos, stop := pos }

end ModuleName

/-- Port of the `Workspace` static effect: the `.malgo-work` mirror
directory plus the module-path cache. Carried inside `Ctx`. -/
structure Workspace where
  dir : Path .abs .dir
  moduleMap : IO.Ref (Std.TreeMap ModuleName ArtifactPath)

namespace Workspace

def workDirName : IO String := do
  return (← IO.getEnv "MALGO_WORK_DIR").getD ".malgo-work-lean"

/-- Port of `runWorkspaceOnPwd`: create the work dir under the current
directory and set up an empty module map. -/
def setup : IO Workspace := do
  let pwd ← IO.currentDir
  let raw := pwd / (← workDirName)
  IO.FS.createDirAll raw
  let abs ← IO.FS.realPath raw
  let dir ← IO.ofExcept (Path.parseAbsDir abs |>.mapError IO.userError)
  return { dir, moduleMap := ← IO.mkRef {} }

def registerModule (ws : Workspace) (name : ModuleName) (path : ArtifactPath) : IO Unit :=
  ws.moduleMap.modify (·.insert name path)

private def listSubDirectories (dir : System.FilePath) : IO (Array System.FilePath) := do
  let entries ← dir.readDir
  entries.filterMapM fun e => do
    if (← e.path.isDir) then return some e.path else return none

private def findFileIn (dirs : Array System.FilePath) (fileName : String) :
    IO (Option System.FilePath) := do
  for dir in dirs do
    let candidate := dir / fileName
    if (← candidate.pathExists) && !(← candidate.isDir) then
      return some candidate
  return none

/-- Resolve a path string relative to the directory containing the
workspace (the project root). Port of `parseArtifactPathFromPwd`. -/
def parseArtifactPathFromPwd (ws : Workspace) (path : System.FilePath) : IO ArtifactPath := do
  let pwd := ws.dir.parent
  let originRaw ← IO.FS.realPath (pwd.toFilePath / path)
  let originPath ← IO.ofExcept (Path.parseAbsFile originRaw |>.mapError IO.userError)
  let relPath ← IO.ofExcept (pwd.stripProperPrefix originPath |>.mapError IO.userError)
  return { rawPath := path, originPath, relPath, targetPath := ws.dir / relPath }

/-- Resolve a path string relative to the module that mentions it.
Port of `parseArtifactPath`. -/
def parseArtifactPath (ws : Workspace) (from_ : ArtifactPath) (path : System.FilePath) :
    IO ArtifactPath := do
  let base := from_.originPath.parent
  let originRaw ← IO.FS.realPath (base.toFilePath / path)
  let originPath ← IO.ofExcept (Path.parseAbsFile originRaw |>.mapError IO.userError)
  let originBase := ws.dir.parent
  let relPath ← IO.ofExcept (originBase.stripProperPrefix originPath |>.mapError IO.userError)
  return { rawPath := path, originPath, relPath, targetPath := ws.dir / relPath }

/-- Breadth-first search of the workspace mirror for `<name>.mlg`,
registering the result. Port of `searchAndRegister`. -/
private partial def searchAndRegister (ws : Workspace) (name : ModuleName) : IO ArtifactPath := do
  match name with
  | .artifact path => return path
  | .moduleName raw =>
    let fileName := raw ++ ".mlg"
    let rec search (dirs : Array System.FilePath) : IO System.FilePath := do
      if dirs.isEmpty then
        throw (IO.userError s!"Module not found: {raw}")
      match ← findFileIn dirs fileName with
      | some file => return file
      | none => search (← dirs.flatMapM listSubDirectories)
    let file ← search #[ws.dir.toFilePath]
    let rel := toString (file.toString.drop (ws.dir.toFilePath.toString.length + 1))
    let path ← ws.parseArtifactPathFromPwd rel
    ws.registerModule name path
    return path

def getModulePath (ws : Workspace) (name : ModuleName) : IO ArtifactPath := do
  match (← ws.moduleMap.get).get? name with
  | some path => return path
  | none => ws.searchAndRegister name

end Workspace

/-- Artifact (de)serialization. Haskell derives this via `binary`;
the Lean port uses whatever byte format each type chooses (JSON via
`ToJson`/`FromJson` in practice). -/
class Resource (α : Type) where
  toBytes : α → ByteArray
  ofBytes : ByteArray → Except String α

namespace Resource

/-- Atomic save: write to a temp file in the same directory, then rename
over the target (port of `atomicWriteByteString`). -/
def save [Resource α] (path : ArtifactPath) (ext : String) (content : α) : IO Unit := do
  let target := (path.targetPath.replaceExtension ext).toFilePath
  IO.FS.createDirAll (target.parent.getD (System.FilePath.mk "."))
  let tmp := System.FilePath.mk (target.toString ++ ".tmp")
  IO.FS.writeBinFile tmp (toBytes content)
  IO.FS.rename tmp target

/-- Load from the workspace mirror, falling back to (and caching from)
the origin file. Port of `Resource.load`. -/
def load [Resource α] (path : ArtifactPath) (ext : String) : IO α := do
  let target := (path.targetPath.replaceExtension ext).toFilePath
  if (← target.pathExists) then
    IO.ofExcept ((ofBytes (← IO.FS.readBinFile target) : Except String α).mapError IO.userError)
  else
    let origin := (path.originPath.replaceExtension ext).toFilePath
    let content ← IO.FS.readBinFile origin
    IO.FS.createDirAll (target.parent.getD (System.FilePath.mk "."))
    IO.FS.writeBinFile target content
    IO.ofExcept ((ofBytes content : Except String α).mapError IO.userError)

end Resource

instance : Resource ByteArray where
  toBytes := id
  ofBytes := .ok

/-- Pragmas collected per module (`# key value` header lines). -/
structure Pragma where
  toMap : Std.TreeMap ModuleName (List String) := {}

def Pragma.insert (name : ModuleName) (values : List String) (p : Pragma) : Pragma :=
  { toMap := p.toMap.alter name fun old => some (values ++ old.getD []) }

end Malgo
