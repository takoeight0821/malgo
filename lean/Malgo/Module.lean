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

/-- Same "compare via a projection" trick as `Path`'s `Std.TransOrd`
instance: `ArtifactPath`'s `Ord` is exactly `compareOn relPath`. -/
instance : Std.TransOrd ArtifactPath :=
  inferInstanceAs (Std.TransCmp (compareOn ArtifactPath.relPath))

end ArtifactPath

inductive ModuleName where
  | moduleName (raw : String)
  | artifact (path : ArtifactPath)
  /-- Not in Haskell: a path-literal import as written in the source.
  Exists only between the pure parse and import resolution (the Haskell
  parser resolves paths mid-parse via IO); never reaches dumps or later
  passes. -/
  | rawPath (path : String)
  deriving BEq, Ord, Repr

namespace ModuleName

/-- `deriving Ord`'s generated `compare` is a plain `match` on both
constructors, so it iota-reduces under `cases <;> simp` exactly like
`Option`'s hand-written instance (Std's own template for a small sum
type) — the ordering laws hold for the same reason `Option`'s do,
falling through to `String`'s/`ArtifactPath`'s `Std.TransOrd` on the
three same-constructor cases.

Lean's typeclass search indexes instances by their literal head symbol,
not up to unfolding: `Std.TransOrd String`'s registered head is
`@Ord.compare String _`, so once `Ord.compare`'s cascading unfold (below)
reduces every same-constructor case down to a raw `String.compare` call
(`ModuleName`/`ArtifactPath`'s fields all bottom out at `String`),
searches for `Std.TransCmp String.compare` miss it even though the two
are definitionally identical. These two instances re-register the same
proof under the head that's actually needed. -/
instance : Std.OrientedCmp String.compare :=
  inferInstanceAs (Std.OrientedCmp (compare : String → String → Ordering))

instance : Std.TransCmp String.compare :=
  inferInstanceAs (Std.TransCmp (compare : String → String → Ordering))

instance : Std.OrientedOrd ModuleName where
  eq_swap {a b} := by
    cases a <;> cases b <;>
      simp only [Ord.compare, instOrdModuleName.ord, Ordering.then_eq] <;>
      first
        | rfl
        | exact Std.OrientedCmp.eq_swap

instance : Std.TransOrd ModuleName where
  isLE_trans {a b c} hab hbc := by
    cases a <;> cases b <;> cases c <;>
      (try (simp_all [Ord.compare, instOrdModuleName.ord, Ordering.then_eq]; done))
    all_goals
      simp only [Ord.compare, instOrdModuleName.ord, Ordering.then_eq] at *
      apply Std.TransCmp.isLE_trans <;> assumption

def toStr : ModuleName → String
  | .moduleName raw => raw
  | .artifact path => path.relPath.toFilePath.toString
  | .rawPath path => path

/-- Basename of the module without extension; used in `Id` dumps. -/
def digest : ModuleName → String
  | .moduleName raw => raw
  | .artifact path =>
    match path.relPath.filename.splitExtension with
    | some (name, _) => name.toFilePath.toString
    | none => path.relPath.filename.toFilePath.toString
  | .rawPath path => path

instance : Pretty ModuleName := ⟨toStr⟩

instance : ToSExpr ModuleName where
  toSExpr
    | .moduleName raw => .atom (.symbol raw)
    | .artifact path => .atom (.str path.relPath.toFilePath.toString)
    | .rawPath path => .atom (.str path)

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

/-- Resolve `.`/`..` components lexically; fallback for nonexistent
paths, where Haskell `canonicalizePath` still succeeds but
`IO.FS.realPath` throws. -/
private def lexicalNormalize (p : System.FilePath) : System.FilePath := Id.run do
  let mut stack : List String := []
  for c in p.components do
    match c with
    | "" | "." => pure ()
    | ".." => stack := stack.drop 1
    | c => stack := c :: stack
  return System.FilePath.mk
    (System.FilePath.pathSeparator.toString ++
      System.FilePath.pathSeparator.toString.intercalate stack.reverse)

/-- Port of Haskell `canonicalizePath`: resolve symlinks when the path
exists, normalize lexically when it does not (the argument must already
be absolute, which both call sites guarantee). -/
private def canonicalizePath (p : System.FilePath) : IO System.FilePath := do
  try
    IO.FS.realPath p
  catch _ =>
    return lexicalNormalize p

/-- Resolve a path string relative to the directory containing the
workspace (the project root). Port of `parseArtifactPathFromPwd`. -/
def parseArtifactPathFromPwd (ws : Workspace) (path : System.FilePath) : IO ArtifactPath := do
  let pwd := ws.dir.parent
  let originRaw ← canonicalizePath (pwd.toFilePath / path)
  let originPath ← IO.ofExcept (Path.parseAbsFile originRaw |>.mapError IO.userError)
  let relPath ← IO.ofExcept (pwd.stripProperPrefix originPath |>.mapError IO.userError)
  return { rawPath := path, originPath, relPath, targetPath := ws.dir / relPath }

/-- Resolve a path string relative to the module that mentions it.
Port of `parseArtifactPath`. -/
def parseArtifactPath (ws : Workspace) (from_ : ArtifactPath) (path : System.FilePath) :
    IO ArtifactPath := do
  let base := from_.originPath.parent
  let originRaw ← canonicalizePath (base.toFilePath / path)
  let originPath ← IO.ofExcept (Path.parseAbsFile originRaw |>.mapError IO.userError)
  let originBase := ws.dir.parent
  let relPath ← IO.ofExcept (originBase.stripProperPrefix originPath |>.mapError IO.userError)
  return { rawPath := path, originPath, relPath, targetPath := ws.dir / relPath }

/-- Breadth-first search of the workspace mirror for `<name>.mlg`,
registering the result. Port of `searchAndRegister`. -/
private partial def searchAndRegister (ws : Workspace) (name : ModuleName) : IO ArtifactPath := do
  match name with
  | .artifact path => return path
  | .rawPath path =>
    throw (IO.userError s!"unresolved import path reached the workspace: {path}")
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

/-- Atomic save: write to a uniquely-named temp file in the same
directory, then rename over the target (port of
`atomicWriteByteString`). The unique name (`.writeNew` open, which fails
on an existing file) keeps concurrent writers of a shared artifact from
interleaving into one temp file; rename is last-writer-wins, which is
fine because the payload is a deterministic function of the source. -/
def save [Resource α] (path : ArtifactPath) (ext : String) (content : α) : IO Unit := do
  let target := (path.targetPath.replaceExtension ext).toFilePath
  IO.FS.createDirAll (target.parent.getD (System.FilePath.mk "."))
  let mut written := false
  for n in [0:1000] do
    let tmp := System.FilePath.mk s!"{target}.{n}.tmp"
    let handle? ← try
        some <$> IO.FS.Handle.mk tmp .writeNew
      catch _ => pure none
    if let some handle := handle? then
      try
        handle.write (toBytes content)
        handle.flush
        IO.FS.rename tmp target
      catch e =>
        try IO.FS.removeFile tmp catch _ => pure ()
        throw e
      written := true
      break
  unless written do
    throw (IO.userError s!"Resource.save: could not create a temp file for {target}")

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
