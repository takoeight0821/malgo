/-! Port of `src/Malgo/Path.hs`: phantom-typed wrappers around
`System.FilePath` for type-safe path manipulation. -/

namespace Malgo

inductive PathBase where
  | abs
  | rel
  deriving BEq, Repr

inductive PathType where
  | file
  | dir
  deriving BEq, Repr

/-- A typed path. `b` is `.abs` or `.rel`, `t` is `.file` or `.dir`.
Directory paths are stored without a trailing separator. -/
structure Path (b : PathBase) (t : PathType) where
  toFilePath : System.FilePath
  deriving Repr

namespace Path

instance : BEq (Path b t) := ⟨fun a b => a.toFilePath == b.toFilePath⟩

instance : Ord (Path b t) := ⟨fun a b => compare a.toFilePath.toString b.toFilePath.toString⟩

/-- `Path`'s `Ord` is exactly `compareOn (·.toFilePath.toString)`, so its
`Std.TransCmp` proof obligations are inherited for free from `String`'s
(via `Std`'s generic "compare via a projection" combinator) — riding on
this unblocks any `Std.TreeMap`/`TreeSet` proof keyed by `Path`,
`ArtifactPath`, or (transitively) `ModuleName`. -/
instance : Std.TransOrd (Path b t) :=
  inferInstanceAs (Std.TransCmp (compareOn (fun p : Path b t => p.toFilePath.toString)))

instance : ToString (Path b t) := ⟨fun p => p.toFilePath.toString⟩

private def dropTrailingPathSeparator (fp : System.FilePath) : System.FilePath :=
  let s := fp.toString
  if s.length > 1 && s.endsWith System.FilePath.pathSeparator.toString then
    System.FilePath.mk (toString (s.dropEnd 1))
  else
    fp

/-- Parse an absolute file path. Fails if not absolute. -/
def parseAbsFile (fp : System.FilePath) : Except String (Path .abs .file) :=
  if fp.isAbsolute then .ok ⟨fp⟩
  else .error s!"Expected absolute file path: {fp}"

/-- Parse an absolute directory path. Fails if not absolute. -/
def parseAbsDir (fp : System.FilePath) : Except String (Path .abs .dir) :=
  if fp.isAbsolute then .ok ⟨dropTrailingPathSeparator fp⟩
  else .error s!"Expected absolute directory path: {fp}"

/-- Create a relative file path (no validation). -/
def mkRelFile (fp : System.FilePath) : Path .rel .file := ⟨fp⟩

/-- Parent directory; Haskell `takeDirectory` yields `"."` for bare names. -/
def parent (p : Path b t) : Path b .dir :=
  ⟨p.toFilePath.parent.getD (System.FilePath.mk ".")⟩

/-- Combine a directory path with a relative path. -/
def join (dir : Path b .dir) (rel : Path .rel t) : Path b t :=
  ⟨dir.toFilePath / rel.toFilePath⟩

instance : HDiv (Path b .dir) (Path .rel t) (Path b t) := ⟨join⟩

/-- Replace the extension of a file path. Accepts `ext` with or without a
leading dot, mirroring Haskell `FilePath.replaceExtension`. -/
def replaceExtension (ext : String) (p : Path b .file) : Path b .file :=
  ⟨p.toFilePath.withExtension (toString (ext.dropWhile (· == '.')))⟩

/-- Split the extension from a file path; `none` when there is no extension. -/
def splitExtension (p : Path b .file) : Option (Path b .file × String) :=
  match p.toFilePath.extension with
  | none => none
  | some ext => some (⟨p.toFilePath.withExtension ""⟩, "." ++ ext)

/-- The filename component of a path. -/
def filename (p : Path b .file) : Path .rel .file :=
  ⟨System.FilePath.mk (p.toFilePath.fileName.getD p.toFilePath.toString)⟩

/-- Strip a directory prefix, yielding a relative path. Fails if the prefix
does not match. -/
def stripProperPrefix (dir : Path b .dir) (p : Path b t) : Except String (Path .rel t) :=
  let dirStr := dir.toFilePath.toString ++ System.FilePath.pathSeparator.toString
  let pStr := p.toFilePath.toString
  if pStr.startsWith dirStr then
    .ok ⟨System.FilePath.mk (toString (pStr.drop dirStr.length))⟩
  else
    .error s!"Path {pStr} is not inside {dir.toFilePath}"

end Path

end Malgo
