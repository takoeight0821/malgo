import Lean.Data.Json
import Std.Data.TreeMap
import Std.Data.TreeSet
import Malgo.Prelude
import Malgo.Path
import Malgo.Id
import Malgo.Module
import Malgo.Syntax.Extension

/-! Port of `src/Malgo/Interface.hs`: the module interface consumed by the
Rename pass for imports, plus its on-disk (`.mlgi`) serialization.

The placeholder previously living in `Malgo/Rename/RnEnv.lean` is absorbed
here; `RnEnv.lean` re-exports `Interface`/`externalFromInterface` into the
`Malgo.Rename` namespace so the renamer's consumption surface (and the test
harness's `Malgo.Rename.Interface`) is unchanged.

Serialization deviates from Haskell (which derives `Binary`): the Lean port
emits JSON via `Lean.ToJson`/`FromJson`, and the `Std.TreeMap`/`TreeSet`
fields are written as sorted arrays (`toList` is ascending) so the encoding
is deterministic. Not wire-compatible with Haskell's `.mlgi` — by design. -/

namespace Malgo

open Lean (Json ToJson FromJson toJson fromJson?)

/-! ## Leaf codecs shared by `.mlgi` and `.sqt`

`ModuleName`/`ArtifactPath`/`Assoc` are needed here for the `Interface`
codec; `Malgo/Sequent/Core/Json.lean` reuses them (it imports this file). -/

instance : ToJson (Path b t) where
  toJson p := Json.str p.toFilePath.toString

instance : ToJson ArtifactPath where
  toJson p :=
    Json.arr #[Json.str p.rawPath.toString, Json.str p.originPath.toFilePath.toString,
      Json.str p.relPath.toFilePath.toString, Json.str p.targetPath.toFilePath.toString]

instance : FromJson ArtifactPath where
  fromJson? j := do
    match (← j.getArr?).toList with
    | [raw, origin, rel, target] =>
      let originPath ← Path.parseAbsFile (System.FilePath.mk (← origin.getStr?))
      let targetPath ← Path.parseAbsFile (System.FilePath.mk (← target.getStr?))
      return {
        rawPath := System.FilePath.mk (← raw.getStr?),
        originPath,
        relPath := Path.mkRelFile (System.FilePath.mk (← rel.getStr?)),
        targetPath }
    | _ => .error "ArtifactPath: expected a 4-element array"

instance : ToJson ModuleName where
  toJson
    | .moduleName raw => Json.arr #[Json.str "m", Json.str raw]
    | .artifact path => Json.arr #[Json.str "a", toJson path]
    | .rawPath path => Json.arr #[Json.str "r", Json.str path]

instance : FromJson ModuleName where
  fromJson? j := do
    match (← j.getArr?).toList with
    | [tag, v] =>
      match ← tag.getStr? with
      | "m" => return .moduleName (← v.getStr?)
      | "a" => return .artifact (← fromJson? v)
      | "r" => return .rawPath (← v.getStr?)
      | other => .error s!"ModuleName: unknown tag {other}"
    | _ => .error "ModuleName: expected a 2-element array"

instance : ToJson Syntax.Assoc where
  toJson
    | .leftA => Json.str "left"
    | .rightA => Json.str "right"
    | .neutralA => Json.str "neutral"

instance : FromJson Syntax.Assoc where
  fromJson? j := do
    match ← j.getStr? with
    | "left" => return .leftA
    | "right" => return .rightA
    | "neutral" => return .neutralA
    | other => .error s!"Assoc: unknown value {other}"

/-! ## Interface -/

/-- Port of `Malgo.Interface.Interface`: only the fields the Rename pass
consumes for imports (keyed by raw name, matching Haskell
`buildInterface`'s `Map.mapKeys (·.name)`). -/
structure Interface where
  moduleName : ModuleName
  infixInfo : Std.TreeMap String (Syntax.Assoc × Int) := {}
  dependencies : Std.TreeSet ModuleName := {}
  exportedIdentList : List String := []
  exportedTypeIdentList : List String := []

/-- Turn a raw exported name into its `external` `Id` in the interface's
module (port of `externalFromInterface`). -/
def externalFromInterface (i : Interface) (name : String) : Id :=
  { name, moduleName := i.moduleName, sort := .external }

instance : ToJson Interface where
  toJson i :=
    Json.arr #[
      toJson i.moduleName,
      Json.arr ((i.infixInfo.toList.map fun (k, a, p) =>
        Json.arr #[Json.str k, toJson a, toJson p]).toArray),
      Json.arr ((i.dependencies.toList.map toJson).toArray),
      toJson i.exportedIdentList,
      toJson i.exportedTypeIdentList ]

instance : FromJson Interface where
  fromJson? j := do
    match (← j.getArr?).toList with
    | [mn, infixJ, depsJ, idents, tyIdents] =>
      let infixInfo ← (← infixJ.getArr?).toList.foldlM (init := ({} : Std.TreeMap String (Syntax.Assoc × Int)))
        fun acc (e : Json) => do
          match (← e.getArr?).toList with
          | [k, a, p] => return acc.insert (← k.getStr?) (← fromJson? a, ← p.getInt?)
          | _ => .error "Interface.infixInfo: expected a 3-element array"
      let dependencies ← (← depsJ.getArr?).toList.foldlM (init := ({} : Std.TreeSet ModuleName))
        fun acc (e : Json) => do return acc.insert (← fromJson? e)
      return {
        moduleName := ← fromJson? mn,
        infixInfo,
        dependencies,
        exportedIdentList := ← fromJson? idents,
        exportedTypeIdentList := ← fromJson? tyIdents }
    | _ => .error "Interface: expected a 5-element array"

instance : Resource Interface where
  toBytes i := (toJson i).compress.toUTF8
  ofBytes b :=
    match String.fromUTF8? b with
    | none => .error "Interface: invalid UTF-8"
    | some s => Lean.Json.parse s >>= fromJson?

/-- Load the interface for a module directly from disk (port of
`loadInterfaceFromDisk`); bypasses any query cache. -/
def loadInterfaceFromDisk (ws : Workspace) (modName : ModuleName) : IO Interface := do
  let modPath ← ws.getModulePath modName
  Resource.load modPath ".mlgi"

end Malgo
