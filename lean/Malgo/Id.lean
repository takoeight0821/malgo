import Malgo.Prelude
import Malgo.Module
import Malgo.SExpr
import Malgo.Monad

/-! Port of `src/Malgo/Id.hs`: resolved identifiers. -/

namespace Malgo

/-- Identifier sort. `internal`/`temporal` carry the uniq that the
interpreter's `Env` fast path keys on. -/
inductive IdSort where
  | external
  | internal (uniq : Nat)
  | temporal (uniq : Nat)
  deriving BEq, Ord, Repr

instance : Pretty IdSort := ⟨fun s => (repr s).pretty⟩

structure Id where
  name : String
  moduleName : ModuleName
  sort : IdSort
  deriving BEq, Ord, Repr

namespace Id

instance : Pretty Id where
  pretty i :=
    match i.sort with
    | .external => i.name
    | .internal uniq => s!"#[{pretty i.moduleName} {i.name} {uniq}]"
    | .temporal uniq => s!"$[{pretty i.moduleName} {i.name} {uniq}]"

instance : ToSExpr Id where
  toSExpr i :=
    match i.sort with
    | .external => .atom (.symbol i.name)
    | .internal uniq => .atom (.symbol s!"#{i.moduleName.digest}.{i.name}_{uniq}")
    | .temporal uniq => .atom (.symbol s!"${i.moduleName.digest}.{i.name}_{uniq}")

def toText (i : Id) : String :=
  match i.sort with
  | .external => s!"{i.moduleName.toStr}.{i.name}"
  | .internal uniq => s!"{i.moduleName.toStr}.#{i.name}_{uniq}"
  | .temporal uniq => s!"{i.moduleName.toStr}.${i.name}_{uniq}"

def isExternal (i : Id) : Bool :=
  i.sort matches .external

end Id

/-- `Meta` pairs an `Id` with pass-specific metadata. The Haskell field
is called `meta`, but `meta` is a keyword in Lean, hence `info`. -/
structure Meta (α : Type u) where
  info : α
  id : Id
  deriving BEq, Repr

instance : Pretty (Meta α) := ⟨fun m => pretty m.id⟩

instance : ToSExpr (Meta α) := ⟨fun m => toSExpr m.id⟩

def newTemporalId (moduleName : ModuleName) (name : String) : MalgoM Id := do
  return { name, moduleName, sort := .temporal (← getUniq) }

def newInternalId (moduleName : ModuleName) (name : String) : MalgoM Id := do
  return { name, moduleName, sort := .internal (← getUniq) }

def newExternalId (moduleName : ModuleName) (name : String) : Id :=
  { name, moduleName, sort := .external }

end Malgo
