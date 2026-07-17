import Std.Data.TreeMap
import Std.Data.TreeSet
import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.Interface
import Malgo.Syntax.Extension

/-! Port of `src/Malgo/Rename/RnEnv.hs`: the name-resolution environment
plus a minimal `Interface` used for import handling.

Deviations from Haskell:
- `RenameError` gains an `other` constructor. Haskell signals the
  non-`RenameError` failures (precedence errors, misplaced `let`/`with`,
  duplicate pattern variables) through `Malgo.Prelude.errorOn`, which
  prints to stderr and calls `exitFailure`. The Lean port has a single
  error channel, so those messages become `RenameError.other`.
- `genBuiltinRnEnv` is pure: builtin primitive types are `external` ids,
  which carry no uniq, so no `MalgoM` is needed.
- The lookup functions return `Except RenameError Id` (pure); the pass
  lifts them into its monad. -/

namespace Malgo.Rename

open Malgo Malgo.Syntax

/-- Resolved identifier: a possibly module-qualified `Id`. -/
abbrev Resolved := Qualified Id

inductive RenameError where
  | noSuchNameInScope (range : Range) (name : String) (candidates : List Resolved)
  | notInScope (range : Range) (name : String)
  | noSuchNameInModule (range : Range) (name : String) (moduleName : ModuleName)
      (candidates : List Resolved)
  | notInModule (range : Range) (name : String) (moduleName : ModuleName)
  | duplicateName (range : Range) (name : String)
  | duplicateNames (range : Range) (names : List String)
  /-- Not in Haskell's `RenameError`: carries an `errorOn`-style message. -/
  | other (range : Range) (message : String)

private def candidatesStr (cands : List Resolved) : String :=
  toString (cands.map fun q => pretty q.value)

/-- Render a `RenameError` to the uniform `CompileError` message. Mirrors
the Haskell `Pretty RenameError` instance in spirit; exact whitespace of
the prettyprinter `vsep` layout is not reproduced. -/
def RenameError.render : RenameError → String
  | .noSuchNameInScope _ name cands =>
    s!"Not in scope: '{name}'. Did you mean {candidatesStr cands}"
  | .notInScope _ name => s!"Not in scope: '{name}'"
  | .noSuchNameInModule _ name m cands =>
    s!"Not in scope: '{name}' in {pretty m}. Did you mean {candidatesStr cands}"
  | .notInModule _ name m => s!"Not in scope: '{name}' in {pretty m}"
  | .duplicateName _ name => s!"Duplicate name: '{name}'"
  | .duplicateNames _ names => s!"Duplicate names: {names}"
  | .other _ msg => msg

def RenameError.rangeOf : RenameError → Option Range
  | .noSuchNameInScope r .. => some r
  | .notInScope r _ => some r
  | .noSuchNameInModule r .. => some r
  | .notInModule r .. => some r
  | .duplicateName r _ => some r
  | .duplicateNames r _ => some r
  | .other r _ => some r

instance : Inhabited RenameError :=
  ⟨.other ⟨SourcePos.initial "", SourcePos.initial ""⟩ ""⟩

/-- Port of `RnEnv`. Maps a raw name to every resolved binding it can
denote (same-module `implicit`, or `explicit` for module-qualified
imports). Haskell `Map`/`Set` become `Std.TreeMap`/`Std.TreeSet`. -/
structure RnEnv where
  resolvedVarIdentMap : Std.TreeMap String (List Resolved) := {}
  resolvedTypeIdentMap : Std.TreeMap String (List Resolved) := {}
  constructors : Std.TreeSet Id := {}
  moduleNames : Std.TreeSet ModuleName := {}

/-- Prepend the new resolved var bindings, keeping per-key insertion order
identical to Haskell's `foldr (Map.insertWith (<>) k [v])`. -/
def insertVarIdent (newEnv : List (String × Resolved)) (env : RnEnv) : RnEnv :=
  { env with
    resolvedVarIdentMap :=
      newEnv.foldr (fun (k, v) m => m.alter k fun old => some (v :: old.getD [])) env.resolvedVarIdentMap }

def insertTypeIdent (newEnv : List (String × Resolved)) (env : RnEnv) : RnEnv :=
  { env with
    resolvedTypeIdentMap :=
      newEnv.foldr (fun (k, v) m => m.alter k fun old => some (v :: old.getD [])) env.resolvedTypeIdentMap }

def addConstructors (cons : List Id) (env : RnEnv) : RnEnv :=
  { env with constructors := cons.foldl (fun s c => s.insert c) env.constructors }

def isConstructor (con : Id) (env : RnEnv) : Bool :=
  env.constructors.contains con

/-- Generate the `RnEnv` of primitive types (all `external`). -/
def genBuiltinRnEnv : RnEnv :=
  let m := ModuleName.moduleName "Builtin"
  let names := ["Int32#", "Int64#", "Float#", "Double#", "Char#", "String#", "Ptr#"]
  let typeMap : Std.TreeMap String (List Resolved) :=
    names.foldl (fun acc n => acc.insert n [({ visibility := .implicit, value := newExternalId m n } : Resolved)]) {}
  { resolvedVarIdentMap := {}, resolvedTypeIdentMap := typeMap, constructors := {}, moduleNames := {} }

/-- Resolve a variable name already registered in the environment. -/
def lookupVar (env : RnEnv) (pos : Range) (name : String) : Except RenameError Id :=
  match env.resolvedVarIdentMap.get? name with
  | some names =>
    match names.find? (fun q => q.visibility == .implicit) with
    | some q => .ok q.value
    | none => .error (.noSuchNameInScope pos name names)
  | none => .error (.notInScope pos name)

def lookupType (env : RnEnv) (pos : Range) (name : String) : Except RenameError Id :=
  match env.resolvedTypeIdentMap.get? name with
  | some names =>
    match names.find? (fun q => q.visibility == .implicit) with
    | some q => .ok q.value
    | none => .error (.noSuchNameInScope pos name names)
  | none => .error (.notInScope pos name)

def lookupQualifiedVar (env : RnEnv) (pos : Range) (modName : ModuleName) (name : String) :
    Except RenameError Id :=
  match env.resolvedVarIdentMap.get? name with
  | some names =>
    match names.find? (fun q => q.visibility == .explicit modName) with
    | some q => .ok q.value
    | none => .error (.noSuchNameInModule pos name modName names)
  | none => .error (.notInModule pos name modName)

/-! ## Interface

The real `Interface` (and `externalFromInterface`) now live in
`Malgo/Interface.lean`. Re-export them into `Malgo.Rename` so the renamer's
consumption surface — and the test harness's `Malgo.Rename.Interface` —
stays unchanged. -/

export Malgo (Interface externalFromInterface)

end Malgo.Rename
