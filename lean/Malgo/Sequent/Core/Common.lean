import Malgo.Prelude
import Malgo.Module
import Malgo.SExpr
import Malgo.Sequent.Fun

/-! Shared wrapper types for the Core IR pipeline (Full/Flat/Join): each
top-level definition (a `Range`, its own name, its CPS return-continuation
name, and a body of the stage's own `Statement` type) and the `Program`
that holds a list of them plus module dependencies. Full/Flat/Join stay
three separate `mutual inductive` families for `Producer`/`Consumer`/
`Statement`/`Branch` -- they really do differ structurally (Flat adds
`join`; Join replaces `Consumer`-typed return slots with a plain `Name`)
-- but the `Definition`/`Program` wrapper around them, and the `sym`
s-expression-tag helper their dumps use, were previously duplicated
byte-for-byte across all three files, differing only in which stage's
`Statement` each closed over. -/

namespace Malgo.Sequent.Core

open Malgo.Sequent.Fun (Name)

def sym (s : String) : SExpr := .atom (.symbol s)

structure DefinitionOf (α : Type) where
  range : Range
  name : Name
  ret : Name
  body : α

structure ProgramOf (α : Type) where
  definitions : List (DefinitionOf α)
  dependencies : List ModuleName

instance [ToSExpr α] : ToSExpr (DefinitionOf α) where
  toSExpr d := .list [Malgo.toSExpr d.name, Malgo.toSExpr d.ret, Malgo.toSExpr d.body]

instance [ToSExpr α] : ToSExpr (ProgramOf α) where
  toSExpr p :=
    .list <|
      (p.definitions.map Malgo.toSExpr)
      ++ [.list (p.dependencies.map Malgo.toSExpr)]

end Malgo.Sequent.Core
