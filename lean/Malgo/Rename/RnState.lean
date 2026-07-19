import Std.Data.TreeMap
import Std.Data.TreeSet
import Malgo.Id
import Malgo.Module
import Malgo.Syntax.Extension

/-! Port of `src/Malgo/Rename/RnState.hs`: the mutable state threaded
through the renamer.

The Haskell `Pretty RnState` instance is debug-only (never observed by a
golden), so it is omitted here. -/

namespace Malgo.Rename

open Malgo Malgo.Syntax

/-- Port of `RnState`. `infixInfo` is keyed by the resolved `Id`
(Haskell `Map RnId (Assoc, Int)`); the exported-name lists hold raw
names (`PsId = String`) and are built in reverse registration order. -/
structure RnState where
  infixInfo : Std.TreeMap Id (Assoc × Int) := {}
  dependencies : Std.TreeSet ModuleName := {}
  exportedIdentifiers : List String := []
  exportedTypeIdentifiers : List String := []

end Malgo.Rename
