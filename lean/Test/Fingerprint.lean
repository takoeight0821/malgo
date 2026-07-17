import Malgo.Sequent.Core.Flat
import Malgo.Sequent.Core.Join

/-! Port of `test/Malgo/Sequent/Core/Fingerprint.hs`: the format-immune IR
parity tool. It counts IR constructors over Flat and Join programs
(order-insensitive) and renders a canonical `k=v ...` line. The committed
`.golden/Malgo.Sequent.ToCore/{flat,join}-fingerprint/<Case>/golden` files
are this output and must byte-match, so the counting and rendering mirror
the Haskell exactly.

Haskell accumulates a 22-field `Stats` monoid where each node contributes
`mempty {field = 1}` and `<>` sums fields. Here each `*Stats` returns the
list of the constructor keys it encountered (with multiplicity) — `mempty
{k=1}` becomes `["k"]` and `<>`/`foldMap` become list append/`flatMap` —
and `renderStats` tallies. Counting is order-insensitive, so the traversal
order (and `Data.Map.elems` vs plain list for `Object` fields) is
irrelevant to the result. -/

namespace Malgo.Test.Fingerprint

open Malgo.Sequent.Core

/-! ## Flat IR statistics -/

mutual

partial def flatStmtStats : Flat.Statement → List String
  | .cut p c => "cuts" :: (flatProdStats p ++ flatConsStats c)
  | .join _ _ c s => "joins" :: (flatConsStats c ++ flatStmtStats s)
  | .primitive _ _ ps c => "primitives" :: (ps.flatMap flatProdStats ++ flatConsStats c)
  | .invoke _ _ c => "invokes" :: flatConsStats c
  | .externalCall _ _ ps c => "externalCalls" :: (ps.flatMap flatProdStats ++ flatConsStats c)
  | .binOp _ _ l r c => "binOps" :: (flatProdStats l ++ flatProdStats r ++ flatConsStats c)
  | .ifz _ p t e => "ifzs" :: (flatProdStats p ++ flatStmtStats t ++ flatStmtStats e)

partial def flatProdStats : Flat.Producer → List String
  | .var _ _ => ["vars"]
  | .literal _ _ => ["literals"]
  | .construct _ _ ps cs => "constructs" :: (ps.flatMap flatProdStats ++ cs.flatMap flatConsStats)
  | .lambda _ _ s => "lambdas" :: flatStmtStats s
  | .object _ fs => "objects" :: fs.flatMap (fun (_, _, s) => flatStmtStats s)
  | .mu _ _ s => "mus" :: flatStmtStats s
  | .cocase _ bs => "cocases" :: bs.flatMap (fun (_, _, s) => flatStmtStats s)

partial def flatConsStats : Flat.Consumer → List String
  | .label _ _ => ["labels"]
  | .apply _ ps cs => "applies" :: (ps.flatMap flatProdStats ++ cs.flatMap flatConsStats)
  | .project _ _ c => "projects" :: flatConsStats c
  | .«then» _ _ s => "thens" :: flatStmtStats s
  | .finish _ => ["finishes"]
  | .select _ bs => "selects" :: bs.flatMap flatBranchStats
  | .destructor _ _ ps c => "destructors" :: (ps.flatMap flatProdStats ++ flatConsStats c)

partial def flatBranchStats : Flat.Branch → List String
  | .branch _ _ s => flatStmtStats s

end

/-! ## Join IR statistics

Join consumers on `Cut`/`Primitive`/`Invoke`/`ExternalCall`/`BinOp`/
`Apply`/`Project`/`Destructor` are hoisted to `Name`s, so — like the
Haskell — they are counted only by their owning node and not recursed. -/

mutual

partial def joinStmtStats : Join.Statement → List String
  | .cut p _ => "cuts" :: joinProdStats p
  | .join _ _ c s => "joins" :: (joinConsStats c ++ joinStmtStats s)
  | .primitive _ _ ps _ => "primitives" :: ps.flatMap joinProdStats
  | .invoke _ _ _ => ["invokes"]
  | .externalCall _ _ ps _ => "externalCalls" :: ps.flatMap joinProdStats
  | .binOp _ _ l r _ => "binOps" :: (joinProdStats l ++ joinProdStats r)
  | .ifz _ p t e => "ifzs" :: (joinProdStats p ++ joinStmtStats t ++ joinStmtStats e)

partial def joinProdStats : Join.Producer → List String
  | .var _ _ => ["vars"]
  | .literal _ _ => ["literals"]
  | .construct _ _ ps _ => "constructs" :: ps.flatMap joinProdStats
  | .lambda _ _ s => "lambdas" :: joinStmtStats s
  | .object _ fs => "objects" :: fs.flatMap (fun (_, _, s) => joinStmtStats s)
  | .mu _ _ s => "mus" :: joinStmtStats s
  | .cocase _ bs => "cocases" :: bs.flatMap (fun (_, _, s) => joinStmtStats s)

partial def joinConsStats : Join.Consumer → List String
  | .label _ _ => ["labels"]
  | .apply _ ps _ => "applies" :: ps.flatMap joinProdStats
  | .project _ _ _ => ["projects"]
  | .«then» _ _ s => "thens" :: joinStmtStats s
  | .finish _ => ["finishes"]
  | .select _ bs => "selects" :: bs.flatMap joinBranchStats
  | .destructor _ _ ps _ => "destructors" :: ps.flatMap joinProdStats

partial def joinBranchStats : Join.Branch → List String
  | .branch _ _ s => joinStmtStats s

end

/-! ## Rendering -/

/-- The stat keys in the exact order Haskell's `renderStats` sorts them
(the literal list is already alphabetically sorted, and `Data.List.sort`
keeps it so). -/
def statKeys : List String :=
  ["applies", "binOps", "cocases", "constructs", "cuts", "definitions",
   "destructors", "externalCalls", "finishes", "ifzs", "invokes", "joins",
   "labels", "lambdas", "literals", "mus", "objects", "primitives",
   "projects", "selects", "thens", "vars"]

/-- Tally the collected keys and render `k=v` for each nonzero count, in
`statKeys` order, space-separated. -/
def renderStats (keys : List String) : String :=
  let pairs := statKeys.filterMap fun k =>
    let v := (keys.filter (· == k)).length
    if v > 0 then some s!"{k}={v}" else none
  " ".intercalate pairs

def fingerprintFlat (p : Flat.Program) : String :=
  renderStats (p.definitions.flatMap fun (_, _, _, stmt) => "definitions" :: flatStmtStats stmt)

def fingerprintJoin (p : Join.Program) : String :=
  renderStats (p.definitions.flatMap fun (_, _, _, stmt) => "definitions" :: joinStmtStats stmt)

/-! ## Sanity checks -/

private def r0 : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
private def nm (s : String) : Malgo.Sequent.Fun.Name :=
  { name := s, moduleName := .moduleName "t", sort := .external }

#guard fingerprintFlat
    { definitions := [(r0, nm "f", nm "r", .cut (.var r0 (nm "x")) (.finish r0))],
      dependencies := [] }
  == "cuts=1 definitions=1 finishes=1 vars=1"

-- Join's `Cut` consumer is a `Name`, so it contributes no `finishes`/`labels`.
#guard fingerprintJoin
    { definitions := [(r0, nm "f", nm "r", .cut (.var r0 (nm "x")) (nm "k"))],
      dependencies := [] }
  == "cuts=1 definitions=1 vars=1"

-- Two definitions accumulate; an empty program renders the empty string.
#guard fingerprintFlat { definitions := [], dependencies := [] } == ""

end Malgo.Test.Fingerprint
