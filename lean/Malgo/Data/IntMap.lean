/-! Proof-free big-endian Patricia trie keyed on `Nat`, mirroring Haskell
`Data.IntMap` for non-negative keys. Needed because `Std.HashMap`/`TreeMap`
bundle well-formedness proofs and are rejected in nested-inductive
positions — the interpreter stores an `IntMap Value` inside `Value`'s
`Env`. Uniq keys are always `Nat` (`Uniq` starts at 0 and increments). -/

namespace Malgo

inductive IntMap (α : Type u) where
  /-- Empty map. Only ever appears at the root. -/
  | nil
  | tip (key : Nat) (val : α)
  /-- `msk` is the branch bit (a power of two); `pfx` is the shared high
  bits above it (`key / (2 * msk)`). Left has the branch bit clear. -/
  | bin (pfx : Nat) (msk : Nat) (left right : IntMap α)
  deriving Repr

namespace IntMap

def empty : IntMap α := .nil

instance : EmptyCollection (IntMap α) := ⟨.nil⟩

def isEmpty : IntMap α → Bool
  | .nil => true
  | _ => false

private def prefixOf (key msk : Nat) : Nat :=
  key / (2 * msk)

/-- `key` is actually stored somewhere in `t`. -/
def HasKey (t : IntMap α) (key : Nat) : Prop :=
  match t with
  | .nil => False
  | .tip k _ => key = k
  | .bin _ _ l r => l.HasKey key ∨ r.HasKey key

/-- Every mask nested inside `t` is strictly below `msk` — masks shrink
strictly toward the leaves. -/
def MaskBound (msk : Nat) : IntMap α → Prop
  | .nil => True
  | .tip _ _ => True
  | .bin _ msk' _ _ => msk' < msk

/-- Well-formedness: every `bin pfx msk l r` node's branch bit `msk` is a
single bit, every key in `l`/`r` agrees with `pfx` above that bit and is
routed to the correct side by it, and masks strictly shrink toward the
leaves. Direct formalization of the doc comments on `bin` and `link`. -/
inductive WF : IntMap α → Prop
  | nil : WF .nil
  | tip (k : Nat) (v : α) : WF (.tip k v)
  | bin (pfx msk : Nat) (l r : IntMap α)
      (hmsk : ∃ i, msk = 2 ^ i)
      (hl_pfx : ∀ k, l.HasKey k → prefixOf k msk = pfx)
      (hl_bit : ∀ k, l.HasKey k → k &&& msk == 0)
      (hr_pfx : ∀ k, r.HasKey k → prefixOf k msk = pfx)
      (hr_bit : ∀ k, r.HasKey k → ¬ (k &&& msk == 0))
      (hl_bound : l.MaskBound msk) (hr_bound : r.MaskBound msk)
      (hl : l.WF) (hr : r.WF) :
      WF (.bin pfx msk l r)

theorem wf_empty : (empty : IntMap α).WF := .nil

def lookup? (key : Nat) : IntMap α → Option α
  | .nil => none
  | .tip k v => if key == k then some v else none
  | .bin pfx msk l r =>
    if prefixOf key msk != pfx then none
    else if key &&& msk == 0 then l.lookup? key
    else r.lookup? key

/-- Join two subtrees whose prefixes (`p1`, `p2`) disagree. -/
private def link (p1 : Nat) (t1 : IntMap α) (p2 : Nat) (t2 : IntMap α) : IntMap α :=
  let msk := Nat.shiftLeft 1 (Nat.log2 (p1 ^^^ p2))
  let pfx := prefixOf p1 msk
  if p1 &&& msk == 0 then .bin pfx msk t1 t2 else .bin pfx msk t2 t1

def insert (key : Nat) (val : α) : IntMap α → IntMap α
  | .nil => .tip key val
  | t@(.tip k _) =>
    if key == k then .tip key val
    else link key (.tip key val) k t
  | t@(.bin pfx msk l r) =>
    if prefixOf key msk != pfx then
      link key (.tip key val) (pfx * (2 * msk)) t
    else if key &&& msk == 0 then
      .bin pfx msk (l.insert key val) r
    else
      .bin pfx msk l (r.insert key val)

theorem lookup_link_self (p1 : Nat) (t1 : IntMap α) (p2 : Nat) (t2 : IntMap α) :
    (link p1 t1 p2 t2).lookup? p1 = t1.lookup? p1 := by
  simp only [link]
  split <;> simp_all [lookup?, prefixOf]

theorem lookup_insert (key : Nat) (val : α) (m : IntMap α) :
    (m.insert key val).lookup? key = some val := by
  induction m with
  | nil => simp [insert, lookup?]
  | tip k v =>
    simp only [insert]
    split
    · simp [lookup?]
    · rw [lookup_link_self]; simp [lookup?]
  | bin pfx msk l r ihl ihr =>
    simp only [insert]
    split
    · rw [lookup_link_self]; simp [lookup?]
    · split
      · simp_all [lookup?]
      · simp_all [lookup?]

def contains (key : Nat) (t : IntMap α) : Bool :=
  (t.lookup? key).isSome

/-- Ascending key order (left subtree holds the smaller keys). -/
def toList : IntMap α → List (Nat × α)
  | .nil => []
  | .tip k v => [(k, v)]
  | .bin _ _ l r => l.toList ++ r.toList

def ofList (xs : List (Nat × α)) : IntMap α :=
  xs.foldl (fun m (k, v) => m.insert k v) .nil

def size : IntMap α → Nat
  | .nil => 0
  | .tip _ _ => 1
  | .bin _ _ l r => l.size + r.size

end IntMap

private def testMap : IntMap String :=
  IntMap.ofList [(5, "e"), (1, "a"), (9, "i"), (0, "z"), (16, "p"), (1, "A")]

#guard testMap.lookup? 1 == some "A"
#guard testMap.lookup? 5 == some "e"
#guard testMap.lookup? 16 == some "p"
#guard testMap.lookup? 7 == none
#guard testMap.toList == [(0, "z"), (1, "A"), (5, "e"), (9, "i"), (16, "p")]
#guard testMap.size == 5
#guard (IntMap.empty : IntMap Nat).isEmpty
#guard ((IntMap.ofList (List.range 200 |>.map fun n => (n * 7 % 199, n))).toList.map (·.1))
  == (List.range 200 |>.map (fun n => n * 7 % 199)).eraseDups.mergeSort (· ≤ ·)

end Malgo
