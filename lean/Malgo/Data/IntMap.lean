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

/-! ## Bit-arithmetic facts used by `link`'s well-formedness proof -/

private theorem testBit_disagree {p1 p2 : Nat} (h : p1 ≠ p2) :
    Nat.testBit p1 (Nat.log2 (p1 ^^^ p2)) ≠ Nat.testBit p2 (Nat.log2 (p1 ^^^ p2)) := by
  have hxor : p1 ^^^ p2 ≠ 0 := by
    intro hz
    apply h
    apply Nat.eq_of_testBit_eq (x := p1) (y := p2)
    intro i
    have hi := Nat.testBit_xor p1 p2 i
    rw [hz] at hi
    cases hb1 : Nat.testBit p1 i <;> cases hb2 : Nat.testBit p2 i <;> simp_all
  have hlog := Nat.testBit_log2 hxor
  rw [Nat.testBit_xor] at hlog
  cases hb1 : Nat.testBit p1 (Nat.log2 (p1 ^^^ p2)) <;>
    cases hb2 : Nat.testBit p2 (Nat.log2 (p1 ^^^ p2)) <;> simp_all

private theorem testBit_agree_above {p1 p2 : Nat} (h : p1 ≠ p2) (i : Nat)
    (hi : i > Nat.log2 (p1 ^^^ p2)) :
    Nat.testBit p1 i = Nat.testBit p2 i := by
  have hxor : p1 ^^^ p2 ≠ 0 := by
    intro hz
    apply h
    apply Nat.eq_of_testBit_eq (x := p1) (y := p2)
    intro j
    have hj := Nat.testBit_xor p1 p2 j
    rw [hz] at hj
    cases hb1 : Nat.testBit p1 j <;> cases hb2 : Nat.testBit p2 j <;> simp_all
  have hlt : p1 ^^^ p2 < 2 ^ i := by
    have hb := (Nat.log2_eq_iff hxor (k := Nat.log2 (p1 ^^^ p2))).mp rfl
    calc p1 ^^^ p2 < 2 ^ (Nat.log2 (p1 ^^^ p2) + 1) := hb.2
      _ ≤ 2 ^ i := Nat.pow_le_pow_right (by omega) (by omega)
  have hfalse := Nat.testBit_lt_two_pow hlt
  rw [Nat.testBit_xor] at hfalse
  cases hb1 : Nat.testBit p1 i <;> cases hb2 : Nat.testBit p2 i <;> simp_all

private theorem testBit_true_le_log2 {n i : Nat} (h : Nat.testBit n i = true) :
    i ≤ Nat.log2 n := by
  by_cases hle : i ≤ Nat.log2 n
  · exact hle
  · exfalso
    have hgt : Nat.log2 n < i := by omega
    have hn0 : n ≠ 0 := by intro hz; rw [hz] at h; simp at h
    have hb := (Nat.log2_eq_iff hn0 (k := Nat.log2 n)).mp rfl
    have hlt : n < 2 ^ i := by
      calc n < 2 ^ (Nat.log2 n + 1) := hb.2
        _ ≤ 2 ^ i := Nat.pow_le_pow_right (by omega) (by omega)
    have := Nat.testBit_lt_two_pow hlt
    simp_all

private theorem and_pow_two_eq_zero_iff {n L : Nat} :
    n &&& (2 ^ L) = 0 ↔ Nat.testBit n L = false := by
  constructor
  · intro he
    have h0 := Nat.zero_testBit L
    rw [← he, Nat.testBit_and, Nat.testBit_two_pow] at h0
    simpa using h0
  · intro hf
    apply Nat.eq_of_testBit_eq
    intro j
    by_cases hj : j = L
    · subst hj
      simp [Nat.testBit_and, hf]
    · simp_all [Nat.testBit_and, Nat.testBit_two_pow]
      intro _
      omega

private theorem shiftLeft_one_eq_two_pow (L : Nat) : Nat.shiftLeft 1 L = 2 ^ L := by
  show (1 : Nat) <<< L = 2 ^ L
  rw [Nat.shiftLeft_eq, Nat.one_mul]

private theorem prefixOf_eq_iff {x y b : Nat} :
    prefixOf x (2 ^ b) = prefixOf y (2 ^ b) ↔ ∀ i, i > b → Nat.testBit x i = Nat.testBit y i := by
  unfold prefixOf
  have hrw : (2 : Nat) * 2 ^ b = 2 ^ (b + 1) := by rw [Nat.pow_succ, Nat.mul_comm]
  rw [hrw]
  constructor
  · intro heq i hi
    have hi' : i - (b + 1) + (b + 1) = i := by omega
    have h1 : Nat.testBit (x / 2 ^ (b + 1)) (i - (b + 1)) = Nat.testBit x i := by
      rw [Nat.testBit_div_two_pow, hi']
    have h2 : Nat.testBit (y / 2 ^ (b + 1)) (i - (b + 1)) = Nat.testBit y i := by
      rw [Nat.testBit_div_two_pow, hi']
    rw [← h1, ← h2, heq]
  · intro hagree
    apply Nat.eq_of_testBit_eq
    intro i
    rw [Nat.testBit_div_two_pow, Nat.testBit_div_two_pow]
    exact hagree (i + (b + 1)) (by omega)

private theorem prefixOf_mul_self {pfx msk : Nat} (hmsk : msk > 0) :
    prefixOf (pfx * (2 * msk)) msk = pfx := by
  unfold prefixOf
  exact Nat.mul_div_cancel pfx (by omega)

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
      (hln : l ≠ .nil) (hrn : r ≠ .nil)
      (hl : l.WF) (hr : r.WF) :
      WF (.bin pfx msk l r)

theorem wf_empty : (empty : IntMap α).WF := .nil

/-- Every well-formed tree other than `nil` actually stores a key — a `bin`
node can never be hollow, so the mask/prefix facts `hl_pfx`/`hr_pfx` etc.
always have a real witness to apply to. -/
theorem exists_hasKey_of_wf {t : IntMap α} (h : t.WF) (hne : t ≠ .nil) :
    ∃ k, t.HasKey k := by
  cases h with
  | nil => exact absurd rfl hne
  | tip k v => exact ⟨k, rfl⟩
  | bin pfx msk l r _ _ _ _ _ _ _ hln _ hl hr =>
    obtain ⟨k, hk⟩ := exists_hasKey_of_wf hl hln
    exact ⟨k, Or.inl hk⟩

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

/-- `t`'s own branch bit is below `L`, and every key it stores agrees with
`rep` on every bit from `L` up — i.e. `rep` is a valid stand-in for `t` at
that bit level, whether or not `rep` itself is actually stored in `t`. -/
private def Represents (L : Nat) (t : IntMap α) (rep : Nat) : Prop :=
  t.MaskBound (Nat.shiftLeft 1 L) ∧ ∀ k, t.HasKey k → ∀ i, i ≥ L → Nat.testBit k i = Nat.testBit rep i

private theorem represents_self {k : Nat} (val : α) (L : Nat) :
    Represents L (IntMap.tip k val) k := by
  refine ⟨trivial, ?_⟩
  intro k' hk' i _
  simp only [HasKey] at hk'
  subst hk'
  rfl

private theorem represents_disagree {key pfx msk : Nat} {l r : IntMap α}
    (hwf : (IntMap.bin pfx msk l r).WF) (hdis : prefixOf key msk ≠ pfx) :
    Represents (Nat.log2 (key ^^^ (pfx * (2 * msk)))) (IntMap.bin pfx msk l r) (pfx * (2 * msk)) := by
  cases hwf with
  | bin _ _ _ _ hmsk hl_pfx _ hr_pfx _ hl_bound hr_bound _ _ hl hr =>
    obtain ⟨b, hb⟩ := hmsk
    subst hb
    have hrep : prefixOf (pfx * (2 * 2 ^ b)) (2 ^ b) = pfx := prefixOf_mul_self (Nat.two_pow_pos b)
    have hdis' : prefixOf key (2 ^ b) ≠ prefixOf (pfx * (2 * 2 ^ b)) (2 ^ b) := by
      rw [hrep]; exact hdis
    have hb' : ¬ (∀ i, i > b → Nat.testBit key i = Nat.testBit (pfx * (2 * 2 ^ b)) i) := by
      intro hcontra
      apply hdis'
      exact (prefixOf_eq_iff (x := key) (y := pfx * (2 * 2 ^ b)) (b := b)).mpr hcontra
    have hwit : ∃ i, i > b ∧ Nat.testBit key i ≠ Nat.testBit (pfx * (2 * 2 ^ b)) i := by
      by_cases hex : ∃ i, i > b ∧ Nat.testBit key i ≠ Nat.testBit (pfx * (2 * 2 ^ b)) i
      · exact hex
      · exfalso
        apply hb'
        intro i hi
        by_cases hbad : Nat.testBit key i = Nat.testBit (pfx * (2 * 2 ^ b)) i
        · exact hbad
        · exact absurd ⟨i, hi, hbad⟩ hex
    obtain ⟨i, hib, hitest⟩ := hwit
    have hxor : Nat.testBit (key ^^^ (pfx * (2 * 2 ^ b))) i = true := by
      rw [Nat.testBit_xor]
      cases hc1 : Nat.testBit key i <;> cases hc2 : Nat.testBit (pfx * (2 * 2 ^ b)) i <;> simp_all
    have hile : i ≤ Nat.log2 (key ^^^ (pfx * (2 * 2 ^ b))) := testBit_true_le_log2 hxor
    have hbltL : b < Nat.log2 (key ^^^ (pfx * (2 * 2 ^ b))) := by omega
    refine ⟨?_, ?_⟩
    · show (2:Nat) ^ b < Nat.shiftLeft 1 (Nat.log2 (key ^^^ (pfx * (2 * 2 ^ b))))
      rw [shiftLeft_one_eq_two_pow]
      exact Nat.pow_lt_pow_right (by omega) hbltL
    · intro k' hk' j hj
      have hk'pfx : prefixOf k' (2 ^ b) = pfx := by
        cases hk' with
        | inl hl' => exact hl_pfx k' hl'
        | inr hr' => exact hr_pfx k' hr'
      have hk'rep : prefixOf k' (2 ^ b) = prefixOf (pfx * (2 * 2 ^ b)) (2 ^ b) := by
        rw [hk'pfx, hrep]
      have hk'above : ∀ i, i > b → Nat.testBit k' i = Nat.testBit (pfx * (2 * 2 ^ b)) i :=
        (prefixOf_eq_iff (x := k') (y := pfx * (2 * 2 ^ b)) (b := b)).mp hk'rep
      exact hk'above j (by omega)

/-- When `key` is routed toward `l` at branch bit `msk` (agreeing with `l`'s
own stored prefix and taking the same side of the branch bit as `l`'s keys),
`key` is a valid representative for `l` at that level — freshly derived from
`l`'s own well-formedness fields, with no need for `l` to actually contain
any particular key. -/
private theorem represents_of_agree {key pfx msk : Nat} {l : IntMap α}
    (hl_pfx : ∀ k, l.HasKey k → prefixOf k msk = pfx)
    (hl_bit_eq : ∀ k, l.HasKey k → (k &&& msk == 0) = (key &&& msk == 0))
    (hl_bound : l.MaskBound msk) (hkeypfx : prefixOf key msk = pfx)
    (hmskpow : ∃ i, msk = 2 ^ i) :
    Represents (Nat.log2 msk) l key := by
  obtain ⟨b, hb⟩ := hmskpow
  subst hb
  rw [Nat.log2_two_pow]
  refine ⟨?_, ?_⟩
  · rw [shiftLeft_one_eq_two_pow]; exact hl_bound
  · intro k hk i hi
    have heq : prefixOf k (2 ^ b) = prefixOf key (2 ^ b) := by rw [hl_pfx k hk, hkeypfx]
    by_cases hib : i = b
    · rw [hib]
      have hkbit := hl_bit_eq k hk
      cases hb1 : k &&& 2 ^ b == 0 <;> cases hb2 : key &&& 2 ^ b == 0 <;>
        simp_all [and_pow_two_eq_zero_iff]
    · exact (prefixOf_eq_iff (x := k) (y := key) (b := b)).mp heq i (by omega)

theorem link_tip_wf {p1 : Nat} (val : α) {p2 : Nat} {t2 : IntMap α} (hne : p1 ≠ p2) (h2 : t2.WF)
    (hne2 : t2 ≠ .nil) (hrep2 : Represents (Nat.log2 (p1 ^^^ p2)) t2 p2) :
    (link p1 (IntMap.tip p1 val) p2 t2).WF := by
  obtain ⟨hbound2, hagree2⟩ := hrep2
  rw [shiftLeft_one_eq_two_pow] at hbound2
  have hpfxeq : prefixOf p1 (2 ^ Nat.log2 (p1 ^^^ p2)) = prefixOf p2 (2 ^ Nat.log2 (p1 ^^^ p2)) :=
    (prefixOf_eq_iff (x := p1) (y := p2) (b := Nat.log2 (p1 ^^^ p2))).mpr (testBit_agree_above hne)
  have hdisagree := testBit_disagree hne
  simp only [link]
  rw [shiftLeft_one_eq_two_pow]
  by_cases hbit : Nat.testBit p1 (Nat.log2 (p1 ^^^ p2)) = true
  · have hcond : ¬ (p1 &&& 2 ^ Nat.log2 (p1 ^^^ p2) == 0) := by
      simp [and_pow_two_eq_zero_iff, hbit]
    rw [if_neg (by simpa using hcond)]
    have hbit2 : Nat.testBit p2 (Nat.log2 (p1 ^^^ p2)) = false := by
      by_cases hb2 : Nat.testBit p2 (Nat.log2 (p1 ^^^ p2)) = false
      · exact hb2
      · exfalso; apply hdisagree; simp_all
    refine WF.bin _ _ t2 (IntMap.tip p1 val) ⟨_, rfl⟩ ?_ ?_ ?_ ?_ hbound2 trivial hne2 (by simp) h2
      (WF.tip p1 val)
    · intro k hk
      rw [(prefixOf_eq_iff (x := k) (y := p2) (b := Nat.log2 (p1 ^^^ p2))).mpr
        (fun i hi => hagree2 k hk i (by omega)), hpfxeq]
    · intro k hk
      have hk2 := hagree2 k hk (Nat.log2 (p1 ^^^ p2)) (Nat.le_refl _)
      simp [and_pow_two_eq_zero_iff, hk2, hbit2]
    · intro k hk
      simp only [HasKey] at hk
      subst hk
      rfl
    · intro k hk
      simp only [HasKey] at hk
      subst hk
      simp [and_pow_two_eq_zero_iff, hbit]
  · have hbit' : Nat.testBit p1 (Nat.log2 (p1 ^^^ p2)) = false := by
      simpa using hbit
    have hcond : (p1 &&& 2 ^ Nat.log2 (p1 ^^^ p2) == 0) := by
      simp [and_pow_two_eq_zero_iff, hbit']
    rw [if_pos (by simpa using hcond)]
    have hbit2 : Nat.testBit p2 (Nat.log2 (p1 ^^^ p2)) = true := by
      by_cases hb2 : Nat.testBit p2 (Nat.log2 (p1 ^^^ p2)) = true
      · exact hb2
      · exfalso; apply hdisagree; simp_all
    refine WF.bin _ _ (IntMap.tip p1 val) t2 ⟨_, rfl⟩ ?_ ?_ ?_ ?_ trivial hbound2 (by simp) hne2
      (WF.tip p1 val) h2
    · intro k hk
      simp only [HasKey] at hk
      subst hk
      rfl
    · intro k hk
      simp only [HasKey] at hk
      subst hk
      simp [and_pow_two_eq_zero_iff, hbit']
    · intro k hk
      rw [(prefixOf_eq_iff (x := k) (y := p2) (b := Nat.log2 (p1 ^^^ p2))).mpr
        (fun i hi => hagree2 k hk i (by omega)), hpfxeq]
    · intro k hk
      have hk2 := hagree2 k hk (Nat.log2 (p1 ^^^ p2)) (Nat.le_refl _)
      simp [and_pow_two_eq_zero_iff, hk2, hbit2]

theorem hasKey_link (p1 : Nat) (t1 : IntMap α) (p2 : Nat) (t2 : IntMap α) (k : Nat) :
    (link p1 t1 p2 t2).HasKey k ↔ t1.HasKey k ∨ t2.HasKey k := by
  simp only [link]
  split
  · simp only [HasKey]
  · simp only [HasKey]
    constructor
    · intro h
      cases h with
      | inl h => exact Or.inr h
      | inr h => exact Or.inl h
    · intro h
      cases h with
      | inl h => exact Or.inr h
      | inr h => exact Or.inl h

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

theorem hasKey_insert (key : Nat) (val : α) (m : IntMap α) (k : Nat) :
    (m.insert key val).HasKey k ↔ k = key ∨ m.HasKey k := by
  induction m with
  | nil => simp [insert, HasKey]
  | tip k' v =>
    simp only [insert]
    split
    · rename_i heq
      simp only [beq_iff_eq] at heq
      simp only [HasKey]
      constructor
      · intro h; exact Or.inl h
      · intro h
        cases h with
        | inl h => exact h
        | inr h => rw [h, heq]
    · rename_i hne
      rw [hasKey_link]
      simp only [HasKey]
  | bin pfx msk l r ihl ihr =>
    simp only [insert]
    split
    · rw [hasKey_link]
      simp only [HasKey]
    · split
      · simp only [HasKey, ihl]
        constructor
        · intro h
          cases h with
          | inl h =>
            cases h with
            | inl h => exact Or.inl h
            | inr h => exact Or.inr (Or.inl h)
          | inr h => exact Or.inr (Or.inr h)
        · intro h
          cases h with
          | inl h => exact Or.inl (Or.inl h)
          | inr h =>
            cases h with
            | inl h => exact Or.inl (Or.inr h)
            | inr h => exact Or.inr h
      · simp only [HasKey, ihr]
        constructor
        · intro h
          cases h with
          | inl h => exact Or.inr (Or.inl h)
          | inr h =>
            cases h with
            | inl h => exact Or.inl h
            | inr h => exact Or.inr (Or.inr h)
        · intro h
          cases h with
          | inl h => exact Or.inr (Or.inl h)
          | inr h =>
            cases h with
            | inl h => exact Or.inl h
            | inr h => exact Or.inr (Or.inr h)

private theorem insert_ne_nil (key : Nat) (val : α) (m : IntMap α) : m.insert key val ≠ .nil := by
  cases m with
  | nil => simp [insert]
  | tip k v =>
    simp only [insert]
    split
    · simp
    · simp only [link]; split <;> simp
  | bin pfx msk l r =>
    simp only [insert]
    split
    · simp only [link]; split <;> simp
    · split <;> simp

/-- Combined strengthening for the induction: `insert` preserves `WF`, and
whenever the ambient key was already a valid representative for `m` at some
bound `b` (i.e. everything `m` stores agrees with it from bit `b` up), the
result stays bounded by that same `b` — this is what lets a freshly created
`link` deep inside a recursive `insert` stay correctly nested under its
ancestors' masks. -/
private theorem insert_wf_aux (key : Nat) (val : α) :
    ∀ {m : IntMap α}, m.WF →
      (m.insert key val).WF ∧
        ∀ b, Represents b m key → (m.insert key val).MaskBound (Nat.shiftLeft 1 b) := by
  intro m
  induction m with
  | nil =>
    intro _
    refine ⟨WF.tip key val, ?_⟩
    intro b _
    simp [insert, MaskBound]
  | tip k v =>
    intro _
    simp only [insert]
    split
    · refine ⟨WF.tip key val, ?_⟩
      intro b _
      simp [MaskBound]
    · rename_i hne'
      have hne : key ≠ k := by simpa [beq_iff_eq] using hne'
      refine ⟨link_tip_wf val hne (WF.tip k v) (by simp) (represents_self v _), ?_⟩
      intro b hrep
      obtain ⟨_, hagree⟩ := hrep
      have hltb : Nat.log2 (key ^^^ k) < b := by
        by_cases hlt : Nat.log2 (key ^^^ k) < b
        · exact hlt
        · exfalso
          apply testBit_disagree hne
          exact (hagree k rfl (Nat.log2 (key ^^^ k)) (by omega)).symm
      show (link key (IntMap.tip key val) k (IntMap.tip k v)).MaskBound (Nat.shiftLeft 1 b)
      simp only [link]
      split <;> simp only [MaskBound] <;>
        (simp only [shiftLeft_one_eq_two_pow]; exact Nat.pow_lt_pow_right (by omega) hltb)
  | bin pfx msk l r ihl ihr =>
    intro h
    cases h with
    | bin _ _ _ _ hmsk hl_pfx hl_bit hr_pfx hr_bit hl_bound hr_bound hln hrn hl hr =>
    have hwf' : (IntMap.bin pfx msk l r).WF :=
      WF.bin pfx msk l r hmsk hl_pfx hl_bit hr_pfx hr_bit hl_bound hr_bound hln hrn hl hr
    obtain ⟨b0, hb0⟩ := hmsk
    simp only [insert]
    split
    · rename_i hdis'
      have hdis : prefixOf key msk ≠ pfx := by simpa using hdis'
      have hne : key ≠ pfx * (2 * msk) := by
        intro he
        apply hdis
        rw [he]
        exact prefixOf_mul_self (by rw [hb0]; exact Nat.two_pow_pos b0)
      refine ⟨link_tip_wf val hne hwf' (by simp) (represents_disagree hwf' hdis), ?_⟩
      intro b hrep
      obtain ⟨hbound_bin, hagree_bin⟩ := hrep
      rw [shiftLeft_one_eq_two_pow] at hbound_bin
      have hb0b : b0 < b := by
        simp only [MaskBound] at hbound_bin
        rw [hb0] at hbound_bin
        exact (Nat.pow_lt_pow_iff_right (by omega)).mp hbound_bin
      have hrepv : prefixOf (pfx * (2 * msk)) (2 ^ b0) = pfx := by
        rw [← hb0]
        exact prefixOf_mul_self (by rw [hb0]; exact Nat.two_pow_pos b0)
      obtain ⟨k', hk'⟩ := exists_hasKey_of_wf hwf' (by simp)
      have hk'_pfx : prefixOf k' (2 ^ b0) = pfx := by
        rw [← hb0]
        cases hk' with
        | inl h => exact hl_pfx k' h
        | inr h => exact hr_pfx k' h
      have hk'rep : ∀ i, i > b0 → Nat.testBit k' i = Nat.testBit (pfx * (2 * msk)) i :=
        (prefixOf_eq_iff (x := k') (y := pfx * (2 * msk)) (b := b0)).mp (by rw [hk'_pfx, hrepv])
      have hltb : Nat.log2 (key ^^^ (pfx * (2 * msk))) < b := by
        by_cases hlt : Nat.log2 (key ^^^ (pfx * (2 * msk))) < b
        · exact hlt
        · exfalso
          have hxor0 : key ^^^ (pfx * (2 * msk)) ≠ 0 := by
            intro hz
            apply hne
            apply Nat.eq_of_testBit_eq
            intro i
            have := Nat.testBit_xor key (pfx * (2 * msk)) i
            rw [hz] at this
            cases hc1 : Nat.testBit key i <;> cases hc2 : Nat.testBit (pfx * (2 * msk)) i <;> simp_all
          have hlog := Nat.testBit_log2 hxor0
          rw [Nat.testBit_xor] at hlog
          have hge : Nat.log2 (key ^^^ (pfx * (2 * msk))) ≥ b := by omega
          have h1 : Nat.testBit k' (Nat.log2 (key ^^^ (pfx * (2 * msk)))) =
              Nat.testBit key (Nat.log2 (key ^^^ (pfx * (2 * msk)))) :=
            hagree_bin k' hk' (Nat.log2 (key ^^^ (pfx * (2 * msk)))) hge
          have h2 : Nat.testBit k' (Nat.log2 (key ^^^ (pfx * (2 * msk)))) =
              Nat.testBit (pfx * (2 * msk)) (Nat.log2 (key ^^^ (pfx * (2 * msk)))) :=
            hk'rep (Nat.log2 (key ^^^ (pfx * (2 * msk)))) (by omega)
          rw [h1] at h2
          cases hc1 : Nat.testBit key (Nat.log2 (key ^^^ (pfx * (2 * msk)))) <;>
            cases hc2 : Nat.testBit (pfx * (2 * msk)) (Nat.log2 (key ^^^ (pfx * (2 * msk)))) <;>
            simp_all
      show (link key (IntMap.tip key val) (pfx * (2 * msk)) (IntMap.bin pfx msk l r)).MaskBound
        (Nat.shiftLeft 1 b)
      simp only [link]
      split <;> simp only [MaskBound] <;>
        (simp only [shiftLeft_one_eq_two_pow]; exact Nat.pow_lt_pow_right (by omega) hltb)
    · rename_i hagreepfx
      have hagreepfx' : prefixOf key msk = pfx := by
        simpa using hagreepfx
      split
      · rename_i hbit
        obtain ⟨hlWF, hlBoundFn⟩ := ihl hl
        have hlBound := hlBoundFn (Nat.log2 msk)
          (represents_of_agree hl_pfx (fun k hk => (hl_bit k hk).trans hbit.symm) hl_bound
            hagreepfx' ⟨b0, hb0⟩)
        rw [hb0, Nat.log2_two_pow, shiftLeft_one_eq_two_pow] at hlBound
        rw [← hb0] at hlBound
        refine ⟨?_, ?_⟩
        · refine WF.bin pfx msk _ r ⟨b0, hb0⟩ ?_ ?_ hr_pfx hr_bit hlBound hr_bound ?_ hrn hlWF hr
          · intro k hk
            rw [hasKey_insert] at hk
            cases hk with
            | inl h => rw [h]; exact hagreepfx'
            | inr h => exact hl_pfx k h
          · intro k hk
            rw [hasKey_insert] at hk
            cases hk with
            | inl h => rw [h]; exact hbit
            | inr h => exact hl_bit k h
          · exact insert_ne_nil key val l
        · intro b hrep
          obtain ⟨hbound_bin, hagree_bin⟩ := hrep
          rw [shiftLeft_one_eq_two_pow] at hbound_bin ⊢
          show (msk : Nat) < 2 ^ b
          exact hbound_bin
      · rename_i hbit
        obtain ⟨hrWF, hrBoundFn⟩ := ihr hr
        have hrBound := hrBoundFn (Nat.log2 msk)
          (represents_of_agree hr_pfx
            (fun k hk => by
              have h1 := hr_bit k hk
              cases hb1 : k &&& msk == 0 <;> cases hb2 : key &&& msk == 0 <;> simp_all)
            hr_bound hagreepfx' ⟨b0, hb0⟩)
        rw [hb0, Nat.log2_two_pow, shiftLeft_one_eq_two_pow] at hrBound
        rw [← hb0] at hrBound
        refine ⟨?_, ?_⟩
        · refine WF.bin pfx msk l _ ⟨b0, hb0⟩ hl_pfx hl_bit ?_ ?_ hl_bound hrBound hln ?_ hl hrWF
          · intro k hk
            rw [hasKey_insert] at hk
            cases hk with
            | inl h => rw [h]; exact hagreepfx'
            | inr h => exact hr_pfx k h
          · intro k hk
            rw [hasKey_insert] at hk
            cases hk with
            | inl h => rw [h]; exact hbit
            | inr h => exact hr_bit k h
          · exact insert_ne_nil key val r
        · intro b hrep
          obtain ⟨hbound_bin, hagree_bin⟩ := hrep
          rw [shiftLeft_one_eq_two_pow] at hbound_bin ⊢
          show (msk : Nat) < 2 ^ b
          exact hbound_bin

theorem insert_wf (key : Nat) (val : α) {m : IntMap α} (h : m.WF) : (m.insert key val).WF :=
  (insert_wf_aux key val h).1

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
