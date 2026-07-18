/-
Line- and word-level diffing between two rendered IR stages, for the MET
(M-exp-Tracer) debug tool and its golden tests. Ported from
`src/Malgo/Debug/DiffView.hs`.

The line/token diff is `Data.Algorithm.Diff`'s `getGroupedDiff` (Diff 1.0.2), a
Myers O(ND) diff. Its tie-breaking when several longest common subsequences
exist is load-bearing: the ~89 `Malgo.Debug.PrettyIR` golden files pin
`renderUnifiedDiff`'s output byte-for-byte. This port reimplements Diff's exact
Myers machinery (`DL`/`dstep`/`addsnake`/`lcs`) faithfully rather than a plain
DP-LCS, because a DP-LCS with either simple tie rule diverges from Myers on
inputs with larger alphabets (verified empirically against the pinned library).
-/

namespace Malgo.Debug.DiffView

/-! ## Generic Myers diff (port of Data.Algorithm.Diff 1.0.2) -/

/-- A value is either from the first list, the second, or from both. -/
inductive PolyDiff (α β : Type) where
  | first : α → PolyDiff α β
  | second : β → PolyDiff α β
  | both : α → β → PolyDiff α β
  deriving Repr, DecidableEq, BEq, Inhabited

/-- A non-diagonal step in the edit path: advance the first (`F`) or second (`S`) list. -/
inductive DI where
  | F
  | S
  deriving Repr, DecidableEq, BEq, Inhabited

/-- A diagonal endpoint carrying the edit path taken to reach it. -/
structure DL where
  poi : Nat
  poj : Nat
  path : List DI
  deriving Repr, Inhabited

/-- `Ord DL`: `x ≤ y = if poi x == poi y then poj x > poj y else poi x ≤ poi y`. -/
def DL.le (x y : DL) : Bool :=
  if x.poi == y.poi then x.poj > y.poj else x.poi ≤ y.poi

def DL.max (x y : DL) : DL := if DL.le x y then y else x

/-- Extend a `DL` along the diagonal while the two lists agree. -/
partial def addsnake (cd : Nat → Nat → Bool) (dl : DL) : DL :=
  if cd dl.poi dl.poj then
    addsnake cd { dl with poi := dl.poi + 1, poj := dl.poj + 1 }
  else dl

private def nextDLs (cd : Nat → Nat → Bool) : List DL → List DL
  | [] => []
  | dl :: rest =>
    let pdl := dl.path
    let dl' := addsnake cd { dl with poi := dl.poi + 1, path := DI.F :: pdl }
    let dl'' := addsnake cd { dl with poj := dl.poj + 1, path := DI.S :: pdl }
    dl' :: dl'' :: nextDLs cd rest

private def pairMaxes : List DL → List DL
  | [] => []
  | [x] => [x]
  | x :: y :: rest => DL.max x y :: pairMaxes rest

def dstep (cd : Nat → Nat → Bool) (dls : List DL) : List DL :=
  match nextDLs cd dls with
  | [] => []
  | hd :: rst => hd :: pairMaxes rst

/-- Mirror of Diff's `concat . iterate (dstep cd) . …`: scan each level in list
    order for the first `DL` that reached the end, deepening a level otherwise. -/
partial def lcsLoop (cd : Nat → Nat → Bool) (lena lenb : Nat) (dls : List DL) : List DI :=
  match dls.find? (fun dl => dl.poi == lena && dl.poj == lenb) with
  | some dl => dl.path
  | none => lcsLoop cd lena lenb (dstep cd dls)

def lcsPath (eq : α → α → Bool) (as bs : List α) : List DI :=
  let arA := as.toArray
  let arB := bs.toArray
  let lena := arA.size
  let lenb := arB.size
  let cd := fun (i j : Nat) =>
    match arA[i]?, arB[j]? with
    | some a, some b => eq a b
    | _, _ => false
  lcsLoop cd lena lenb [addsnake cd ⟨0, 0, []⟩]

private partial def markup (eq : α → α → Bool) :
    List α → List α → List DI → List (PolyDiff α α)
  | x :: xs, y :: ys, ds =>
    if eq x y then PolyDiff.both x y :: markup eq xs ys ds
    else match ds with
      | DI.F :: ds' => PolyDiff.first x :: markup eq xs (y :: ys) ds'
      | DI.S :: ds' => PolyDiff.second y :: markup eq (x :: xs) ys ds'
      | [] => []
  | x :: xs, [], DI.F :: ds' => PolyDiff.first x :: markup eq xs [] ds'
  | [], y :: ys, DI.S :: ds' => PolyDiff.second y :: markup eq [] ys ds'
  | _, _, _ => []

def getDiffBy (eq : α → α → Bool) (as bs : List α) : List (PolyDiff α α) :=
  markup eq as bs (lcsPath eq as bs).reverse

def getDiff [BEq α] (as bs : List α) : List (PolyDiff α α) :=
  getDiffBy (· == ·) as bs

private partial def goFirsts : List (PolyDiff α α) → List α × List (PolyDiff α α)
  | PolyDiff.first x :: xs => let (fs, rest) := goFirsts xs; (x :: fs, rest)
  | xs => ([], xs)

private partial def goSeconds : List (PolyDiff α α) → List α × List (PolyDiff α α)
  | PolyDiff.second x :: xs => let (fs, rest) := goSeconds xs; (x :: fs, rest)
  | xs => ([], xs)

private partial def goBoth : List (PolyDiff α α) → List (α × α) × List (PolyDiff α α)
  | PolyDiff.both x y :: xs => let (fs, rest) := goBoth xs; ((x, y) :: fs, rest)
  | xs => ([], xs)

private partial def groupGo : List (PolyDiff α α) → List (PolyDiff (List α) (List α))
  | PolyDiff.first x :: xs =>
    let (fs, rest) := goFirsts xs; PolyDiff.first (x :: fs) :: groupGo rest
  | PolyDiff.second x :: xs =>
    let (fs, rest) := goSeconds xs; PolyDiff.second (x :: fs) :: groupGo rest
  | PolyDiff.both x y :: xs =>
    let (fs, rest) := goBoth xs
    let (fxs, fys) := fs.unzip
    PolyDiff.both (x :: fxs) (y :: fys) :: groupGo rest
  | [] => []

def getGroupedDiffBy (eq : α → α → Bool) (as bs : List α) :
    List (PolyDiff (List α) (List α)) :=
  groupGo (getDiffBy eq as bs)

def getGroupedDiff [BEq α] (as bs : List α) : List (PolyDiff (List α) (List α)) :=
  getGroupedDiffBy (· == ·) as bs

-- The two documented Diff doctests, pinned as regression checks for the
-- tie-breaking rule (empty ⟹ prefer neither; must match the pinned library).
#guard getDiff "abcde".toList "acdf".toList ==
  [PolyDiff.both 'a' 'a', .first 'b', .both 'c' 'c', .both 'd' 'd', .first 'e', .second 'f']
#guard getGroupedDiff "abcde".toList "acdf".toList ==
  [PolyDiff.both ['a'] ['a'], .first ['b'], .both ['c', 'd'] ['c', 'd'],
   .first ['e'], .second ['f']]

/-! ## Haskell-compatible `Data.Text` line helpers -/

/-- `Data.Text.lines`: split on `'\n'`; a trailing newline does not yield a
    trailing empty line, and the empty text yields no lines. -/
def hsLines (s : String) : List String :=
  if s.isEmpty then []
  else
    let parts := s.splitOn "\n"
    if s.toList.getLast? == some '\n' then parts.dropLast else parts

/-- `Data.Text.unlines`: append `'\n'` after every line. -/
def hsUnlines (ls : List String) : String :=
  String.join (ls.map (· ++ "\n"))

/-! ## DiffView data types -/

inductive LineTag where
  | unchanged
  | added
  | removed
  deriving Repr, DecidableEq, BEq, Inhabited

/-- One word-level run within a line, tagged with how it differs from the
    other side. -/
structure Span where
  tag : LineTag
  text : String
  deriving Repr, DecidableEq, BEq, Inhabited

structure DiffLine where
  lineTag : LineTag
  spans : List Span
  deriving Repr, DecidableEq, BEq, Inhabited

structure SideBySideRow where
  leftTag : Option LineTag
  leftSpans : Option (List Span)
  rightTag : Option LineTag
  rightSpans : Option (List Span)
  deriving Repr, DecidableEq, BEq, Inhabited

/-! ## Tokenization -/

inductive CharClass where
  | spaceC
  | identC
  | punctC
  deriving DecidableEq, BEq

/-- ASCII `Data.Char.isSpace`: space, tab, newline, vtab, formfeed, carriage return. -/
def isSpaceC (c : Char) : Bool :=
  let n := c.toNat
  c == ' ' || (9 ≤ n && n ≤ 13)

/-- ASCII `Data.Char.isAlphaNum`. -/
def isAlphaNumC (c : Char) : Bool := c.isAlpha || c.isDigit

def classify (c : Char) : CharClass :=
  if isSpaceC c then .spaceC
  else if isAlphaNumC c || c == '_' || c == '$' || c == '#' || c == '\'' then .identC
  else .punctC

/-- `Data.List.groupBy` for an equivalence relation, comparing adjacent
    elements (identical result to first-element comparison for an equivalence). -/
private def groupByAdj (eq : α → α → Bool) : List α → List (List α)
  | [] => []
  | x :: xs =>
    match groupByAdj eq xs with
    | [] => [[x]]
    | grp :: gs =>
      match grp with
      | [] => [x] :: gs
      | y :: _ => if eq x y then (x :: grp) :: gs else [x] :: grp :: gs

/-- Split a line into maximal runs of identifier / whitespace / punctuation
    characters. Concatenating the tokens reconstructs the line exactly. -/
def tokenize (s : String) : List String :=
  (groupByAdj (fun a b => classify a == classify b) s.toList).map String.ofList

/-! ## Word- and line-level diffs -/

/-- Word-level diff between two single lines, returning annotated spans for the
    "before" and "after" side respectively. -/
def diffWords (before after : String) : List Span × List Span :=
  let grouped := getGroupedDiff (tokenize before) (tokenize after)
  let toBefore : PolyDiff (List String) (List String) → List Span
    | .both ts _ => [⟨.unchanged, String.join ts⟩]
    | .first ts => [⟨.removed, String.join ts⟩]
    | .second _ => []
  let toAfter : PolyDiff (List String) (List String) → List Span
    | .both _ ts => [⟨.unchanged, String.join ts⟩]
    | .first _ => []
    | .second ts => [⟨.added, String.join ts⟩]
  ((grouped.map toBefore).flatten, (grouped.map toAfter).flatten)

private def wordPair (b a : String) : List (LineTag × List Span) :=
  let (bSpans, aSpans) := diffWords b a
  [(.removed, bSpans), (.added, aSpans)]

private def pairLines (bs as : List String) : List (LineTag × List Span) :=
  let n := min bs.length as.length
  let bTail := bs.drop n
  let aTail := as.drop n
  (List.zipWith wordPair (bs.take n) (as.take n)).flatten
    ++ bTail.map (fun l => (LineTag.removed, [⟨.removed, l⟩]))
    ++ aTail.map (fun l => (LineTag.added, [⟨.added, l⟩]))

private partial def pairedGo :
    List (PolyDiff (List String) (List String)) → List (LineTag × List Span)
  | [] => []
  | .both bs _ :: rest =>
    bs.map (fun l => (LineTag.unchanged, [⟨.unchanged, l⟩])) ++ pairedGo rest
  | .first bs :: .second as :: rest => pairLines bs as ++ pairedGo rest
  | .first bs :: rest =>
    bs.map (fun l => (LineTag.removed, [⟨.removed, l⟩])) ++ pairedGo rest
  | .second as :: rest =>
    as.map (fun l => (LineTag.added, [⟨.added, l⟩])) ++ pairedGo rest

/-- Groups line-diff blocks, pairing a same-position replace (a `First` run
    immediately followed by a `Second` run) into word-diffed line pairs. -/
def pairedLineDiff (before after : String) : List (LineTag × List Span) :=
  pairedGo (getGroupedDiff (hsLines before) (hsLines after))

/-- A git-style plus/minus listing between two texts, line by line, with
    word-level spans inside each changed line. -/
def unifiedDiff (before after : String) : List DiffLine :=
  (pairedLineDiff before after).map (fun (tag, spans) => ⟨tag, spans⟩)

private def linePrefix : LineTag → String
  | .unchanged => "  "
  | .added => "+ "
  | .removed => "- "

private def renderLine (dl : DiffLine) : String :=
  linePrefix dl.lineTag ++ String.join (dl.spans.map (·.text))

/-- Render a unified diff as plain text, one line per `DiffLine`, prefixed with
    `"  "`/`"+ "`/`"- "`. This exact text is pinned by the PrettyIR goldens. -/
def renderUnifiedDiff (before after : String) : String :=
  hsUnlines ((unifiedDiff before after).map renderLine)

private def rowFor (b a : String) : SideBySideRow :=
  let (bSpans, aSpans) := diffWords b a
  ⟨some .removed, some bSpans, some .added, some aSpans⟩

private def pairRows (bs as : List String) : List SideBySideRow :=
  let n := min bs.length as.length
  let bTail := bs.drop n
  let aTail := as.drop n
  List.zipWith rowFor (bs.take n) (as.take n)
    ++ bTail.map (fun l => ⟨some .removed, some [⟨.removed, l⟩], none, none⟩)
    ++ aTail.map (fun l => ⟨none, none, some .added, some [⟨.added, l⟩]⟩)

private partial def sbsGo :
    List (PolyDiff (List String) (List String)) → List SideBySideRow
  | [] => []
  | .both bs _ :: rest =>
    bs.map (fun l =>
      (⟨some .unchanged, some [⟨.unchanged, l⟩], some .unchanged,
        some [⟨.unchanged, l⟩]⟩ : SideBySideRow)) ++ sbsGo rest
  | .first bs :: .second as :: rest => pairRows bs as ++ sbsGo rest
  | .first bs :: rest =>
    bs.map (fun l =>
      (⟨some .removed, some [⟨.removed, l⟩], none, none⟩ : SideBySideRow)) ++ sbsGo rest
  | .second as :: rest =>
    as.map (fun l =>
      (⟨none, none, some .added, some [⟨.added, l⟩]⟩ : SideBySideRow)) ++ sbsGo rest

/-- Pairs lines for a left/right panel view. -/
def sideBySide (before after : String) : List SideBySideRow :=
  sbsGo (getGroupedDiff (hsLines before) (hsLines after))

end Malgo.Debug.DiffView
