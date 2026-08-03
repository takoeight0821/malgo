/-! Port of the `prettyprinter` package (pinned `1.7.2`, matching
`cabal.project.freeze`; the exact algorithm below is transcribed from
`Prettyprinter.Internal`), restricted to what `Malgo.Debug.PrettyIR` and
`Malgo.Lint.Diagnostic` actually need through `Malgo.Prelude`'s `render`:

  * `render = renderStrict . layoutSmart defaultLayoutOptions` is the ONLY
    layout entry point used anywhere in the Haskell source (`grep -rn
    LayoutOptions src/` turns up nothing else), and `defaultLayoutOptions`
    is never varied (`AvailablePerLine 80 1.0`) — so `layoutPretty`,
    `layoutCompact`, `layoutUnbounded`, and the `PageWidth`/`LayoutOptions`
    types themselves are not ported: the line length (80) and ribbon
    fraction (1.0, giving a ribbon width of exactly 80) are hardcoded into
    `fitsSmart`/`remainingWidth` instead.
  * No annotations: every calling module instantiates `Doc ann` at `ann =
    Void` (or leaves it fully polymorphic and never annotates), so
    `Annotated`/`WithPageWidth`/`SAnnPush`/`SAnnPop` are dropped rather than
    carried as dead weight.

Constructors are named to match Haskell's `Internal.hs` exactly where doing
so introduces no ambiguity (`nest`, `column`, `nesting`, `flatAlt` are
identity-equivalent between the raw constructor and the public combinator
in Haskell too, so one name serves both roles here). Two constructors
collide with a public combinator of the same Haskell name and are
disambiguated: `Line` (the raw hard line break) is `rawLine` here, since
`hardline`/`line`/`line'` are all built from it via `flatAlt`; `Cat` (raw
binary concatenation) is `append` here, exposed as `++` via `Append`. -/

namespace Malgo.Doc

/-- Port of `Prettyprinter.Internal.Doc` (sans `ann`/`Annotated`/`WithPageWidth`
— see the module doc). `text`'s invariant matches Haskell's `Text` node:
length ≥ 2 and no embedded `'\n'` (`fromString`, below, is what enforces
this on arbitrary input — use `text` directly only when the invariant is
already known to hold, e.g. from `atom`). -/
inductive Doc where
  | fail
  | empty
  | char (c : Char)
  | text (len : Nat) (s : String)
  | rawLine
  | flatAlt (whenBroken whenFlat : Doc)
  | append (a b : Doc)
  | nest (i : Int) (d : Doc)
  | union (a b : Doc)
  | column (f : Int → Doc)
  | nesting (f : Int → Doc)

instance : Append Doc := ⟨Doc.append⟩

instance : Inhabited Doc := ⟨.empty⟩

/-- `open Malgo.Doc` only brings names declared directly in this namespace
into scope as bare identifiers — constructors of the nested `Doc` type
still need `Doc.foo`/dot-notation. These four thin re-exports are what let
downstream callers write `nest`/`column`/`nesting`/`flatAlt` bare, matching
how Haskell's `Prettyprinter` exports them (there, `nest`/`column`/
`nesting`/`flatAlt` and the internal `Nest`/`Column`/`Nesting`/`FlatAlt`
constructors are two different, case-distinguished names to begin with). -/
def nest (i : Int) (d : Doc) : Doc := .nest i d
def column (f : Int → Doc) : Doc := .column f
def nesting (f : Int → Doc) : Doc := .nesting f
def flatAlt (whenBroken whenFlat : Doc) : Doc := .flatAlt whenBroken whenFlat

/-- Port of `Prettyprinter.Internal.changesUponFlattening`'s local `flatten`:
flatten a `Doc`, without reporting whether anything actually changed (used
only where the caller already knows `NeverFlat` doesn't apply, i.e. inside
`changesUponFlattening` itself, mirroring the Haskell `where`-bound
helper). -/
partial def flatten : Doc → Doc
  | .flatAlt _ y => flatten y
  | .append x y => .append (flatten x) (flatten y)
  | .nest i x => .nest i (flatten x)
  | .rawLine => .fail
  | .union x _ => flatten x
  | .column f => .column (fun n => flatten (f n))
  | .nesting f => .nesting (fun n => flatten (f n))
  | .fail => .fail
  | .empty => .empty
  | .char c => .char c
  | .text l s => .text l s

/-- Port of `Prettyprinter.Internal.FlattenResult`. -/
inductive FlattenResult where
  | flattened (d : Doc)
  | alreadyFlat
  | neverFlat

instance : Inhabited FlattenResult := ⟨.alreadyFlat⟩

/-- Port of `Prettyprinter.Internal.changesUponFlattening`. -/
partial def changesUponFlattening : Doc → FlattenResult
  | .flatAlt _ y => .flattened (flatten y)
  | .rawLine => .neverFlat
  | .union x _ => .flattened x
  | .nest i x =>
    match changesUponFlattening x with
    | .flattened x' => .flattened (.nest i x')
    | .alreadyFlat => .alreadyFlat
    | .neverFlat => .neverFlat
  | .column f => .flattened (.column (fun n => flatten (f n)))
  | .nesting f => .flattened (.nesting (fun n => flatten (f n)))
  | .append x y =>
    -- Haskell's `case (changesUponFlattening x, changesUponFlattening y) of
    -- (NeverFlat, _) -> NeverFlat; ...` is lazy: it never forces the second
    -- tuple component once the first is `NeverFlat`. A literal Lean port via
    -- a two-column `match` is STRICT — it always evaluates both sides —
    -- turning what should be an early-exit into an unconditional recursion
    -- into `y` on every `Cat`/`append` node, compounding into exponential
    -- blowup on any deeply Cat-chained document (e.g. a function body built
    -- from many `++`-joined statements). Nest the match on `x` first so `y`
    -- is only ever inspected when `x` didn't already decide the answer.
    match changesUponFlattening x with
    | .neverFlat => .neverFlat
    | .flattened x' =>
      match changesUponFlattening y with
      | .neverFlat => .neverFlat
      | .flattened y' => .flattened (.append x' y')
      | .alreadyFlat => .flattened (.append x' y)
    | .alreadyFlat =>
      match changesUponFlattening y with
      | .neverFlat => .neverFlat
      | .flattened y' => .flattened (.append x y')
      | .alreadyFlat => .alreadyFlat
  | .empty => .alreadyFlat
  | .char _ => .alreadyFlat
  | .text .. => .alreadyFlat
  | .fail => .neverFlat

/-- Port of `Prettyprinter.Internal.group`. -/
def group (x : Doc) : Doc :=
  match x with
  | .union .. => x
  | .flatAlt a b =>
    match changesUponFlattening b with
    | .flattened b' => .union b' a
    | .alreadyFlat => .union b a
    | .neverFlat => a
  | _ =>
    match changesUponFlattening x with
    | .flattened x' => .union x' x
    | .alreadyFlat => x
    | .neverFlat => x

/-- Port of `Prettyprinter.Internal.unsafeTextWithoutNewlines`: wrap a
newline-free string in the smallest matching `Doc` constructor. -/
def atom (s : String) : Doc :=
  assert! !s.contains '\n'
  match s.toList with
  | [] => .empty
  | [c] => .char c
  | _ => .text s.length s

/-- Hard line break. Port of `Prettyprinter.Internal.hardline` (`= Line`). -/
def hardline : Doc := .rawLine

/-- Port of `line = FlatAlt Line (Char ' ')`: a line break that `group`
prefers to flatten to a single space. -/
def line : Doc := .flatAlt hardline (.char ' ')

/-- Port of `line' = FlatAlt Line mempty`: like `line`, but flattens to
nothing instead of a space. -/
def line' : Doc := .flatAlt hardline .empty

/-- Port of `softline = Union (Char ' ') Line`. -/
def softline : Doc := .union (.char ' ') hardline

/-- Port of `softline' = Union mempty Line`. -/
def softline' : Doc := .union .empty hardline

/-- Port of `Prettyprinter.Internal.concatWith` (a `foldr1`, so associativity
matches Haskell exactly — not that it's observable here: `Doc.append`'s
tree shape never affects the rendered character stream, only nested
`group`/`flatAlt` placement, and none of `concatWith`'s callers below wrap
individual list elements in either). -/
def concatWith (f : Doc → Doc → Doc) : List Doc → Doc
  | [] => .empty
  | [x] => x
  | x :: xs => f x (concatWith f xs)

/-- Port of `(<+>) x y = x <> Char ' ' <> y`. -/
def appendSpace (x y : Doc) : Doc := x ++ .char ' ' ++ y

@[inherit_doc appendSpace] scoped infixr:60 " <+> " => appendSpace

def hsep : List Doc → Doc := concatWith (· <+> ·)
def vsep : List Doc → Doc := concatWith (fun x y => x ++ line ++ y)
def vcat : List Doc → Doc := concatWith (fun x y => x ++ line' ++ y)
def hcat : List Doc → Doc := concatWith (· ++ ·)

/-- Port of `sep = group . vsep`. -/
def sep (xs : List Doc) : Doc := group (vsep xs)

/-- Port of `cat = group . vcat`. -/
def cat (xs : List Doc) : Doc := group (vcat xs)

/-- Port of `punctuate`: append `p` to every element but the last. -/
def punctuate (p : Doc) : List Doc → List Doc
  | [] => []
  | [d] => [d]
  | d :: ds => (d ++ p) :: punctuate p ds

/-- Port of `encloseSep`. -/
def encloseSep (l r s : Doc) (ds : List Doc) : Doc :=
  match ds with
  | [] => l ++ r
  | [d] => l ++ d ++ r
  | _ :: _ =>
    let seps := l :: List.replicate (ds.length - 1) s
    cat (List.zipWith (· ++ ·) seps ds) ++ r

/-- Port of `list = group . encloseSep (flatAlt "[ " "[") (flatAlt " ]" "]") ", "`. -/
def list (ds : List Doc) : Doc :=
  group (encloseSep (.flatAlt (atom "[ ") (atom "[")) (.flatAlt (atom " ]") (atom "]")) (atom ", ") ds)

/-- Port of `tupled = group . encloseSep (flatAlt "( " "(") (flatAlt " )" ")") ", "`. -/
def tupled (ds : List Doc) : Doc :=
  group (encloseSep (.flatAlt (atom "( ") (atom "(")) (.flatAlt (atom " )") (atom ")")) (atom ", ") ds)

def lbrace : Doc := .char '{'
def rbrace : Doc := .char '}'
def lparen : Doc := .char '('
def rparen : Doc := .char ')'

def dquotes (x : Doc) : Doc := .char '"' ++ x ++ .char '"'
def squotes (x : Doc) : Doc := .char '\'' ++ x ++ .char '\''
def parens (x : Doc) : Doc := lparen ++ x ++ rparen

/-- Port of `instance Pretty Text where pretty = vsep . map
unsafeTextWithoutNewlines . T.splitOn "\n"` — the general "embed arbitrary
text, including embedded newlines, as a `Doc`" primitive. Every `pretty x`
call on a `Text`/`String`-shaped field in the Haskell source (identifiers,
messages, string-literal contents, module names, ...) ports to this. -/
def fromString (s : String) : Doc :=
  match s.splitOn "\n" with
  | [single] => atom single
  | parts => vsep (parts.map atom)

/-- Port of `Prettyprinter.Internal.SimpleDocStream` (sans annotations).

The recursive tail of every emitting constructor is a `Thunk`, not a bare
`SimpleDocStream`. This is load-bearing, not stylistic: Haskell's `best`
builds a `SimpleDocStream` lazily, so `let x' = best ...; let y' = best ...`
at a `Union` (= every `group`) only forces as much of each alternative as
`fits`/`initialIndentation` actually inspect before picking one — the
rejected branch's remainder is never materialized. A literal strict Lean
port (`rest : SimpleDocStream`, built via ordinary recursive calls) has no
such cutoff: constructing `.char c (best ...)` requires fully evaluating the
`best ...` argument (all of it, to the end of the document) before the
`.char` node can exist at all, since constructor arguments are call-by-value.
Since every `group` in a real document re-derives both alternatives this
way, and both alternatives typically contain further nested `group`s, the
cost compounds multiplicatively with nesting depth — genuinely exponential,
confirmed empirically (a 162-line rendered IR that hung for minutes). Wrapping
the tail in `Thunk` restores Haskell's cutoff: `best`'s emitting cases stash
their continuation in `Thunk.mk (fun _ => ...)` instead of calling it inline,
so producing one node is O(depth to the next real token), not O(rest of
document) — `fits` (which walks node-by-node until it finds `w < 0` or a
`Line`) and `selectNicer` (which never inspects the branch it rejects beyond
what `fits` already touched) then get the same short-circuiting Haskell's
laziness gives them for free. -/
inductive SimpleDocStream where
  | fail
  | empty
  | char (c : Char) (rest : Thunk SimpleDocStream)
  | text (len : Nat) (s : String) (rest : Thunk SimpleDocStream)
  | line (indent : Int) (rest : Thunk SimpleDocStream)

instance : Inhabited SimpleDocStream := ⟨.empty⟩

/-- Port of the `LayoutPipeline` continuation list from
`layoutWadlerLeijen` (sans `UndoAnn`). -/
inductive Pipeline where
  | nil
  | cons (indent : Int) (doc : Doc) (rest : Pipeline)

/-- Hardcoded from the only `LayoutOptions` ever constructed in the Haskell
source, `defaultLayoutOptions = LayoutOptions (AvailablePerLine 80 1.0)` —
see the module doc. -/
def lineLength : Int := 80

/-- Port of `remainingWidth` with `ribbonFraction` fixed to `1.0`: the
ribbon width is then exactly `lineLength` (`floor (80 * 1.0) = 80`,
already within `[0, 80]`), so no `Float` arithmetic is needed. -/
def remainingWidth (lineIndent currentColumn : Int) : Int :=
  min (lineLength - currentColumn) (lineIndent + lineLength - currentColumn)

/-- Port of `layoutSmart`'s local `fits`. -/
partial def fitsSmart (lineIndent currentColumn : Int) (initialIndentY : Option Int)
    (sds : SimpleDocStream) : Bool :=
  let minNestingLevel := match initialIndentY with
    | some i => min i currentColumn
    | none => currentColumn
  let rec go (w : Int) (sds' : SimpleDocStream) : Bool :=
    if w < 0 then false
    else match sds' with
      | .fail => false
      | .empty => true
      | .char _ rest => go (w - 1) rest.get
      | .text l _ rest => go (w - (l : Int)) rest.get
      | .line i rest => if minNestingLevel < i then go (lineLength - i) rest.get else true
  go (remainingWidth lineIndent currentColumn) sds

/-- Port of `layoutWadlerLeijen`'s local `initialIndentation`. -/
def initialIndentation : SimpleDocStream → Option Int
  | .line i _ => some i
  | _ => none

/-- Port of `layoutWadlerLeijen`'s local `selectNicer`, specialized to the
`layoutSmart` `FittingPredicate` (`fitsSmart`). -/
def selectNicer (lineIndent currentColumn : Int) (x y : SimpleDocStream) : SimpleDocStream :=
  if fitsSmart lineIndent currentColumn (initialIndentation y) x then x else y

/-- Port of `layoutWadlerLeijen`'s local `best`. Every case that constructs an
emitting `SimpleDocStream` node stashes its continuation in a `Thunk` rather
than computing it inline — see the doc comment on `SimpleDocStream` for why
this is required for correctness (not just performance). -/
partial def best (nl cc : Int) : Pipeline → SimpleDocStream
  | .nil => .empty
  | .cons i d ds =>
    match d with
    | .fail => .fail
    | .empty => best nl cc ds
    | .char c => .char c (Thunk.mk fun _ => best nl (cc + 1) ds)
    | .text l t => .text l t (Thunk.mk fun _ => best nl (cc + (l : Int)) ds)
    | .rawLine =>
      -- `x` must be forced to (at least) WHNF regardless — its own shape
      -- decides `i'` — but under the `Thunk`-tailed representation, WHNF is
      -- cheap (O(depth to the next real token), not O(rest of document)).
      let x := best i i ds
      let i' := match x with
        | .empty => 0
        | .line .. => 0
        | _ => i
      .line i' (Thunk.mk fun _ => x)
    | .flatAlt x _ => best nl cc (.cons i x ds)
    | .append x y => best nl cc (.cons i x (.cons i y ds))
    | .nest j x => best nl cc (.cons (i + j) x ds)
    | .union x y =>
      -- Both alternatives are computed only to WHNF here (cheap, per the
      -- `SimpleDocStream` doc comment); `selectNicer`/`fitsSmart` may walk
      -- further into the accepted one, but the rejected one is returned
      -- (when `y` wins) or discarded (when `x` wins) without ever forcing
      -- past whatever `fitsSmart` itself already touched.
      selectNicer nl cc (best nl cc (.cons i x ds)) (best nl cc (.cons i y ds))
    | .column f => best nl cc (.cons i (f cc) ds)
    | .nesting f => best nl cc (.cons i (f i) ds)

/-- Port of `layoutSmart defaultLayoutOptions`. -/
def layoutSmart (d : Doc) : SimpleDocStream := best 0 0 (.cons 0 d .nil)

/-- Port of `textSpaces n = T.replicate n " "` (`Data.Text.replicate` clamps
a negative count to the empty string; `Int.toNat` already clamps negative
`Int`s to `0`, so this matches without an extra guard). -/
def textSpaces (n : Int) : String := String.ofList (List.replicate n.toNat ' ')

/-- Port of `Prettyprinter.Render.Text.renderStrict` (`renderLazy` composed
with `TL.toStrict`, minus the annotation cases). Accumulates into an
`Array` and joins once, rather than repeated `String.append`, to avoid the
quadratic blowup `SimpleDocStream`'s cons-list shape would otherwise cause
on the larger traces (`Malgo.Debug.Pipeline`'s full-corpus golden). -/
partial def streamToString (sds : SimpleDocStream) : String :=
  let rec go (acc : Array String) : SimpleDocStream → Array String
    | .fail => panic! "Malgo.Doc: SFail reached the renderer (a hardline occurred inside a flattened branch)"
    | .empty => acc
    | .char c rest => go (acc.push (String.singleton c)) rest.get
    | .text _ t rest => go (acc.push t) rest.get
    | .line i rest => go ((acc.push "\n").push (textSpaces i)) rest.get
  String.join (go #[] sds).toList

/-- Port of `Malgo.Prelude.render = renderStrict . layoutSmart defaultLayoutOptions`. -/
def render (d : Doc) : String := streamToString (layoutSmart d)

end Malgo.Doc
