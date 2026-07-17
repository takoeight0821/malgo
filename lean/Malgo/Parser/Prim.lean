import Malgo.Prelude

/-! Megaparsec-mini: the parser-combinator core the CStyle grammar needs,
with megaparsec's consumed-flag backtracking semantics (`<|>` only tries
the alternative when the first branch failed without consuming; `attempt`
is megaparsec `try`). Also ports the lexer layer of
`src/Malgo/Parser/Core.hs` and the `Text.Megaparsec.Char.Lexer` pieces
CStyle uses (`decimal`, `float`, `charLiteral`).

The parser is pure: input is a `List Char` (sources are small; the M4
hyperfine gate guards overall performance). Line/column tracking follows
megaparsec (1-based, tab stop 8). Error messages are simple
`unexpected/expecting` reports for now; byte-parity with
`errorBundlePretty` is milestone M3 and only affects regenerated
error goldens. -/

namespace Malgo.Parser

inductive ErrorItem where
  | tokens (s : String)
  | label (s : String)
  | endOfInput
  deriving BEq, Repr

def ErrorItem.render : ErrorItem → String
  | .tokens s => s!"'{s}'"
  | .label s => s
  | .endOfInput => "end of input"

structure PError where
  sourceName : String
  offset : Nat
  line : Nat
  col : Nat
  unexpected : Option ErrorItem := none
  expected : List ErrorItem := []
  deriving Repr, Inhabited

/-- Megaparsec keeps the furthest error; at equal offsets, expected
sets are unioned. -/
def PError.merge (a b : PError) : PError :=
  if a.offset > b.offset then a
  else if b.offset > a.offset then b
  else { a with
    unexpected := a.unexpected <|> b.unexpected,
    expected := a.expected ++ b.expected.filter (!a.expected.contains ·) }

def PError.render (e : PError) : String :=
  let pos := s!"{e.sourceName}:{e.line}:{e.col}:"
  let unex := match e.unexpected with
    | some item => s!"unexpected {item.render}"
    | none => "parse error"
  let expc := match e.expected with
    | [] => ""
    | items => s!", expecting {", ".intercalate (items.map ErrorItem.render)}"
  s!"{pos} {unex}{expc}"

structure PState where
  sourceName : String
  rest : List Char
  offset : Nat
  line : Nat
  col : Nat

def PState.ofInput (sourceName : String) (input : String) : PState :=
  { sourceName, rest := input.toList, offset := 0, line := 1, col := 1 }

def PState.sourcePos (s : PState) : SourcePos :=
  { sourceName := s.sourceName, line := s.line, column := s.col }

def PState.errorAt (s : PState) (unexpected : Option ErrorItem)
    (expected : List ErrorItem) : PError :=
  { sourceName := s.sourceName, offset := s.offset, line := s.line, col := s.col,
    unexpected, expected }

/-- Advance over one character, tracking line/column like megaparsec
(newline resets, tab jumps to the next 8-wide stop). -/
def PState.advance (s : PState) (c : Char) (rest : List Char) : PState :=
  let (line, col) :=
    if c == '\n' then (s.line + 1, 1)
    else if c == '\t' then (s.line, s.col + 8 - ((s.col - 1) % 8))
    else (s.line, s.col + 1)
  { s with rest, offset := s.offset + 1, line, col }

inductive Reply (α : Type) where
  | ok (a : α) (s : PState) (consumed : Bool)
  | err (e : PError) (consumed : Bool)

instance : Inhabited (Reply α) := ⟨.err default false⟩

def P (α : Type) : Type := PState → Reply α

namespace P

protected def pure (a : α) : P α := fun s => .ok a s false

protected def bind (p : P α) (f : α → P β) : P β := fun s =>
  match p s with
  | .ok a s' c₁ =>
    match f a s' with
    | .ok b s'' c₂ => .ok b s'' (c₁ || c₂)
    | .err e c₂ => .err e (c₁ || c₂)
  | .err e c => .err e c

instance : Monad P where
  pure := P.pure
  bind := P.bind

/-- Megaparsec `<|>`: the alternative runs only if `p` failed without
consuming input; both failing unconsumed merges the errors. -/
protected def orElse (p : P α) (q : Unit → P α) : P α := fun s =>
  match p s with
  | .err e₁ false =>
    match q () s with
    | .err e₂ false => .err (e₁.merge e₂) false
    | r => r
  | r => r

protected def fail (msg : String) : P α := fun s =>
  .err (s.errorAt (some (.label msg)) []) false

instance : Alternative P where
  failure := P.fail "empty"
  orElse := P.orElse

instance : Inhabited (P α) := ⟨P.fail "inhabited"⟩

/-- Megaparsec `try`: failure never counts as consuming. -/
def attempt (p : P α) : P α := fun s =>
  match p s with
  | .err e _ => .err e false
  | r => r

def lookAhead (p : P α) : P α := fun s =>
  match p s with
  | .ok a _ _ => .ok a s false
  | .err e _ => .err e false

def notFollowedBy (p : P α) : P Unit := fun s =>
  match p s with
  | .ok _ _ _ => .err (s.errorAt (some (.label "unexpected input")) []) false
  | .err _ _ => .ok () s false

def getSourcePos : P SourcePos := fun s => .ok s.sourcePos s false

def eof : P Unit := fun s =>
  match s.rest with
  | [] => .ok () s false
  | c :: _ => .err (s.errorAt (some (.tokens (toString c))) [.endOfInput]) false

def satisfy (pred : Char → Bool) (expected : List ErrorItem := []) : P Char := fun s =>
  match s.rest with
  | [] => .err (s.errorAt (some .endOfInput) expected) false
  | c :: cs =>
    if pred c then .ok c (s.advance c cs) true
    else .err (s.errorAt (some (.tokens (toString c))) expected) false

def char (c : Char) : P Char :=
  satisfy (· == c) [.tokens (toString c)]

def oneOf (cs : String) : P Char :=
  satisfy (cs.toList.contains ·) (cs.toList.map fun c => .tokens (toString c))

/-- Megaparsec `string`/`tokens`: atomic — a partial match fails without
consuming. -/
def string (t : String) : P String := fun s =>
  let rec go (cs : List Char) (st : PState) : Option PState :=
    match cs with
    | [] => some st
    | c :: cs' =>
      match st.rest with
      | c' :: rest' => if c == c' then go cs' (st.advance c' rest') else none
      | [] => none
  match go t.toList s with
  | some st => .ok t st (!t.isEmpty)
  | none =>
    .err (s.errorAt (s.rest.head?.map fun c => .tokens (toString c)) [.tokens t]) false

/-- Always succeeds; consumes the matching span. -/
partial def takeWhileP (pred : Char → Bool) : P String := fun s =>
  let rec go (st : PState) (acc : List Char) : PState × List Char :=
    match st.rest with
    | c :: cs => if pred c then go (st.advance c cs) (c :: acc) else (st, acc)
    | [] => (st, acc)
  let (st, acc) := go s []
  .ok (String.ofList acc.reverse) st (st.offset != s.offset)

/-- Megaparsec `many`, except a zero-width success stops the loop
instead of raising a runtime error. -/
partial def many (p : P α) : P (List α) := fun s =>
  let rec go (st : PState) (acc : List α) (consumed : Bool) : Reply (List α) :=
    match p st with
    | .ok a st' true => go st' (a :: acc) true
    | .ok _ _ false => .ok acc.reverse st consumed
    | .err _ false => .ok acc.reverse st consumed
    | .err e true => .err e true
  go s [] false

def some (p : P α) : P (NEList α) := do
  let x ← p
  let xs ← many p
  return ⟨x, xs⟩

def choice (ps : List (P α)) : P α :=
  match ps with
  | [] => P.fail "empty choice"
  | p :: ps => ps.foldl (fun acc q => acc <|> q) p

/-- Haskell `Malgo.Parser.Core.optional`: note the `try`. -/
def optional (p : P α) : P (Option α) :=
  attempt (some' <$> p) <|> pure none
where some' (a : α) : Option α := Option.some a

def option (a : α) (p : P α) : P α :=
  p <|> pure a

def sepBy1 (p : P α) (sep : P β) : P (NEList α) := do
  let x ← p
  let xs ← many (sep *> p)
  return ⟨x, xs⟩

def sepBy (p : P α) (sep : P β) : P (List α) :=
  (NEList.toList <$> sepBy1 p sep) <|> pure []

/- `sepEndBy1`/`sepEndBy` allow an optional trailing separator. Defined
by interleaving (like `parser-combinators`) rather than via `sepBy1`, so a
trailing separator followed by an unconsumed element failure stays an
unconsumed failure the recursion can recover from. -/
mutual
partial def sepEndBy1 (p : P α) (sep : P β) : P (NEList α) := do
  let x ← p
  (do
    _ ← sep
    let xs ← sepEndBy p sep
    return ⟨x, xs⟩)
  <|> pure ⟨x, []⟩

partial def sepEndBy (p : P α) (sep : P β) : P (List α) :=
  (NEList.toList <$> sepEndBy1 p sep) <|> pure []
end

def between (open_ : P α) (close : P β) (p : P γ) : P γ := do
  _ ← open_
  let x ← p
  _ ← close
  return x

partial def manyTill (p : P α) (endP : P β) : P (List α) := fun s =>
  let rec go (st : PState) (acc : List α) (consumed : Bool) : Reply (List α) :=
    match endP st with
    | .ok _ st' c => .ok acc.reverse st' (consumed || c)
    | .err e true => .err e true
    | .err _ false =>
      match p st with
      | .ok a st' c => go st' (a :: acc) (consumed || c)
      | .err e c => .err e (consumed || c)
  go s [] false

/-- `makeExprParser` one-level `InfixL` replacement. -/
partial def chainl1 (term : P α) (op : P (α → α → α)) : P α := do
  let x ← term
  go x
where
  go (x : α) : P α :=
    (do
      let f ← op
      let y ← term
      go (f x y))
    <|> pure x

/-- `makeExprParser` one-level `InfixR` replacement. -/
partial def chainr1 (term : P α) (op : P (α → α → α)) : P α := do
  let x ← term
  (do
    let f ← op
    let y ← chainr1 term op
    pure (f x y))
  <|> pure x

def run (p : P α) (sourceName : String) (input : String) : Except PError α :=
  match p (PState.ofInput sourceName input) with
  | .ok a _ _ => .ok a
  | .err e _ => .error e

/-! ## Character classes (ASCII; extend if a source file needs Unicode) -/

def letterChar : P Char := satisfy Char.isAlpha [.label "letter"]
def alphaNumChar : P Char := satisfy Char.isAlphanum [.label "alphanumeric character"]
def digitChar : P Char := satisfy Char.isDigit [.label "digit"]

/-! ## Lexer layer (port of `Malgo.Parser.Core`) -/

private def isSpaceChar (c : Char) : Bool :=
  c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0b' || c == '\x0c'

private def space1 : P Unit := do
  let s ← takeWhileP isSpaceChar
  if s.isEmpty then P.fail "whitespace" else pure ()

private def skipLineComment : P Unit := do
  _ ← string "--"
  _ ← takeWhileP (· != '\n')

private partial def skipBlockComment : P Unit := do
  _ ← string "{-"
  skipRest
where
  skipRest : P Unit := do
    _ ← takeWhileP (· != '-')
    (do _ ← string "-}"; pure ()) <|> (do _ ← char '-'; skipRest) <|> pure ()

/-- `L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")` -/
partial def space : P Unit := fun s =>
  match (space1 <|> skipLineComment <|> skipBlockComment) s with
  | .ok _ s' true => space s'
  | _ => .ok () s false

def lexeme (p : P α) : P α := do
  let x ← p
  space
  return x

def symbol (t : String) : P Unit := do
  _ ← lexeme (string t)

/-- `L.decimal` (unsigned) wrapped in `lexeme` like `Core.decimal`. -/
def decimal : P Int := lexeme do
  let ds ← some digitChar
  return ds.toList.foldl (fun acc d => acc * 10 + (d.toNat - '0'.toNat)) (0 : Int)

def skipPragma : P Unit := lexeme do
  _ ← char '#'
  _ ← takeWhileP (· != '\n')

def identStart : P Char := letterChar <|> char '_'

def identContinue : P Char := alphaNumChar <|> char '_' <|> char '#'

def rawIdent : P String := do
  let c ← identStart
  let cs ← many identContinue
  return String.ofList (c :: cs)

def reservedKeywords : List String :=
  ["class", "def", "data", "exists", "forall", "foreign", "goto", "impl",
   "import", "infix", "infixl", "infixr", "label", "let", "type", "module", "with"]

def reserved (w : String) : P Unit :=
  if reservedKeywords.contains w then
    lexeme do
      _ ← string w
      notFollowedBy identContinue
  else
    P.fail s!"reserved keyword: {w}"

def anyReserved : P Unit :=
  choice (reservedKeywords.map fun w => attempt (reserved w))

def ident : P String := lexeme do
  notFollowedBy anyReserved
  rawIdent

def operatorChar : P Char := oneOf "+-*/\\%=><:;|&!#."

def reservedOperators : List String :=
  ["=>", "=", ":", "|", "->", ";", ".", ",", "!", "#|", "|#", "~"]

def reservedOperator (w : String) : P Unit :=
  if reservedOperators.contains w then
    lexeme do
      _ ← string w
      notFollowedBy operatorChar
  else
    P.fail s!"reserved symbol: {w}"

def anyReservedOperator : P Unit :=
  choice (reservedOperators.map fun w => attempt (reservedOperator w))

def operator : P String := lexeme do
  notFollowedBy anyReservedOperator
  let cs ← some operatorChar
  return String.ofList cs.toList

/-- `manyUnaryOp`: one or more unary operators, applied in parse order. -/
def manyUnaryOp (singleUnaryOp : P (α → α)) : P (α → α) := do
  let fs ← some singleUnaryOp
  return fun x => fs.toList.foldl (fun acc f => f acc) x

def captureRange (action : P (Range → β)) : P β := do
  let start ← getSourcePos
  let result ← action
  let stop ← getSourcePos
  return result { start, stop }

/-! ## Literal helpers (`Text.Megaparsec.Char.Lexer` pieces CStyle uses) -/

/-- `L.charLiteral`: one possibly-escaped character as it appears inside
a Haskell string literal. Covers the escapes Malgo sources use
(single-char escapes and decimal codes); extend if a golden demands
the full Haskell `read` syntax. -/
def charLiteral : P Char :=
  (do
    _ ← char '\\'
    (char 'n' *> pure '\n') <|>
    (char 't' *> pure '\t') <|>
    (char 'r' *> pure '\r') <|>
    (char '\\' *> pure '\\') <|>
    (char '\'' *> pure '\'') <|>
    (char '"' *> pure '"') <|>
    (char '0' *> pure '\x00') <|>
    (char 'a' *> pure '\x07') <|>
    (char 'b' *> pure '\x08') <|>
    (char 'f' *> pure '\x0c') <|>
    (char 'v' *> pure '\x0b') <|>
    (do
      let ds ← some digitChar
      let n := ds.toList.foldl (fun acc d => acc * 10 + (d.toNat - '0'.toNat)) 0
      if n.isValidChar then pure (Char.ofNat n) else P.fail "invalid character code"))
  <|> satisfy (fun _ => true) [.label "literal character"]

/-- `L.float`: an unsigned float; requires a fractional part and/or
exponent (megaparsec rejects a bare integer). -/
def float : P Float := do
  let intPart ← some digitChar
  let frac ← optional (attempt do
    _ ← char '.'
    some digitChar)
  let exp ← optional (attempt do
    _ ← char 'e' <|> char 'E'
    let sign ← option '+' (char '+' <|> char '-')
    let ds ← some digitChar
    pure (sign, ds))
  if frac.isNone && exp.isNone then
    P.fail "float"
  else
    let digitsVal (ds : NEList Char) : Float :=
      ds.toList.foldl (fun acc d => acc * 10 + Float.ofNat (d.toNat - '0'.toNat)) 0
    let base := digitsVal intPart
    let withFrac := match frac with
      | .none => base
      | .some ds => base + digitsVal ds / Float.ofNat (10 ^ ds.toList.length)
    let value := match exp with
      | .none => withFrac
      | .some (sign, ds) =>
        let e := ds.toList.foldl (fun acc d => acc * 10 + (d.toNat - '0'.toNat)) 0
        if sign == '-' then withFrac / Float.ofNat (10 ^ e)
        else withFrac * Float.ofNat (10 ^ e)
    pure value

end P

end Malgo.Parser
