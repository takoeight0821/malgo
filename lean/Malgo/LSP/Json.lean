/-! Minimal JSON value type with encoder and decoder.

Port of `Malgo.LSP.Json` (`malgo-lsp/src/Malgo/LSP/Json.hs`). A from-scratch
JSON codec — no library dependency (Haskell deliberately avoids `aeson`; Lean
likewise avoids `Lean.Data.Json` here). `JObject` is an ordered assoc list, not
a map: field order is preserved for the wire encoding. -/

namespace Malgo.LSP.Json

/-- Minimal JSON value representation. `object` fields are an ordered
    assoc list (order matters on the wire), matching Haskell's `JObject`. -/
inductive JValue where
  | null
  | bool (b : Bool)
  | number (n : Float)
  | string (s : String)
  | array (xs : List JValue)
  | object (fields : List (String × JValue))
  deriving Repr, Inhabited, BEq

/-- Build a JSON object from key-value pairs. -/
def jObject (fields : List (String × JValue)) : JValue := .object fields

/-- Create a key-value pair for a JSON object. -/
def kv (k : String) (v : JValue) : String × JValue := (k, v)

@[inherit_doc kv] infixr:60 " .= " => kv

/-- Look up a required field in a JSON object. -/
def jLookup (key : String) : JValue → Option JValue
  | .object kvs => List.lookup key kvs
  | _ => none

/-- Look up an optional field in a JSON object (returns `null` if missing). -/
def jLookupMaybe (key : String) : JValue → Option JValue
  | .object kvs => some ((List.lookup key kvs).getD .null)
  | _ => none

/-- Extract a `String` from a `string`. -/
def jText : JValue → Option String
  | .string t => some t
  | _ => none

private def intOfFloat (f : Float) : Int :=
  if f < 0 then - Int.ofNat (-f).toUInt64.toNat else Int.ofNat f.toUInt64.toNat

/-- Extract an `Int` from a `number` (rounded to nearest). -/
def jInt : JValue → Option Int
  | .number n => some (intOfFloat n.round)
  | _ => none

/-- Extract a `Bool` from a `bool`. -/
def jBool : JValue → Option Bool
  | .bool b => some b
  | _ => none

/-- Extract a list from an `array`. -/
def jArray : JValue → Option (List JValue)
  | .array xs => some xs
  | _ => none

/-- Extract fields from an `object`. -/
def jFields : JValue → Option (List (String × JValue))
  | .object kvs => some kvs
  | _ => none

/-! ## Encoder -/

private def hexDigit (d : Nat) : Char :=
  if d < 10 then Char.ofNat ('0'.toNat + d) else Char.ofNat ('a'.toNat + d - 10)

private def hex4 (n : Nat) : String :=
  String.ofList [hexDigit (n / 4096 % 16), hexDigit (n / 256 % 16), hexDigit (n / 16 % 16), hexDigit (n % 16)]

private def escapeChar (c : Char) : String :=
  match c with
  | '"' => "\\\""
  | '\\' => "\\\\"
  | '\n' => "\\n"
  | '\r' => "\\r"
  | '\t' => "\\t"
  | _ => if c.toNat < 0x20 then "\\u" ++ hex4 c.toNat else String.singleton c

private def buildString (s : String) : String :=
  "\"" ++ s.foldl (fun acc c => acc ++ escapeChar c) "" ++ "\""

private def encodeNumber (n : Float) : String :=
  if n.isNaN || n.isInf then "null"
  else if n == n.floor then toString (intOfFloat n)
  else n.toString

private partial def buildValue : JValue → String
  | .null => "null"
  | .bool true => "true"
  | .bool false => "false"
  | .number n => encodeNumber n
  | .string s => buildString s
  | .array xs => "[" ++ String.intercalate "," (xs.map buildValue) ++ "]"
  | .object kvs =>
    "{" ++ String.intercalate "," (kvs.map (fun (k, v) => buildString k ++ ":" ++ buildValue v)) ++ "}"

/-- Encode a `JValue` to a JSON `String`. Numbers with no fractional part print
    as bare integer literals (no `.0`), matching Haskell — critical so a
    JSON-RPC `id` round-trips as `1`, not `1.0`. -/
def encodeJson (v : JValue) : String := buildValue v

/-! ## Decoder -/

private def isWs (c : Char) : Bool :=
  c == ' ' || c == '\t' || c == '\n' || c == '\r'

private def isDigit (c : Char) : Bool := '0' ≤ c && c ≤ '9'

private def isHexDigit (c : Char) : Bool :=
  isDigit c || ('a' ≤ c && c ≤ 'f') || ('A' ≤ c && c ≤ 'F')

private def hexVal (c : Char) : Nat :=
  if isDigit c then c.toNat - '0'.toNat
  else if 'a' ≤ c && c ≤ 'f' then c.toNat - 'a'.toNat + 10
  else if 'A' ≤ c && c ≤ 'F' then c.toNat - 'A'.toNat + 10
  else 0

private def dropWs (s : List Char) : List Char := s.dropWhile isWs

private partial def parseStringChars (acc : List Char) : List Char → Option (JValue × List Char)
  | '"' :: rest => some (.string (String.ofList acc.reverse), rest)
  | '\\' :: '"' :: rest => parseStringChars ('"' :: acc) rest
  | '\\' :: '\\' :: rest => parseStringChars ('\\' :: acc) rest
  | '\\' :: '/' :: rest => parseStringChars ('/' :: acc) rest
  | '\\' :: 'n' :: rest => parseStringChars ('\n' :: acc) rest
  | '\\' :: 'r' :: rest => parseStringChars ('\r' :: acc) rest
  | '\\' :: 't' :: rest => parseStringChars ('\t' :: acc) rest
  | '\\' :: 'b' :: rest => parseStringChars (Char.ofNat 8 :: acc) rest
  | '\\' :: 'f' :: rest => parseStringChars (Char.ofNat 12 :: acc) rest
  | '\\' :: 'u' :: a :: b :: c :: d :: rest =>
    if isHexDigit a && isHexDigit b && isHexDigit c && isHexDigit d then
      let code := hexVal a * 4096 + hexVal b * 256 + hexVal c * 16 + hexVal d
      -- Astral (non-BMP) characters are encoded as a UTF-16 surrogate
      -- pair of two `\uXXXX` escapes (what e.g. Python's `json.dumps
      -- (ensure_ascii=True)` produces). `Char.ofNat` rejects the whole
      -- surrogate range [0xD800, 0xDFFF] as an invalid Unicode scalar
      -- value (falling back to NUL) — unlike Haskell's `chr`, which
      -- accepts any Int as a `Char` including individually-invalid
      -- surrogate halves — so a lone `Char.ofNat` per escape would
      -- silently corrupt every astral character into two NUL bytes.
      -- Detect a high surrogate and combine it with an immediately
      -- following low-surrogate escape into the real codepoint; a
      -- genuinely lone/unpaired surrogate (malformed input) falls back
      -- to U+FFFD (the standard Unicode replacement character) instead
      -- of NUL, which is closer to what real JSON decoders do and at
      -- least doesn't inject a silent NUL byte into the string.
      if 0xD800 ≤ code && code ≤ 0xDBFF then
        match rest with
        | '\\' :: 'u' :: a2 :: b2 :: c2 :: d2 :: rest' =>
          if isHexDigit a2 && isHexDigit b2 && isHexDigit c2 && isHexDigit d2 then
            let code2 := hexVal a2 * 4096 + hexVal b2 * 256 + hexVal c2 * 16 + hexVal d2
            if 0xDC00 ≤ code2 && code2 ≤ 0xDFFF then
              let combined := 0x10000 + (code - 0xD800) * 0x400 + (code2 - 0xDC00)
              parseStringChars (Char.ofNat combined :: acc) rest'
            else
              parseStringChars (Char.ofNat 0xFFFD :: acc) rest
          else
            parseStringChars (Char.ofNat 0xFFFD :: acc) rest
        | _ => parseStringChars (Char.ofNat 0xFFFD :: acc) rest
      else if 0xDC00 ≤ code && code ≤ 0xDFFF then
        parseStringChars (Char.ofNat 0xFFFD :: acc) rest
      else
        parseStringChars (Char.ofNat code :: acc) rest
    else
      parseStringChars ('\\' :: acc) ('u' :: a :: b :: c :: d :: rest)
  | c :: rest => parseStringChars (c :: acc) rest
  | [] => none

private def parseString (s : List Char) : Option (JValue × List Char) := parseStringChars [] s

private def digitsToNat (ds : List Char) : Nat :=
  ds.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0

/-- Parse a numeric substring (the whole slice, mirroring Haskell's
    `reads … [(n, "")]` full-consume requirement) into a `Float`.

    A `.`/`e`/`E` marker MUST be followed by at least one digit — JSON's
    number grammar requires it, and so does Haskell's `reads` (`reads
    "5." :: [(Double,String)]` yields `[(5.0, ".")]`, a non-empty leftover
    that fails the full-consume check). The `.span isDigit` calls below
    only bound how many digits get *consumed*; they don't validate that
    any were found, so each marker branch explicitly rejects a
    zero-digit tail instead of silently letting the empty leftover pass
    the final full-consume check (which is what let `"5."`, `"1e"`,
    `"1e+"`, and `"1.e5"` through as valid numbers before this fix). -/
private def parseFloatComplete (chars : List Char) : Option Float :=
  let (neg, r0) := match chars with
    | '-' :: t => (true, t)
    | _ => (false, chars)
  let (intDs, r1) := r0.span isDigit
  if intDs.isEmpty then none
  else
    let fracResult : Option (List Char × List Char) := match r1 with
      | '.' :: t =>
        let (fd, r) := t.span isDigit
        if fd.isEmpty then none else some (fd, r)
      | _ => some ([], r1)
    match fracResult with
    | none => none
    | some (fracDs, r2) =>
      let expResult : Option (Bool × List Char × List Char) := match r2 with
        | e :: t =>
          if e == 'e' || e == 'E' then
            let (esign, t2) := match t with
              | '+' :: x => (false, x)
              | '-' :: x => (true, x)
              | _ => (false, t)
            let (ed, r) := t2.span isDigit
            if ed.isEmpty then none else some (esign, ed, r)
          else some (false, [], r2)
        | [] => some (false, [], r2)
      match expResult with
      | none => none
      | some (expNeg, expDs, r3) =>
        if !r3.isEmpty then none
        else
          let mantissa := digitsToNat (intDs ++ fracDs)
          let e : Int := (if expNeg then -(Int.ofNat (digitsToNat expDs)) else Int.ofNat (digitsToNat expDs))
                           - Int.ofNat fracDs.length
          let mag : Float := if e ≥ 0 then Float.ofScientific mantissa false e.toNat
                             else Float.ofScientific mantissa true (-e).toNat
          some (if neg then -mag else mag)

private def parseNumber (s : List Char) : Option (JValue × List Char) :=
  let (numChars, rest) :=
    s.span (fun c => isDigit c || c == '.' || c == 'e' || c == 'E' || c == '+' || c == '-')
  match parseFloatComplete numChars with
  | some n => some (.number n, rest)
  | none => none

private def parseKey (s : List Char) : Option (String × List Char) :=
  match s with
  | '"' :: rest =>
    match parseString rest with
    | some (.string k, rest') => some (k, rest')
    | _ => none
  | _ => none

mutual
  private partial def parseValue (s : List Char) : Option (JValue × List Char) :=
    let s := dropWs s
    match s with
    | 'n' :: 'u' :: 'l' :: 'l' :: rest => some (.null, rest)
    | 't' :: 'r' :: 'u' :: 'e' :: rest => some (.bool true, rest)
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest => some (.bool false, rest)
    | '"' :: rest => parseString rest
    | '[' :: rest => parseArray rest
    | '{' :: rest => parseObject rest
    | c :: _ => if c == '-' || isDigit c then parseNumber s else none
    | [] => none

  private partial def parseArray (s : List Char) : Option (JValue × List Char) :=
    match dropWs s with
    | ']' :: rest => some (.array [], rest)
    | _ => parseArrayGo [] s

  private partial def parseArrayGo (acc : List JValue) (s : List Char) : Option (JValue × List Char) :=
    match parseValue s with
    | some (v, rest) =>
      match dropWs rest with
      | ',' :: rest' => parseArrayGo (v :: acc) rest'
      | ']' :: rest' => some (.array (v :: acc).reverse, rest')
      | _ => none
    | none => none

  private partial def parseObject (s : List Char) : Option (JValue × List Char) :=
    match dropWs s with
    | '}' :: rest => some (.object [], rest)
    | _ => parseObjectGo [] s

  private partial def parseObjectGo (acc : List (String × JValue)) (s : List Char) :
      Option (JValue × List Char) :=
    match parseKey (dropWs s) with
    | some (key, rest) =>
      match dropWs rest with
      | ':' :: rest' =>
        match parseValue rest' with
        | some (v, rest'') =>
          match dropWs rest'' with
          | ',' :: rest''' => parseObjectGo ((key, v) :: acc) rest'''
          | '}' :: rest''' => some (.object ((key, v) :: acc).reverse, rest''')
          | _ => none
        | none => none
      | _ => none
    | none => none
end

/-- Decode a JSON `String` to a `JValue`. Returns `none` on any malformed
    input (matching Haskell's `Maybe JValue`: full parse required, trailing
    content must be whitespace only). -/
def decodeJson (s : String) : Option JValue :=
  match parseValue s.toList with
  | some (v, rest) => if rest.all isWs then some v else none
  | none => none

-- Regression checks for the boundary-review fixes above: a `.`/`e`/`E`
-- marker with no digits after it must reject the whole number (matching
-- Haskell's `reads` full-consume requirement), and a UTF-16 surrogate
-- pair must combine into its real astral codepoint rather than each half
-- separately collapsing to NUL.
#guard decodeJson "5." == none
#guard decodeJson "1e" == none
#guard decodeJson "1e+" == none
#guard decodeJson "1.e5" == none
#guard decodeJson "5.0" == some (.number 5.0)
#guard decodeJson "1e5" == some (.number 100000.0)
#guard decodeJson "\"\\ud83d\\ude00\"" == some (.string (String.singleton (Char.ofNat 0x1F600)))
#guard decodeJson "\"\\ud800\"" == some (.string (String.singleton (Char.ofNat 0xFFFD)))

end Malgo.LSP.Json
