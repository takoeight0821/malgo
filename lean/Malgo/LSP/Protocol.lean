import Malgo.LSP.Json

/-! Port of `malgo-lsp/src/Malgo/LSP/Protocol.hs`: minimal LSP protocol
types, only the subset used by malgo-lsp. -/

namespace Malgo.LSP

open Malgo.LSP.Json

/-- An LSP document URI (e.g., `file:///path/to/file.mlg`). -/
structure Uri where
  unUri : String
  deriving BEq, Ord, Repr

/-- A normalized URI, suitable for use as a map key. -/
structure NormalizedUri where
  unNormalizedUri : String
  deriving BEq, Ord, Repr

/-- Normalize a URI. The Haskell comment says "lowercase the scheme and host",
but the code lowercases the whole URI text; we port the literal behavior. -/
def toNormalizedUri (u : Uri) : NormalizedUri :=
  NormalizedUri.mk u.unUri.toLower

/-- Convert back to a `Uri`. -/
def fromNormalizedUri (u : NormalizedUri) : Uri :=
  Uri.mk u.unNormalizedUri

private def hexVal (c : Char) : Nat :=
  if c.isDigit then c.toNat - '0'.toNat
  else if 'a' ≤ c ∧ c ≤ 'f' then c.toNat - 'a'.toNat + 10
  else if 'A' ≤ c ∧ c ≤ 'F' then c.toNat - 'A'.toNat + 10
  else 0

/-- Decode percent-encoded characters (`%XX`) in a URI path. -/
def percentDecode : List Char → List Char
  | [] => []
  | '%' :: a :: b :: rest =>
    if a.isHexDigit ∧ b.isHexDigit then
      Char.ofNat (hexVal a * 16 + hexVal b) :: percentDecode rest
    else '%' :: percentDecode (a :: b :: rest)
  | c :: rest => c :: percentDecode rest

/-- Extract a file path from a `file://` URI. Returns `none` for non-file URIs. -/
def uriToFilePath (u : Uri) : Option String :=
  if "file://".isPrefixOf u.unUri then
    some (String.ofList (percentDecode (u.unUri.toList.drop 7)))
  else
    none

/-- An LSP position (0-indexed line and character). -/
structure LspPosition where
  line : Int
  character : Int
  deriving BEq, Repr

/-- An LSP range (start and end positions). -/
structure LspRange where
  start : LspPosition
  «end» : LspPosition
  deriving BEq, Repr

/-- An LSP diagnostic message. -/
structure Diagnostic where
  range : LspRange
  message : String
  source : Option String
  deriving BEq, Repr

/-- Create an error-severity diagnostic (source is always `"malgo"`). -/
def mkDiagnostic (r : LspRange) (msg : String) : Diagnostic :=
  { range := r, message := msg, source := some "malgo" }

def encodePosition (p : LspPosition) : JValue :=
  jObject
    [ ("line", .number (Float.ofInt p.line)),
      ("character", .number (Float.ofInt p.character)) ]

def encodeRange (r : LspRange) : JValue :=
  jObject
    [ ("start", encodePosition r.start),
      ("end", encodePosition r.«end») ]

/-- Encode a `Diagnostic` to JSON. Severity is unconditionally 1 (Error). -/
def encodeDiagnostic (d : Diagnostic) : JValue :=
  jObject
    [ ("range", encodeRange d.range),
      ("severity", .number 1),
      ("source", d.source.elim .null .string),
      ("message", .string d.message) ]

/-- Encode a hover response (markdown content with optional range) to JSON.
The `range` key is omitted entirely when `mRange` is `none`. -/
def encodeHover (content : String) (mRange : Option LspRange) : JValue :=
  jObject <|
    [ ("contents",
        jObject
          [ ("kind", .string "markdown"),
            ("value", .string content) ]) ]
      ++ (mRange.elim [] (fun r => [("range", encodeRange r)]))

end Malgo.LSP
