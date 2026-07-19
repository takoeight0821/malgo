import Malgo.Data.ShowFloat

/-! Port of `src/Malgo/SExpr.hs` plus the parts of s-cargot 0.1.6.0
(`Data.SCargot.Print`) that Malgo uses: `encodeOne (basicPrint atomToText)`.
Golden files compare against this output byte-for-byte, so the printer
mirrors s-cargot exactly: swing indentation, `indentAmount = 2`,
`maxWidth = 80`, split decision `flatWidth + ambientIndent > 80`. -/

namespace Malgo

/-- Haskell `Data.Char.showLitChar` names for control characters 0–31. -/
private def asciiTab : Array String :=
  #["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
    "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI",
    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
    "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US"]

/-- Port of Haskell `showLitChar` applied with an empty continuation
(`showLitChar c ""`), which is how `SExpr.hs` calls it — the `\&`
protection can never fire, so it is omitted. -/
def showLitChar (c : Char) : String :=
  let n := c.toNat
  if n > 127 then "\\" ++ toString n
  else if n == 127 then "\\DEL"
  else if c == '\\' then "\\\\"
  else if n >= 32 then String.singleton c
  else if c == '\x07' then "\\a"
  else if c == '\x08' then "\\b"
  else if c == '\x0c' then "\\f"
  else if c == '\n' then "\\n"
  else if c == '\r' then "\\r"
  else if c == '\t' then "\\t"
  else if c == '\x0b' then "\\v"
  else "\\" ++ asciiTab[n]!

inductive Atom where
  | symbol (s : String)
  | int (n : Int) (suffix : Option String)
  | float (n : Float32)
  | double (n : Float)
  | char (c : Char)
  | str (s : String)
  deriving BEq, Repr

namespace Atom

end Atom

-- `Malgo.haskellShowFloat` (Haskell `show @Double` parity) lives in
-- `Malgo.Data.ShowFloat`; both the golden dumps and the interpreter's
-- `to_string`/print primitives use it.

namespace Atom

/-- Port of `atomToText`. -/
def render : Atom → String
  | .symbol t => t
  | .int n none => toString n
  | .int n (some t) => toString n ++ "_" ++ t
  | .float n => haskellShowFloat32 n ++ "_f32"
  | .double n => haskellShowFloat n ++ "_f64"
  | .char c => "'" ++ showLitChar c ++ "'"
  | .str t => "\"" ++ String.join (t.toList.map showLitChar) ++ "\""

instance : Coe String Atom := ⟨.symbol⟩

end Atom

/-- Proper-list s-expressions. Haskell's `SExpr` also has improper
(dotted) lists, but Malgo only ever builds `S.L`/`S.A`. -/
inductive SExpr where
  | atom (a : Atom)
  | list (xs : List SExpr)
  deriving BEq, Repr, Inhabited

namespace SExpr

/-- s-cargot `Intermediate`, restricted to proper lists. `contentWidth` is
the flat width of head + items + separating spaces, excluding the two
parentheses (s-cargot's `sizeSum`). -/
private inductive Inter where
  | atom (text : String)
  | list (contentWidth : Nat) (head : Inter) (items : Array Inter)
  | empty

private instance : Inhabited Inter := ⟨.empty⟩

/-- s-cargot `sizeOf`: flat width including parentheses. Atom width is the
codepoint count (`T.length`), which `String.length` matches. -/
private def Inter.width : Inter → Nat
  | .atom t => t.length
  | .empty => 2
  | .list w _ _ => w + 2

private partial def toInter : SExpr → Inter
  | .atom a => .atom a.render
  | .list [] => .empty
  | .list (x :: xs) =>
    let hd := toInter x
    let items := xs.map toInter
    let w := items.foldl (fun acc i => acc + 1 + i.width) hd.width
    .list w hd items.toArray

/-- s-cargot `indentPrintSExpr'` specialized to `basicPrint`:
`swingIndent = Swing`, `indentAmount = 2`, `maxWidth = 80`. -/
private partial def pp (maxWidth ind : Nat) : Inter → String
  | .empty => "()"
  | .atom t => t
  | .list contentWidth hd items =>
    let hdStr := pp maxWidth (ind + 1) hd
    let body :=
      if items.isEmpty then ""
      else if contentWidth + ind > maxWidth then
        -- Swing: each item on its own line at ind + indentAmount + 1('(')
        let nextInd := ind + 2 + 1
        let pad := String.ofList (List.replicate nextInd ' ')
        String.join (items.toList.map fun i => "\n" ++ pad ++ pp maxWidth nextInd i)
      else
        " " ++ " ".intercalate (items.toList.map (pp maxWidth (ind + 1)))
    "(" ++ hdStr ++ body ++ ")"

/-- s-cargot `encodeOne (basicPrint atomToText)`. -/
def encodeOne (e : SExpr) : String :=
  pp 80 0 (toInter e)

/-- s-cargot `encode`: elements separated by blank lines. -/
def encode (es : List SExpr) : String :=
  "\n\n".intercalate (es.map encodeOne)

end SExpr

class ToSExpr (α : Type u) where
  toSExpr : α → SExpr

export ToSExpr (toSExpr)

/-- Haskell `sShow` (the `encodeOne` default; the `[a]` instance's
`encode` override is `sShowList`). -/
def sShow [ToSExpr α] (a : α) : String :=
  (toSExpr a).encodeOne

def sShowList [ToSExpr α] (as : List α) : String :=
  SExpr.encode (as.map toSExpr)

instance : ToSExpr Empty := ⟨fun e => nomatch e⟩
instance : ToSExpr SExpr := ⟨id⟩
instance : ToSExpr String := ⟨fun s => .atom (.symbol s)⟩
instance : ToSExpr Int := ⟨fun n => .atom (.int n none)⟩
instance : ToSExpr Nat := ⟨fun n => .atom (.int n none)⟩

instance [ToSExpr α] [ToSExpr β] : ToSExpr (α × β) :=
  ⟨fun (a, b) => .list [toSExpr a, toSExpr b]⟩

instance [ToSExpr α] [ToSExpr β] [ToSExpr γ] : ToSExpr (α × β × γ) :=
  ⟨fun (a, b, c) => .list [toSExpr a, toSExpr b, toSExpr c]⟩

instance [ToSExpr α] : ToSExpr (List α) :=
  ⟨fun xs => .list (xs.map toSExpr)⟩

-- Flat-fit and escaping sanity checks.
#guard SExpr.encodeOne (.list [.atom (.symbol "a"), .atom (.symbol "b")]) == "(a b)"
#guard SExpr.encodeOne (.list []) == "()"
#guard Atom.render (.int (-3) (some "Int64#")) == "-3_Int64#"
#guard Atom.render (.str "a\nb\"") == "\"a\\nb\"\""
#guard Atom.render (.char '\t') == "'\\t'"
#guard showLitChar '\x00' == "\\NUL"
#guard showLitChar 'あ' == "\\12354"

end Malgo
