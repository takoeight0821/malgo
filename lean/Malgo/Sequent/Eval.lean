import Std.Data.HashMap
import Std.Data.TreeMap
import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.Monad
import Malgo.Pass
import Malgo.Data.IntMap
import Malgo.SExpr
import Malgo.Sequent.Fun
import Malgo.Sequent.Core.Join

/-! Port of `src/Malgo/Sequent/Eval.hs`: the small-step CPS interpreter for
the Join IR.

The one thing that cannot be ported line-by-line is Haskell's
`Value.Consumer :: (forall es. … Value -> Eff es ()) -> Value` — a negative
occurrence, illegal in a Lean inductive. It is defunctionalized into
`ConsumerK`: the only consumer closures Eval.hs ever builds are
(1) `fromConsumer env k` (a syntactic `Consumer` closed over an `Env`),
(2) the `Mu` producer's `writeIORef ref`, and (3) `matchExpand`'s
`writeIORef ref . Just`. These become `ConsumerK.run`/`.writeSlot`/
`.writeSlotOpt`. The IORefs become integer slots in a store held OUTSIDE the
`Value` inductive (`EvalCtx.slots`), so nothing negative or reference-typed
sits inside `Value`.

Effect mapping: `Reader Env` → an explicit `Env` parameter; `Reader
Toplevels`/`Reader Handlers`/the IORef store live in `EvalCtx`; `Error
EvalError` + `IOE` → `EvalM := ReaderT EvalCtx (ExceptT EvalError IO)`. The
only uniq minted is `finish` in `evalProgram` (in `MalgoM`, before dropping
into `EvalM`), so no uniq-ordering parity concern exists here. -/

namespace Malgo.Sequent.Eval

open Malgo
open Malgo.Sequent.Fun (Name Literal Tag Pattern)

abbrev JProducer := Malgo.Sequent.Core.Join.Producer
abbrev JConsumer := Malgo.Sequent.Core.Join.Consumer
abbrev JStatement := Malgo.Sequent.Core.Join.Statement
abbrev JBranch := Malgo.Sequent.Core.Join.Branch
abbrev JProgram := Malgo.Sequent.Core.Join.Program

def jstmtRange (s : JStatement) : Range := Malgo.Sequent.Core.Join.Statement.range s
def jprodRange (p : JProducer) : Range := Malgo.Sequent.Core.Join.Producer.range p

/-! ## Values (defunctionalized consumers, IORef-free) -/

mutual

inductive Value where
  | immediate (lit : Literal)
  | struct (tag : Tag) (values : List Value)
  | function (env : Env) (params : List Name) (statement : JStatement)
  | record (env : Env) (fields : List (String × Name × JStatement))
  | consumer (k : ConsumerK)

/-- Defunctionalized consumer closure. `run` is `fromConsumer env k`;
`writeSlot`/`writeSlotOpt` write to an integer store slot (the `Mu` ref and
`matchExpand`'s `Maybe` ref respectively). Both writes are the same store
operation; the `Opt` variant differs only in how its slot is seeded
(absent, meaning `Nothing`) and read at the allocation site.

`bigStep` is the fourth shape, built only by `Malgo.Sequent.BigStepEval`
(the `mkConsumerValue` closure over a result slot + captured env + consumer
AST). BigStepEval interprets it in its own mutual block; the arm in
`applyConsumerK` below exists because `BigStepEval` reuses the small-step
`matchPat`/`matchExpand`, whose `Expand` path drives the small-step
`evalStatement` over a record env that (in a big-step run) may hold
`bigStep` consumers. -/
inductive ConsumerK where
  | run (env : Env) (k : JConsumer)
  | writeSlot (slot : Nat)
  | writeSlotOpt (slot : Nat)
  | bigStep (slot : Nat) (env : Env) (k : JConsumer)

/-- Haskell `Env {localBindings :: IntMap Value, externalBindings :: Map
Name Value}`. `localBindings` keys are Internal/Temporal uniqs (the fast
path); externals are an ascending assoc list (`Std.TreeMap` cannot nest in
an inductive). -/
inductive Env where
  | mk (localBindings : Malgo.IntMap Value) (externalBindings : List (Id × Value))

end

instance : Inhabited Value := ⟨.struct .tuple []⟩
instance : Inhabited Env := ⟨.mk .nil []⟩

/-! ## Value equality, zero test, text rendering -/

/-- Structural equality on `Immediate`/`Struct` only, `False` otherwise —
mirrors Haskell's hand-written `Eq Value`. -/
partial def valueEq : Value → Value → Bool
  | .immediate a, .immediate b => a == b
  | .struct a as, .struct b bs =>
    a == b && as.length == bs.length && (as.zip bs).all (fun (x, y) => valueEq x y)
  | _, _ => false

instance : BEq Value := ⟨valueEq⟩

def isZeroValue : Value → Bool
  | .immediate (.int32 n) => n == 0
  | .immediate (.int64 n) => n == 0
  | .immediate (.float n) => n == 0
  | .immediate (.double n) => n == 0
  | _ => false

/-- Convert any Value to a text representation for printing (Haskell
`valueToText`). Floats render via `Malgo.haskellShowFloat`/
`Malgo.haskellShowFloat32` (Haskell-`show` parity within the goldens'
decimal range; see their docstrings for limits). -/
partial def valueToText : Value → String
  | .immediate (.int32 n) => toString n.toInt
  | .immediate (.int64 n) => toString n.toInt
  | .immediate (.float f) => Malgo.haskellShowFloat32 f
  | .immediate (.double d) => Malgo.haskellShowFloat d
  | .immediate (.char c) => String.singleton c
  | .immediate (.string s) => s
  | .struct .tuple [] => "{}"
  | .struct .tuple values => "{" ++ String.intercalate ", " (values.map valueToText) ++ "}"
  | .struct (.tag name) [] => name
  | .struct (.tag name) values => name ++ "(" ++ String.intercalate ", " (values.map valueToText) ++ ")"
  | .function .. => "<function>"
  | .record .. => "<record>"
  | .consumer .. => "<consumer>"

/-! ## Errors -/

inductive EvalError where
  | undefinedVariable (range : Range) (name : Name)
  | expectConsumer (range : Range) (value : Value)
  | expectFunction (range : Range) (value : Value)
  | expectRecord (range : Range) (value : Value)
  | expectNumber (range : Range) (value : Value)
  | noSuchField (range : Range) (field : String) (value : Value)
  | noMatch (range : Range) (value : Value)
  | primitiveNotImplemented (range : Range) (name : String) (values : List Value)
  | invalidArguments (range : Range) (name : String) (values : List Value)
  /-- Haskell `div`/`mod` throw a divide-by-zero `ArithException`;
  `Int.fdiv/fmod` would silently yield 0/`a`. -/
  | divideByZero (range : Range)
  /-- Control-flow, not an error: `malgo_exit_success`. Haskell throws the
  `ExitSuccess` exception; `IO.Process.exit` would kill the whole host
  process (including the golden test runner). `evalProgram` catches this
  and treats it as successful termination. -/
  | exitSuccess

instance : Inhabited Range := ⟨{ start := SourcePos.initial "", stop := SourcePos.initial "" }⟩
instance : Inhabited Id := ⟨{ name := "", moduleName := .moduleName "", sort := .external }⟩
instance : Inhabited EvalError := ⟨.noMatch default default⟩

/-- Error messages. Values are rendered via `valueToText`; Haskell embeds
`show value` (its derived `Show`) instead, so error strings are not
byte-parity — no golden exercises an eval error, so this is intentional. -/
def EvalError.render : EvalError → String
  | .undefinedVariable range name => s!"{pretty range}: Undefined variable: {pretty name}"
  | .expectConsumer range value => s!"{pretty range}: Expecting a consumer, but got: {valueToText value}"
  | .expectFunction range value => s!"{pretty range}: Expecting a function, but got: {valueToText value}"
  | .expectRecord range value => s!"{pretty range}: Expecting a record, but got: {valueToText value}"
  | .expectNumber range value => s!"{pretty range}: Expecting a number, but got: {valueToText value}"
  | .noSuchField range field value => s!"{pretty range}: No such field: {field} in {valueToText value}"
  | .noMatch range value => s!"{pretty range}: No match for {valueToText value}"
  | .primitiveNotImplemented range name values =>
    s!"{pretty range}: Primitive {name} is not implemented for " ++
      String.intercalate ", " (values.map valueToText)
  | .invalidArguments range name values =>
    s!"{pretty range}: Invalid arguments for {name}: " ++
      String.intercalate ", " (values.map valueToText)
  | .divideByZero range => s!"{pretty range}: divide by zero"
  | .exitSuccess => "ExitSuccess"

def EvalError.range? : EvalError → Option Range
  | .undefinedVariable r _ => some r
  | .expectConsumer r _ => some r
  | .expectFunction r _ => some r
  | .expectRecord r _ => some r
  | .expectNumber r _ => some r
  | .noSuchField r _ _ => some r
  | .noMatch r _ => some r
  | .primitiveNotImplemented r _ _ => some r
  | .invalidArguments r _ _ => some r
  | .divideByZero r => some r
  | .exitSuccess => none

/-! ## Handlers and the evaluation context -/

/-- IO surface the primitives use. Two smart constructors below: `real`
(process handles) and `buffered` (in-memory stdin + stdout accumulator ref)
— the golden tests need in-process stdout capture. -/
structure Handlers where
  stdin : IO (Option Char)
  stdout : Char → IO Unit
  stderr : Char → IO Unit
  arguments : List String

/-- Read one UTF-8 codepoint from stdin; I/O errors read as EOF, like
Haskell's `getChar \`catch\` \(_ :: IOException) -> Nothing`. A malformed
lead byte falls back to its Latin-1 interpretation rather than aborting. -/
private def readCharUtf8 : IO (Option Char) := do
  try
    let stdin ← IO.getStdin
    let head ← stdin.read 1
    if head.size == 0 then return none
    let b0 := head[0]!
    let extra :=
      if b0.toNat < 0x80 then 0
      else if b0.toNat < 0xC0 then 0  -- stray continuation byte
      else if b0.toNat < 0xE0 then 1
      else if b0.toNat < 0xF0 then 2
      else 3
    if extra == 0 then
      return some (Char.ofNat b0.toNat)
    let rest ← stdin.read (USize.ofNat extra)
    match String.fromUTF8? (head ++ rest) with
    | some s => return s.toList.head?
    | none => return some (Char.ofNat b0.toNat)
  catch _ =>
    return none

/-- Process-handle handlers. -/
def Handlers.real (arguments : List String) : Handlers where
  stdin := readCharUtf8
  stdout := fun c => do (← IO.getStdout).putStr (String.singleton c)
  stderr := fun c => do (← IO.getStderr).putStr (String.singleton c)
  arguments := arguments

/-- In-memory handlers for golden capture. Returns the handlers plus the
stdout accumulator ref (read after the run). `stderr` goes to a separate,
discarded ref, exactly mirroring the Haskell test harness
(`runEvalWithStdin`), and `arguments` defaults to `[]`. -/
def Handlers.buffered (input : String) : IO (Handlers × IO.Ref String) := do
  let inRef ← IO.mkRef input.toList
  let outRef ← IO.mkRef ""
  let errRef ← IO.mkRef ""
  let handlers : Handlers :=
    { stdin := do
        match ← inRef.get with
        | [] => return none
        | c :: cs => inRef.set cs; return (some c)
      stdout := fun c => outRef.modify (·.push c)
      stderr := fun c => errRef.modify (·.push c)
      arguments := [] }
  return (handlers, outRef)

/-- Toplevel definitions, keyed by name (Haskell `Toplevels`). -/
abbrev Toplevels := Std.TreeMap Name (Name × JStatement)

structure EvalCtx where
  slots : IO.Ref (Std.HashMap Nat Value)
  nextSlot : IO.Ref Nat
  handlers : Handlers
  toplevels : Toplevels

abbrev EvalM := ReaderT EvalCtx (ExceptT EvalError IO)

/-! ## Environment -/

def nameToIntKey (n : Name) : Option Nat :=
  match n.sort with
  | .internal u => some u
  | .temporal u => some u
  | .external => none

def Env.localBindings : Env → Malgo.IntMap Value
  | .mk l _ => l

def Env.externalBindings : Env → List (Id × Value)
  | .mk _ e => e

def emptyEnv : Env := .mk .nil []

/-- Insert into an ascending assoc list keyed by `Id`, overwriting an
existing key (mirrors `Map.insert`). -/
def insertExternal (k : Id) (v : Value) : List (Id × Value) → List (Id × Value)
  | [] => [(k, v)]
  | (k', v') :: rest =>
    match compare k k' with
    | .lt => (k, v) :: (k', v') :: rest
    | .eq => (k, v) :: rest
    | .gt => (k', v') :: insertExternal k v rest

def extendEnv (name : Name) (value : Value) (env : Env) : Env :=
  match nameToIntKey name with
  | some key => .mk (env.localBindings.insert key value) env.externalBindings
  | none => .mk env.localBindings (insertExternal name value env.externalBindings)

/-- Batch extend. `foldr` so the first pair wins on a duplicate key,
matching Haskell `extendEnv'`. -/
def extendEnv' (pairs : List (Name × Value)) (env : Env) : Env :=
  pairs.foldr (fun (n, v) e => extendEnv n v e) env

def lookupEnv (env : Env) (range : Range) (name : Name) : EvalM Value :=
  match nameToIntKey name with
  | some key =>
    match env.localBindings.lookup? key with
    | some v => pure v
    | none => throw (.undefinedVariable range name)
  | none =>
    match env.externalBindings.find? (fun (k, _) => k == name) with
    | some (_, v) => pure v
    | none => throw (.undefinedVariable range name)

def lookupToplevel (range : Range) (name : Name) : EvalM (Name × JStatement) := do
  match (← read).toplevels.get? name with
  | some v => pure v
  | none => throw (.undefinedVariable range name)

/-! ## Store (defunctionalized IORefs) -/

def freshSlot : EvalM Nat := do
  (← read).nextSlot.modifyGet (fun n => (n, n + 1))

def storeSet (slot : Nat) (v : Value) : EvalM Unit := do
  (← read).slots.modify (·.insert slot v)

def storeGet? (slot : Nat) : EvalM (Option Value) := do
  return (← (← read).slots.get).get? slot

/-! ## Record/codata field lookup and record-expand intersection -/

def lookupField (k : String) : List (String × Name × JStatement) → Option (Name × JStatement)
  | [] => none
  | (k', v) :: rest => if k == k' then some v else lookupField k rest

/-- Ascending intersection of an `Expand` pattern's fields with a record's
fields (mirrors `Map.intersectionWith (,)` + `Map.toList`). -/
def intersectFields :
    List (String × Pattern) → List (String × Name × JStatement) →
    List (String × Pattern × Name × JStatement)
  | [], _ => []
  | (k, pat) :: ps, fields =>
    match lookupField k fields with
    | some (name, stmt) => (k, pat, name, stmt) :: intersectFields ps fields
    | none => intersectFields ps fields

/-! ## IO helpers -/

def putTextTo (stream : Char → IO Unit) (text : String) : IO Unit :=
  text.toList.forM stream

partial def readAllContents (h : Handlers) (acc : List Char) : IO (List Char) := do
  match ← h.stdin with
  | some c => readAllContents h (c :: acc)
  | none => return acc.reverse

partial def readLineChars (h : Handlers) (acc : List Char) : IO (List Char) := do
  match ← h.stdin with
  | some '\n' => return acc.reverse
  | some c => readLineChars h (c :: acc)
  | none => return acc.reverse

/-! ## Arithmetic and comparison primitives -/

def addValue (range : Range) : Value → Value → EvalM Value
  | .immediate (.int32 a), .immediate (.int32 b) => pure (.immediate (.int32 (a + b)))
  | .immediate (.int64 a), .immediate (.int64 b) => pure (.immediate (.int64 (a + b)))
  | .immediate (.float a), .immediate (.float b) => pure (.immediate (.float (a + b)))
  | .immediate (.double a), .immediate (.double b) => pure (.immediate (.double (a + b)))
  | a, b => throw (.invalidArguments range "malgo_add" [a, b])

def subValue (range : Range) : Value → Value → EvalM Value
  | .immediate (.int32 a), .immediate (.int32 b) => pure (.immediate (.int32 (a - b)))
  | .immediate (.int64 a), .immediate (.int64 b) => pure (.immediate (.int64 (a - b)))
  | .immediate (.float a), .immediate (.float b) => pure (.immediate (.float (a - b)))
  | .immediate (.double a), .immediate (.double b) => pure (.immediate (.double (a - b)))
  | a, b => throw (.invalidArguments range "malgo_sub" [a, b])

def mulValue (range : Range) : Value → Value → EvalM Value
  | .immediate (.int32 a), .immediate (.int32 b) => pure (.immediate (.int32 (a * b)))
  | .immediate (.int64 a), .immediate (.int64 b) => pure (.immediate (.int64 (a * b)))
  | .immediate (.float a), .immediate (.float b) => pure (.immediate (.float (a * b)))
  | .immediate (.double a), .immediate (.double b) => pure (.immediate (.double (a * b)))
  | a, b => throw (.invalidArguments range "malgo_mul" [a, b])

/-- Integer division/modulo use floor semantics (`Int.fdiv`/`Int.fmod`) to
match Haskell `div`/`mod`; Lean's native `Int./` truncates toward zero. -/
def divValue (range : Range) : Value → Value → EvalM Value
  | .immediate (.int32 a), .immediate (.int32 b) =>
    if b == 0 then throw (.divideByZero range)
    else pure (.immediate (.int32 (Int32.ofInt (Int.fdiv a.toInt b.toInt))))
  | .immediate (.int64 a), .immediate (.int64 b) =>
    if b == 0 then throw (.divideByZero range)
    else pure (.immediate (.int64 (Int64.ofInt (Int.fdiv a.toInt b.toInt))))
  | .immediate (.float a), .immediate (.float b) => pure (.immediate (.float (a / b)))
  | .immediate (.double a), .immediate (.double b) => pure (.immediate (.double (a / b)))
  | a, b => throw (.invalidArguments range "malgo_div" [a, b])

def modValue (range : Range) : Value → Value → EvalM Value
  | .immediate (.int32 a), .immediate (.int32 b) =>
    if b == 0 then throw (.divideByZero range)
    else pure (.immediate (.int32 (Int32.ofInt (Int.fmod a.toInt b.toInt))))
  | .immediate (.int64 a), .immediate (.int64 b) =>
    if b == 0 then throw (.divideByZero range)
    else pure (.immediate (.int64 (Int64.ofInt (Int.fmod a.toInt b.toInt))))
  | a, b => throw (.invalidArguments range "malgo_mod" [a, b])

def negValue (range : Range) : Value → EvalM Value
  | .immediate (.int32 a) => pure (.immediate (.int32 (-a)))
  | .immediate (.int64 a) => pure (.immediate (.int64 (-a)))
  | .immediate (.float a) => pure (.immediate (.float (-a)))
  | .immediate (.double a) => pure (.immediate (.double (-a)))
  | a => throw (.invalidArguments range "malgo_neg" [a])

def eqValue (_range : Range) (a b : Value) : EvalM Value :=
  pure (.immediate (.int32 (if a == b then 1 else 0)))

def neValue (_range : Range) (a b : Value) : EvalM Value :=
  pure (.immediate (.int32 (if a != b then 1 else 0)))

private def boolI32 (b : Bool) : Value := .immediate (.int32 (if b then 1 else 0))

def ltValue (range : Range) : Value → Value → EvalM Value
  | .immediate (.int32 a), .immediate (.int32 b) => pure (boolI32 (a.toInt < b.toInt))
  | .immediate (.int64 a), .immediate (.int64 b) => pure (boolI32 (a.toInt < b.toInt))
  | .immediate (.float a), .immediate (.float b) => pure (boolI32 (a < b))
  | .immediate (.double a), .immediate (.double b) => pure (boolI32 (a < b))
  | a, b => throw (.invalidArguments range "malgo_lt" [a, b])

def leValue (range : Range) : Value → Value → EvalM Value
  | .immediate (.int32 a), .immediate (.int32 b) => pure (boolI32 (a.toInt ≤ b.toInt))
  | .immediate (.int64 a), .immediate (.int64 b) => pure (boolI32 (a.toInt ≤ b.toInt))
  | .immediate (.float a), .immediate (.float b) => pure (boolI32 (a ≤ b))
  | .immediate (.double a), .immediate (.double b) => pure (boolI32 (a ≤ b))
  | a, b => throw (.invalidArguments range "malgo_le" [a, b])

def gtValue (range : Range) : Value → Value → EvalM Value
  | .immediate (.int32 a), .immediate (.int32 b) => pure (boolI32 (a.toInt > b.toInt))
  | .immediate (.int64 a), .immediate (.int64 b) => pure (boolI32 (a.toInt > b.toInt))
  | .immediate (.float a), .immediate (.float b) => pure (boolI32 (a > b))
  | .immediate (.double a), .immediate (.double b) => pure (boolI32 (a > b))
  | a, b => throw (.invalidArguments range "malgo_gt" [a, b])

def geValue (range : Range) : Value → Value → EvalM Value
  | .immediate (.int32 a), .immediate (.int32 b) => pure (boolI32 (a.toInt ≥ b.toInt))
  | .immediate (.int64 a), .immediate (.int64 b) => pure (boolI32 (a.toInt ≥ b.toInt))
  | .immediate (.float a), .immediate (.float b) => pure (boolI32 (a ≥ b))
  | .immediate (.double a), .immediate (.double b) => pure (boolI32 (a ≥ b))
  | a, b => throw (.invalidArguments range "malgo_ge" [a, b])

def binaryPrim (range : Range) (name : String) (f : Range → Value → Value → EvalM Value) :
    List Value → EvalM Value
  | [a, b] => f range a b
  | values => throw (.invalidArguments range name values)

def unaryPrim (range : Range) (name : String) (f : Range → Value → EvalM Value) :
    List Value → EvalM Value
  | [a] => f range a
  | values => throw (.invalidArguments range name values)

def toStringPrim (range : Range) (name : String) : List Value → EvalM Value
  | [.immediate (.int32 n)] => pure (.immediate (.string (toString n.toInt)))
  | [.immediate (.int64 n)] => pure (.immediate (.string (toString n.toInt)))
  | [.immediate (.float n)] => pure (.immediate (.string (Malgo.haskellShowFloat32 n)))
  | [.immediate (.double n)] => pure (.immediate (.string (Malgo.haskellShowFloat n)))
  | [.immediate (.string s)] => pure (.immediate (.string s))
  | values => throw (.invalidArguments range name values)

/-- Haskell `reads @Int` with the `[(n, "")]` full-consumption pattern:
leading whitespace, arbitrarily nested parentheses, `-` (whitespace may
follow the sign — `lex` skips it), and `0x`/`0X` hex / `0o`/`0O` octal
prefixes. Trailing input (even whitespace) rejects, as in Haskell. -/
private partial def readsIntAux : List Char → Option (Int × List Char)
  | cs =>
    let cs := cs.dropWhile Char.isWhitespace
    match cs with
    | '(' :: rest => do
      let (n, rest') ← readsIntAux rest
      match rest'.dropWhile Char.isWhitespace with
      | ')' :: rest'' => some (n, rest'')
      | _ => none
    | '-' :: rest => do
      let rest := rest.dropWhile Char.isWhitespace
      let (n, rest') ← readsUnsigned rest
      some (-n, rest')
    | cs => readsUnsigned cs
where
  digitsIn (base : Nat) (isDigit : Char → Bool) (toVal : Char → Nat) (cs : List Char) :
      Option (Int × List Char) :=
    let (ds, rest) := (cs.takeWhile isDigit, cs.dropWhile isDigit)
    if ds.isEmpty then none
    else some ((ds.foldl (fun acc c => acc * base + toVal c) 0 : Nat), rest)
  readsUnsigned : List Char → Option (Int × List Char)
    | '0' :: x :: rest =>
      if x == 'x' || x == 'X' then
        digitsIn 16 (fun c => c.isDigit || ('a' ≤ c.toLower && c.toLower ≤ 'f'))
          (fun c => if c.isDigit then c.toNat - '0'.toNat else c.toLower.toNat - 'a'.toNat + 10)
          rest
      else if x == 'o' || x == 'O' then
        digitsIn 8 (fun c => '0' ≤ c && c ≤ '7') (fun c => c.toNat - '0'.toNat) rest
      else
        digitsIn 10 Char.isDigit (fun c => c.toNat - '0'.toNat) ('0' :: x :: rest)
    | cs => digitsIn 10 Char.isDigit (fun c => c.toNat - '0'.toNat) cs

def readsInt (s : String) : Option Int :=
  match readsIntAux s.toList with
  | some (n, []) => some n
  | _ => none

/-- Haskell `toEnum @Char`: errors outside `0..0x10FFFF`. Deviation: Haskell
`Char` admits surrogates (D800–DFFF) but Lean `Char` cannot represent them,
so they error here too — unreachable from well-formed programs. -/
private def intToChar (range : Range) (prim : String) (values : List Value) (n : Int) :
    EvalM Char :=
  if n < 0 || n > 0x10FFFF || !(n.toNat.isValidChar) then
    throw (.invalidArguments range prim values)
  else
    pure (Char.ofNat n.toNat)

/-- Char at a codepoint index (`malgo_string_at`). -/
def stringAtImpl (range : Range) (i : Int64) (s : String) : EvalM Value :=
  let idx := i.toInt
  if idx ≥ 0 ∧ idx.toNat < s.length then
    match s.toList[idx.toNat]? with
    | some c => pure (.immediate (.char c))
    | none => throw (.invalidArguments range "malgo_string_at" [.immediate (.int64 i), .immediate (.string s)])
  else throw (.invalidArguments range "malgo_string_at" [.immediate (.int64 i), .immediate (.string s)])

/-! ## Primitive dispatch (`fetchPrimitive`) -/

def fetchPrimitive (range : Range) (name : String) (values : List Value) : EvalM Value := do
  if name == "reuseHint" then
    match values with
    | [value] => pure value
    | _ => throw (.invalidArguments range "reuseHint" values)
  else if name == "malgo_unsafe_cast" then
    match values with
    | [value] => pure value
    | _ => throw (.invalidArguments range "malgo_unsafe_cast" values)
  else if name.startsWith "malgo_add_" then binaryPrim range name addValue values
  else if name.startsWith "malgo_sub_" then binaryPrim range name subValue values
  else if name.startsWith "malgo_mul_" then binaryPrim range name mulValue values
  else if name.startsWith "malgo_div_" then binaryPrim range name divValue values
  else if name.startsWith "malgo_mod_" then binaryPrim range name modValue values
  else if name.startsWith "malgo_neg_" then unaryPrim range name negValue values
  else if name.startsWith "malgo_eq_" then binaryPrim range name eqValue values
  else if name.startsWith "malgo_ne_" then binaryPrim range name neValue values
  else if name.startsWith "malgo_lt_" then binaryPrim range name ltValue values
  else if name.startsWith "malgo_le_" then binaryPrim range name leValue values
  else if name.startsWith "malgo_gt_" then binaryPrim range name gtValue values
  else if name.startsWith "malgo_ge_" then binaryPrim range name geValue values
  else if name.startsWith "malgo_" && name.endsWith "to_string" then toStringPrim range name values
  else
    let h := (← read).handlers
    match name with
    | "malgo_print_string" =>
      match values with
      | [.immediate (.string text)] => do putTextTo h.stdout text; pure (.struct .tuple [])
      | _ => throw (.invalidArguments range "malgo_print_string" values)
    | "malgo_newline" => do h.stdout '\n'; pure (.struct .tuple [])
    | "malgo_print_char" =>
      match values with
      | [.immediate (.char c)] => do h.stdout c; pure (.struct .tuple [])
      | _ => throw (.invalidArguments range "malgo_print_char" values)
    | "malgo_get_contents" => do
      let cs ← readAllContents h []
      pure (.immediate (.string (String.ofList cs)))
    | "malgo_string_append" =>
      match values with
      | [.immediate (.string a), .immediate (.string b)] => pure (.immediate (.string (a ++ b)))
      | _ => throw (.invalidArguments range "malgo_string_append" values)
    | "malgo_string_length" =>
      match values with
      | [.immediate (.string s)] => pure (.immediate (.int64 (Int64.ofNat s.length)))
      | _ => throw (.invalidArguments range "malgo_string_length" values)
    | "malgo_string_at" =>
      match values with
      | [.immediate (.int64 i), .immediate (.string s)] => stringAtImpl range i s
      | _ => throw (.invalidArguments range "malgo_string_at" values)
    | "malgo_string_cons" =>
      match values with
      | [.immediate (.char c), .immediate (.string s)] =>
        pure (.immediate (.string (String.ofList (c :: s.toList))))
      | _ => throw (.invalidArguments range "malgo_string_cons" values)
    | "malgo_substring" =>
      match values with
      | [.immediate (.string s), .immediate (.int64 start), .immediate (.int64 e)] =>
        let st := start.toInt.toNat
        let len := (e.toInt - start.toInt).toNat
        pure (.immediate (.string (String.ofList ((s.toList.drop st).take len))))
      | _ => throw (.invalidArguments range "malgo_substring" values)
    | "malgo_string_reverse" =>
      match values with
      | [.immediate (.string s)] => pure (.immediate (.string (String.ofList s.toList.reverse)))
      | _ => throw (.invalidArguments range "malgo_string_reverse" values)
    | "malgo_print" =>
      match values with
      | [value] => do putTextTo h.stdout (valueToText value); pure (.struct .tuple [])
      | _ => throw (.invalidArguments range "malgo_print" values)
    | "malgo_str_len" =>
      match values with
      | [.immediate (.string s)] => pure (.immediate (.int64 (Int64.ofNat s.length)))
      | _ => throw (.invalidArguments range "malgo_str_len" values)
    | "malgo_str_at" =>
      match values with
      | [.immediate (.string s), .immediate (.int64 i)] => stringAtImpl range i s
      | _ => throw (.invalidArguments range "malgo_str_at" values)
    | "malgo_str_sub" =>
      match values with
      | [.immediate (.string s), .immediate (.int64 start), .immediate (.int64 len)] =>
        pure (.immediate (.string (String.ofList ((s.toList.drop start.toInt.toNat).take len.toInt.toNat))))
      | _ => throw (.invalidArguments range "malgo_str_sub" values)
    | "malgo_str_to_int" =>
      match values with
      | [.immediate (.string s)] =>
        match readsInt s with
        | some n => pure (.immediate (.int64 (Int64.ofInt n)))
        | none => throw (.invalidArguments range "malgo_str_to_int" [.immediate (.string s)])
      | _ => throw (.invalidArguments range "malgo_str_to_int" values)
    | "malgo_int_to_str" =>
      match values with
      | [.immediate (.int64 n)] => pure (.immediate (.string (toString n.toInt)))
      | _ => throw (.invalidArguments range "malgo_int_to_str" values)
    | "malgo_rune_to_str" =>
      match values with
      | [.immediate (.char c)] => pure (.immediate (.string (String.singleton c)))
      | _ => throw (.invalidArguments range "malgo_rune_to_str" values)
    | "malgo_int_to_rune" =>
      match values with
      | [.immediate (.int64 n)] => do
        pure (.immediate (.char (← intToChar range "malgo_int_to_rune" values n.toInt)))
      | _ => throw (.invalidArguments range "malgo_int_to_rune" values)
    | "malgo_rune_to_int" =>
      match values with
      | [.immediate (.char c)] => pure (.immediate (.int64 (Int64.ofNat c.toNat)))
      | _ => throw (.invalidArguments range "malgo_rune_to_int" values)
    | "malgo_is_digit" =>
      match values with
      | [.immediate (.char c)] => pure (boolI32 c.isDigit)
      | _ => throw (.invalidArguments range "malgo_is_digit" values)
    | "malgo_is_lower" =>
      match values with
      | [.immediate (.char c)] => pure (boolI32 c.isLower)
      | _ => throw (.invalidArguments range "malgo_is_lower" values)
    | "malgo_is_upper" =>
      match values with
      | [.immediate (.char c)] => pure (boolI32 c.isUpper)
      | _ => throw (.invalidArguments range "malgo_is_upper" values)
    | "malgo_is_alphanum" =>
      match values with
      | [.immediate (.char c)] => pure (boolI32 c.isAlphanum)
      | _ => throw (.invalidArguments range "malgo_is_alphanum" values)
    | "malgo_char_ord" =>
      match values with
      | [.immediate (.char c)] => pure (.immediate (.int32 (Int32.ofNat c.toNat)))
      | _ => throw (.invalidArguments range "malgo_char_ord" values)
    | "malgo_int32_t_to_char" =>
      match values with
      | [.immediate (.int32 n)] => do
        pure (.immediate (.char (← intToChar range "malgo_int32_t_to_char" values n.toInt)))
      | _ => throw (.invalidArguments range "malgo_int32_t_to_char" values)
    | "malgo_read_file" =>
      match values with
      | [.immediate (.string path)] => do
        let contents ← IO.FS.readFile (System.FilePath.mk path)
        pure (.immediate (.string contents))
      | _ => throw (.invalidArguments range "malgo_read_file" values)
    | "malgo_write_file" =>
      match values with
      | [.immediate (.string path), .immediate (.string content)] => do
        IO.FS.writeFile (System.FilePath.mk path) content
        pure (.struct .tuple [])
      | _ => throw (.invalidArguments range "malgo_write_file" values)
    | "malgo_get_line" => do
      let cs ← readLineChars h []
      pure (.immediate (.string (String.ofList cs)))
    | "malgo_get_args" =>
      pure (.immediate (.string (String.intercalate "\n" h.arguments)))
    | "malgo_exit_success" =>
      throw .exitSuccess
    | "malgo_stderr_string" =>
      match values with
      | [.immediate (.string text)] => do putTextTo h.stderr text; pure (.struct .tuple [])
      | _ => throw (.invalidArguments range "malgo_stderr_string" values)
    | "malgo_string_to_int32" =>
      match values with
      | [.immediate (.string s)] =>
        match readsInt s with
        | some n => pure (.immediate (.int32 (Int32.ofInt n)))
        | none => throw (.invalidArguments range "malgo_string_to_int32" [.immediate (.string s)])
      | _ => throw (.invalidArguments range "malgo_string_to_int32" values)
    | "malgo_string_to_int64" =>
      match values with
      | [.immediate (.string s)] =>
        match readsInt s with
        | some n => pure (.immediate (.int64 (Int64.ofInt n)))
        | none => throw (.invalidArguments range "malgo_string_to_int64" [.immediate (.string s)])
      | _ => throw (.invalidArguments range "malgo_string_to_int64" values)
    | _ => throw (.primitiveNotImplemented range name values)

/-! ## The evaluator -/

mutual

partial def jump (env : Env) (range : Range) (name : Name) (value : Value) : EvalM Unit := do
  match ← lookupEnv env range name with
  | .consumer k => applyConsumerK k value
  | _ => throw (.expectConsumer range value)

partial def applyConsumerK : ConsumerK → Value → EvalM Unit
  | .run env k, value => evalConsumer env k value
  | .writeSlot slot, value => storeSet slot value
  | .writeSlotOpt slot, value => storeSet slot value
  -- Reached only when BigStepEval's `matchExpand` (shared `matchPat`) drives
  -- this small-step evalStatement over a big-step record env; drive the
  -- consumer for its effects. The captured slot is irrelevant on that path
  -- (nothing reads it), so the two evaluators stay observationally equal.
  | .bigStep _ env k, value => evalConsumer env k value

partial def evalStatement (env : Env) : JStatement → EvalM Unit
  | .cut (.mu _ name stmt) consumer => do
    let consumerValue ← lookupEnv env (jstmtRange stmt) consumer
    evalStatement (extendEnv name consumerValue env) stmt
  | .cut producer consumer => do
    let value ← evalProducer env producer
    jump env (jprodRange producer) consumer value
  | .join _ label consumer statement => do
    let value := Value.consumer (.run env consumer)
    evalStatement (extendEnv label value env) statement
  | .primitive range name producers consumer => do
    let vals ← producers.mapM (evalProducer env)
    let result ← fetchPrimitive range name vals
    jump env range consumer result
  | .invoke range name consumer => do
    let (ret, statement) ← lookupToplevel range name
    let covalue ← lookupEnv env range consumer
    evalStatement (extendEnv ret covalue env) statement
  | .externalCall range name producers consumer => do
    let vals ← producers.mapM (evalProducer env)
    let result ← fetchPrimitive range name vals
    jump env range consumer result
  | .binOp range op lhs rhs consumer => do
    let l ← evalProducer env lhs
    let r ← evalProducer env rhs
    let result ← fetchPrimitive range op [l, r]
    jump env range consumer result
  | .ifz _ cond thenB elseB => do
    let c ← evalProducer env cond
    if isZeroValue c then evalStatement env thenB else evalStatement env elseB

partial def evalProducer (env : Env) : JProducer → EvalM Value
  | .var range name => lookupEnv env range name
  | .literal _ literal => pure (.immediate literal)
  | .construct range tag producers consumers => do
    let ps ← producers.mapM (evalProducer env)
    let cs ← consumers.mapM (lookupEnv env range)
    pure (.struct tag (ps ++ cs))
  | .lambda _ parameters statement => pure (.function env parameters statement)
  | .object _ fields => pure (.record env fields)
  | .mu _ name stmt => do
    let slot ← freshSlot
    storeSet slot (.struct .tuple [])
    evalStatement (extendEnv name (.consumer (.writeSlot slot)) env) stmt
    return (← storeGet? slot).getD (.struct .tuple [])

partial def evalConsumer (env : Env) : JConsumer → Value → EvalM Unit
  | .label range label, given => do
    match ← lookupEnv env range label with
    | .consumer k => applyConsumerK k given
    | covalue => throw (.expectConsumer range covalue)
  | .apply range producers consumers, given => do
    let ps ← producers.mapM (evalProducer env)
    let cs ← consumers.mapM (lookupEnv env range)
    match given with
    | .function fenv parameters statement =>
      evalStatement (extendEnv' (parameters.zip (ps ++ cs)) fenv) statement
    | _ => throw (.expectFunction range given)
  | .project range field consumer, given => do
    let covalue ← lookupEnv env range consumer
    match given with
    | .record renv fields =>
      match lookupField field fields with
      | some (name, statement) => evalStatement (extendEnv name covalue renv) statement
      | none => throw (.noSuchField range field given)
    | _ => throw (.expectRecord range given)
  | .«then» _ name statement, given =>
    evalStatement (extendEnv name given env) statement
  | .finish _, _ => pure ()
  | .select range branches, given => selectGo env range given branches

partial def selectGo (env : Env) (range : Range) (given : Value) : List JBranch → EvalM Unit
  | [] => throw (.noMatch range given)
  | .branch _ pattern statement :: rest => do
    match ← matchPat env pattern given with
    | some bindings => evalStatement (extendEnv' bindings env) statement
    | none => selectGo env range given rest

partial def matchPat (env : Env) : Pattern → Value → EvalM (Option (List (Name × Value)))
  | .pvar _ name, value => pure (some [(name, value)])
  | .pliteral _ lit, .immediate lit' => pure (if lit == lit' then some [] else none)
  | .destruct _ tag pats, .struct tag' values =>
    if tag == tag' then matchMany env pats values else pure none
  | .expand _ patterns, .record renv fields =>
    matchExpand renv (intersectFields patterns fields)
  | _, _ => pure none

partial def matchMany (env : Env) : List Pattern → List Value → EvalM (Option (List (Name × Value)))
  | [], [] => pure (some [])
  | p :: ps, v :: vs => do
    match ← matchPat env p v with
    | none => pure none
    | some bindings =>
      match ← matchMany env ps vs with
      | none => pure none
      | some rest => pure (some (bindings ++ rest))
  | _, _ => pure none

partial def matchExpand (renv : Env) :
    List (String × Pattern × Name × JStatement) → EvalM (Option (List (Name × Value)))
  | [] => pure (some [])
  | (_, pattern, retName, statement) :: rest => do
    let slot ← freshSlot
    evalStatement (extendEnv retName (.consumer (.writeSlotOpt slot)) renv) statement
    match ← storeGet? slot with
    | none => pure none
    | some v =>
      match ← matchPat renv pattern v with
      | none => pure none
      | some bindings =>
        match ← matchExpand renv rest with
        | none => pure none
        | some restB => pure (some (bindings ++ restB))

end

/-! ## Program entry -/

/-- Port of `evalProgram`/`EvalPass.runPassImpl`. Locates `main` among the
toplevels (ascending name order, matching Haskell's `Map.keys … & find`),
mints the single `finish` continuation, and runs the interpreter, bridging
`EvalError` into `CompileError`.

The synthetic entry statement mirrors Haskell exactly:
`Join finish (Finish) (Join return (Apply [Construct Tuple [] []] [finish]) body)`. -/
def evalProgram (moduleName : ModuleName) (handlers : Handlers) (program : JProgram) :
    MalgoM Unit := do
  let toplevels : Toplevels :=
    program.definitions.foldl (fun m (_, name, ret, stmt) => m.insert name (ret, stmt)) {}
  match toplevels.toList.find? (fun (k, _) => k.name == "main") with
  | none => pure ()  -- No main function
  | some (_, (ret, statement)) => do
    let finish ← newTemporalId moduleName "finish"
    let slots ← IO.mkRef (∅ : Std.HashMap Nat Value)
    let nextSlot ← IO.mkRef 0
    let ctx : EvalCtx := { slots, nextSlot, handlers, toplevels }
    let r := jstmtRange statement
    let entry : JStatement :=
      .join r finish (.finish r)
        (.join r ret (.apply r [.construct r .tuple [] []] [finish]) statement)
    let result ← MalgoM.io (((evalStatement emptyEnv entry).run ctx).run)
    match result with
    | .ok () => pure ()
    | .error .exitSuccess => pure ()  -- malgo_exit_success: clean termination
    | .error e => throw { passName := "Eval", message := e.render, range? := e.range? }

end Malgo.Sequent.Eval
