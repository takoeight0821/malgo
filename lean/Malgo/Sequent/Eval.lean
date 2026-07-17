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

/-! ## Values (defunctionalized consumers, IORef-free) -/

mutual

inductive Value where
  | immediate (lit : Literal)
  | struct (tag : Tag) (values : List Value)
  | function (env : Env) (params : List Name) (statement : JStatement)
  | record (env : Env) (fields : List (String × Name × JStatement))
  | codata (env : Env) (branches : List (String × List Name × JStatement))
  | consumer (k : ConsumerK)

/-- Defunctionalized consumer closure. `run` is `fromConsumer env k`;
`writeSlot`/`writeSlotOpt` write to an integer store slot (the `Mu` ref and
`matchExpand`'s `Maybe` ref respectively). Both writes are the same store
operation; the `Opt` variant differs only in how its slot is seeded
(absent, meaning `Nothing`) and read at the allocation site. -/
inductive ConsumerK where
  | run (env : Env) (k : JConsumer)
  | writeSlot (slot : Nat)
  | writeSlotOpt (slot : Nat)

/-- Haskell `Env {localBindings :: IntMap Value, externalBindings :: Map
Name Value}`. `localBindings` keys are Internal/Temporal uniqs (the fast
path); externals are an ascending assoc list (`Std.TreeMap` cannot nest in
an inductive). -/
inductive Env where
  | mk (localBindings : Malgo.IntMap Value) (externalBindings : List (Id × Value))

end

instance : Inhabited Value := ⟨.struct .tuple []⟩
instance : Inhabited Env := ⟨.mk .nil []⟩

#check Value
#check ConsumerK
#check Env

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

instance : Inhabited Range := ⟨{ start := SourcePos.initial "", stop := SourcePos.initial "" }⟩
instance : Inhabited Id := ⟨{ name := "", moduleName := .moduleName "", sort := .external }⟩
instance : Inhabited EvalError := ⟨.noMatch default default⟩

/-! ## Handlers and the evaluation context -/

/-- IO surface the primitives use. Two constructors: `real` (process
handles) and `buffered` (in-memory stdin + stdout accumulator ref) — the
golden tests need in-process stdout capture. -/
structure Handlers where
  stdin : IO (Option Char)
  stdout : Char → IO Unit
  stderr : Char → IO Unit
  arguments : List String

structure EvalCtx where
  slots : IO.Ref (Std.HashMap Nat Value)
  nextSlot : IO.Ref Nat
  handlers : Handlers
  toplevels : Std.TreeMap Name (Name × JStatement)

abbrev EvalM := ReaderT EvalCtx (ExceptT EvalError IO)

end Malgo.Sequent.Eval
