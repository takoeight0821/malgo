import Std.Data.TreeSet
import Malgo.Prelude
import Malgo.Sequent.Fun

/-! Port of `src/Malgo/Backend/Zig/Ir.hs`: the Zig backend's internal IR,
produced by `ClosureConv.convertProgram` from (normalized) Join IR and
printed to Zig text by `Emit.emitProgram`.

The IR is first-order and in ANF: every value is produced by a named `Let`
and every operand position is a variable. Closure conversion has already
happened — captures are explicit index reads (`ReadCapture`) against the
function's own closure object (the `self` parameter of the self-passing
calling convention `fn (self, args) rt.Value`), and every nested Lambda /
escaping join / Object field has been lifted into its own `Func`. This is
exactly the shape the Perceus pass needs: reference counting reduces to
counting variable occurrences.

`Dup`/`Drop` are only ever inserted by the Perceus pass; `ClosureConv.
convertProgram` never emits them.

Note on `DropReuse`/`MkStructReuse` (inserted only by `Reuse`): a reuse
token is an ordinary `Name`, not its own `Expr`, because `rt.dropReuse`
returns `?rt.Value` while every other `Expr` yields a plain `rt.Value` —
keeping the two-instruction form (rather than folding the pairing into one
node) is also what keeps a future interprocedural extension (threading a
token through a closure's captures) possible without another IR change.

Type order deviates from the Haskell source's declaration order (which
relies on Haskell's mutual-recursion-across-the-whole-module default): only
`Block`/`Terminator` are genuinely mutually recursive (`Terminator.if`
embeds two `Block`s, `Block` ends in a `Terminator`), so that pair is the
one `mutual` block; everything else is a plain linear dependency chain
(`Path → Test → Guard`, `Path → Expr → Stmt`). -/

namespace Malgo.Backend.Zig.Ir

open Malgo.Sequent.Fun (Name Literal Tag)

instance : Inhabited Name :=
  ⟨{ name := "", moduleName := .moduleName "", sort := .external }⟩

inductive Path where
  | root (name : Name)
  | field (path : Path) (index : Nat)
  deriving BEq

partial def Path.root' : Path → Name
  | .root n => n
  | .field p _ => p.root'

inductive Test where
  /-- `path.kind == .<kindName>` -/
  | kindIs (path : Path) (kindName : String)
  /-- `path.kind == .strukt and rt.tagEq(path.payload.strukt.tag, tag)` -/
  | tagEq (path : Path) (tag : Tag)
  /-- kind + payload equality against a literal -/
  | litEq (path : Path) (l : Literal)
  deriving BEq

def Test.path : Test → Path
  | .kindIs p _ => p
  | .tagEq p _ => p
  | .litEq p _ => p

inductive Guard where
  /-- Conjunction of borrowing tests; empty means always-true. -/
  | and (tests : List Test)
  /-- `rt.isZero(v)` (borrows). -/
  | isZero (v : Name)
  deriving BEq

/-- Every `Expr` yields exactly one `rt.Value`. -/
inductive Expr where
  /-- `rt.mk*` — a fresh, owned allocation. -/
  | lit (l : Literal)
  /-- `rt.mkStruct` — consumes one reference per field. -/
  | mkStruct (tag : Tag) (fields : List Name)
  /-- `rt.mkClosure(&fn, captures)` — consumes one reference per capture. -/
  | mkClosure (fn : Name) (captures : List Name)
  /-- `rt.mkRecord(fields, sharedCaptures)` — ONE captures array shared by
  every field function (mirroring the evaluator's `Record env fields`);
  consumes one reference per capture. -/
  | mkRecord (fields : List (String × Name)) (sharedCaptures : List Name)
  /-- `rt.<name>(&.{args})` — borrows its arguments, returns owned. Covers
  Join IR's `Primitive`, `ExternalCall` and `BinOp` uniformly. -/
  | prim (name : String) (args : List Name)
  /-- Borrowed alias: a chain of `.payload.strukt.fields[i]` reads
  (pattern-match bindings). No reference is created or consumed. -/
  | readPath (path : Path)
  /-- Borrowed: `rt.capturesOf(self)[i]`. Only appears as the leading
  statements of a `closureFn`/`fieldFn` body. -/
  | readCapture (self : Name) (index : Nat)
  /-- `rt.forceField(v, field)` — consumes one reference of the record,
  returns the field's (owned) value. Record fields are call-by-name
  (`Eval` re-runs the field statement per force), so forcing twice
  legitimately consumes two references. -/
  | force (v : Name) (field : String)
  /-- `rt.panicUnimplemented(..)` (the `Cocase` stub). `noreturn`: the
  printer truncates the rest of the block after it, and RcCheck treats it
  as terminating the path. -/
  | panicExpr (msg : String)
  /-- `rt.mkStructReuse(tok, tag, fields)` — inserted only by `Reuse`.
  Consumes the reuse token `tok` (bound by a matching `dropReuse`) exactly
  once, plus one reference per field as usual; recycles `tok`'s allocation
  in place when the runtime marked it reusable, else falls back to a fresh
  `mkStruct`. -/
  | mkStructReuse (tok : Name) (tag : Tag) (fields : List Name)
  deriving BEq

inductive Stmt where
  | let (name : Name) (expr : Expr)
  /-- Inserted only by Perceus. -/
  | dup (name : Name)
  /-- Inserted only by Perceus. -/
  | drop (name : Name)
  /-- `const tok = rt.dropReuse(x, arity);` — inserted only by `Reuse`,
  replacing a plain `drop` whose value is about to be rebuilt by a
  same-arity `mkStructReuse` later in this same statement list. Consumes
  `x`'s reference; binds `tok` (an `?rt.Value`, non-null when the runtime
  recycled `x`'s allocation). -/
  | dropReuse (tok x : Name) (arity : Nat)
  deriving BEq

mutual

/-- Every terminator except `if`/`panic` consumes one reference of each of
its operands (the call moves them into the callee).

The call terminators do not emit a native Zig call: each returns an `rt.Action`
that the runtime's `rt.run` trampoline dispatches (see `Emit`). The RC contract
above is unaffected — an Action carries exactly the references a direct call
would have moved. -/
inductive Terminator where
  /-- `return rt.applyCovalue(k, v)` -/
  | applyCo (k v : Name)
  /-- `return rt.callClosure(f, &.{args})` -/
  | callClosure (f : Name) (args : List Name)
  /-- `return rt.staticCall(&<fn>, &.{args})` (Join IR's `Invoke`) -/
  | staticCall (fn : Name) (args : List Name)
  /-- `return rt.projectField(v, field, k)` -/
  | project (v : Name) (field : String) (k : Name)
  /-- `return rt.applyDestructor(v, name, &.{args})` -/
  | destruct (v : Name) (name : String) (args : List Name)
  /-- `return rt.done(v)` (Join IR's `Finish`; the generated `main` owns the
  result, which comes back out of the trampoline) -/
  | «return» (v : Name)
  /-- `Ifz` and every lowered `Select` arm. The guard only borrows. -/
  | «if» (guard : Guard) (thenB elseB : Block)
  /-- No-match fall-through and similar. RC-exempt (`noreturn`). -/
  | panic (msg : String)
  deriving BEq

inductive Block where
  | mk (stmts : List Stmt) (terminator : Terminator)
  deriving BEq

end

def Block.stmts : Block → List Stmt
  | .mk ss _ => ss

def Block.terminator : Block → Terminator
  | .mk _ t => t

-- `partial def`s over the IR (in ClosureConv/Peephole/Perceus/…) require
-- `Inhabited` return types; these give every core node a canonical default.
instance : Inhabited Path := ⟨.root default⟩
instance : Inhabited Test := ⟨.kindIs default ""⟩
instance : Inhabited Guard := ⟨.and []⟩
instance : Inhabited Expr := ⟨.panicExpr ""⟩
instance : Inhabited Stmt := ⟨.drop default⟩
instance : Inhabited Terminator := ⟨.panic ""⟩
instance : Inhabited Block := ⟨.mk [] (.panic "")⟩

/-- How a function receives its `self` argument. `topLevelFn`s are called
directly with the immortal `rt.no_self` sentinel and ignore it;
`closureFn`s/`fieldFn`s receive the closure/record object itself and read
their captures out of it. The distinction matters to Perceus: only
`closureFn`/`fieldFn` own (and must consume) their `self`. -/
inductive FuncKind where
  | topLevelFn
  | closureFn
  | fieldFn
  deriving BEq

structure Func where
  range : Range
  name : Name
  kind : FuncKind
  /-- The name bound to the `self` parameter. Referenced only by
  `ReadCapture` reads and (after Perceus) a `Drop`; unused for
  `topLevelFn`. The printer renders it as the literal `self`. -/
  selfVar : Name
  /-- Bound from `args[i]` in order. A top-level definition's list is
  `[retName]` (the return continuation). -/
  params : List Name
  body : Block
  deriving BEq

structure Program where
  funcs : List Func
  /-- The bootstrap function's name, when the compiled module has a
  top-level `main` (mirroring `Eval.evalProgram`, a module without one
  compiles to a no-op executable). -/
  entry : Option Name
  deriving BEq

mutual

partial def freeVarsBlock (b : Block) : Std.TreeSet Name :=
  go b.stmts
where
  go : List Stmt → Std.TreeSet Name
    | [] => freeVarsTerminator b.terminator
    | .let x e :: rest => (freeVarsExpr e).union ((go rest).erase x)
    | .dup x :: rest => (go rest).insert x
    | .drop x :: rest => (go rest).insert x
    | .dropReuse tok x _ :: rest => ((go rest).erase tok).insert x

partial def freeVarsTerminator : Terminator → Std.TreeSet Name
  | .applyCo k v => Std.TreeSet.ofList [k, v]
  | .callClosure f args => Std.TreeSet.ofList (f :: args)
  | .staticCall _ args => Std.TreeSet.ofList args
  | .project v _ k => Std.TreeSet.ofList [v, k]
  | .destruct v _ args => Std.TreeSet.ofList (v :: args)
  | .«return» v => Std.TreeSet.ofList [v]
  | .«if» guard t e => ((freeVarsGuard guard).union (freeVarsBlock t)).union (freeVarsBlock e)
  | .panic _ => {}

partial def freeVarsGuard : Guard → Std.TreeSet Name
  | .and tests => Std.TreeSet.ofList (tests.map (Test.path · |>.root'))
  | .isZero v => Std.TreeSet.ofList [v]

partial def freeVarsExpr : Expr → Std.TreeSet Name
  | .lit _ => {}
  | .mkStruct _ vs => Std.TreeSet.ofList vs
  | .mkClosure _ vs => Std.TreeSet.ofList vs
  | .mkRecord _ vs => Std.TreeSet.ofList vs
  | .prim _ vs => Std.TreeSet.ofList vs
  | .mkStructReuse tok _ vs => Std.TreeSet.ofList (tok :: vs)
  | .readPath p => Std.TreeSet.ofList [p.root']
  | .readCapture self _ => Std.TreeSet.ofList [self]
  | .force v _ => Std.TreeSet.ofList [v]
  | .panicExpr _ => {}

end

/-- `suffixFreeVars stmts term [i]! == freeVarsBlock (Block.mk (stmts.drop
i) term)` for every `i` in `[0 .. stmts.length]`, computed in one
bottom-up pass. Callers that walk a statement list and, at each position,
need the free variables of everything from there on (e.g. an unused-
binding check) would otherwise call `freeVarsBlock` afresh on each
shrinking suffix, making that walk quadratic in the block's length;
indexing into this list instead keeps it linear. Result is in the same
order as a right fold: `result[0]!` is the free vars of the whole (stmts,
term); `result.getLast!` is the free vars of `term` alone (length
`stmts.length + 1`). -/
def suffixFreeVars (stmts : List Stmt) (term : Terminator) : List (Std.TreeSet Name) :=
  let step (s : Stmt) (live : Std.TreeSet Name) : Std.TreeSet Name :=
    match s with
    | .let x e => (freeVarsExpr e).union (live.erase x)
    | .dup x => live.insert x
    | .drop x => live.insert x
    | .dropReuse tok x _ => (live.erase tok).insert x
  stmts.foldr (init := [freeVarsTerminator term]) fun s acc => step s acc.head! :: acc

/-- Operands a terminator consumes one reference of (`if`/`panic` consume
nothing), with multiplicity. -/
def termOperands : Terminator → List Name
  | .applyCo k v => [k, v]
  | .callClosure f args => f :: args
  | .staticCall _ args => args
  | .project v _ k => [v, k]
  | .destruct v _ args => v :: args
  | .«return» v => [v]
  | .«if» .. => []
  | .panic _ => []

-- Free-variable sanity checks: a variable read in a leaf position is free;
-- a `let`-bound name is not free once bound; `suffixFreeVars`'s head
-- matches a direct `freeVarsBlock` call.
private def n (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }

#guard freeVarsTerminator (.«return» (n "x")) == Std.TreeSet.ofList [n "x"]
#guard freeVarsBlock (.mk [.let (n "x") (.lit (.int32 0))] (.«return» (n "x"))) == ({} : Std.TreeSet Name)
#guard freeVarsBlock (.mk [] (.«return» (n "y"))) == Std.TreeSet.ofList [n "y"]
#guard (suffixFreeVars [.let (n "x") (.readPath (.root (n "z")))] (.«return» (n "x"))).head!
  == Std.TreeSet.ofList [n "z"]

end Malgo.Backend.Zig.Ir
