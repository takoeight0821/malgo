import Malgo.Prelude
import Malgo.Monad
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Core.Normalize
import Malgo.Sequent.Core.Escape
import Malgo.Sequent.Fun
import Malgo.Backend.Go.Runtime

/-! Lowers Join IR directly to Go source text.

There is no intermediate IR and no closure conversion. Go has real closures
and a garbage collector, so a Malgo lambda becomes a Go function literal and
a record field becomes a Go closure over its environment — the machinery the
Zig backend needs (ANF, lambda lifting, an explicit captures array, the
self-passing convention, Perceus/Reuse/RcCheck) has no counterpart here.

What Go does *not* give us is guaranteed tail calls, and this IR is CPS. So
the trampoline survives: a generated function returns an `Action` naming the
call it wants and `run` dispatches in a loop. See `runtime/go/runtime.go`.

Every Malgo name becomes a Go variable of type `Value`, including
continuations — a continuation is an `Fn`, and `Fn` satisfies `Value`. That
uniformity is what lets `cut`, `join` and `apply` share one representation
instead of the two the Zig backend distinguishes.

`Normalize` must run first. It eliminates `Cut (Mu x s) k`, and with that
gone no `Mu` reaches a producer position the `ir-invariants` gate allows, so
the emitter has no `Mu` case to write. The one below throws rather than
emitting anything, and is unreachable for a program that came through
`compileToGo`. -/

namespace Malgo.Backend.Go

open Malgo.Sequent.Fun (Name Literal Tag Pattern)
open Malgo.Sequent.Core
open Malgo.Sequent.Core.Join
open Malgo.Sequent.Core.Normalize (normalizeStatement)
open Malgo.Sequent.Core.Escape (Ownership OwnershipMap LocalEnv classifyJoins classifyJoinsConsumer)

/-! ## Mangling -/

/-- Escape one character into a Go identifier fragment. `_` is escaped along
with everything else so that the escape sequences introduced here are the
only underscores in the output, which keeps `mangleText` injective: two
different Malgo names can never mangle to the same Go identifier. -/
def mangleChar (c : Char) : String :=
  if c.isAlphanum then String.singleton c else "_u" ++ toString c.toNat ++ "_"

def mangleText (s : String) : String :=
  String.join (s.toList.map mangleChar)

/-- Mangle an `Id` into a Go identifier. `Id.toText` already separates the
three sorts (`Mod.name`, `Mod.#name_uniq`, `Mod.$name_uniq`), so mangling it
is injective. The `m_` prefix means a generated identifier can never collide
with a Go keyword or with a name from the embedded runtime. -/
def mangleId (x : Malgo.Id) : String :=
  "m_" ++ mangleText x.toText

/-! ## Literals -/

private def hexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat (n + 48) else Char.ofNat (n - 10 + 97)

private def hexPad (width n : Nat) : String :=
  let rec go (i : Nat) (n : Nat) (acc : String) : String :=
    match i with
    | 0 => acc
    | i + 1 => go i (n / 16) (String.singleton (hexDigit (n % 16)) ++ acc)
  go width n ""

/-- Escape a string for a Go interpreted string literal. Non-ASCII is written
as `\uXXXX`/`\UXXXXXXXX` rather than raw bytes so the generated source is
pure ASCII whatever the source file's encoding was. -/
def escapeGoString (s : String) : String :=
  String.join <| s.toList.map fun c =>
    match c with
    | '"' => "\\\""
    | '\\' => "\\\\"
    | '\n' => "\\n"
    | '\r' => "\\r"
    | '\t' => "\\t"
    | c =>
      let n := c.toNat
      if n ≥ 0x20 && n < 0x7F then String.singleton c
      else if n ≤ 0xFFFF then "\\u" ++ hexPad 4 n
      else "\\U" ++ hexPad 8 n

/-- Render a Haskell-`show`-formatted float as a Go expression. The finite
cases (`3.5`, `1.0e-2`) are already valid Go float literals; the three
non-finite spellings are not, and have to go through `math`. -/
private def goFloatLit (shown : String) (cast : String) : String :=
  match shown with
  | "Infinity" => cast ++ "(math.Inf(1))"
  | "-Infinity" => cast ++ "(math.Inf(-1))"
  | "NaN" => cast ++ "(math.NaN())"
  | s => s

def compileLiteral : Literal → String
  | .int32 n => "mkInt32(" ++ toString n.toInt ++ ")"
  | .int64 n => "mkInt64(" ++ toString n.toInt ++ ")"
  | .float f => "mkFloat(" ++ goFloatLit (Malgo.haskellShowFloat32 f) "float32" ++ ")"
  | .double d => "mkDouble(" ++ goFloatLit (Malgo.haskellShowFloat d) "float64" ++ ")"
  -- Emitted numerically: a character literal would need its own escaping
  -- rules, and the codepoint is what `mkChar` takes anyway.
  | .char c => "mkChar(" ++ toString c.toNat ++ ")"
  | .string s => "mkString(\"" ++ escapeGoString s ++ "\")"

/-- Tag text is *not* mangled: the runtime prints it directly, and
`Eval.valueToText` prints the constructor's source name. -/
def compileTag : Tag → String
  | .tuple => "tupleTag"
  | .tag t => "\"" ++ escapeGoString t ++ "\""

/-! ## Pattern tests

A branch compiles to a nest of Go `if` statements rather than one boolean
guard, because sub-patterns bind variables the body needs in scope. Each
level binds its fields to distinct names before recursing, so a sibling
sub-pattern's scrutinee is never shadowed by an earlier sibling's block. -/

private def litTest (scrut tmp : String) : Literal → String
  | .int32 n => s!"{tmp}, ok := {scrut}.(*Int32); ok && {tmp}.V == {n.toInt}"
  | .int64 n => s!"{tmp}, ok := {scrut}.(*Int64); ok && {tmp}.V == {n.toInt}"
  | .float f =>
    s!"{tmp}, ok := {scrut}.(*Float); ok && {tmp}.V == {goFloatLit (Malgo.haskellShowFloat32 f) "float32"}"
  | .double d =>
    s!"{tmp}, ok := {scrut}.(*Double); ok && {tmp}.V == {goFloatLit (Malgo.haskellShowFloat d) "float64"}"
  | .char c => s!"{tmp}, ok := {scrut}.(*Char); ok && {tmp}.V == {c.toNat}"
  | .string s => s!"{tmp}, ok := {scrut}.(*Str); ok && {tmp}.V == \"{escapeGoString s}\""

/-! ## The emitter -/

private def err (message : String) : CompileError :=
  { passName := "Go", message }

/-- The trampoline's argument-slot count. The runtime's `Action` has exactly
this many named slots, and a generated function has exactly this many
parameters. -/
private def maxCallArgs : Nat := 2

/-- Positional parameter name for slot `i` of a generated function. -/
private def argSlot (i : Nat) : String := s!"a{i}"

/-- Call shapes the trampoline provides. More than `maxCallArgs` operands
cannot occur: `ToFun` builds single-parameter lambdas and singleton applies,
and `ToCore` appends exactly one consumer. Checked rather than assumed, so a
front-end change that broke the assumption fails loudly here instead of
silently truncating a call. -/
private def tailCall (fn : String) (args : List String) : MalgoM String :=
  match args with
  | [] => pure s!"tail0({fn})"
  | [a] => pure s!"tail1({fn}, {a})"
  | [a, b] => pure s!"tail2({fn}, {a}, {b})"
  | _ => throw (err s!"call with {args.length} operands exceeds maxArgs ({maxCallArgs})")

mutual

/-- A producer compiles to a Go *expression* of type `Value`. Producers that
contain statements (a lambda's body, a record field's body) become function
literals, so `ind` is the indentation those statements start at.

A lambda body and a record field body are each their own function scope, so
they start from a freshly computed `OwnershipMap` and an empty `LocalEnv`.
Nothing is lost by dropping the enclosing env: `classifyJoins` marks any join
free in a nested producer as `Escaping`, so no name in that env could have
been `Local` here anyway. -/
partial def compileProducer (ind : String) (ownership : OwnershipMap) (env : LocalEnv) :
    Producer → MalgoM String
  | .var _ n => pure (mangleId n)
  | .literal _ lit => pure (compileLiteral lit)
  | .construct _ tag ps ks => do
    let pargs ← ps.mapM (compileProducer ind ownership env)
    let kargs := ks.map mangleId
    let allArgs := pargs ++ kargs
    let suffix := if allArgs.isEmpty then "" else ", " ++ ", ".intercalate allArgs
    pure s!"mkStruct({compileTag tag}{suffix})"
  | .lambda _ names stmt => do
    let inner := ind ++ "\t"
    if names.length > maxCallArgs then
      throw (err s!"lambda with {names.length} parameters exceeds maxArgs ({maxCallArgs})")
    let binds := String.join <| names.mapIdx fun i n =>
      s!"{inner}{mangleId n} := {argSlot i}\n{inner}_ = {mangleId n}\n"
    let body ← compileStatement inner (classifyJoins stmt) {} stmt
    pure s!"Fn(func(a0, a1 Value) Action \{\n{binds}{body}{ind}})"
  | .object _ fields => do
    let inner := ind ++ "\t\t"
    -- Ascending field order, matching the interpreter and the Zig runtime's
    -- `NamedField` slice. The runtime never iterates a map for this reason.
    let sorted := fields.toArray.qsort (fun a b => a.1 < b.1) |>.toList
    let entries ← sorted.mapM fun (fieldName, ret, stmt) => do
      let body ← compileStatement inner (classifyJoins stmt) {} stmt
      pure s!"{ind}\t\{Name: \"{escapeGoString fieldName}\", Code: Fn(func(a0, a1 Value) Action \{\n\
        {inner}{mangleId ret} := a0\n{inner}_ = {mangleId ret}\n{body}{ind}\t})},\n"
    pure s!"mkRecord([]NamedField\{\n{String.join entries}{ind}})"
  | .mu _ _ _ =>
    throw (err "Mu in producer position should have been eliminated by Normalize")

/-- Reify a consumer as a Go closure, for an `Escaping` join. The closure is
its own function scope, so — like a lambda body — it reclassifies its own
joins and starts with an empty `LocalEnv`. -/
partial def compileConsumer (ind : String) : Consumer → MalgoM String
  | .label _ n => pure (mangleId n)
  | .finish _ => pure "identityKont"
  | c => do
    let inner := ind ++ "\t"
    let ownership := classifyJoinsConsumer c
    let body ← applyConsumer inner ownership {} c "a0"
    pure s!"Fn(func(a0, a1 Value) Action \{\n{body}{ind}})"

/-- Emit `consumer` applied to `value`, a Go expression already holding the
produced value, as statements in the current scope. This is what replaces a
`Local` join: instead of allocating a closure and bouncing the trampoline
through it, the consumer's body lands here.

Mirrors the Zig backend's `ClosureConv.convertApply`. `compileConsumer` also
routes through it, so the two paths cannot drift. -/
partial def applyConsumer (ind : String) (ownership : OwnershipMap) (env : LocalEnv)
    (consumer : Consumer) (value : String) : MalgoM String :=
  match consumer with
  | .label _ n => pure s!"{ind}return applyCo({mangleId n}, {value})\n"
  | .finish _ => pure s!"{ind}return done({value})\n"
  | .apply _ ps ks => do
    let pargs ← ps.mapM (compileProducer ind ownership env)
    let kargs := ks.map mangleId
    let call ← tailCall s!"asFn({value})" (pargs ++ kargs)
    pure s!"{ind}return {call}\n"
  | .project _ field ret =>
    pure s!"{ind}return projectField({value}, \"{escapeGoString field}\", {mangleId ret})\n"
  | .«then» _ name stmt => do
    let body ← compileStatement ind ownership env stmt
    pure s!"{ind}{mangleId name} := {value}\n{ind}_ = {mangleId name}\n{body}"
  | .select _ branches => do
    let arms ← branches.mapM (compileBranch ind ownership env value)
    -- Falling past every arm means no pattern matched. `malgoPanic` exits,
    -- but Go's flow analysis does not know that, so the enclosing function
    -- still needs a terminating statement after it.
    pure s!"{String.join arms}{ind}malgoPanic(\"no matching branch\")\n\
      {ind}return Action\{}\n"

partial def compileBranch (ind : String) (ownership : OwnershipMap) (env : LocalEnv)
    (scrut : String) : Branch → MalgoM String
  | .branch _ pat stmt => do
    let body ← compilePattern ind "b" scrut pat []
      (fun ind' _ => compileStatement ind' ownership env stmt)
    -- Each arm gets its own block so that one arm's bindings cannot collide
    -- with the next one's.
    pure s!"{ind}\{\n{body}{ind}}\n"

/-- Wrap `mkBody` in whatever tests and bindings `pat` requires. `path` names
this node in the pattern tree and keeps every generated temporary distinct,
so no binding is ever shadowed by a nested one.

`bound` carries the Go identifiers already declared in this arm. The renamer
gives every wildcard in a clause the same `Id`, so one pattern can ask to
bind the same name twice, and a second `:=` on it is a Go compile error. A
repeat discards its scrutinee instead: the name is a wildcard (Malgo has no
non-linear patterns), so nothing ever reads either binding. -/
partial def compilePattern (ind path scrut : String) (pat : Pattern) (bound : List String)
    (mkBody : String → List String → MalgoM String) : MalgoM String :=
  match pat with
  | .pvar _ n => do
    let goName := mangleId n
    if bound.contains goName then
      -- Only a wildcard may legitimately repeat. Silently keeping the first
      -- binding for a genuinely non-linear pattern would compile, not crash,
      -- and only diverge from the interpreter in the output — so refuse.
      if n.name != "_" then
        throw (err s!"pattern binds '{n.name}' more than once")
      let body ← mkBody ind bound
      pure s!"{ind}_ = {scrut}\n{body}"
    else
      let body ← mkBody ind (goName :: bound)
      pure s!"{ind}{goName} := {scrut}\n{ind}_ = {goName}\n{body}"
  | .pliteral _ lit => do
    let inner := ind ++ "\t"
    let body ← mkBody inner bound
    pure s!"{ind}if {litTest scrut s!"t{path}" lit} \{\n{body}{ind}}\n"
  | .destruct _ tag pats => do
    let inner := ind ++ "\t"
    let tmp := s!"t{path}"
    let fieldNames := pats.mapIdx fun i _ => s!"f{path}_{i}"
    let binds := String.join <| fieldNames.mapIdx fun i f =>
      s!"{inner}{f} := {tmp}.Fields[{i}]\n{inner}_ = {f}\n"
    let body ← compilePatternList inner path 0 fieldNames pats bound mkBody
    pure s!"{ind}if {tmp}, ok := {scrut}.(*Struct); ok && {tmp}.Tag == {compileTag tag} \
      && len({tmp}.Fields) == {pats.length} \{\n{binds}{body}{ind}}\n"
  | .expand _ fields => do
    let inner := ind ++ "\t"
    let fieldNames := fields.mapIdx fun i _ => s!"f{path}_{i}"
    -- Record fields are call-by-name, so matching one forces it. `forceField`
    -- is the only place the native stack grows with nesting depth.
    let binds := String.join <| (fields.zip fieldNames).map fun ((fieldName, _), f) =>
      s!"{inner}{f} := forceField({scrut}, \"{escapeGoString fieldName}\")\n{inner}_ = {f}\n"
    let body ← compilePatternList inner path 0 fieldNames (fields.map Prod.snd) bound mkBody
    pure s!"{ind}if _, ok := {scrut}.(*Record); ok \{\n{binds}{body}{ind}}\n"

/-- Chain sibling sub-patterns so that each one's bindings stay in scope for
the next one and for the body. -/
partial def compilePatternList (ind path : String) (idx : Nat)
    (scruts : List String) (pats : List Pattern) (bound : List String)
    (mkBody : String → List String → MalgoM String) : MalgoM String :=
  match scruts, pats with
  | s :: ss, p :: ps =>
    compilePattern ind s!"{path}_{idx}" s p bound
      (fun ind' bound' => compilePatternList ind' path (idx + 1) ss ps bound' mkBody)
  | _, _ => mkBody ind bound

/-- Hand `valueExpr` to the consumer named `k`. If `k` is a `Local` join its
body is inlined here, which removes both the closure and the trampoline
bounce; otherwise the value goes to the closure the usual way.

The temporary is named after `k`, which cannot collide with any mangled
identifier: `mangleText` escapes every `_`, so a mangled name never ends in
a bare `_v`. -/
partial def sendToConsumer (ind : String) (ownership : OwnershipMap) (env : LocalEnv)
    (k : Name) (valueExpr : String) : MalgoM String :=
  match env.get? k with
  | none => pure s!"{ind}return applyCo({mangleId k}, {valueExpr})\n"
  | some consumer => do
    let tmp := s!"{mangleId k}_v"
    let body ← applyConsumer ind ownership env consumer tmp
    pure s!"{ind}{tmp} := {valueExpr}\n{ind}_ = {tmp}\n{body}"

/-- A statement compiles to a Go statement sequence ending in a `return`. -/
partial def compileStatement (ind : String) (ownership : OwnershipMap) (env : LocalEnv) :
    Statement → MalgoM String
  | .cut producer consumer => do
    let p ← compileProducer ind ownership env producer
    sendToConsumer ind ownership env consumer p
  | .join _ name consumer body =>
    match ownership.get? name with
    | some .Local =>
      -- Emit nothing: the consumer is recorded and lands at its use site.
      -- Every `Local` join has exactly one use site, so this never
      -- duplicates a body (see wiki/2026-09-12-go-backend-performance-
      -- investigation.md for the corpus-wide count).
      compileStatement ind ownership (env.insert name consumer) body
    | _ => do
      let c ← compileConsumer ind consumer
      let rest ← compileStatement ind ownership env body
      -- Declared before it is assigned so the consumer's own body may refer
      -- to it: a join point that loops is bound to itself.
      pure s!"{ind}var {mangleId name} Value\n{ind}{mangleId name} = {c}\n\
        {ind}_ = {mangleId name}\n{rest}"
  | .primitive _ name producers consumer => do
    let args ← producers.mapM (compileProducer ind ownership env)
    sendToConsumer ind ownership env consumer s!"{name}({", ".intercalate args})"
  | .externalCall _ name producers consumer => do
    let args ← producers.mapM (compileProducer ind ownership env)
    sendToConsumer ind ownership env consumer s!"{name}({", ".intercalate args})"
  | .binOp _ op lhs rhs consumer => do
    let l ← compileProducer ind ownership env lhs
    let r ← compileProducer ind ownership env rhs
    sendToConsumer ind ownership env consumer s!"{op}({l}, {r})"
  | .invoke _ name consumer =>
    -- A top-level definition is a Go function, assignable to `Fn` directly;
    -- it needs no `asFn`. `invoke`'s consumer always escapes, so it is never
    -- in `env`.
    pure s!"{ind}return tail1({mangleId name}, {mangleId consumer})\n"
  | .ifz _ cond thenS elseS => do
    let c ← compileProducer ind ownership env cond
    let inner := ind ++ "\t"
    let t ← compileStatement inner ownership env thenS
    let e ← compileStatement inner ownership env elseS
    pure s!"{ind}if isZero({c}) \{\n{t}{ind}} else \{\n{e}{ind}}\n"

end

/-- One Go function per top-level definition. The single parameter is the
definition's return continuation. -/
def compileDefinition (d : Definition) : MalgoM String := do
  -- `d.body` is already normalized by `compileToGo`, which `classifyJoins`
  -- requires: it assumes no `Consumer.label` in a join's consumer slot.
  let body ← compileStatement "\t" (classifyJoins d.body) {} d.body
  pure s!"func {mangleId d.name}(a0, a1 Value) Action \{\n\
    \t{mangleId d.ret} := a0\n\t_ = {mangleId d.ret}\n{body}}\n"

private def goPrelude : String :=
  "package main\n\nimport (\n\t\"errors\"\n\t\"math\"\n\t\"os\"\n\t\"os/exec\"\n\
   \t\"strconv\"\n\t\"strings\"\n\t\"unicode/utf8\"\n)\n"

/-- Entry point, if the module defines one. A module without `main` compiles
to a program that does nothing, rather than to a link error — the same choice
the Zig backend makes, and the reason the golden sweep needs no skip list for
library-only testcases. -/
private def entryCall (moduleName : ModuleName) (program : Program) : String :=
  let mainId : Malgo.Id := { name := "main", moduleName, sort := .external }
  if program.definitions.any (fun d => d.name == mainId) then
    -- `main` is handed a consumer that receives the program's entry function
    -- and applies it to unit and the finishing continuation.
    s!"\trun({mangleId mainId}, Fn(func(a0, a1 Value) Action \{\n\
      \t\treturn tail2(asFn(a0), unit(), identityKont)\n\t}), nil)\n"
  else
    ""

/-- Compile a Join IR program to a complete, self-contained Go program.

`Normalize` runs here, per definition, rather than being expected of the
caller: it is what makes `compileProducer`'s `Mu` case unreachable. -/
def compileToGo (moduleName : ModuleName) (program : Program) : MalgoM String := do
  let normalizedDefs := program.definitions.map fun d =>
    { d with body := normalizeStatement d.body }
  let normalized : Program := { program with definitions := normalizedDefs }
  let defs ← normalized.definitions.mapM compileDefinition
  pure <| goPrelude ++ goRuntime
    ++ "\n// ===== Definitions =====\n\n" ++ "\n".intercalate defs
    ++ "\n// ===== Entry point =====\n\n"
    ++ "func main() {\n\tinitRuntime()\n\tsetArgv(os.Args)\n"
    ++ entryCall moduleName normalized
    ++ "\treportStats()\n}\n"

/-! ## Unit checks -/

section Test
private def r0 : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
private def extId (s : String) : Malgo.Id :=
  { name := s, moduleName := .moduleName "T", sort := .external }

#guard mangleId (extId "main") == "m_T_u46_main"
-- Distinct sorts cannot collide: `Id.toText` separates them with `#`/`$`.
#guard mangleId { name := "x", moduleName := .moduleName "T", sort := .internal 3 }
  == "m_T_u46__u35_x_u95_3"
#guard mangleId { name := "x", moduleName := .moduleName "T", sort := .temporal 3 }
  == "m_T_u46__u36_x_u95_3"
#guard escapeGoString "a\"b\\c" == "a\\\"b\\\\c"
#guard escapeGoString "β" == "\\u03b2"
#guard compileLiteral (.int32 (-5)) == "mkInt32(-5)"
#guard compileTag .tuple == "tupleTag"
#guard compileTag (.tag "Cons") == "\"Cons\""
end Test

end Malgo.Backend.Go
