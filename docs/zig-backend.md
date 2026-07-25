# The Zig Backend

`malgo compile SOURCE [-o OUT] [--opt debug|release-safe|release-fast]` compiles a
`.mlg` module to a native executable by generating Zig source text and invoking the
`zig` toolchain (Zig 0.16, pinned in `mise.toml`). This document covers the backend's
pipeline, generated-code conventions, and the tools available for inspecting and
debugging it. For the reference-counting memory model specifically, see
[`perceus-gc.md`](perceus-gc.md).

## Where it sits in the compiler

Every backend (interpreter, Scheme, Zig) shares the same front end and the same Join
IR. `Malgo.Driver.compileFromAST`'s `TargetZig` branch, and `compileToExecutable`
(used by `malgo compile`), both run:

```
Source (.mlg)
  -> ParserPass -> RenamePass -> [InferPass] -> [RefinePass]
  -> ToFunPass -> ToCorePass -> FlatPass -> JoinPass
  -> ZigPass
```

`ToCorePass` runs `Malgo.Sequent.SaturateCtor.saturateProgram` and
`Malgo.Sequent.ReuseSpecialize.specializeProgram` before CPS conversion — both operate
on Fun IR and are shared by every backend, not Zig-specific:

- **SaturateCtor** inlines a fully- (or over-) saturated call of a data constructor
  (`Cons x xs`) directly into a `Construct` producer, instead of invoking the
  constructor's own curried closure. Arguments need not be immediate values.
- **ReuseSpecialize** inserts a `reuseHint scrutinee` primitive call immediately
  before a reconstruction that rebuilds a value of the same shape it just matched
  against (the classic "insert x into a list, rebuilding one cons cell" pattern) —
  the hint marks the scrutinee's last use for the Zig backend's `Reuse` pass (below)
  to recognize, without affecting any other backend's semantics.

`ZigPass` (`Malgo.Backend.Zig`, `src/Malgo/Backend/Zig/`) then takes Join IR and
produces Zig source text:

```
Join IR
  -> Normalize (Mu/Label elimination)
  -> ClosureConv.convertProgram (closure conversion, produces the backend's own ANF IR)
  -> Peephole (scrutinee-tuple elimination)
  -> Perceus (dup/drop insertion)
  -> Reuse (Drop/MkStruct -> reuse-token pairing)
  -> RcCheck (linearity + reuse-token static verification)
  -> Emit (Zig text; runtime embedded via file-embed from runtime/zig/runtime.zig)
```

Each stage is a pure `Ir.Program -> Ir.Program` (or `-> Text`) function; `Malgo.Backend.Zig`'s
`runPassImpl` just threads the program through them in order, wrapping any
`RcCheck` violation as a compile error rather than emitting broken code.

## The backend's own IR

`Malgo.Backend.Zig.ClosureConv.convertProgram` translates Join IR into a first-order,
ANF IR (`Malgo.Backend.Zig.Ir`) that closure conversion has already normalized:

- Every value is produced by a named `Let`; every operand position is a variable.
- Captures are explicit index reads (`ReadCapture`) against a function's own closure
  object — the self-passing calling convention below — rather than free variables.
- Every nested `Lambda`/escaping join/`Object` field has been lifted into its own
  top-level `Func`.

This is exactly the shape Perceus needs: with every operand a bare variable and every
binding named, reference counting reduces to counting occurrences (see
[`perceus-gc.md`](perceus-gc.md)).

`ClosureConv`'s escaping-join analysis (`classifyJoins`) decides, for each Join-IR
`Join`-bound consumer, whether it can be compiled as a same-function inline
substitution (`Local`) or must be reified as a heap-allocated closure (`Escaping`):
a name escapes if any use of it crosses into a separately-compiled unit — passed to
`Invoke`, an `Apply`'s return continuation, a `Destructor`'s or `Project`'s
continuation, or free in a nested `Lambda`/`Object` field/`Cocase` branch/`Mu` body
that gets lifted into its own function. `initialClassifyJoinsWithEscaping` computes
both the ownership map and each node's escaping-name set in one bottom-up traversal —
computing them as two separate top-down-recursive functions (as the direct-escaping
rule's most naive form would) re-scans a chain of nested `Join`s' shrinking
continuation once per enclosing `Join`, which is quadratic in the chain's length.
`Ir.suffixFreeVars` gives the analogous "each shrinking suffix's free variables in one
pass" primitive over the backend IR itself, used by `Perceus` and `Emit`.

## Calling convention

Every generated Zig function shares one signature,
`fn (self: rt.Value, args: []const rt.Value) rt.Action`:

- A closure or record field or codata branch receives the closure/record/codata
  object itself as `self` and reads its captures out of it
  (`rt.capturesOf(self)` under the hood, `ReadCapture` in the IR).
- A top-level definition is called directly with `rt.no_self` (an immortal sentinel)
  and ignores it.

Self-passing is what makes Perceus's "a call consumes one reference of the callee"
rule implementable: the callee dups the captures it still needs, then drops `self`
itself — the caller has no post-call point to do either, since every call in this IR
is a tail call.

### Trampoline

A generated function never *performs* a call. It returns an `rt.Action` — either
`{code, self, argv}` naming the call to make next, or `done(v)` carrying the finished
value — and `rt.run` dispatches in a loop until something is `done`.

This is not a stylistic choice. Zig does not guarantee tail-call optimization, so
emitting these tail calls as native `return f(...)` meant nothing ever returned until
the program exited: the stack grew by one frame (~98.6 bytes, measured) per reduction
step, and any program of more than ~150k steps died with SIGSEGV — `fib 16` was enough
([issue #360](https://github.com/takoeight0821/malgo/issues/360)). `@call(.always_tail)`
is not a substitute: Zig requires the callee's signature to match the caller's, which
rules out helpers like `applyCovalue(Value, Value)`, and a genuine tail call would
release the frame holding the `&[_]rt.Value{...}` argument slice before the callee read
it. An `Action` carries its arguments in a fixed inline array (`MAX_ARGS`, currently 4;
the front end tops out at 2) precisely so no argument outlives its storage.

**An Action is a move, not a borrow.** It carries exactly the references a direct call
would have transferred — one of the callee into `self`, one of each operand into
`argv` — and `rt.run` is strictly RC-neutral: no dup, no drop, and it never discards an
Action without dispatching it. That is why the IR and every RC pass
(`Perceus`, `RcCheck`, `Reuse`) are untouched by this: they model a single frame and
only assert that a terminator's operands leave it, which is still true when they leave
into an Action.

Native stack is now O(maximum dynamic `Force` nesting depth) rather than O(total
reduction steps). `Ir.Force` is an expression in the middle of a block, so `rt.forceField`
has to return a plain value and runs a *nested* trampoline to get one — three frames per
nesting level. In the current corpus record fields are only ever forced as siblings
(depth 1); `MALGO_RC_STATS=1` reports `force_depth_max` so this stays measured rather
than assumed.

## Data representation

Every value at runtime is a `*Object` (`runtime/zig/runtime.zig`): a reference count,
a `Kind` tag, and a `Payload` union covering unboxed scalars (`int32`/`int64`/`float`/
`double`/`char`/`unit`) and heap-shaped payloads (`string`, `strukt` — tagged tuples
and data constructors, `closure`, `record`, `codata`). A `Tag` is either an anonymous
tuple marker or a `[]const u8` pointing at the generated code's own `.rodata` (a
constructor name never needs a heap allocation of its own).

## Building and testing

- `mise run build` runs `hpack && cabal build`, covering the compiler itself.
- `zig test -lc runtime/zig/runtime.zig` runs the runtime's own unit tests
  (`-lc` links libc explicitly; required on Linux since the runtime calls
  `std.c.write`/`std.c.getenv` directly — masked on macOS, where libc is always
  linked via libSystem).
- `bash scripts/zig-golden.sh` (CI job `zig-golden`) compiles every golden testcase
  through `malgo compile` and diffs its stdout byte-for-byte against the
  interpreter's own golden output for the same program, failing the whole run on any
  mismatch or reported leak. `Malgo.Sequent.Eval` (the interpreter) is the semantic
  oracle for the compiler as a whole: any observable divergence between it and the
  Zig backend is a Zig-backend bug, not a spec ambiguity to resolve in the backend's
  favor.
- Hspec unit tests for the individual passes live under
  `test/Malgo/Backend/Zig/*Spec.hs` (`Malgo.Backend.Zig.PeepholeSpec`,
  `PerceusSpec`, `ReuseSpec`) plus `Malgo.Backend.ZigSpec` for the pass composition
  end-to-end and `Malgo.Debug.PrettyIRSpec` for the renderer used by MET (below).

## Debugging tools

- **MET** (`app/met`, `mise run met --option source=path/to/file.mlg`): a browser UI
  that traces a `.mlg` file through every stage above (and the front-end stages
  before it) and renders each one for side-by-side or unified diffing. See
  [`met-tool.md`](met-tool.md).
- **`scripts/rctrace.py`**: correlates a `MALGO_RC_TRACE=1` run's JSON-lines trace
  log with the compile-time symbolic names the trace carries, to answer "who still
  holds a reference to this object right now" without manually grepping raw
  pointer addresses. See the "Debugging" section of [`perceus-gc.md`](perceus-gc.md).
- **`MALGO_RC_STATS=1`** on a compiled binary prints
  `MALGO-STATS: total_allocs=<N> reuse_hits=<N>` to stderr at exit — a quick way to
  measure the `Reuse` pass's effect on allocation count without full tracing.
- **`RcCheck`** runs unconditionally on every compile (see
  [`perceus-gc.md`](perceus-gc.md)) — no flag is needed to catch a Perceus/Reuse bug
  as a compile error rather than a runtime use-after-free.
- **`Malgo.Debug.Pipeline.runTrace`** (MET's own machinery, `runTrace srcPath
  useInfer malgo2025 :: IO [Stage]`) can be driven directly from a REPL or a
  one-off script when a browser isn't convenient — it returns every stage's
  rendered text as a plain list, with no HTTP server involved.

## Known limitations

- The Malgo evaluator written in Malgo (`runtime/malgo/compiler/`, exercised by
  `scripts/selfhost-golden.sh`) targets the interpreter's semantics; it does not
  itself compile through the Zig backend.
- `Object` reuse (the `Reuse` pass) only ever recycles a single backing array per
  Object — a `record`'s separate fields array, a `codata`'s separate branches array,
  and a `string`'s byte buffer all fall back to an ordinary drop/allocate pair rather
  than being recycled in place. See [`perceus-gc.md`](perceus-gc.md) for why this is
  a deliberate scope limit, not a bug.
