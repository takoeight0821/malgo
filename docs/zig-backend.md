# The Zig Backend

`malgo compile SOURCE [-o OUT] [--opt debug|release-safe|release-fast]` compiles a
`.mlg` module to a native executable by generating Zig source text and invoking the
`zig` toolchain (Zig 0.16, pinned in `mise.toml`). This document covers the backend's
pipeline, generated-code conventions, and the tools available for inspecting and
debugging it. For the reference-counting memory model specifically, see
[`perceus-gc.md`](perceus-gc.md).

## Where it sits in the compiler

Both backends (interpreter, Zig) share the same front end and the same Join
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

`ZigPass` (`Malgo.Backend.Zig`, `lean/Malgo/Backend/Zig/`) then takes Join IR and
produces Zig source text:

```
Join IR
  -> Normalize (Mu/Label elimination)
  -> ClosureConv.convertProgram (closure conversion, produces the backend's own ANF IR)
  -> Peephole (scrutinee-tuple elimination)
  -> Perceus (dup/drop insertion)
  -> Reuse (Drop/MkStruct -> reuse-token pairing)
  -> RcCheck (linearity + reuse-token static verification)
  -> Emit (Zig text; runtime embedded via include_str from runtime/zig/runtime.zig)
```

Each stage is a pure `Ir.Program -> Ir.Program` (or `-> String`) function; `Malgo.Backend.Zig`'s
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

Every generated Zig function shares one signature, `rt.CodeFn` —
`fn (self: rt.Value, a0: rt.Value, a1: rt.Value) rt.Value`:

- A closure or record field or codata branch receives the closure/record/codata
  object itself as `self` and reads its captures out of it
  (`rt.capturesOf(self)` under the hood, `ReadCapture` in the IR).
- A top-level definition is called directly with `rt.no_self` (an immortal sentinel)
  and ignores it.

Self-passing is what makes Perceus's "a call consumes one reference of the callee"
rule implementable: the callee dups the captures it still needs, then drops `self`
itself — the caller has no post-call point to do either, since every call in this IR
is a tail call.

### Guaranteed tail calls

Every call a generated function makes is `@call(.always_tail, ...)`, which Zig
either compiles to a jump or rejects at compile time. The native stack stays
flat however many reduction steps a program takes, and a `Finish` is a plain
`return`.

This replaced a trampoline — a generated function returned an `rt.Action`
naming the next call, and `rt.run` dispatched in a loop. That existed because
emitting these calls as plain `return f(...)` meant nothing ever returned
until the program exited: the stack grew by one frame (~98.6 bytes, measured)
per reduction step, and any program of more than ~150k steps died with SIGSEGV
— `fib 16` was enough ([issue #360](https://github.com/takoeight0821/malgo/issues/360)).

Two constraints made `@call(.always_tail)` look unusable at the time, and both
are addressed rather than worked around. The approach is Deegen's, from
[luajit-remake](https://github.com/luajit-remake/luajit-remake) — it generates
interpreters whose bytecode handlers dispatch by `[[clang::musttail]]`, and
solves the same two constraints by unifying every handler's prototype and
keeping arguments in registers:

- **Caller and callee must share a prototype.** Every handler now has exactly
  `fn (self: rt.Value, a0: rt.Value, a1: rt.Value) rt.Value`, and the helpers
  that do not (`applyCovalue`, `callClosure`, `staticCall`, `projectField`,
  `applyDestructor`) are `inline fn`, so their tail call lands in the caller's
  frame where the prototype does match. A non-inline helper is a compile
  error, which is what keeps the convention honest — the runtime's own
  `destructorProbe` exists because a `test` block's prototype does not match
  either.
- **Arguments must not outlive the frame.** They are two by-value parameters
  rather than a `[]const rt.Value` slice pointing into an `Action`. A callee
  knows its own arity, so an unused slot is the immortal `rt.no_self`
  sentinel — never `undefined`, so a stray `dup`/`drop` on it is a no-op.

Both the toolchain and the runtime's unit tests pass `-fllvm`. Zig's
self-hosted x86_64 backend cannot emit a tail call at all, and it is the
default for Debug on x86_64 — so without the flag this backend builds fine on
aarch64-macos and in every release mode, and fails on exactly one
configuration. Release modes already use LLVM, so the cost falls only on
Debug: 2.5s → 8.7s on the 20MB self-hosted evaluator, proportionally less on a
golden-sized case.

`MAX_ARGS` is still 2, for the reason #407 established: the front end cannot
produce more (`ToFun` builds single-parameter lambdas and singleton applies;
`ToCore` appends exactly one consumer), verified across 220k+ generated call
sites.

The RC passes are unaffected. A tail call is a **move, not a borrow**, exactly
as the Action it replaced was: it transfers one reference of the callee into
`self` and one of each operand into `a0`/`a1`. `Perceus` and `RcCheck` model a
single frame, and "these references leave this frame here" is still true.

`forceField` is the one place native stack still grows, and by one frame per
level of dynamic `Force` nesting rather than per reduction step: `Ir.Force` is
a mid-block expression, so it makes an ordinary call and everything the
field's code goes on to do is a tail call that stays flat.

Measured on Darwin arm64, `--opt release-fast`, 20 runs under hyperfine:

| | trampoline | tail calls | |
|---|---|---|---|
| `BenchFibDeep` | 318.0 ms ± 6.0 | **239.2 ms ± 4.1** | **1.33x** |
| selfhost Level 1 | 257.0 ms ± 44.3 | **210.3 ms ± 1.9** | **1.22x** |
| selfhost Level 2 | 271.5 s | **217.7 s** | **1.25x** |

Level 2 is one serial run each rather than a hyperfine series — it is the
16 minutes #385 exists to keep out of CI. Its 1.62e10 dispatches lose 53.8s,
or 3.3ns each, which is the microbenchmark's 4ns diluted by the work between
dispatches. Chez ran the same case in 52.8s in the same session, so #385's
`l2_ratio` moves from **5.14x to 4.12x**.

Those three seconds figures live here rather than in `bench/perf-baseline.json`:
`scripts/perf-baseline.sh`'s `record_ratio` replaces `.l2_ratio` wholesale and
times with whole-second `$SECONDS`, so it can neither keep an extra field nor
reproduce a decimal. The JSON holds what that script can write; this table holds
the measurement.

Reverting is a supported move if a target ever needs it. The trampoline is the
parent of the commit that introduced this section, and it is what made the
backend buildable without LLVM — `@call(.always_tail)` needs the LLVM backend
(see `-fllvm` below), so a target LLVM does not serve means going back to it.

Every counter is unchanged — `dispatches` 18,815,851 and 9,028,449
respectively, `total_allocs` and `reuse_hits` identical — so the two
conventions perform the same reductions and the same allocations. `run`
counted each loop iteration; `rt.countDispatch()` in each function's prologue
counts the same events, `identityCode` included.

Not at the same cost, though. The counter was structurally free under the
trampoline — the loop existed anyway — and is now a deliberate store in the
prologue of every generated function. Measured at `fib 32` (546M dispatches,
~6.9s, paired interleaved runs, which is the window needed to resolve it):
**6.961s with, 6.873s without — 1.26%, and 112 KB of the evaluator's `__text`,
2.7%.** Per-dispatch wall time is 12.7ns here against 13.4ns at Level 2, so
that is ≈2.6s of L2's 217.7s.

It stays on. The counter is what both ratchets read (`zig-deep-recursion.sh`
and `perf-baseline.sh`), from a `--opt release-fast` binary, and always-on is
what makes the instrumented binary the same one that was timed. Gating it on
`builtin.mode` the way `rc_trace_supported` is gated would be worse than the
1.26%: both ratchets would then read `dispatches=0` from a release-fast build
and — before the zero-floor added alongside this — take the "improved" branch
and exit 0, measuring nothing while reporting success.

## Data representation

Every value at runtime is a `*Object` (`runtime/zig/runtime.zig`): a reference count,
a `Kind` tag, and a `Payload` union covering unboxed scalars (`int32`/`int64`/`float`/
`double`/`char`/`unit`) and heap-shaped payloads (`string`, `strukt` — tagged tuples
and data constructors, `closure`, `record`, `codata`). A `Tag` is either an anonymous
tuple marker or a `[]const u8` pointing at the generated code's own `.rodata` (a
constructor name never needs a heap allocation of its own).

## Building and testing

- `mise run build` runs `lake build`, covering the compiler itself.
- `mise run zig-runtime-test` runs the runtime's own unit tests
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
- Unit gates for the individual passes live in `lean/Test/Main.lean`:
  `ZigReuse` (the `Reuse` pass on hand-built IR), `ZigCorpus` (every testcase's
  emitted Zig checked for linearity), `IrInvariants`, and `ReuseSpec`
  (`ReuseSpecialize`'s hint insertion). MET's renderer is gated by its own
  golden cases in the same file.

## Debugging tools

- **MET** (`malgo debug-trace path/to/file.mlg`): renders a `.mlg` file's trip
  through every stage above (and the front-end stages before it) into one
  self-contained HTML page, for side-by-side or unified diffing. See
  [`met-tool.md`](met-tool.md).
- **`scripts/rctrace.py`**: correlates a `MALGO_RC_TRACE=1` run's JSON-lines trace
  log with the compile-time symbolic names the trace carries, to answer "who still
  holds a reference to this object right now" without manually grepping raw
  pointer addresses. See the "Debugging" section of [`perceus-gc.md`](perceus-gc.md).
- **`MALGO_RC_STATS=1`** on a compiled binary prints
  `MALGO-STATS: total_allocs=<N> reuse_hits=<N> dispatches=<N> force_depth_max=<N>`
  to stderr at exit — a quick way to measure the `Reuse` pass's effect on allocation
  count without full tracing. The counters themselves are always on (only the
  reporting is env-gated), so an instrumented run and a timed run measure the same
  binary. They are deterministic and machine-independent, which is why #385 requires
  them in any before/after claim and treats wall clock as insufficient.
- **`mise run perf-baseline`** (`scripts/perf-baseline.sh`) records and compares those
  counters against `bench/perf-baseline.json` across four tiers — `fib-shallow`,
  `fib-deep`, `selfhost-l1`, `selfhost-l2`. Gates are directional and only
  `total_allocs`, `dispatches` and `force_depth_max` are enforced; `reuse_hits` is
  reported, because it is a ratio whose denominator moves whenever an optimization
  removes allocations. `--update` rewrites the baseline, and that diff is the
  before/after claim. `fib-deep` is gated for free inside
  `scripts/zig-deep-recursion.sh`, which already ran the instrumented binary and
  previously discarded the numbers.
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
