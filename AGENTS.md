# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Malgo?

Malgo is a statically typed functional programming language with an interpreter and Zig, Go and Chez Scheme backends, written in Lean 4. Source files use the `.mlg` extension.

## Build, Test, and Development Commands

```bash
mise run setup            # Install elan (toolchain pinned by lean/lean-toolchain)
mise run build            # lake build
mise run test             # Run the test suite
mise run test -- --match Parser   # Run cases matching "Parser"
mise run test -- --update # Regenerate golden outputs in place
mise run exec -- eval examples/malgo/Hello.mlg
```

The compiler lands at `lean/.lake/build/bin/malgo`.

## Project Structure

- `lean/Malgo/` - the compiler (modules `Malgo.*`)
- `lean/Main.lean` - CLI entry point (`malgo eval ...`)
- `lean/Test/Main.lean` - the whole test suite: golden cases plus the
  non-golden gates (infer, zig-reuse, zig-corpus, ir-invariants,
  reuse-specialize, parser-surface, panic-gate, primitive-coverage)
- `runtime/malgo/` - Malgo runtime/stdlib (`Builtin.mlg`, `Prelude.mlg`)
- `runtime/zig/runtime.zig` - the Zig backend's runtime
- `runtime/go/runtime.go` - the Go backend's runtime
- `examples/malgo/` - Sample `.mlg` programs
- `test/testcases/` - Test input files; `.golden/` - golden test outputs

## Compilation Pipeline Architecture

The pipeline is orchestrated in `lean/Malgo/Driver.lean`:

```
Source (.mlg)
    ↓
ParserPass → RenamePass → [InferPass] → [RefinePass]
    ↓
ToFunPass → ToCorePass → FlatPass → JoinPass
    ↓
EvalPass (Interpreter) | SchemePass (--target scheme)
                       | ZigPass   (--target zig / malgo compile [--target zig])
                       | GoPass    (--target go  / malgo compile --target go)
```

**Note**: InferPass and RefinePass can be skipped for fast evaluation without type checking.

`ToCorePass` runs `Malgo.Sequent.SaturateCtor.saturateProgram` first thing, before CPS
conversion: it inlines a fully(-or-over-)saturated call of a data constructor
(`Cons x xs`, or `Cons (f x) (mapList f xs)` — arguments need not be
immediate) directly into `Fun.Construct`, instead of invoking the
constructor's own curried closure. This is shared by every backend
(Eval/Scheme/Zig/Go) and every direct caller of `toCore`, not Zig-specific.

### Chez Scheme Backend

`malgo eval --target scheme SOURCE` lowers Join IR directly to Chez Scheme
source text (`lean/Malgo/Backend/Scheme.lean`, `Driver.compileScheme`) — no
closure-conversion/RC pass in between, since Scheme has native closures and
GC. [nix-config](https://github.com/takoeight0821/nix-config) is a standing
consumer: it compiles `.mlg` task scripts to `.scm` and runs them with
`chez-scheme` (`docs/plans/2026-08-11-chez-scheme-backend-and-nix-config-scripting.md`
has the rationale). Correctness gate: `bash scripts/scheme-golden.sh`,
mirroring `zig-golden.sh`'s structure.

Two invariants in `lean/Malgo/Backend/Scheme.lean`:

- `schemeRuntime`'s `malgo-print-value` must match `Eval.lean`'s
  `valueToText`: `Name(arg1, arg2)` for tagged constructors, `{arg1, arg2}`
  for tuples. Tuples and constructors share the `(list 'tag arg...)`
  representation, so the printer special-cases the reserved tag `"tuple"`
  from `compileTag`.
- `compileStatement`'s `.cut` case compiles `cut (mu a. c) b` by direct
  substitution, `(let ((a b)) c)`, not the generic `(b producer)`: a
  `mu`-bound producer (what `label`/`goto` desugar to) is a closure awaiting
  its consumer. Every other `Producer` variant is a first-order value, for
  which the generic case is correct.

### Zig Backend (native executables)

`malgo compile SOURCE [-o OUT] [--opt debug|release-safe|release-fast]` compiles
via Zig to a native executable (Zig 0.16 pinned in `mise.toml`). Pipeline inside
`ZigPass` (`lean/Malgo/Backend/Zig/`):

```
Join IR (already saturated — see SaturateCtor above) → Normalize (Mu/Label elimination)
        → ClosureConv.convertProgram (ANF Ir, closure conversion)
        → Peephole (scrutinee-tuple elimination)
        → Perceus (dup/drop insertion) → Reuse (Drop/MkStruct → reuse-token pairing)
        → RcCheck (linearity + reuse-token assert)
        → Emit (Zig text, runtime embedded via include_str from runtime/zig/runtime.zig)
```

- Memory: Perceus reference counting. Every produced binary leak-checks itself
  at exit (`MALGO-LEAK` on stderr + exit 83 on failure).
- Calling convention is self-passing: `fn(self, args)`; the callee dups its
  captures then drops `self`.
- Allocation-reduction passes (M10/M11): `Peephole` removes the scrutinee
  tuple a multi-parameter clause match otherwise allocates. `Reuse` pairs a
  Perceus `Drop` with a later `MkStruct` in the same block into
  `DropReuse`/`MkStructReuse`, letting the runtime (`rt.dropReuse`/
  `rt.mkStructReuse`) recycle a uniquely-referenced Object in place (Koka-style
  FBIP, generalized to any same-arity payload, not just literal cell reuse).
  Set `MALGO_RC_STATS=1` when running a compiled binary to print
  `MALGO-STATS: total_allocs=<N> reuse_hits=<N> dispatches=<N> force_depth_max=<N>`
  to stderr.
- Perf baseline (#399): `mise run perf-baseline` compares those counters against
  `bench/perf-baseline.json` over the counter tiers `fib-shallow`, `fib-deep`,
  `selfhost-l1` and `selfhost-l2`, plus the `l2-ratio` tier. `-- --tier=all --update`
  reseeds the baseline; `all` includes `l2-ratio`, so it needs Chez (`scheme`)
  on PATH. A performance claim in a PR carries that reseeded JSON diff as its
  before/after evidence. The counters are deterministic and machine-independent.
  Counter gates are a **ratchet**: `total_allocs` and `dispatches` may not rise,
  `force_depth_max` may not change at all (#382 rests on it being 1), and
  `reuse_hits` is reported rather than gated — it falls whenever an optimization
  removes allocations, so it is not a standalone signal. `l2-ratio` is the
  Zig/Chez wall-clock ratio on Level 2 and runs locally only, never in CI: it
  fails when the ratio grows more than 15% over a baseline recorded on the same
  OS and architecture, and skips the comparison on any other machine. Wall
  clock from `--timing` is informational and never gated. `fib-deep` and
  `selfhost-l1` are gated inside `zig-deep-recursion.sh` and `selfhost-golden.sh`,
  which already run those binaries, so CI pays ~1s rather than a new job.
- Small `int32`s (`-128..1024`) are interned as `IMMORTAL` statics by `rt.mkInt32`,
  so they cost no allocation and no RC traffic; and RC tracing is compiled out of
  `release-fast` entirely. `docs/perceus-gc.md` describes both.
- Calling convention is guaranteed tail calls: every call a generated function
  makes is `@call(.always_tail, ...)`, which Zig compiles to a jump or rejects
  at compile time, so the native stack stays flat. This requires a shared
  prototype for every handler (non-matching helpers are `inline fn`) and
  arguments in by-value parameters rather than a slice into the caller's
  frame. See `docs/zig-backend.md` for the design and measurements.
- Golden parity harness: `bash scripts/zig-golden.sh` (CI job `lean-zig-golden`)
  compiles every golden testcase and diffs stdout byte-for-byte against the
  interpreter's goldens, failing on any leak.
- Deep-recursion gate: `bash scripts/zig-deep-recursion.sh` (same CI job)
  compiles `bench/fixtures/BenchFibDeep.mlg` release-fast and runs it — 18.8M
  dispatches, which pre-#360 would have needed ~1.85 GB of native stack. Every
  golden-sweep case is shallow, so this is still the only thing that catches an
  emitter regression to plain calls — Zig rejects an `.always_tail` it cannot
  honor, but never demands that a call be one. Kept out of the sweep because its cases run
  `--opt debug`, where DebugAllocator makes a case this long ~13s.
- Runtime unit tests: `mise run zig-runtime-test` (`-lc` is required on
  Linux since the runtime calls `std.c.write`/`std.c.getenv` directly; macOS
  masks this because it always links libc via libSystem).
- **After editing `runtime/zig/runtime.zig`, run `mise run bust-runtime`
  before rebuilding.** Lake does not reliably track the `include_str` that
  embeds it, so `lake build` can report success while the binary keeps
  emitting the previous runtime text. CI does this unconditionally in
  `lean-zig-golden` and `lean-selfhost`.
- The interpreter (`Malgo.Sequent.Eval`) is the semantic oracle: any observable
  divergence in the Zig backend is a bug, matched against `Eval.lean`.

### Go Backend (native executables)

`malgo compile --target go SOURCE [-o OUT] [--opt ...]` compiles via Go to a
native executable (Go pinned in `mise.toml`). There is no
intermediate IR and no closure conversion: `Malgo.Backend.Go.compileToGo`
lowers Join IR straight to Go text, which is the whole backend.

```
Join IR → Normalize (Mu/Label elimination) → classifyJoins → Go text → go build
```

Go has real closures and a GC, so everything the Zig backend needs in order
to survive without them is absent here: no ANF, no lambda lifting, no
captures array, no self-passing convention, no Perceus/Reuse/RcCheck, no leak
check. `MAX_ARGS = 2` and the `dispatches` counter are shared with the Zig
runtime. The trampoline is Go's alone — Go has no `musttail` and no way to
pick a calling convention, so this CPS IR's tail calls stay real calls.

- Values: every concrete type in a `Value` is one machine word (`*Int32`,
  `*Str`, `Fn`, …) so putting one into the interface never allocates; a bare
  `string` would be two words and allocate on every conversion. Small
  `int32`s (`-128..1024`) are interned, same range and reason as Zig's.
- `Malgo.Sequent.Core.Escape.classifyJoins` (shared with the Zig backend)
  splits join points into `Local` and `Escaping`. A `Local` join emits no
  closure at all: its consumer is recorded and inlined at its single use
  site, which removes both the allocation and a trampoline bounce. This is
  what brings Go's dispatch count to parity with Zig's.
- `Str` caches its codepoint length and an all-ASCII flag — two scalars, so
  no extra allocation. Caching the decoded `[]rune` instead was measured at
  twice the runtime.
- A generated function takes two positional `Value` parameters, not an
  `args []Value` slice: no slice header per dispatch and no bounds check per
  argument. `Action` carries the same two slots and no argument count, since
  each function knows its own arity.
- **Record fields are an ascending `[]NamedField` slice, never a map.** Go
  randomizes map iteration order, so a map would make output nondeterministic.
- `forceField` (a nested `run`) is the only place native stack grows with
  nesting. It is reached from `Pattern.expand` alone; `Consumer.project` is a
  terminator and needs none. Measured `force_depth_max`: 0 without records,
  1 with them.
- Toolchain: `GOTOOLCHAIN=local` plus an old `go` directive in the generated
  `go.mod`, so a build never downloads a toolchain; the program imports only
  the standard library, so nothing is fetched. Both matter because the
  development sandbox has no egress. The generated source is left at `OUT.go`
  for inspection, as the Zig backend leaves `OUT.zig`.
- Gates: `bash scripts/go-golden.sh` (every interpreter golden case, plus a panic gate) and
  `bash scripts/go-deep-recursion.sh`. The latter's failure signature differs
  from the Zig gate's — Go prints `fatal error: goroutine stack exceeds ...`
  and exits 2 where Zig gets SIGSEGV.
- **After editing `runtime/go/runtime.go`, run `mise run bust-runtime`** —
  same `include_str` staleness as the Zig runtime.
- Primitive coverage is checked mechanically: the `primitive-coverage` gate
  greps the embedded runtime for `func <name>(`, so a missing primitive fails
  the test suite rather than only a golden diff. This works because the Go
  runtime names each function after the `foreign import` it serves.

When comparing backends' wall clock, run from the repo root with a *relative*
source path: path length changes the self-hosted evaluator's work by up to 3x.
Go's dispatch count is at parity with Zig's; the remaining gap is per-dispatch
cost. Chez recompiles the script on every run, so it loses end to end on short
programs but executes long pure computation fastest.
`wiki/2026-09-12-go-backend-performance-investigation.md` has the measurements
and what was tried and rejected.

### Intermediate Representations

All under `lean/Malgo/`.

| IR | Module | Purpose |
|----|--------|---------|
| Fun IR | `Sequent/Fun.lean` | Functional, close to AST |
| Core IR | `Sequent/Core/Full.lean` | Sequent calculus, explicit control |
| Flat IR | `Sequent/Core/Flat.lean` | No nested computations |
| Join IR | `Sequent/Core/Join.lean` | Normalized, explicit join points (final) |

### Key Modules

- `Malgo.Driver` - Pipeline orchestration
- `Malgo.Syntax` - Phase-indexed AST
- `Malgo.Pass` - Compiler pass abstraction
- `Malgo.Parser.*` - Parsing (CStyle grammar; the historical Regular-syntax parser was removed)
- `Malgo.Rename.*` - Name resolution and desugaring
- `Malgo.Sequent.Eval` - Interpreter for Join IR
- `Malgo.Monad` - `MalgoM`, the compiler's monad (`ReaderT Ctx (EIO CompileError)`)
- `Malgo.Features` - Feature flag system

## Self-Hosting Levels

Malgo has two self-hosting levels:

| Level | Description | Script | CI job(s) |
|-------|-------------|--------|--------|
| Level 1 | The Malgo evaluator written in Malgo (`runtime/malgo/compiler/`) evaluates arbitrary Malgo programs | `scripts/selfhost-golden.sh` | `lean-selfhost` |
| Level 2 | Level 1 evaluator evaluates `Main.mlg` which evaluates a Malgo program (metacircular interpreter) | `scripts/selfhost-level2.sh` | `l2-build` + `l2-case` (matrix) |

**Level 2 runs all five cases on master pushes and the nightly cron, and one case
(`Fib`) on a pull request that touches the Zig backend, `runtime/zig/`,
`runtime/malgo/`, the L2 harness, or `lean.yml`** — the inputs Level 2 has that
Level 1 does not. `LEAN_SELFHOST_L2=1` in `lean/ci-gates.env` is the kill switch;
the `l2` step of the `gates` job decides the rest and publishes it as
`l2Run`/`l2Cases`. To keep every CI job under 10 minutes, `l2-build` compiles
the evaluator once (~3 min) and uploads it as an artifact, and `l2-case` runs
one case per job from that artifact, each on its own runner.

`l2-build` sets `MALGO_ZIG_MCPU=baseline`, and it is the only thing that does.
The evaluator it uploads runs on a *different* runner, GitHub's x86_64 fleet is
mixed, and Zig otherwise compiles for the building host's CPU — an evaluator
built where AVX-512 exists dies with `SIGILL` where it does not. Nothing else in
the repo moves a compiled program between machines, so nothing else pays for a
portable binary.

Level 1 always runs on the **Zig backend**, and Level 2 does by default:
`Main.mlg` is compiled to a native binary with `malgo compile --opt release-fast`
and that binary is the evaluator. `selfhost-golden.sh` has no target switch.
`selfhost-level2.sh` also accepts `TARGET=scheme` (runs the evaluator under
Chez) as a manual cross-implementation reference; CI does not use it. The `l2-ratio`
perf tier builds its own Chez evaluator in `perf-baseline.sh` and does not call
this script.

```bash
# Level 1: ./malgoc <testcase.mlg>
bash scripts/selfhost-golden.sh

# Level 2: ./malgoc runtime/malgo/compiler/Main.mlg <testcase.mlg>
# In level 2, the inner Main.mlg's applyBuiltin "getRawArgs" drops the first
# arg (which is Main.mlg's own path) so that the inner sees only the test case
# argument. parseIntString32/64 are added to the inner evaluator's makeBaseEnv
# so that the inner Lexer can tokenize integer literals when evaluating
# nested Malgo sources.
bash scripts/selfhost-level2.sh

# Building the evaluator and running cases can be split, which is how CI keeps
# any one job under 10 minutes -- see the header of selfhost-level2.sh for
# BUILD_ONLY / EVAL_BIN / L2_CASES.
BUILD_ONLY=1 bash scripts/selfhost-level2.sh
EVAL_BIN=.malgo-work/malgoc L2_CASES=Fib bash scripts/selfhost-level2.sh
```

## Coding Style

- **Module naming**: `Malgo.Foo.Bar` → `lean/Malgo/Foo/Bar.lean`
- Prefer `def`/`abbrev` over `partial def`; a `partial def` is a place a
  termination argument was skipped, and #379 tracks the ones that block
  proofs.
- `#guard` for build-time assertions is the house style for pure functions;
  a gate in `lean/Test/Main.lean` for anything needing `IO`/`MalgoM`.

## Testing

- One executable: `lean/Test/Main.lean`, run by `mise run test`.
- Golden tests under `.golden/`, as
  `<Group>/<Case>/golden`. `mise run test -- --update` rewrites them;
  a mismatch also drops an `actual` next to the `golden`.
- Filter with `-- --match PATTERN` (matches `Group/Case`).

## Commits & PRs

- Conventional Commits format (see `.gitmessage`)
- Example: `feat(parser): support C-style apply`
- Quality gate: `mise run test`

## History

Malgo was written in Haskell until 2026-07, and that implementation was the
semantic oracle while the Lean 4 port was built against it. It has been
removed; `PORTING.md` records the module-by-module mapping and why the
retirement criteria were overridden.

These documents record the state at the time they were written, and some
describe the Haskell period: the dated files under `docs/plans/`,
`docs/reports/` and `wiki/`, the measurement notes under `bench/` (everything
except `perf-baseline.json` and `fixtures/`), and the milestone tables in
`PORTING.md` and `lean/README.md`. Leave existing ones as written — do not
"correct" them to the current layout. New plans go in `docs/plans/` as new
dated files (see the `design` skill).

## Agent skills

### Issue tracker

Issues live as GitHub Issues in this repo (`gh` CLI). See `docs/agents/issue-tracker.md`.

### Triage labels

Default five-role vocabulary (`needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, `wontfix`). See `docs/agents/triage-labels.md`.

### Domain docs

Single-context layout: `CONTEXT.md` + `docs/adr/` at the repo root. See `docs/agents/domain.md`.
