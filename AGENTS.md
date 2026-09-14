# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Malgo?

Malgo is a statically typed functional programming language with an interpreter and a native (Zig) backend, written in Lean 4. Source files use the `.mlg` extension.

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
(Eval/Scheme/Zig) and every direct caller of `toCore`, not Zig-specific.

### Chez Scheme Backend

`malgo eval --target scheme SOURCE` lowers Join IR directly to Chez Scheme
source text (`lean/Malgo/Backend/Scheme.lean`, `Driver.compileScheme`) — no
closure-conversion/RC pass in between, since Scheme has native closures and
GC. This backend has been added and removed twice before (see `lean/README.md`'s
M4 entry and the git history around #386/#401/#404) as a disposable
performance-comparison tool; it is being kept this time because
[nix-config](https://github.com/takoeight0821/nix-config) is a standing
consumer, compiling `.mlg` task scripts to `.scm` and running them with
`chez-scheme` — faster and more stable than the Zig backend for this kind of
workload (`docs/plans/2026-08-11-chez-scheme-backend-and-nix-config-scripting.md`
has the full rationale). Unlike its two prior lives, it now has a real
correctness gate: `bash scripts/scheme-golden.sh` (73/73, mirroring
`zig-golden.sh`'s structure).

Two bugs found while adding that gate, both in `schemeRuntime`'s
`malgo-print-value` and `compileStatement`'s `.cut` case (present since the
Haskell-era original, not introduced by this restoration):

- **Constructor/tuple printing used S-expression syntax** (`(Name arg1 arg2)`)
  instead of matching `Eval.lean`'s `valueToText` (`Name(arg1, arg2)` for
  tagged constructors, `{arg1, arg2}` for tuples — tuples and constructors
  share the same `(list 'tag arg...)` Scheme representation, keyed off the
  reserved tag string `"tuple"` from `compileTag`, so the printer must special-
  case it).
- **`cut (mu a. c) b` compiled backwards.** A `mu`-bound producer (what
  `label`/`goto` desugar to) compiles to a Scheme closure awaiting its
  consumer as an argument, but the generic `.cut` case handed that closure
  *to* the consumer as a value instead of applying it *with* the consumer —
  the classic mu-reduction (`c[a := b]`) needs direct substitution
  (`(let ((a b)) c)`), not the generic case's `(b producer)`. This is why
  `label`/`goto` (and *only* that construct) broke: every other `Producer`
  variant compiles to a plain first-order value, for which the generic case
  is correct.

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
  `bench/perf-baseline.json` over four tiers (`fib-shallow`, `fib-deep`,
  `selfhost-l1`, `selfhost-l2`); `-- --tier=all --update` reseeds it, and that diff
  is the before/after claim #385 requires. The counters are deterministic and
  machine-independent; wall clock is recorded only via `--timing` and never gated.
  Gates are a **ratchet**: `total_allocs` and `dispatches` may not rise,
  `force_depth_max` may not change at all (#382 rests on it being 1), and
  `reuse_hits` is reported rather than gated — it falls whenever an optimization
  removes allocations, so it is not a standalone signal. `fib-deep` and
  `selfhost-l1` are gated inside `zig-deep-recursion.sh` and `selfhost-golden.sh`,
  which already run those binaries, so CI pays ~1s rather than a new job.
- Small `int32`s (`-128..1024`) are interned as `IMMORTAL` statics by `rt.mkInt32`,
  so they cost no allocation and no RC traffic; and RC tracing is compiled out of
  `release-fast` entirely. Both are #385 work — see `docs/perceus-gc.md`.
- Calling convention is guaranteed tail calls: every call a generated function
  makes is `@call(.always_tail, ...)`, which Zig compiles to a jump or rejects
  at compile time, so the native stack stays flat. Emitting them as plain
  `return f(..)` grew the stack by one frame per reduction step and SIGSEGV'd
  past ~150k steps (#360); a trampoline (`rt.Action` + `rt.run`) held the line
  until the two constraints on `.always_tail` were addressed — a shared
  prototype for every handler, with the non-matching helpers as `inline fn`,
  and arguments in by-value parameters rather than a slice into the caller's
  frame. Worth 1.33x on `BenchFibDeep`, 1.22x on Level 1 and 1.25x on Level 2 (271.5s
  -> 217.7s), with every counter unchanged. The IR and the RC passes are unaffected: a tail call moves
  exactly the references an Action did. See `docs/zig-backend.md`.
- Golden parity harness: `bash scripts/zig-golden.sh` (CI job `zig-golden`)
  compiles every golden testcase and diffs stdout byte-for-byte against the
  interpreter's goldens, failing on any leak.
- Deep-recursion gate: `bash scripts/zig-deep-recursion.sh` (same CI jobs)
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
native executable (Go 0.26 era, pinned in `mise.toml`). There is no
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
- Gates: `bash scripts/go-golden.sh` (86/86 plus a 3/3 panic gate) and
  `bash scripts/go-deep-recursion.sh`. The latter's failure signature differs
  from the Zig gate's — Go prints `fatal error: goroutine stack exceeds ...`
  and exits 2 where Zig gets SIGSEGV.
- **After editing `runtime/go/runtime.go`, run `mise run bust-runtime`** —
  same `include_str` staleness as the Zig runtime.
- Primitive coverage is checked mechanically: the `primitive-coverage` gate
  greps the embedded runtime for `func <name>(`, so a missing primitive fails
  the test suite rather than only a golden diff. This works because the Go
  runtime names each function after the `foreign import` it serves.

Measured 2026-09-13 on Darwin arm64, `--opt release-fast`, `hyperfine` over 20
runs, from the repo root with a *relative* source path — path length changes
the self-hosted evaluator's work by up to 3x, so measurements are only
comparable at equal path length.

| | selfhost Level 1 | `BenchFibDeep` | Level 1 `dispatches` |
|---|---|---|---|
| Zig | 0.21s | 0.24s | 9,028,449 |
| Go | 0.27s | 0.30s | 9,028,448 |
| Chez | 0.68s | 0.16s | — |

Dispatch counts are at parity with Zig. The remaining 1.3x on wall clock is
per-dispatch cost, which the paragraph below identifies as Go's floor.

Chez's column needs splitting to be read correctly: it compiles the script on
every run, which is 0.12s for `BenchFibDeep` and 0.58s for the Level 1
evaluator. Its *execution* is therefore 0.04s and 0.10s — 2.6x to 7.8x faster
than Go's. That gap is structural and cannot be closed: Go's trampoline alone
costs 0.068s on `BenchFibDeep`, more than Chez spends running the whole
program. Only the number of dispatches can fall, and it is already at Zig's.

End to end, which is what a script run pays, Go wins everywhere except long
pure computation: 2.5x on Level 1 and ~15x on the short programs in
`examples/malgo/` (0.01s against Chez's 0.16s of startup).
`wiki/2026-09-12-go-backend-performance-investigation.md` records what else
was tried and measured (interface boxing, `[]rune` caching, generics,
reflection, reshaping the trampoline — all rejected on measurement).

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
`l2Run`/`l2Cases`. It was off entirely for a while (#385) because a single job
running all five cases took ~16-27 minutes against a target of keeping any one CI
job under 10. #385 closed by splitting it: `l2-build` compiles the evaluator once
(~3 min) and uploads it as an artifact; `l2-case` runs one case per job from that
artifact (no contention between cases since each gets its own runner).

`l2-build` sets `MALGO_ZIG_MCPU=baseline`, and it is the only thing that does.
The evaluator it uploads runs on a *different* runner, GitHub's x86_64 fleet is
mixed, and Zig otherwise compiles for the building host's CPU — an evaluator
built where AVX-512 exists dies with `SIGILL` where it does not. Nothing else in
the repo moves a compiled program between machines, so nothing else pays for a
portable binary.

Both levels run on the **Zig backend**: `Main.mlg` is compiled to a native binary with
`malgo compile --opt release-fast` and that binary is the evaluator. A Scheme backend
existed briefly as a Chez-based cross-implementation performance reference for #385
(so the "how much faster is Zig" claim had a control to measure against) and was
removed again once #385 closed (#400) — see the git history around #385/#400/#404
if that measurement ever needs to be redone from scratch.

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
- Golden tests under `.golden/`, in hspec-golden's directory layout
  (`<Group>/<Case>/golden`). `mise run test -- --update` rewrites them;
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
retirement criteria were overridden. Documents under `docs/plans/`,
`docs/reports/`, `bench/` and `wiki/` describe that period and are left as
written — do not "correct" them to the current layout.

## Agent skills

### Issue tracker

Issues live as GitHub Issues in this repo (`gh` CLI). See `docs/agents/issue-tracker.md`.

### Triage labels

Default five-role vocabulary (`needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, `wontfix`). See `docs/agents/triage-labels.md`.

### Domain docs

Single-context layout: `CONTEXT.md` + `docs/adr/` at the repo root. See `docs/agents/domain.md`.
