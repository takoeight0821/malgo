# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Malgo?

Malgo is a statically typed functional programming language with an interpreter written in Haskell. Source files use the `.mlg` extension.

## Build, Test, and Development Commands

```bash
mise run setup       # Install toolchain (GHC 9.12.4, cabal, hpack, ormolu)
mise run build       # Format + build (hpack && cabal build)
mise run test        # Run test suite
mise run test -- --match=Parser  # Run tests matching "Parser"
mise run format      # Format with Ormolu
mise run exec        # Run executable (cabal exec malgo-exe)
mise run setup-hls   # Setup HLS for editor integration
mise run reset       # Reset golden test outputs
```

After `cabal install`, use: `malgo eval examples/malgo/Hello.mlg`

## Project Structure

- `src/Malgo/` - Haskell source (modules `Malgo.*`)
- `app/malgo/Main.hs` - CLI entry point (`malgo eval ...`)
- `runtime/malgo/` - Malgo runtime/stdlib (`Builtin.mlg`, `Prelude.mlg`)
- `examples/malgo/` - Sample `.mlg` programs
- `test/Malgo/` - Hspec tests mirroring source structure (`*Spec.hs`)
- `test/testcases/` - Test input files; `.golden/` - golden test outputs

## Compilation Pipeline Architecture

The pipeline is orchestrated in `src/Malgo/Driver.hs`:

```
Source (.mlg)
    ↓
ParserPass → RenamePass → [InferPass] → [RefinePass]
    ↓
ToFunPass → ToCorePass → FlatPass → JoinPass
    ↓
EvalPass (Interpreter) | SchemePass (--target scheme) | ZigPass (--target zig / malgo compile)
```

**Note**: InferPass and RefinePass can be skipped for fast evaluation without type checking.

`ToCorePass` runs `Malgo.Sequent.SaturateCtor.saturateProgram` first thing, before CPS
conversion: it inlines a fully(-or-over-)saturated call of a data constructor
(`Cons x xs`, or `Cons (f x) (mapList f xs)` — arguments need not be
immediate) directly into `Fun.Construct`, instead of invoking the
constructor's own curried closure. This is shared by every backend
(Eval/Scheme/Zig) and every direct caller of `toCore`, not Zig-specific.

### Zig Backend (native executables)

`malgo compile SOURCE [-o OUT] [--opt debug|release-safe|release-fast]` compiles
via Zig to a native executable (Zig 0.16 pinned in `mise.toml`). Pipeline inside
`ZigPass` (`src/Malgo/Backend/Zig/`):

```
Join IR (already saturated — see SaturateCtor above) → Normalize (Mu/Label elimination)
        → ClosureConv.convertProgram (ANF Ir, closure conversion)
        → Peephole (scrutinee-tuple elimination)
        → Perceus (dup/drop insertion) → Reuse (Drop/MkStruct → reuse-token pairing)
        → RcCheck (linearity + reuse-token assert)
        → Emit (Zig text, runtime embedded via file-embed from runtime/zig/runtime.zig)
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
- Calling convention is a trampoline: a generated function returns an
  `rt.Action` (the next call, or `done(v)`) and `rt.run` dispatches in a loop.
  Zig does not guarantee tail calls, so emitting this CPS IR's tail calls as
  native `return f(..)` grew the stack by one frame per reduction step and
  SIGSEGV'd past ~150k steps (#360). The IR and the RC passes are unaffected —
  an Action carries exactly the references a direct call moved.
- Golden parity harness: `bash scripts/zig-golden.sh` (CI job `zig-golden`)
  compiles every golden testcase and diffs stdout byte-for-byte against the
  interpreter's goldens, failing on any leak.
- Deep-recursion gate: `bash scripts/zig-deep-recursion.sh` (same CI jobs)
  compiles `bench/fixtures/BenchFibDeep.mlg` release-fast and runs it — 18.8M
  dispatches, which pre-#360 would have needed ~1.85 GB of native stack. Every
  golden-sweep case is shallow, so this is the only thing that catches a
  trampoline regression. Kept out of the sweep because its cases run
  `--opt debug`, where DebugAllocator makes a case this long ~13s.
- Runtime unit tests: `zig test -lc runtime/zig/runtime.zig` (`-lc` is required on
  Linux since the runtime calls `std.c.write`/`std.c.getenv` directly; macOS
  masks this because it always links libc via libSystem).
- **After editing `runtime/zig/runtime.zig`, run `mise run lean-bust-runtime`
  before rebuilding the Lean port.** Lake does not reliably track the
  `include_str` that embeds it, so `lake build` can report success while the
  binary keeps emitting the previous runtime text. Haskell's `embedStringFile`
  has no such problem. CI does this unconditionally in `lean-zig-golden`.
- The interpreter (`Malgo.Sequent.Eval`) is the semantic oracle: any observable
  divergence in the Zig backend is a bug, matched against Eval.hs, not Scheme.

### Intermediate Representations

| IR | Module | Purpose |
|----|--------|---------|
| Fun IR | `Sequent/Fun.hs` | Functional, close to AST |
| Core IR | `Sequent/Core/Full.hs` | Sequent calculus, explicit control |
| Flat IR | `Sequent/Core/Flat.hs` | No nested computations |
| Join IR | `Sequent/Core/Join.hs` | Normalized, explicit join points (final) |

### Key Modules

- `Malgo.Driver` - Pipeline orchestration
- `Malgo.Syntax` - Phase-indexed AST with type families for extensibility
- `Malgo.Pass` - Compiler pass abstraction
- `Malgo.Parser.*` - Parsing (Regular and CStyle variants)
- `Malgo.Rename.*` - Name resolution and desugaring
- `Malgo.Sequent.Eval` - Interpreter for Join IR
- `Malgo.Monad` - Effectful monad stack runner
- `Malgo.Features` - Feature flag system

## Self-Hosting Levels

Malgo has two self-hosting levels, each tested by a CI job:

| Level | Description | Script | CI job |
|-------|-------------|--------|--------|
| Level 1 | The Malgo evaluator written in Malgo (`runtime/malgo/compiler/`) evaluates arbitrary Malgo programs | `scripts/selfhost-golden.sh` | `self-hosted golden` |
| Level 2 | Level 1 evaluator evaluates `Main.mlg` which evaluates a Malgo program (metacircular interpreter) | `scripts/selfhost-level2.sh` | `self-hosted level 2 metacircular` |

```bash
# Level 1: scheme --script main.scm <testcase.mlg>
bash scripts/selfhost-golden.sh

# Level 2: scheme --script main.scm runtime/malgo/compiler/Main.mlg <testcase.mlg>
# In level 2, the inner Main.mlg's applyBuiltin "getRawArgs" drops the first
# arg (which is Main.mlg's own path) so that the inner sees only the test case
# argument. parseIntString32/64 are added to the inner evaluator's makeBaseEnv
# so that the inner Lexer can tokenize integer literals when evaluating
# nested Malgo sources.
bash scripts/selfhost-level2.sh
```

## Coding Style

- **Formatter**: Ormolu - run `mise run format` before commits
- **Linting**: HLint rules in `.hlint.yaml`
  - Alias `Data.Text` as `T`, `Data.ByteString` as `BS`, lazy variants `TL`/`BL`
  - Avoid `Debug.Trace`
- **Module naming**: `Malgo.Foo.Bar` → `src/Malgo/Foo/Bar.hs`
- Prefer explicit imports
- Package metadata from `package.yaml` (hpack) - don't edit `.cabal` directly

## Testing

- Framework: Hspec with discovery (`test/Spec.hs`)
- Each spec exposes `spec :: Spec` and ends with `*Spec.hs`
- Golden tests under `.golden/` - reset with `mise run reset`
- Run with details: `cabal test --test-show-details=direct`

## Commits & PRs

- Conventional Commits format (see `.gitmessage`)
- Example: `feat(parser): support C-style apply`
- Quality gate: `mise run format && mise run test`

## Lean 4 port (`lean/`)

A full second implementation of Malgo — parser through every backend
(Eval/Scheme/Zig), the query-based compilation engine, the linter, and
the MET debug tracer (`malgo debug-trace`) — exists in Lean 4 under
`lean/`, at parity with this Haskell implementation (which
remains the semantic oracle: the committed `.golden/` tree, the
self-hosted-compiler stress test, and `scripts/zig-golden.sh` gate both
implementations, not just this one). See `lean/README.md` for build
commands and milestone-by-milestone status, and `PORTING.md` at the repo
root for the file-by-file module mapping and the Haskell-retirement
criteria (not yet met — do not treat this port as a reason to stop
maintaining the Haskell code).

If you change semantics anywhere under `src/Malgo/` (not just fixing a
typo or adding a comment), the same change almost certainly needs to land
in `lean/Malgo/` in the same PR — check `PORTING.md`'s module table for
the corresponding Lean file, and re-run `bash scripts/lean-parity.sh`
before considering the change complete.
