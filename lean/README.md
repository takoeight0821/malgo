# Malgo — Lean 4 port

Full port of the Haskell implementation (`src/`) to Lean 4, tracked by the
plan of 2026-07-14. The Haskell implementation is the semantic oracle: the
committed `.golden/` tree, the selfhost scripts, and `scripts/zig-golden.sh`
gate both implementations. Groups whose output legitimately differs from
Haskell (float formatting, megaparsec error text, PrettyIR layout) are
regenerated into `.golden-lean/` with the same layout; the test runner
checks `.golden-lean/` first (an explicit override wins), then the shared
`.golden/`. `--update` only ever writes `.golden-lean/` — the shared tree
stays Haskell-owned.

## Building

```bash
mise run lean-setup   # install elan once; toolchain pinned by lean-toolchain
mise run lean-build   # lake build
mise run lean-test    # lake test (golden runner; -- --match PAT --update)
bash scripts/lean-parity.sh   # cross-implementation parity (needs both binaries built)
```

The workspace mirror honors `MALGO_WORK_DIR` and defaults to
`.malgo-work-lean` so both toolchains can share a checkout. `.mlgi`/`.sqt`
artifacts are JSON (`Lean.ToJson`/`FromJson`), not wire-compatible with the
Haskell `binary` formats — each toolchain's workspace is self-consistent.

The hidden `malgo dump --stage flat-fingerprint|join-fingerprint SOURCE`
subcommand (both implementations) prints a format-immune IR constructor
count for `scripts/lean-parity.sh`'s `fingerprint` mode.

## Porting status

| Milestone | Status | Modules |
|---|---|---|
| M0 support layer | done | Prelude, Path, SExpr (s-cargot-exact printer), Module, Features, Monad, Pass, Id, Data/IntMap |
| M1 walking skeleton | done — `lake test` 337/337 | Parser/Prim+CStyle, Syntax (phase-indexed), Rename, Data/Graph (SCC), Sequent/{Fun,ToFun,SaturateCtor,ReuseSpecialize,ToCore,Core.{Full,Flat,Join}}, Eval (defunctionalized ConsumerK), Driver, CLI. Gates: Parser 3/3, Rename 18/18, ToFun 18/18, ToCore 225/225 (incl. all 73 join dumps + fingerprints), Eval 73/73 stdout goldens; `malgo eval` verified end-to-end incl. the mirror-seeding protocol |
| M2 done | `lake test` 418/418; `scripts/lean-parity.sh` 292/292 (eval/bigstep/fingerprint); CI `LEAN_PARITY=1` | + BigStepEval (73/73), Forth (8/8), Query/Engine (memoized QueryDB, JSON `.mlgi`/`.sqt`, `reverseDepClosure` for M7), wired into `Driver.compileAndEval` as the actual `malgo eval` compilation path (each dependency parsed/renamed/lowered once, not the M1-era double-rename). The M1 test harness (`Test/Main.lean`) still calls `Driver.compileToRenamed/compileToFun/compileToJoin` directly — that path's uniq-numbering is what the golden files' byte parity depends on |
| M3 done | `lake test` 434/434 goldens + 94/94 infer; `scripts/lean-parity.sh` 292/292 + 8/8 error mode | Elaborate (codata/copattern desugaring, 16/16 golden), constraint-based HM `Infer`/`Infer.Constraint`/`Infer.Unify` (level-based let-polymorphism, row polymorphism, equi-recursive `TMu` via de Bruijn indices), wired into the query engine (`fetchInferredModule`/`buildDepsEnv` — kept genuinely strict to match a latent Haskell CLI defect on Prelude+Builtin re-export diamonds; the lake-test gate uses a separate `buildDepsEnvLenient` mirroring what Haskell's own test suite actually exercises — see `test/testcases/malgo/error/README.md` for the full story). Error-mode parity infra (`.expect` sidecars, `lean-parity.sh --mode error`) |
| M4 done | `scripts/selfhost-golden.sh` (L1) 73/73; `scripts/selfhost-level2.sh` (L2, metacircular) 5/5 | `Backend/Scheme` wired into the CLI (`Driver.compileScheme`, `malgo eval --target scheme`); fixed a `Sequent/Core/Json.lean` `.sqt` codec arity bug found by the selfhost gate (the only real bug the whole self-hosted-compiler stress test surfaced) |
| M5 done | `scripts/zig-golden.sh` 73/73, zero `MALGO-LEAK`/timeouts/mismatches; CI `LEAN_ZIG=1` | `Backend/Zig/{Ir,Normalize,ClosureConv,Peephole,Perceus,Reuse,RcCheck,Emit,Toolchain,Runtime}` + orchestration (`Backend/Zig.lean`), wired into `Driver.compileZig` (`malgo eval --target zig`) and `Driver.compileToNativeExecutable` (`malgo compile`). `Runtime.zigRuntime` embeds `runtime/zig/runtime.zig` via `include_str`. Boundary review fixed 3 findings: `Toolchain.buildExecutable` now checks `zig` is on `PATH` before spawning (Lean's `IO.Process.output` does not raise `IO.Error` for a missing command — it returns `.ok` with a nonzero exit, unlike Haskell's `readProcessWithExitCode`); `Emit.emitStmts` now `panic!`s instead of silently truncating on a `suffixFreeVars`/`stmts` length-mismatch invariant violation; `Driver.lean`'s parse+link+seed-mirror pipeline (duplicated across 4 CLI entry points) extracted into `linkForCli` |
| M6 done | `lake test` 532/532 goldens (incl. 9 Lint + 89 PrettyIR trace) + 94/94 infer; `scripts/lean-parity.sh` 292/292 + 8/8; `scripts/zig-golden.sh` 73/73 (no regression) | `Doc.lean` (new — a from-scratch, byte-faithful port of the `prettyprinter` package's `layoutSmart` algorithm, since `Malgo.Prelude`'s existing `Pretty` is plain-`String`, not a real layout engine); `Lint/{Diagnostic,Rule,Traversal,Rules}.lean` + `Lint.lean`, wired into `malgo lint`; `Debug/{PrettyIR,DiffView,Pipeline}.lean` (every IR renderer, a real Myers-diff port for the golden "DIFFS" sections, and the full-pipeline single-module tracer). Two real bugs found in `Doc.lean` via a genuine multi-minute hang (not a wrong-output mismatch): (1) a two-column Lean `match` on a tuple is strict where Haskell's is lazy and short-circuits — fixed by nesting the match; (2) `best`'s layout-alternative selection computed both branches to completion before choosing, because `SimpleDocStream`'s recursive tail was a plain strict field instead of a `Thunk` — Haskell's algorithm depends on laziness to avoid ever materializing the rejected branch. Both fixes verified end-to-end: a 162-line Zig IR trace went from hanging to 1.3s |
| M7 done | `lake test` 532/532 goldens + 94/94 infer + LSP session gate; `scripts/lean-parity.sh` 292/292 + 8/8 (no regression); manual end-to-end smoke test against the real binary | `LSP/{Json,Protocol,Diagnostics,Server,Server/JsonRpc,Handlers}.lean` + `LSP.lean` (dispatch) + `LspMain.lean` (`malgo-lsp` executable, in `defaultTargets`). A minimal, from-scratch LSP server (no aeson/lsp-types-equivalent, matching Haskell's own choice): `initialize`/`initialized`/`shutdown`/`exit` + `textDocument/{didOpen,didChange,didClose,hover}`, diagnostics from parse/rename failures only (never lint). `Handlers.lean` re-checks an edited file via `Query.Engine`'s `updateSource`+`invalidateModule`+`fetchRenamedModule` — the reverse-dependency invalidation built in M2 specifically for this. Added `Malgo.Monad.runCatching` (`EIO.toBaseIO`-based) since the LSP needs the structured `CompileError` (for its `range?`), not `MalgoM.run`'s stringified one. `Test/LspSession.lean`: a scripted stdio acceptance test (spawns the real binary) — no Haskell reference exists for this (malgo-lsp has zero tests), authored fresh |
| M8 done | `lake test` 532/532 goldens + 94/94 infer + LSP session + MetPage gates; `scripts/lean-parity.sh` 292/292 + 8/8 (no regression); manual run producing a well-formed 12-stage/11-transition trace page | `Debug/MetPage.lean` (new) + a `debug-trace` subcommand in `Main.lean` — replaces Haskell's `app/met` live web server (per the plan's M8 decision, since Lean networking is still `Std.Internal`) with `malgo debug-trace SOURCE [-o trace.html]`: run the pipeline once and render every stage/transition into ONE self-contained static HTML file (side-by-side + diff-patch views per transition, toggled via inline JS; the notation-legend sidebar ported verbatim). `Test/MetPage.lean`: a fresh unit test (the Haskell original is a live server, not a static artifact — nothing to golden-diff) checking the plan's stated gate directly (generated page lists every stage transition) |
| M9 done | `lake test` 532/532 + 94/94 + LSP session + MetPage gates; `scripts/lean-parity.sh` 292/292 + 8/8; `lean-parity` runs nightly in CI in addition to push/PR | Consolidation: `PORTING.md` (repo root — the per-module Haskell↔Lean file mapping and the written Haskell-retirement criteria, not yet met); `bench/lean-vs-haskell.sh` (hyperfine runtime comparison, `bench/lean-vs-haskell.md` for the latest results — found and documented, rather than fixed, a pre-existing Zig-backend crash on deeper recursion depths, reproduced identically by both implementations); a nightly `schedule:` trigger added to `.github/workflows/lean.yml`; removed a vestigial, never-wired `LEAN_SELFHOST_L2` flag from `ci-gates.env` (Level 2 already runs unconditionally alongside Level 1 whenever `LEAN_SELFHOST=1`); `AGENTS.md`/`CLAUDE.md` updated to point at this port |
| LSP removed (2026-07-19) | removed | The M7 `malgo-lsp` executable and every `LSP/*.lean` module were deleted. Root cause: `Test/LspSession.lean`'s scripted stdio session reproducibly hung in CI (Linux runners) on the third message write of a session — never locally (macOS) — with exit 137 (killed by the test's own watchdog). Checkpoint logging (added and later removed across a few debugging commits) narrowed it to a `sendMessage`/`IO.FS.Stream.write` stall on a tiny (38-byte) payload immediately after two larger writes on the same handle had already succeeded and flushed — not an encoding, computation, or pipe-capacity issue, but something Linux/runtime-specific in repeated writes to a piped stdout that wasn't resolved after two rounds of narrowing. Rather than carry a permanently-disabled or flaky gate, the whole LSP server was dropped; `Malgo.Monad.runCatching` (added in M7 solely for the LSP's structured-error needs) was removed as dead code along with it. Haskell's `malgo-lsp` (`malgo-lsp/`) is unaffected and remains the only LSP implementation |

Conventions:
- module paths mirror `src/Malgo/*.hs` 1:1 (`Sequent/ToFun.hs` → `Malgo/Sequent/ToFun.lean`);
- `Std.TreeMap`/`TreeSet` wherever Haskell used `Data.Map`/`Set` and iteration
  order reaches output; `Malgo.IntMap` (a from-scratch Patricia trie, since
  `Std.HashMap`/`TreeMap` bundle well-formedness proofs rejected in
  nested-inductive positions) inside `Value`'s `Env`; `Std.HashMap` only for
  order-invisible caches;
- incremental proofs: `partial def` where recursion is not structural
  (unchanged); a small number of hand-picked modules additionally carry
  real `theorem`s beyond `#guard` spot-checks — see `Data/IntMap.lean`
  (`lookup_insert`, a `WF` well-formedness invariant) for the current
  example; opt-in per module, not a blanket requirement;
- known naming deviation: `Meta.meta` (Haskell) → `Meta.info` (`meta` is a
  Lean keyword).
