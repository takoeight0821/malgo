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
| M6–M9 | not started | see the plan |

Conventions:
- module paths mirror `src/Malgo/*.hs` 1:1 (`Sequent/ToFun.hs` → `Malgo/Sequent/ToFun.lean`);
- `Std.TreeMap`/`TreeSet` wherever Haskell used `Data.Map`/`Set` and iteration
  order reaches output; `Malgo.IntMap` (proof-free Patricia trie) inside
  `Value`'s `Env`; `Std.HashMap` only for order-invisible caches;
- no proofs: `partial def` where recursion is not structural;
- known naming deviation: `Meta.meta` (Haskell) → `Meta.info` (`meta` is a
  Lean keyword).
