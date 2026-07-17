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
```

The workspace mirror honors `MALGO_WORK_DIR` and defaults to
`.malgo-work-lean` so both toolchains can share a checkout. Artifacts are
not wire-compatible with the Haskell `binary` formats.

## Porting status

| Milestone | Status | Modules |
|---|---|---|
| M0 support layer | done | Prelude, Path, SExpr (s-cargot-exact printer), Module, Features, Monad, Pass, Id, Data/IntMap |
| M1 walking skeleton | done — `lake test` 337/337 | Parser/Prim+CStyle, Syntax (phase-indexed), Rename, Data/Graph (SCC), Sequent/{Fun,ToFun,SaturateCtor,ReuseSpecialize,ToCore,Core.{Full,Flat,Join}}, Eval (defunctionalized ConsumerK), Driver, CLI. Gates: Parser 3/3, Rename 18/18, ToFun 18/18, ToCore 225/225 (incl. all 73 join dumps + fingerprints), Eval 73/73 stdout goldens; `malgo eval` verified end-to-end incl. the mirror-seeding protocol |
| M4 Scheme backend | ported early, unverified | Backend/Scheme (selfhost gate pending) |
| M2–M9 | not started | see the plan |

Conventions:
- module paths mirror `src/Malgo/*.hs` 1:1 (`Sequent/ToFun.hs` → `Malgo/Sequent/ToFun.lean`);
- `Std.TreeMap`/`TreeSet` wherever Haskell used `Data.Map`/`Set` and iteration
  order reaches output; `Malgo.IntMap` (proof-free Patricia trie) inside
  `Value`'s `Env`; `Std.HashMap` only for order-invisible caches;
- no proofs: `partial def` where recursion is not structural;
- known naming deviation: `Meta.meta` (Haskell) → `Meta.info` (`meta` is a
  Lean keyword).
