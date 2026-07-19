# Malgo: Haskell → Lean 4 porting status

Full-port tracking for the Lean 4 rewrite under `lean/` (plan:
`docs/`-external, milestone/gate details and conventions in
`lean/README.md`, which is the authoritative day-to-day status doc — this
file is the per-module completeness ledger `lean/README.md`'s own
milestone table doesn't spell out file-by-file).

**Status: M0–M9 complete.** Haskell (`src/`, `malgo-lsp/`, `app/met/`)
remains the semantic oracle throughout; every Lean module below is verified
against it (goldens, `scripts/lean-parity.sh`, the self-hosted compiler
stress test, or a fresh test where no Haskell equivalent exists to
byte-diff against — noted per row).

## Dual-implementation policy

Semantic changes to an already-ported subsystem land in **both**
implementations in the same PR. A golden file changes only when both
implementations agree on the new output, or the PR explicitly marks that
area as not-yet-ported. Never hand-edit `.golden/` to accommodate the Lean
side only — implementation-specific divergence (float formatting, parser
error text, `PrettyIR` layout differences) goes in the parallel
`.golden-lean/` override tree instead (see `lean/README.md`'s intro).

## Core compiler (`src/Malgo/*.hs` ↔ `lean/Malgo/*.lean`)

All 58 Haskell modules under `src/Malgo/` have a Lean counterpart. Five are
organized under different module boundaries in Lean (functionality is
still fully ported — verified by the same gates as everything else):

| Haskell | Lean | Note |
|---|---|---|
| `Parser/Core.hs`, `Parser/Wrapper.hs` | `Parser/Prim.lean`, `Parser/CStyle.lean`, `Parser.lean` | megaparsec's combinator surface is hand-rolled once in `Parser/Prim.lean` rather than split into a `Core`/`Wrapper` pair |
| `Query/Database.hs` | `Query.lean` | `QueryDB` and its cache maps live directly in `Query.lean`; `Query/Engine.lean` still mirrors `Query/Engine.hs` 1:1 |
| `Rename.hs` (re-export module) | `Rename/Pass.lean` | Haskell's top-level `Rename.hs` only re-exports `RenamePass`/`genBuiltinRnEnv`; Lean exposes them directly from `Rename/Pass.lean`/`Rename/RnEnv.lean`, no separate re-export file |
| `Sequent/Core/Fingerprint.hs` | `Test/Fingerprint.lean` | format-immune IR fingerprinting is test-only infrastructure in both languages; Haskell's copy lives in `src/` only because `test/Malgo/Sequent/Core/Fingerprint.hs` was promoted there for `dump --stage`'s CLI reuse (see the M0 plan's shared-oracle-infra step 3) — Lean's CLI (`dump`) and test suite both import the one copy in `Test/`, no need to promote it out of `Test/` |

Every other Haskell module has an identically-pathed Lean file
(`Sequent/ToFun.hs` → `Sequent/ToFun.lean`, `Backend/Zig/Perceus.hs` →
`Backend/Zig/Perceus.lean`, etc.) — see `lean/README.md`'s "Conventions"
section for the naming rule.

### Lean-only infrastructure (no single corresponding Haskell file)

Ported by hand because Haskell gets the same capability for free from a
library dependency the Lean port deliberately doesn't take on (per the
plan's zero-external-Lake-deps decision):

| Lean module | Replaces (Haskell dependency) |
|---|---|
| `Data/IntMap.lean` | a proof-free Patricia trie (Haskell: `Data.IntMap` from `containers`) |
| `Data/Graph.lean` | Tarjan SCC (Haskell: `Data.Graph` from `containers`) |
| `Data/ShowFloat.lean` | shortest-round-trip float formatting (Haskell: GHC's native `Double`/`Float` `show`) |
| `Doc.lean` | the Wadler/Leijen `layoutSmart` layout algorithm (Haskell: the `prettyprinter` package) — see `lean/README.md`'s M6 entry for the two lost-laziness bugs found porting this |
| `Parser/Prim.lean` | megaparsec's combinator primitives (`attempt`/`sepBy`/`chainl1`/etc.) |
| `Sequent/Core/Json.lean` | the `.mlgi`/`.sqt` artifact codec (Haskell: the `binary` package) |

## LSP server: not ported (removed 2026-07-19)

`malgo-lsp/src/Malgo/LSP/*.hs` was ported to `lean/Malgo/LSP/*.lean` in M7,
but the port has since been **removed**. `lean/Test/LspSession.lean`'s
scripted stdio session reproducibly hung in CI (Linux runners only, never
locally) on a stdout write partway through a session; two rounds of
checkpoint-based narrowing localized it to `sendMessage`'s `IO.FS.Stream`
write path but didn't find a root cause. Rather than ship a permanently
flaky or disabled gate, the whole `malgo-lsp` executable and `LSP/*.lean`
were deleted — see the "LSP removed" row in `lean/README.md`'s porting
status table for the full account.

Haskell's `malgo-lsp` is unaffected and is the only `malgo-lsp`
implementation going forward. It still has **zero tests** (`build-lsp.yml`
only checks it compiles) — that gap is now unaddressed by either
implementation, not just Lean's.

## MET / debug tracer (`app/met/*.hs` ↔ `lean`)

Haskell's `met` is a live `servant` web server (`app/met/{Main,Server}.hs`).
Per the plan's M8 decision (Lean networking is still `Std.Internal`), the
Lean port is not a server: `malgo debug-trace SOURCE [-o trace.html]`
renders the whole trace into one self-contained static HTML file
(`lean/Malgo/Debug/MetPage.lean` + the `debug-trace` subcommand in
`lean/Main.lean`). Every stage transition's side-by-side/diff-patch views
are both present, toggled by inline JS instead of Haskell's server-side
`?view=diff` query param. `lean/Test/MetPage.lean` is a fresh unit test
(the Haskell original is a live server, nothing to golden-diff).

## Verification gates (current)

| Gate | Command | Status |
|---|---|---|
| Golden tests + infer + MetPage | `cd lean && lake test` | 532/532 + 94/94 + 1/1 |
| Cross-implementation parity | `bash scripts/lean-parity.sh [--mode error]` | 292/292 + 8/8 |
| Self-hosted compiler (L1) | `MALGO=lean/.lake/build/bin/malgo scripts/selfhost-golden.sh` | 73/73 |
| Self-hosted metacircular (L2) | `MALGO=lean/.lake/build/bin/malgo scripts/selfhost-level2.sh` | 5/5 |
| Zig backend golden parity | `MALGO=lean/.lake/build/bin/malgo scripts/zig-golden.sh` | 73/73, zero leaks |
| Runtime benchmark (Lean vs Haskell) | `bash bench/lean-vs-haskell.sh` | see `bench/lean-vs-haskell.md` |

All of the above run in CI (`.github/workflows/lean.yml`); `lean-parity`
additionally runs on a nightly schedule (independent of push/PR activity)
so a silent divergence is caught within a day even without a code change
triggering it.

## Haskell retirement criteria

Per the original plan's M9 entry — **not yet met, no target date**. Do not
delete or stop maintaining any Haskell code without all of the following
being independently true:

1. Every gate in the table above green on `master` for **1 consecutive
   week**, including the self-hosted metacircular (L2) and the Zig
   backend's leak gate (`MALGO-LEAK`/exit 83) — both are the deepest,
   most end-to-end stress tests available and the ones most likely to
   surface a subtle divergence under real load.
2. All `scripts/lean-parity.sh` modes (`eval`, `bigstep`, `fingerprint`,
   `error`) green **twice consecutively** on independent CI runs (guards
   against a flaky pass being mistaken for a real one).
3. `bench/lean-vs-haskell.sh` shows the Lean binary within a documented,
   explicitly-agreed performance budget of the Haskell one (not
   necessarily equal — just close enough that retiring Haskell isn't a
   regression for real users). No specific number is fixed here yet;
   set one before acting on this criterion.
4. A human maintainer has actually used the Lean `malgo` binary for real
   work (editing/compiling real `.mlg` programs, not just running the test
   suite) and hit no surprises.

When (and only when) all four hold: flip `lean/ci-gates.env` /
`.github/workflows/lean.yml` so the Lean build becomes the default CI
target Haskell's own workflows are compared against (rather than the
reverse, as today), demote Haskell's own CI to a weekly parity check
against Lean, and treat outright deletion of `src/`, `app/`, `malgo-lsp/`,
etc. as a **separate, later decision** — not implied by meeting criteria
1–4 above.
