# Malgo: Haskell → Lean 4 porting status

Full-port tracking for the Lean 4 rewrite under `lean/` (plan:
`docs/`-external, milestone/gate details and conventions in
`lean/README.md`, which is the authoritative day-to-day status doc — this
file is the per-module completeness ledger `lean/README.md`'s own
milestone table doesn't spell out file-by-file).

**Status: complete, and the Haskell implementation has been removed**
(2026-07-26). While the port was under way Haskell (`src/`, `malgo-lsp/`,
`app/met/`) was the semantic oracle, and every Lean module below was
verified against it: goldens, `scripts/lean-parity.sh`, the self-hosted
compiler stress test, or a fresh test where no Haskell equivalent existed
to byte-diff against (noted per row).

This file is now a historical ledger of what was ported from where. The
day-to-day status doc is `lean/README.md`; module paths in the left-hand
columns below refer to files that no longer exist and are kept so a
question of the form "where did X go?" still has an answer, in this
repository's history if not its working tree.

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
| Golden tests + the non-golden gates | `mise run test` | 558 + 94 + 7 + 74 + 73 + 6 + 4 |
| CLI over the corpus | `bash scripts/cli-gate.sh` | 73 + 73 + 8 |
| Self-hosted compiler (L1) | `bash scripts/selfhost-golden.sh` | 73/73 |
| Self-hosted metacircular (L2) | `bash scripts/selfhost-level2.sh` | 5/5, disabled in CI (#385) |
| Zig backend golden parity | `bash scripts/zig-golden.sh` | 73/73, zero leaks |
| Zig deep recursion | `bash scripts/zig-deep-recursion.sh` | 18.8M dispatches, no leak |
| Malgo source lint | `bash scripts/lint-sources.sh examples/malgo test/testcases runtime/malgo` | 0 findings |

All of the above except L2 run in CI (`.github/workflows/lean.yml`), on
every PR and again on a nightly schedule, so a gate nobody's changes
happen to touch still reports within a day.

## Haskell retirement: what the criteria were, and how the decision was made

This section used to set four conditions that all had to hold before any
Haskell code could be deleted, and stated that even meeting all four would
not authorize deletion — that would be "a separate, later decision". The
maintainer took that decision on 2026-07-25, adopting the Lean
implementation and the Zig backend as the project's only ones, and it
overrides what this section required. Recording the gap honestly:

1. **Every gate green on `master` for one consecutive week, including
   L2 and the leak gate.** Partly met. Every gate was green, and the leak
   gate runs on all 73 cases in the Zig sweep. But L2 was *disabled* in
   CI a day earlier (#385: through the Zig backend it costs ~16 minutes
   against a sub-10-minute CI target), so the deepest end-to-end stress
   test was not among the passing gates at the time of the decision. It
   still runs locally, and #385 tracks bringing it back.
2. **All `scripts/lean-parity.sh` modes green twice consecutively.** Met
   while both implementations existed. The script is gone with the second
   implementation; `scripts/cli-gate.sh` inherits the coverage that was
   not about comparing the two.
3. **Lean within an agreed performance budget of Haskell.** Not met, and
   now unmeasurable — the criterion never fixed a number, and there is no
   longer a second implementation to measure against. `bench/lean-vs-haskell.md`
   holds the last comparison, taken before the trampoline (#360) changed
   the Zig backend's calling convention, so it does not describe today's
   binary either. Treat criterion 3 as lapsed rather than satisfied.
4. **A maintainer used the Lean binary for real work.** Met: the Lean
   binary has been the one driving self-hosting and the Zig sweeps since
   #384.

What actually justified acting despite 1 and 3: the Lean implementation is
gated on the same corpus the Haskell one was (547 goldens, the 73-case Zig
byte-parity sweep with its leak gate, Level 1 self-hosting over all 73
cases), and the maintenance cost of keeping two implementations in step
was the thing being paid for a comparison that had stopped finding
divergences.
