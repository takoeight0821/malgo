# MET — M-exp-Tracer

MET is a browser-based tool (`app/met`) that runs a `.mlg` file through the full
compilation pipeline — Parse through the Zig backend's `Reuse` pass — and renders
every stage's intermediate representation in a Malgo-ish ASCII notation, for
side-by-side or unified-diff comparison between adjacent stages. It exists to make
the effect of a specific pass on a specific program visible without instrumenting
the compiler or reading raw IR dumps by hand.

## Running it

```
mise run met --option source="path/to/file.mlg"
mise run met --option source="path/to/file.mlg" --option port=9090
mise run met --option source="path/to/file.mlg" --option flags="--infer --malgo2025"
```

`mise run met` depends on `mise run build` and then runs
`cabal exec met -- SOURCE --port PORT [flags]`. The two flags MET's own CLI accepts:

- `--infer`: also run `InferPass` (mirrors `malgo eval --infer`), adding an
  `Elaborate` stage's input to type-checking.
- `--malgo2025`: also run `ElaboratePass` (mirrors the `malgo2025` feature flag),
  inserting an `Elaborate` stage between `Rename` and `ToFun`.

The server prints `MET: traced <path> (<N> stages). Listening on
http://localhost:<port>` and stays up until killed; every compiler effect needed to
produce the trace runs exactly once, before the HTTP server starts — page handlers
are pure functions over the resulting `[Stage]` list.

## What it shows

The home page (`/`) lists every adjacent stage transition as a link. Each stage
transition page (`/stage/<i>`) offers two views of the same pair of stages:

- **Side-by-side** (default): a two-column table, line-aligned, with word-level
  highlighting on lines that changed.
- **Diff patch** (`?view=diff`): a unified-diff-style rendering, added/removed lines
  highlighted, again with word-level sub-highlighting within a paired
  removed/added line.

The word-level highlighting matters more than it might sound: a single renamed
identifier or a single inserted `dup` no longer paints its whole line red/green —
only the token that actually changed does. This is implemented as a two-pass diff
(`Malgo.Debug.DiffView`, via the `Diff` library): first line-by-line, then, for a
same-position "replace" (a run of removed lines immediately followed by a run of
added lines — the shape every changed statement takes in this renderer), word-by-word
within each paired line.

A sidebar on every page is a legend for MET's own notation — it is explicitly *not*
real Malgo syntax, since several stages (Core IR sequent-calculus notation, the Zig
backend's ANF IR) have no surface-syntax equivalent at all.

## The stages traced

`Malgo.Debug.Pipeline.runTrace` mirrors the exact pass sequence
`Malgo.Query.Engine`'s `LinkedProgram` query and `Malgo.Backend.Zig`'s `ZigPass` run
in production, just without discarding the intermediate values, and without linking
in `Builtin`/`Prelude` (linking every dependency's definitions in would bury the
interesting diff in Prelude noise; every pass rewrites definitions independently of
its imports anyway). In order:

| Stage | Producing pass | Notes |
|---|---|---|
| `Parse` | `ParserPass` | Surface syntax |
| `Rename` | `RenamePass` | |
| `Elaborate` | `ElaboratePass` | Only with `--malgo2025` |
| `ToFun` | `ToFunPass` | Fun IR |
| `SaturateCtor` | `Malgo.Sequent.SaturateCtor` | Inlines saturated constructor calls |
| `ReuseSpecialize` | `Malgo.Sequent.ReuseSpecialize` | Inserts `reuseHint` calls |
| `ToCore` | `toCore` | Core IR (sequent calculus) |
| `Flat` | `flatProgram` | Flat IR |
| `Join` | `joinProgram` | Join IR |
| `ClosureConv` | `ClosureConv.convertProgram` | Zig backend's own ANF IR |
| `Peephole` | `peepholeProgram` | Scrutinee-tuple elimination |
| `Perceus` | `perceusProgram` | `dup`/`drop` insertion |
| `Reuse` | `reuseProgram` | FBIP reuse-token pairing; also runs `RcCheck` and prepends any violation as a comment |

Only `InferPass` is conditionally run (`--infer`) *without* a dedicated stage of its
own — its output feeds `ToFunPass` the same way `RefinePass`'s would in the
production pipeline, but MET does not currently render a `Refine`/typed-AST stage.

See [`zig-backend.md`](zig-backend.md) for what each pass after `Join` actually
does, and [`perceus-gc.md`](perceus-gc.md) for `Perceus`/`Reuse`/`RcCheck` in detail.

## Architecture

- **`Malgo.Debug.Pipeline`** (`src/Malgo/Debug/Pipeline.hs`) drives a single module
  through every pass above, capturing each stage's rendered text into a `Stage {name,
  rendered}` list. This is also used directly by MET's own golden tests
  (`Malgo.Debug.PrettyIRSpec`), independent of the web server.
- **`Malgo.Debug.PrettyIR`** (`src/Malgo/Debug/PrettyIR.hs`) is a best-effort,
  non-round-trippable renderer for every IR from surface syntax through
  `Core.Full`/`Flat`/`Join` to the Zig backend's own IR — the source of the ASCII
  notation the sidebar legend explains.
- **`Malgo.Debug.DiffView`** (`src/Malgo/Debug/DiffView.hs`) implements the
  line-then-word diffing described above, independent of HTML rendering.
- **`app/met`** (`Main.hs` + `Server.hs`) is the CLI entry point (`optparse-applicative`)
  and the `servant` + `warp` + `lucid` HTTP server. `Server.app` takes the
  precomputed `[Stage]` and serves two routes: `/` (the stage list) and
  `/stage/:index` (a transition view, with an optional `?view=diff` query param).

## Known limitations

- Only the target module's own definitions are traced — no `Builtin`/`Prelude`
  linking — so a stage's rendered output will reference imported names that aren't
  themselves defined anywhere in the trace.
- The renderer's notation is invented per IR family and is not parseable back into
  Malgo source or any IR; it exists purely for human comparison between adjacent
  stages, not as a serialization format.
- `RefinePass` has no dedicated rendered stage yet, even under `--infer`.
