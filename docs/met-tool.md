# MET — M-exp-Tracer

MET runs a `.mlg` file through the full compilation pipeline — Parse through the
Zig backend's `Reuse` pass — and renders every stage's intermediate representation
in a Malgo-ish ASCII notation, for side-by-side or unified-diff comparison between
adjacent stages. It exists to make the effect of a specific pass on a specific
program visible without instrumenting the compiler or reading raw IR dumps by hand.

## Running it

```
malgo debug-trace path/to/file.mlg
malgo debug-trace path/to/file.mlg -o /tmp/trace.html
malgo debug-trace path/to/file.mlg --infer --malgo2025
```

It writes one self-contained HTML file (default `trace.html`) and exits. The two
flags:

- `--infer`: also run `InferPass` (mirrors `malgo eval --infer`), adding an
  `Elaborate` stage's input to type-checking.
- `--malgo2025`: also run `ElaboratePass` (mirrors the `malgo2025` feature flag),
  inserting an `Elaborate` stage between `Rename` and `ToFun`.

MET used to be a live `servant`/`warp` server (`app/met`, `mise run met`) serving a
route per transition. It is a static page now because Lean's networking is still
under `Std.Internal`, and because a file is easier to keep, attach to an issue, or
diff against a later run than a process that has to stay up.

## What it shows

The page lists every adjacent stage transition, each with both views of the same
pair of stages and a button to toggle between them (a few lines of inline JS,
in place of the server's `?view=diff` query parameter):

- **Side-by-side** (default): a two-column table, line-aligned, with word-level
  highlighting on lines that changed.
- **Diff patch**: a unified-diff-style rendering, added/removed lines highlighted,
  again with word-level sub-highlighting within a paired removed/added line.

The word-level highlighting matters more than it might sound: a single renamed
identifier or a single inserted `dup` no longer paints its whole line red/green —
only the token that actually changed does. This is implemented as a two-pass diff
(`Malgo.Debug.DiffView`, a Myers diff): first line-by-line, then, for a
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

- **`Malgo.Debug.Pipeline`** (`lean/Malgo/Debug/Pipeline.lean`) drives a single module
  through every pass above, capturing each stage's rendered text into a `Stage {name,
  rendered}` list. This is also what MET's own golden tests use, independent of any
  page rendering.
- **`Malgo.Debug.PrettyIR`** (`lean/Malgo/Debug/PrettyIR.lean`) is a best-effort,
  non-round-trippable renderer for every IR from surface syntax through
  `Core.Full`/`Flat`/`Join` to the Zig backend's own IR — the source of the ASCII
  notation the sidebar legend explains.
- **`Malgo.Debug.DiffView`** (`lean/Malgo/Debug/DiffView.lean`) implements the
  line-then-word diffing described above, independent of HTML rendering.
- **`Malgo.Debug.MetPage`** (`lean/Malgo/Debug/MetPage.lean`) renders the precomputed
  `[Stage]` into the single HTML document, inlining its own CSS and the view-toggle
  script. `lean/Test/MetPage.lean` gates that the generated page lists every stage
  transition.

## Known limitations

- Only the target module's own definitions are traced — no `Builtin`/`Prelude`
  linking — so a stage's rendered output will reference imported names that aren't
  themselves defined anywhere in the trace.
- The renderer's notation is invented per IR family and is not parseable back into
  Malgo source or any IR; it exists purely for human comparison between adjacent
  stages, not as a serialization format.
- `RefinePass` has no dedicated rendered stage yet, even under `--infer`.
