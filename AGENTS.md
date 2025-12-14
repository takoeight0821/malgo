# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Malgo?

Malgo is a statically typed functional programming language with an interpreter written in Haskell. Source files use the `.mlg` extension.

## Build, Test, and Development Commands

```bash
mise run setup       # Install toolchain (GHC 9.12.2, cabal, hpack, ormolu)
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
EvalPass (Interpreter)
```

**Note**: InferPass and RefinePass can be skipped for fast evaluation without type checking.

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
