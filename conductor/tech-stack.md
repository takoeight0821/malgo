# Technology Stack - Malgo

## Core Language & Runtime
- **Lean 4:** The language the compiler, interpreter, and tooling are written in.
  The toolchain version is pinned by `lean/lean-toolchain`.
- **Zig 0.16:** The native backend's toolchain, and the language
  `runtime/zig/runtime.zig` (reference counting, primitives) is written in.
  Pinned in `mise.toml`.

## Compiler & Tooling
- **A hand-written parser combinator library** (`lean/Malgo/Parser/Prim.lean`),
  modelled on megaparsec down to its error reporting.
- **Lake:** Build system and package manager.
- **Mise:** Orchestrates development tasks (setup, build, test, etc.).

## Testing & Quality Assurance
- **`lean/Test/Main.lean`:** One executable holding the whole suite — a
  hand-rolled golden runner plus the non-golden gates.
- **`.golden/`:** Golden outputs, in hspec-golden's directory layout
  (`<Group>/<Case>/golden`), inherited from the Haskell implementation.
- **`scripts/`:** The CI gates that need a built binary — Zig byte-parity,
  deep recursion, self-hosting, CLI, lint.

## History
The compiler was written in Haskell (GHC 9.12.4, Effectful, megaparsec, hspec)
until 2026-07. See `PORTING.md`.
