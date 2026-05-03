# Technology Stack - Malgo

## Core Language & Runtime
- **Haskell (GHC 9.12.4):** The primary language used for the compiler, interpreter, and tooling.
- **Effectful:** Used as the primary effect system for managing side effects in a structured and type-safe manner.

## Compiler & Tooling
- **Megaparsec:** The library used for parsing Malgo source files and intermediate representations.
- **Hpack & Cabal:** Used for package management and building the project.
- **Mise:** Orchestrates development tasks (setup, build, test, etc.).
- **Ormolu:** The code formatter used to maintain a consistent Haskell style.

## Testing & Quality Assurance
- **Hspec:** The primary testing framework.
- **Hspec-golden:** Used for golden testing of compiler outputs and interpreter results.

## LSP Support
- **malgo-lsp:** An internal package providing Language Server Protocol support for the Malgo language.
