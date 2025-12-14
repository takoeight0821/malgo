# malgo

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/malgo-lang/malgo)
[![CI](https://github.com/malgo-lang/malgo/actions/workflows/build.yml/badge.svg)](https://github.com/malgo-lang/malgo/actions/workflows/build.yml)

A statically typed functional programming language.

## Requirement

* [cabal-install](https://www.haskell.org/cabal/)

## Installation

### Installing Malgo

```sh
git clone https://github.com/malgo-lang/malgo
cd malgo
cabal install
```

## Directory Structure

A brief overview of the main directories and files:

```
app/           # CLI entry point (Main.hs)
docs/          # Documentation and references
examples/      # Example Malgo source files
nix/           # Nix-related files
runtime/       # Malgo runtime and standard library
src/           # Main source code (Haskell)
test/          # Test suites and testcases
README.md      # This file
malgo.cabal    # Cabal project file
```

## Usage

To evaluate programs:

```sh
malgo eval [OPTIONS] SOURCE
```

- `eval` — Evaluate a Malgo program.
- `SOURCE` — Path to the source file to evaluate (required).

#### Options
- `--no-opt` — Disable optimizations during compilation.
- `--lambdalift` — Enable lambda lifting.
- `--debug-mode` — Enable debug mode for verbose output.

#### Example

```sh
malgo eval --no-opt --debug-mode examples/malgo/Hello.mlg
```

This will evaluate `examples/malgo/Hello.mlg` with optimizations disabled and debug mode enabled.

## Examples

The `examples/malgo/` directory contains a variety of example programs demonstrating Malgo's features. Here are some highlights:

### Hello, world
File: `examples/malgo/Hello.mlg`
```malgo
module {..} = import "../../runtime/malgo/Builtin.mlg"
module {..} = import "../../runtime/malgo/Prelude.mlg"

def main = {
  putStrLn "Hello, world!"
}
```

### Fibonacci number
File: `examples/malgo/Fib.mlg`
```malgo
module {..} = import "../../runtime/malgo/Builtin.mlg"
module {..} = import "../../runtime/malgo/Prelude.mlg"

infix 4 (<=)
def (<=) = { x y -> leInt32 x y }

infixl 6 (+)
def (+) = { x y -> addInt32 x y }

infixl 6 (-)
def (-) = { x y -> subInt32 x y }

def fib = { n ->
  if (n <= 1)
    { 1 }
    { fib (n - 1) + fib (n - 2) }
}

def main = {
  fib 5 |> toStringInt32 |> putStrLn
}
```

### List operations
File: `examples/malgo/List.mlg`
```malgo
module {..} = import "../../runtime/malgo/Builtin.mlg"
module {..} = import "../../runtime/malgo/Prelude.mlg"

infix 4 (<=)
def (<=) : Int32 -> Int32 -> Bool
def (<=) = {x y -> leInt32 x y}

infixl 6 (+)
def (+) : Int32 -> Int32 -> Int32
def (+) = {x y -> addInt32 x y}

infixl 6 (-)
def (-) : Int32 -> Int32 -> Int32
def (-) = {x y -> subInt32 x y}

def map : (a -> b) -> List a -> List b
def map =
  { _ Nil -> Nil,
    f (Cons x xs) -> Cons (f x) (map f xs)
  }

def sum : List Int32 -> Int32
def sum =
  { Nil -> 0,
    Cons x xs -> x + sum xs
  }

-- [0 .. n]
def below : Int32 -> List Int32
def below = { n ->
  if (n <= 0)
     { [0] }
     { Cons n (below (n - 1)) }
}

def main = {
  sum (map (addInt32 1) (below 10))
    |> toStringInt32
    |> putStrLn
}
```

### Lisp interpreter

https://github.com/malgo-lang/minilisp


## For Developers

This project uses [mise](https://github.com/jdx/mise) for managing development tools and tasks. The `mise.toml` file defines tool versions and common development workflows.

### Toolchain
- **GHC** (via ghcup)
- **cabal-install**
- **hpack**
- **Haskell Language Server (HLS)**
- **go** (for changelog generation)
- **watchexec** (for file watching)
- **git-chglog** (for changelog generation)

### Common Tasks
Run these with `mise run <task>`:

- `setup` — Install and set up all required tools and dependencies.
- `setup-hls` — Build and set up Haskell Language Server for the project.
- `build` — Build the project (`hpack && cabal build`).
- `test` — Run the test suite. Optionally filter tests with `--option match=<pattern>`.
- `exec` — Run the project executable (`cabal exec malgo-exe`).
- `changelog` — Generate the changelog using `git-chglog`.

See `mise.toml` for more details and customization.

## Release Workflow

- **Labels:** Use PR labels `breaking`, `feat`, `fix` following Conventional Commits to drive semantic version bumps.
- **Auto-labeling:** PRs are auto-labeled from commit messages via CI.
- **Release PR:** On pushes to `master`, CI computes the next version, creates a draft GitHub Release, and opens a `release/vX.Y.Z` PR.
- **Publish:** Merging a `release/*` PR to `master` tags the commit and publishes the GitHub Release.
- **Notes:** Releases use generated notes; excluded labels: `skip-changelog`, `duplicate`, `invalid`.

To create labels via GitHub CLI:

```bash
gh label create breaking --description "Major version bump (breaking change)" --color D93F0B
gh label create feat --description "Minor version bump (feature)" --color 0E8A16
gh label create fix --description "Patch version bump (fix)" --color FBCA04
```
