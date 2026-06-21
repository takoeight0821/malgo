# Implementing Forth in Malgo

## Overview

This document surveys Forth's core semantics, existing Haskell/functional implementations, and assesses
what a Forth interpreter written *in* Malgo would look like.

---

## Forth Core Semantics

### Architecture

Forth is a stack-based language with four fundamental components:

| Component | Description |
|-----------|-------------|
| Data Stack | Parameter passing and arithmetic; values are pushed/popped |
| Return Stack | Return addresses for word calls |
| Dictionary | Linked list of word definitions; looked up by name |
| Program Counter | Tracks execution position within a word body |

### Dictionary

The dictionary is the central data structure. Each entry contains:
- **Name field** — the word's string name
- **Link field** — pointer to the previous entry (linked-list traversal)
- **Code field** — address of executable code (or list of execution tokens)
- **Precedence bit** — marks `IMMEDIATE` words that execute at compile time

Two kinds of words:
1. **Code words** — primitive operations (implemented in the host language)
2. **Colon words** — user-defined; a sequence of execution tokens compiled by `: ... ;`

### Word Definitions

```forth
: square  ( n -- n^2 )  dup *  ;
: greet   ." Hello, world!"  cr  ;
```

The colon compiler switches Forth from **interpret mode** to **compile mode**, recording execution
tokens until `;` emits an `EXIT` token.

**Immediate words** (flagged with `IMMEDIATE`) execute during compilation rather than being compiled.
They enable compile-time control structures like `IF`, `BEGIN`, `LOOP`.

### REPL (QUIT loop)

```
QUIT: get line → tokenize → for each token:
        interpret mode: lookup word → execute (or push literal)
        compile  mode: lookup word → if IMMEDIATE execute, else compile token
```

### Minimal Primitive Set

A workable Forth kernel needs roughly 48 primitives:

| Category | Words |
|----------|-------|
| Stack | `DUP DROP SWAP OVER ROT` |
| Arithmetic | `+ - * / MOD` |
| Comparison | `= < > 0=` |
| Memory | `@ ! ALLOT HERE` |
| Control | `IF THEN ELSE BEGIN UNTIL LOOP` |
| Dictionary | `: ; IMMEDIATE` |
| I/O | `EMIT KEY .` |
| System | `QUIT BYE` |

Theoretical minimum (eForth model): 7 assembly-level primitives (`nop nand ! @ um+ special lit`),
with everything else bootstrapped in Forth itself.

---

## Existing Functional Implementations

### Haskell Implementations

| Project | Approach | Notes |
|---------|----------|-------|
| [harrorth](https://github.com/ezag/harrorth) | State machine → effects system | Tutorial-style, documents the learning process |
| [hforth (jrp2014)](https://github.com/jrp2014/hforth) | Generic data/control stacks | Bare-bones, portable framework |
| [forth (olemorud)](https://github.com/olemorud/forth) | Minimal subset | Inspired by Exercism; `+`, `-`, `*`, `/`, `DUP`, `DROP`, `OVER` |
| [forth (reinvdwoerd)](https://github.com/reinvdwoerd/forth) | Haskell clone | General Forth clone |

**"Write a Forth in Haskell"** blog series (glitchbra.in):
- Part 01–02: Pure state machine `ForthState → ForthState`
- Part 03–04: Refactor to effects system (`mtl`/IO) for real I/O
- Part 05: Add new Forth constructs

**Standard Haskell state representation:**
```haskell
data ForthState = ForthState
  { dataStack   :: [Value]
  , returnStack :: [Addr]
  , dictionary  :: Map String Word
  , heap        :: Array Addr Value
  , pc          :: Addr
  , mode        :: Mode   -- Interpret | Compile
  }
```
Typically wrapped in a `State` monad or `StateT IO`.

### Challenges in Pure Functional Languages

| Challenge | Cause | Solution |
|-----------|-------|----------|
| Mutable dictionary | Forth allows runtime redefinition | Immutable `Map`; rebuild on `:` |
| Mutable stacks | Stack operations are destructive | Thread `[Value]` lists through state |
| Dual mode (interpret/compile) | Mode is runtime state | Carry `mode :: Mode` field in state record |
| `IMMEDIATE` words | Execute at compile-time | Flag words in dictionary; check at compile |
| Raw memory (`@ !`) | Pointer arithmetic | Unsafe vector API or `IORef`/`STRef` array |
| I/O side effects | `EMIT`, `KEY` | Wrap state transformer in `IO` |

---

## Malgo Feature Assessment

### Available Building Blocks

**Data types:**
- Numeric: `Int32`, `Int64`, `Float`, `Double`
- Text: `Char`, `String`
- Algebraic: full ADT with pattern matching
- Lists: `data List a = Nil | Cons a (List a)` (built-in)
- Records: named fields
- Map: AVL-tree based ordered map in `runtime/malgo/Map.mlg`

**Control flow:**
- `case` expression with exhaustive pattern matching
- Nested/constructor/literal patterns
- Tail-recursive functions (compiler optimizes tail calls)
- Higher-order functions, closures, lambdas (`{ x y -> expr }`)

**I/O:**
- Input: `getChar`, `getLine`, `getContents`, `getRawArgs`
- Output: `printString`, `putStr`, `putStrLn`, `printInt32`, `flush`
- Files: `readFile`, `writeFile`

**String utilities:**
- `appendString`, `lengthString`, `atString`, `substring`
- `parseIntString32`, `parseIntString64`
- `consString`, `reverseString`

### State Threading Pattern

Malgo has no mutable references in its pure core. State must be threaded explicitly — the same
pattern the self-hosted compiler uses extensively:

```malgo
-- Forth interpreter state as a record
type Stack = List Int64

data ForthState = ForthState
  { dataStack : Stack
  , dictionary : Map String Word
  , compiling : Bool
  , output : String
  }

-- A Forth "step" function
def step : ForthState -> Token -> ForthState
def step state tok = ...
```

The self-hosted compiler at `runtime/malgo/compiler/` demonstrates this pattern with `InferState`
records threaded through all inference passes — it works well and is idiomatic Malgo.

### Mutable Memory

For Forth's `@ !` (memory read/write), options in Malgo:

1. **Functional map** (recommended for a pure implementation): model heap as `Map Int64 Int64`.
   Sufficient for a learning-oriented Forth.
2. **Unsafe vector API**: `runtime/malgo/Vector.mlg` exposes `malgo_new_vector`,
   `malgo_read_vector`, `malgo_write_vector` via `foreign import`. Allows true mutable arrays
   at the cost of safety.

### Word Representation

```malgo
-- Forth values
data ForthVal
  = FInt Int64
  | FStr String
  | FBool Bool

-- A word is either a primitive (built-in action) or a compiled sequence of word names
data Word
  = Prim (ForthState -> ForthState)
  | Compiled (List String)   -- list of word names to execute in sequence

type Dict = Map String Word
```

Higher-order functions (`Prim (ForthState -> ForthState)`) let primitive operations be stored
directly in the dictionary as values — a natural fit for Malgo's first-class functions.

### Feasibility Summary

| Aspect | Assessment |
|--------|------------|
| Data stack | `List Int64` — easy, idiomatic |
| Dictionary | `Map String Word` from `Map.mlg` — ready-made |
| Tokenizer/lexer | String operations sufficient (`substring`, `atString`, `parseIntString64`) |
| REPL loop | `getLine` + recursive `loop` function — straightforward |
| Colon definitions | Track `compiling` flag + accumulate token list — feasible |
| IMMEDIATE words | Flag in `Word` type; check at compile time — feasible |
| Mutable heap (`@ !`) | Use `Map Int64 Int64` (pure) or `Vector.mlg` (unsafe) |
| I/O words (`EMIT` etc.) | `printChar`, `getChar` — available |
| Tail recursion | Supported — deep Forth recursion won't overflow |

**Overall**: A core Forth interpreter is well within Malgo's capabilities. Estimated size:
~1000–1500 lines for a clean implementation covering the minimal primitive set.

---

## Recommended Implementation Plan

### Phase 1 — Token and Value Types

```malgo
data Token = TWord String | TInt Int64 | TStr String

data ForthVal = FInt Int64 | FStr String

data Word
  = Prim { action : ForthState -> ForthState }
  | Compiled { tokens : List String }
  | Immediate { action : ForthState -> ForthState }

type Stack = List ForthVal
type Dict  = Map String Word

data ForthState = ForthState
  { stack : Stack
  , rstack : List (List String)   -- return stack for colon words
  , dict : Dict
  , compiling : Bool
  , compileBuffer : List String
  }
```

### Phase 2 — Core Primitives

Implement as `Prim` entries in the initial dictionary:
`dup`, `drop`, `swap`, `over`, `+`, `-`, `*`, `/`, `mod`, `=`, `<`, `>`,
`emit`, `cr`, `.` (print top of stack).

### Phase 3 — Interpreter Loop

```malgo
def eval : ForthState -> Token -> ForthState
def eval state (TInt n) =
  -- push integer onto stack
  { state | stack = Cons (FInt n) state.stack }
def eval state (TWord w) =
  case lookupMap w state.dict {
    None -> ... -- error: unknown word
    Some word ->
      if state.compiling && not (isImmediate word)
      then -- compile: append to compileBuffer
        { state | compileBuffer = appendList state.compileBuffer [w] }
      else -- interpret: execute
        execWord state word
  }
```

### Phase 4 — Colon Definitions

Handle `:` (start compile mode, capture name) and `;`
(stop compile, install `Compiled compileBuffer` into dictionary).

### Phase 5 — Control Structures

Implement `IF THEN ELSE BEGIN UNTIL` as `Immediate` words that patch jump offsets
(or use a structured representation instead of raw addresses).

### Phase 6 — REPL

```malgo
def repl : ForthState -> ()
def repl state =
  let line = getLine ();
  let tokens = tokenize line;
  let state' = foldl eval state tokens;
  repl state'
```

---

## References

- [Easy Forth (interactive tutorial)](https://skilldrick.github.io/easyforth/)
- [Implementing Forth in Go and C — Eli Bendersky](https://eli.thegreenplace.net/2025/implementing-forth-in-go-and-c/)
- [Write a Forth in Haskell series](https://glitchbra.in/post/write-a-forth-in-haskell-intro/)
- [harrorth — Haskell Forth tutorial](https://github.com/ezag/harrorth)
- [hforth — bare-bones Haskell Forth](https://github.com/jrp2014/hforth)
- [eForth overview](https://chochain.github.io/eforth/)
- [ANS Forth standard words](https://forth-standard.org/standard/words)
- [Malgo runtime builtins](../../runtime/malgo/Builtin.mlg)
- [Malgo Map implementation](../../runtime/malgo/Map.mlg)
- [Malgo self-hosted compiler](../../runtime/malgo/compiler/)
