# Malgo C-Style Syntax Guide

Malgo's C-style syntax is a notation that uses C-like parentheses, braces, and comma separation for function application, data definitions, pattern matching, and more. Unlike the standard ML-style syntax, it aims to improve readability and expressiveness in some cases.

## Empty Arguments in C-Style Syntax

In C-Style Syntax, it is now allowed to write function calls with empty arguments, such as `f()`. Previously, this was represented by passing an empty tuple as an argument (e.g., `f({})`).

With this design, empty argument calls in C-Style Syntax are directly lowered to the Core language's `Invoke` construct. This makes the intent clearer and matches the semantics of zero-argument invocation in the Core IR.

**Design Notes:**
- `f()` is valid and means a function call with no arguments.
- This is lowered to `Invoke` in Core, not a tuple application.
- The previous workaround using an empty tuple (`f({})`) is no longer necessary for zero-argument calls.
- The parser and type checker should distinguish between `f()` (no arguments) and `f({})` (single empty tuple argument).

**Example:**

```malgo
def hello = { () -> "Hello" }
def main = hello()
```

This will be lowered to a Core `Invoke` of `hello` with no arguments.

## Main Features

- **Function Application**: Arguments are passed with parentheses and commas, e.g., `f(x, y)`.
- **Data Definition**: Type parameters and constructor arguments use parentheses and commas, e.g., `data Foo(a, b) = Bar(a) | Baz(b)`.
- **Type Synonym**: `type Pair(a, b) = { fst: a, snd: b }`
- **Record/Tuple**: Expressed as `{ x, y }` or `{ .x -> 1, .y -> 2 }`.
- **List**: `[1, 2, 3]`
- **Pattern Matching**: Multiple patterns can be written comma-separated, e.g., `def f = { (x, y) -> x + y, (z) -> z }`.
- **Copatterns**: Supports copattern syntax starting with `#`, e.g., `def g = { #.field -> ... }`.
- **Infix Operator Declaration**: Specify operator precedence and associativity, e.g., `infixl 5 (+)`.
- **Type Annotation**: Explicitly annotate types, e.g., `def f : Int32 -> Int32`.
- **Import Syntax**: Import entire modules or parts, e.g., `module {..} = import "..."`.

## Examples

```malgo
-- Function definition
def add = { (x, y) -> x + y }

-- Data type definition
data Either(a, b) = Left(a) | Right(b)

-- Record
def person = { .name -> "Alice", .age -> 30 }

-- Tuple
def point = { 1, 2 }

-- List
def nums = [1, 2, 3]

-- Copattern
def show = { #.field -> ... }

-- Infix declaration
infixl 6 (+)
```
