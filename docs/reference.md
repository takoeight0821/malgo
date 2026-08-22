# Malgo Language Reference

**Version:** June 2025

This document describes the syntax and semantics of the Malgo programming language as currently implemented. It is based on the parser (`lean/Malgo/Parser.lean`) and the test suite (`test/testcases/malgo/`). For further details, consult the parser source and test cases directly.

---

## 1. Lexical Structure

### Comments

- **Single-line:** Start with `--` and continue to end of line.
- **Multi-line:** Enclosed in `{- ... -}`. Multi-line comments can be nested.

### Identifiers

- **General:** Identifiers start with a letter or underscore, followed by letters, digits, underscores, or `#`.
- **Lowercase identifiers:** Used for variables and functions (e.g., `myVar`, `foo_bar`).
- **Uppercase identifiers:** Used for types, constructors, and modules (e.g., `List`, `Int32#`).
- **Operators:** Sequences of operator characters (`+-*/\%=><:;|&!#.`), e.g., `+`, `==`, `<|`.

### Reserved Words

```
class def data exists forall foreign impl import infix infixl infixr let type module with
```

### Reserved Operators

```
=> = : | -> ; . , ! #| |#
```

### Literals

- **Integer:** `123`, `42L`, `7l` (Int32 by default, Int64 with `L`/`l` suffix)
- **Float/Double:** `3.14` (Double), `2.71f`/`2.71F` (Float)
- **Char:** `'a'`, `'\n'`
- **String:** `"hello"`
- **Boxed/Unboxed:** Add `#` suffix for unboxed, e.g., `1#`, `"foo"#`

---

## 2. Modules and Imports

### Module Definition

Modules are defined implicitly by file structure. Explicit module declarations are not required, but can be written as:

```malgo
module {..} = import "path/to/Module.mlg"
module MyMod = import "path/to/Module.mlg"
module {foo, bar, (+)} = import "path/to/Module.mlg"
```

### Import Declaration

- **Import all:** `module {..} = import "..."`
- **Import selected:** `module {foo, bar} = import "..."`
- **Import with alias:** `module MyMod = import "..."`
- **Import by module name:** `module Prelude = import "..."`

---

## 3. Declarations

### Data Type Declaration

```malgo
data List a = Nil | Cons a (List a)
data Int = Int# Int64#
```

### Type Synonym Declaration

```malgo
type MyTuple a = (a, a)
type MyString = String
type S = T
```

### Infix Declarations

```malgo
infixl 6 (-)
infixr 2 (<|>)
infix 4 (==)
```

### Foreign Declarations

```malgo
foreign import malgo_print_string : String# -> ()
foreign import malgo_int64_t_to_string : Int64# -> String#
```

### Value Signatures and Definitions

```malgo
def fact : Int64 -> Int64
def fact = { n -> factAcc n 1L }

def (==) : Int64 -> Int64 -> Bool
def (==) = { x y -> eqInt64 x y }
```

---

## 4. Expressions

### Basic Expressions

- **Literals:** `1`, `3.14`, `'a'`, `"foo"`
- **Variables:** `x`, `myVar`
- **Function application:** `f x y`, `add 1 2`

### Functions and Pattern Matching

```malgo
def even = {
  0 -> True,
  n -> odd (subInt32 n 1),
}

def add = { x y -> x + y }
```

### Let Bindings and With Statements

```malgo
let x = 1;
let y = 2;
printString (toStringInt32 (addInt32 x y));

with ctx = expr
with expr
```

### Structured Data

- **Records:** `{ .x -> 1, .y -> 2 }`
- **Lists:** `[1, 2, 3]`, `[]`
- **Tuples:** `(a, b)`, `(1, "foo")`

---

## 5. Patterns

- **Variable pattern:** `x`
- **Literal pattern:** `1`, `'a'`, `"foo"`
- **Constructor pattern:** `Cons x xs`, `Int# x`
- **Tuple pattern:** `(x, y)`
- **Record pattern:** `{ .x -> x, .y -> y }`
- **List pattern:** `[x, y, z]`, `[]`

Example:

```malgo
{ Cons (Int64# x) xs -> ...,
  Nil -> ... }
```

---

## 6. Types

- **Function type:** `a -> b`
- **Type application:** `List Int`
- **Tuple type:** `(Int, String)`
- **Record type:** `{ x : Int, y : Int }`
- **Block type:** `{ a }`
- **Type variable:** `a`, `b`

---

## 7. Operator Precedence and Associativity

Declare operator precedence and associativity using:

```malgo
infixl 6 (+)
infixr 2 (<|>)
infix 4 (==)
```

- `infixl`: left-associative
- `infixr`: right-associative
- `infix`: non-associative
- Precedence: integer (higher binds tighter)

---

## 8. Examples

### Factorial Function

```malgo
def fact = { n -> factAcc n 1L }
def factAcc = { n acc -> if (n == 0L) { acc } { factAcc (n - 1L) (n * acc) } }
def main = { fact 5L |> toStringInt64 |> putStrLn }
```

### Data Type and Foreign Declarations

```malgo
data Int = Int# Int64#
foreign import malgo_int64_t_to_string : Int64# -> String#
```

### Record Construction and Manipulation

```malgo
type Point2D = { x : Int32, y : Int32 }
def zero2D : Point2D
def zero2D = { .x -> 0, .y -> 0 }
def x2D : Point2D -> Int32
def x2D = { { .x -> x, .y -> _ } -> x }
```

### Use of `with` Statements

```malgo
with ctx = expr
with expr
```

### Pattern Matching

```malgo
def even = {
  0 -> True,
  n -> odd (subInt32 n 1),
}
```

### Type Synonym Definition and Usage

```malgo
type MyTuple a = (a, a)
def hello : MyTuple String
def hello = ("hello", "world")
```

---

For the most up-to-date syntax and semantics, refer to `lean/Malgo/Parser.lean` and the test cases in `test/testcases/malgo/`.

<!--
Prompt:

You are tasked with generating a comprehensive reference document (`reference.md`) for the Malgo programming language. This document must describe the **syntax** and **semantics** of Malgo as implemented in the current parser and test suite (as of June 2025).

⚠️ Important:
- **Do not include any planned or speculative syntax features** from `todo.md` or other design proposals.
- Base your descriptions **solely on the current implementation** and the test cases.
- Refer to supporting documentation in the `docs/` directory (e.g., `docs/syntax.md`) for additional details.
- For the most up-to-date syntax, consult `lean/Malgo/Parser.lean` and the test cases in `test/testcases/malgo/`.

---

### Document Structure Guidelines

Your generated `reference.md` should be structured in Markdown and include the following sections:

#### 1. Lexical Structure
- **Comments:** Describe single-line comments (using `--`) and multi-line comments (using nested `{- ... -}`).
- **Identifiers:** Specify the patterns for identifiers, including distinctions between uppercase (types, constructors, modules) and lowercase (variables, functions) identifiers.
- **Reserved Words:** List the reserved keywords (e.g., `class`, `def`, `data`, `let`, etc.).
- **Literals:** Provide details on integer, float/double, char, string, boxed/unboxed literals, including example forms.

#### 2. Modules and Imports
- **Module Definition:** Outline how modules are defined (e.g., syntax for module declarations).
- **Import Declaration:** Explain the different import syntaxes (full import, selected import, aliasing) with examples.

#### 3. Declarations
- **Data Type Declaration:** Describe the syntax for defining algebraic data types.
- **Type Synonym Declaration:** Specify how type synonyms are declared.
- **Infix Declarations:** Explain how operators are declared with associativity and precedence.
- **Foreign Declarations:** Detail the syntax for foreign function interfaces.
- **Value Signatures and Definitions:** Provide examples of defining functions and values with explicit types.

#### 4. Expressions
- **Basic Expressions:** Include literal values, variables, and function applications.
- **Functions and Pattern Matching:** Describe lambda expressions, functions defined with pattern matching, with correct examples.
- **Let Bindings and With Statements:** Show how local bindings (`let`) and context injections (`with`) are expressed.
- **Structured Data:** Explain the syntax for records, lists, and tuples.

#### 5. Patterns
- Define the different forms of patterns, such as variable, literal, constructor, tuple, record, and list patterns with examples.

#### 6. Types
- **Type Expressions:** Document the syntax for function types, type application, tuples, records, blocks, and type variables.

#### 7. Operator Precedence and Associativity
- Describe how operator precedence and associativity are declared (via `infix`, `infixl`, `infixr`).

#### 8. Examples
- Provide illustrative examples that cover key language constructs, such as:
  - A factorial function implementation.
  - Data type and foreign declarations.
  - Record construction and manipulation.
  - Use of `with` statements.
  - Pattern matching examples.
  - A type synonym definition with usage.

---

### Additional Output Requirements
- Use clear, concise technical language in Markdown.
- Ensure all syntax examples precisely reflect the **current implementation**.
- Direct readers to the actual parser source (`lean/Malgo/Parser.lean`) and testcases (`test/testcases/malgo/`) for verification and further details.
- Target the document at readers with a background in functional programming and interest in Malgo’s concrete implementation details.

✅ Your goal is to produce a clear and authoritative language reference that fully documents the **current**, implemented syntax and semantics of Malgo.
-->
