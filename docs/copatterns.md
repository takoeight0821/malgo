# Copatterns in Malgo

## What Are Copatterns?

Copatterns are a lightweight syntax for describing objects, streams, and other coinductive data structures. They are the dual of regular patterns, commonly used in dependently typed languages like Agda.

## Theory and Foundation

**Duality with Patterns:**
- **Patterns** decompose inputs by case analysis on constructors (focus on how data is built)
- **Copatterns** define outputs by specifying observations/projections (focus on how data is observed)

**Theoretical Foundation:**
- Patterns align with algebraic data types and initial algebras
- Copatterns align with codata and final coalgebras
- Based on coinduction and corecursion principles
- Must satisfy productivity instead of termination

**Key Differences:**
- **Recursion discipline:** Patterns require structural decrease; copatterns require guarded corecursion
- **Evaluation:** Patterns consume data; copatterns produce observations
- **Definition style:** Patterns branch on constructors; copatterns branch on which observation is requested

## Benefits

1. **Infinite Data Structures:** Naturally specify infinite structures (streams, trees) through finite observations
2. **Modularity:** Define one observable at a time, mirroring interfaces and objects
3. **Productivity:** Ensures productive definitions for coinductive data
4. **Interactive Systems:** Model reactive systems and objects defined by their responses to observations

## Copatterns in Malgo

Malgo supports copatterns for defining infinite data structures and objects. Here are examples:

### Fibonacci Stream

```malgo
def fib = {
  #.head -> 1,
  #.tail.head -> 1,
  #.tail.tail -> zipWith(addInt32, fib, fib.tail)
}
```

This defines an infinite Fibonacci sequence where:
- The first element (`head`) is 1
- The second element (`tail.head`) is 1
- The rest (`tail.tail`) is computed by adding corresponding elements of `fib` and `fib.tail`

### Higher-Order Stream Operations

```malgo
def zipWith = {
  #(f, xs, ys).head -> f(xs.head, ys.head),
  #(f, xs, ys).tail -> zipWith(f, xs.tail, ys.tail)
}
```

This defines a generic `zipWith` function that:
- Takes a function `f` and two streams `xs` and `ys`
- Applies `f` to the heads of both streams for the result's head
- Recursively applies `zipWith` to the tails for the result's tail

## More Examples

### Constant Stream

```malgo
def ones = {
  #.head -> 1,
  #.tail -> ones
}
```

This creates an infinite stream of ones, where each tail is the same stream.

### Stream Map Function

```malgo
def mapStream = {
  #(f, xs).head -> f(xs.head),
  #(f, xs).tail -> mapStream(f, xs.tail)
}
```

This applies a function to every element of a stream.

### Natural Numbers Stream

```malgo
def nats = {
  #.head -> 0,
  #.tail -> mapStream(addInt32(1), nats)
}
```

This generates the sequence 0, 1, 2, 3, ...

## Syntax Overview

In Malgo, copatterns use the `#` symbol followed by projection patterns:

- `#.field` - Access a field projection
- `#(args).field` - Access a field with function arguments
- `#.field1.field2` - Chain multiple projections

Each copattern clause defines how the object responds to a specific observation, making it ideal for:
- Infinite data structures (streams, sequences)
- Object-oriented style programming
- Reactive systems and processes
- Lazy evaluation scenarios

## Comparison with Regular Patterns

| Aspect | Regular Patterns | Copatterns |
|--------|------------------|------------|
| Purpose | Decompose input data | Define output behavior |
| Focus | How data is constructed | How data is observed |
| Recursion | Structural decrease required | Productivity required |
| Use case | Finite data analysis | Infinite data generation |
| Evaluation | Consume existing data | Produce data on demand |

## AST for Copatterns (`CoPat`)

The core of the design is a recursive `CoPat` data type that mirrors the structure of expressions. It is defined in `lean/Malgo/Syntax.lean` as follows:

```haskell
data CoPat x
  = HoleP (XHoleP x)
  | ApplyP (XApplyP x) (CoPat x) (Pat x)
  | ProjectP (XProjectP x) (CoPat x) Text
```

- **`HoleP`**: Represents the starting point of a copattern, denoted by `#`.
- **`ApplyP`**: Represents the application of a copattern to a regular pattern. For example, in `#.show(x)`, `ApplyP` captures the application of `#.show` to the pattern `x`. The second argument is `Pat x`, not `CoPat x`, because arguments are standard patterns.
- **`ProjectP`**: Represents a field projection. For example, in `#.tail.head`, two `ProjectP` nodes are nested.

This structure allows for parsing complex, chained copatterns like `#(b).ifThen(t).ifElse(e)` into a well-defined tree of applications and projections.

### `Codata` Expression

A new constructor is added to the main `Expr` type to represent a complete copattern-based definition:

```haskell
data Expr x
  = ...
  | Codata (XCodata x) [(CoPat x, Expr x)]
  ...
```

- **`Codata`**: Holds a list of clauses, where each clause is a pair of a `CoPat` (the left-hand side) and an `Expr` (the right-hand side).

### Parsing Strategy

The parser in `lean/Malgo/Parser/CStyle.lean` is updated to build this AST:

1.  A `pCoPat` parser handles the left-hand side of a clause. It starts by parsing the `#` hole and then iteratively applies suffixes for projections (`.field`) and applications (`(pattern)`).
2.  A `pBlock` parser, responsible for `{...}` blocks, is modified to first `try` parsing the content as a `Codata` expression. If it fails (i.e., no `#` is found), it falls back to parsing it as a regular function or record.

This design ensures that the powerful and flexible copattern syntax is robustly parsed into a structured AST, ready for subsequent desugaring and compilation passes.