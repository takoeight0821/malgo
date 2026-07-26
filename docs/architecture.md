# Malgo Interpreter Architecture

## 📁 Module Overview

Malgo’s source tree is organized by compiler phases and core abstractions:

- **Syntax & AST**:
  - `Malgo.Syntax`, `Malgo.Syntax.Extension`: Core AST types, phase-indexed with type families for extensibility.
- **Parsing**:
  - `Malgo.Parser`, `Malgo.Parser.Pass`: Lexing and parsing, producing the initial AST.
- **Renaming**:
  - `Malgo.Rename.Pass`, `Malgo.Rename.RnEnv`, `Malgo.Rename.RnState`: Name resolution and desugaring.
- **Type Inference**:
  - `Malgo.Infer.Pass`, `Malgo.Infer.TypeRep`, `Malgo.Infer.TcEnv`: Type checking, kind inference, and type environment management.
- **Refinement**:
  - `Malgo.Refine.Pass`, `Malgo.Refine.RefineEnv`: Cleans up and statically annotates the AST.
- **Intermediate Representations (IRs)**:
  - `Malgo.Sequent.Fun`, `Malgo.Sequent.Core.Full`, `Malgo.Sequent.Core.Flat`, `Malgo.Sequent.Core.Join`: Multiple IRs for lowering, optimization, and codegen.
- **Evaluation**:
  - `Malgo.Sequent.Eval`: Interpreter for the final IR.
- **Driver**:
  - `Malgo.Driver`: Orchestrates the full pipeline, from parsing to evaluation.
- **Pass Management**:
  - `Malgo.Pass`: Abstracts compiler passes for modularity and error handling.
- **Other**:
  - `Malgo.Interface`, `Malgo.Module`, `Malgo.Monad`, etc.: Module system, effectful monad stack, and project infrastructure.

**Pipeline Orchestration**:

`lean/Malgo/Driver.lean` is the main entry point. It coordinates the pipeline as follows:

```
source file
   ↓
ParserPass → RenamePass → [InferPass] → [RefinePass]
   ↓           ↓             ↓             ↓
 AST    →  Renamed AST  → [Typed AST] → [Refined AST]
   ↓
ToFunPass → ToCorePass → FlatPass → JoinPass
   ↓           ↓           ↓         ↓
 Fun IR  →  Core IR   → Flat IR → Join IR
   ↓
EvalPass (Interpreter)
```

**Note**: InferPass and RefinePass (shown in brackets) can be skipped for evaluation purposes. The ToFunPass can operate directly on the renamed AST, allowing for faster compilation when type checking is not required.

---

## 🔄 Evaluation Pipeline and IR Flow

### Step-by-Step Transformation

1. **Parsing** (`ParserPass`):

   - Reads source code, produces a phase-indexed AST (`Module (Malgo NewParse)`).

2. **Renaming** (`RenamePass`):

   - Resolves names, desugars, produces `Module (Malgo Rename)`.

3. **Type Inference** (`InferPass`) - _Optional for evaluation_:

   - Infers types/kinds, annotates AST, produces `Module (Malgo Infer)`.
   - Can be skipped when type safety guarantees are not required for evaluation.

4. **Refinement** (`RefinePass`) - _Optional for evaluation_:

   - Cleans up AST, removes syntactic sugar, produces `Module (Malgo Refine)`.
   - Can be skipped when evaluation can work directly with the renamed AST.

5. **IR Lowering**:

   - **ToFunPass**: Lowers AST to a functional IR (`Sequent.Fun.Program`). Can operate on either refined AST (after RefinePass) or directly on renamed AST (skipping InferPass and RefinePass).
   - **ToCorePass**: Converts Fun IR to a sequent-style Core IR (`Sequent.Core.Full.Program`).
   - **FlatPass**: Flattens Core IR, removing nested computations (`Sequent.Core.Flat.Program`).
   - **JoinPass**: Normalizes control flow, producing the final IR (`Sequent.Core.Join.Program`).

6. **Evaluation** (`EvalPass`):
   - Interprets the final Join IR (`Sequent.Core.Join.Program`).

### Optional Type Checking and Refinement

The Malgo compiler supports two compilation modes:

**Full Pipeline Mode** (with type checking):

```
Parse → Rename → Infer → Refine → ToFun → ToCore → Flat → Join → Eval
```

**Fast Evaluation Mode** (skipping type checking):

```
Parse → Rename → ToFun → ToCore → Flat → Join → Eval
```

**When to Skip Type Checking:**

- **Rapid prototyping**: Quick evaluation without type safety guarantees
- **Trusted code**: When the code is known to be type-correct
- **Performance**: Faster compilation when type checking overhead is not desired
- **Testing**: Evaluating intermediate representations directly

**Implications of Skipping Type Checking:**

- **No type safety**: Runtime errors may occur for type mismatches
- **Faster compilation**: Eliminates type inference and constraint solving overhead
- **Direct AST lowering**: ToFunPass operates directly on `Module (Malgo Rename)` instead of `Module (Malgo Refine)`

### IRs in Detail

| IR Stage       | Defined In             | Purpose / Abstraction                | Structure-Preserving? | Phase Role            |
| -------------- | ---------------------- | ------------------------------------ | --------------------- | --------------------- |
| Fun IR         | `Sequent/Fun.lean`       | Functional, simple, close to AST     | Mostly                | Initial lowering      |
| Core IR (Full) | `Sequent/Core/Full.lean` | Sequent calculus, explicit control   | Partially             | Pre-optimization      |
| Flat IR        | `Sequent/Core/Flat.lean` | No nested computations, flat control | Lossy (flattens)      | Simplifies codegen    |
| Join IR        | `Sequent/Core/Join.lean` | Normalized, explicit join points     | Lossy                 | Final, for evaluation |

- **Transformations** are mostly structure-preserving until flattening and joinification, which introduce explicit control flow and may lose some high-level structure.
- Each IR is tied to a phase: optimization, analysis, or interpretation.

---

## 📦 Intermediate Representation Design

- **Type Annotations**:
  - Typed ASTs and IRs carry type information, especially after inference.
- **Functional & Effect Modeling**:
  - IRs are purely functional; effects are modeled in the interpreter, not the IR.
- **Typeclasses**:
  - `Pretty`, `ToSExpr`, and others for pretty-printing, serialization, and lowering.
- **Polymorphism**:
  - Parametric types and fixpoint encodings are used for extensibility.
- **Variable Binding**:
  - Named binders (`Id`, `Name`), not De Bruijn indices.

**Example: IR Definition (simplified)**

```haskell
-- From Malgo.Sequent.Core.Join
data Producer where
  Var      :: Range -> Name -> Producer
  Literal  :: Range -> Literal -> Producer
  Lambda   :: Range -> [Name] -> Statement -> Producer
  -- ...

data Statement where
  Cut :: Producer -> Name -> Statement
  Join :: Range -> Name -> Consumer -> Statement -> Statement
  -- ...
```

---

## 🧠 Language Semantics and Evaluation

- **Semantics**:
  - Statically typed, functional language with first-class functions, pattern matching, and algebraic data types.
- **Evaluation**:
  - IR-based, not direct AST-walking.
  - Big-step semantics over the Join IR (`Sequent.Core.Join.Program`).
  - Tail-call optimization and closures are supported.
- **Interpreter**:
  - `Malgo.Sequent.Eval` implements the interpreter, using an explicit environment and handler abstraction.

---

## 🛠️ Extensibility and Implementation Notes

- **Adding Syntax/Semantics**:
  - Extend the AST and add new passes or extend existing ones.
- **Adding IR Stages**:
  - Implement new passes using the `Pass` typeclass and plug into the pipeline.
- **Backends**:
  - The modular IR pipeline allows for alternative backends (e.g., codegen, REPL).
- **Modularity**:
  - Each phase is isolated in its own module, with clear input/output types.
  - Pure logic (e.g., transformations) is separated from impure logic (e.g., IO, diagnostics).

---

## 🔗 Composition and Effects

- **Monad**:
  - `MalgoM = ReaderT Ctx (EIO CompileError)` (`lean/Malgo/Monad.lean`). `Ctx`
    carries the flags, the workspace, and `IO.Ref`s for the unique supply,
    feature flags, and pragmas.
  - A pass that throws its own error type is wrapped into the uniform
    `CompileError` by `Malgo.Pass.wrapError`.
- **Testability**:
  - Golden tests for each phase and IR transformation, plus non-golden gates
    (inference, IR invariants, Zig-backend linearity) in `lean/Test/Main.lean`.
  - `#guard` for build-time assertions on pure functions.

---

## 💡 Advanced Topics

- **Type-Level Programming**:
  - Phase-indexed `Syntax` — the AST is parameterized by compilation phase, so
    a field only present after renaming cannot be read before it.
- **Debugging & Visualization**:
  - Pretty-printing via `Malgo.Doc` (a port of `prettyprinter`'s `layoutSmart`),
    S-expression output via `sShow`.
  - `malgo debug-trace` renders every stage of the pipeline — see
    [`met-tool.md`](met-tool.md).

---

## 📊 Example: Pipeline Flow

```text
Source (.mlg)
  │
  ▼
ParserPass
  │
  ▼
RenamePass
  │
  ▼
InferPass
  │
  ▼
RefinePass
  │
  ▼
ToFunPass → ToCorePass → FlatPass → JoinPass
  │
  ▼
EvalPass (Interpreter)
```

---

## 📄 Output and Debugging

- Use `pShow` for debugging, `pretty` for user output, and `sShow` for AST/IR dumps.
- Build: `mise run build`
- Test: `mise run test`

---

This architecture enables robust, modular, and extensible language implementation, with a clear separation of concerns and a strong focus on IR-driven compilation and interpretation.

<!--
Prompt for this document:

```

You are analyzing a Haskell codebase that implements a programming language interpreter.

Your goal is to write a detailed and technically accurate `architecture.md` document. This document should give a high-level architectural overview while also highlighting how Intermediate Representations (IRs) are defined and used throughout the interpreter pipeline.

> 🧭 Begin by exploring `lean/Malgo/Driver.lean`, which acts as the central coordination point. It provides an entry point for understanding the overall evaluation flow and module interactions.

Please structure your documentation with the following sections:

---

### 📁 Module Overview

- Summarize the layout of the Haskell source tree.
- Identify the roles of key modules (e.g., Lexer, Parser, AST, TypeChecker, IR-related modules, Evaluator).
- Explain how `lean/Malgo/Driver.lean` orchestrates the pipeline across these modules.

---

### 🔄 Evaluation Pipeline and IR Flow

- Describe the step-by-step transformation from source code to result:
  `source code → tokens → AST → IR(s) → evaluation or code generation`.

- Explain:
  - **What intermediate representations are used** (e.g., core IR, typed IR, CPS, ANF).
  - **Where each IR is defined** (e.g., `IR/Core.hs`, `IR/Typed.hs`).
  - **Why each IR exists** – what abstraction or optimization it supports.
  - **How the system transforms from one IR to another**, possibly through distinct passes or monadic phases.

- If multiple IR stages exist, describe their order and role:
  - Is the transformation lossy or structure-preserving?
  - Is each IR tied to a phase (e.g., optimization, interpretation, analysis)?

---

### 📦 Intermediate Representation Design

- Describe the design of the IR types:
  - Use of GADTs or DataKinds?
  - Are terms annotated with type information?
  - Is the IR purely functional, or does it model effects explicitly?

- Detail the usage of common Haskell features in IR modeling:
  - Typeclasses (e.g., `Pretty`, `Eval`, `Lowerable`)
  - Parametric polymorphism or fixpoint types
  - Explicit scoping / variable binding (e.g., De Bruijn indices, named binders)

---

### 🧠 Language Semantics and Evaluation

- Outline the semantics of the interpreted language:
  - Is it statically or dynamically typed?
  - Functional, imperative, or hybrid?
  - Tail-call optimization or first-class functions?

- Clarify whether the evaluation is:
  - AST-walking or IR-based
  - Small-step or big-step semantics
  - Conducted over the final IR or the original AST

---

### 🛠️ Extensibility and Implementation Notes

- Explain how the architecture supports future extensions:
  - Adding new syntax or semantic constructs
  - Adding optimization passes or new IR stages
  - Plugging in backends (e.g., REPL, bytecode interpreter, codegen)

- Comment on modularity and isolation:
  - How pure and impure logic are separated
  - How concerns like parsing, evaluation, and diagnostics are decoupled

---

### 🔗 Composition and Effects

- Describe how data and control flow are managed:
  - Use of monad transformers (e.g., `ExceptT`, `ReaderT`, `StateT`)
  - Custom monads or effect systems (e.g., `MonadEval`, `MonadIR`)
  - Explicit IO boundaries

- Comment on testability:
  - Property-based testing, golden tests, staged testing of IR transformations

---

### 💡 Advanced Topics (optional)

- Use of Haskell-specific features:
  - Template Haskell
  - Type-level programming
  - Rewrite rules or fusion strategies

- Debugging tools or visualization of IRs (e.g., DOT graph output)

---

### 📄 Output Requirements

- Use structured, professional Markdown.
- Include diagrams or ASCII-based flowcharts where relevant.
- Insert short, illustrative Haskell snippets when helpful.
- Target developers familiar with Haskell but not yet with this codebase.

---

The final `architecture.md` should serve as both a bird’s-eye view of the system and a technical reference on the interpreter’s internal architecture and transformation pipeline—especially its use of intermediate representations.
```
-->
