# Complete Malgo-in-Malgo Implementation

Date: 2026-05-17

## Context

The current pull request adds a Malgo-written lexer, parser, tree-walking evaluator, runtime helpers, and a self-hosted golden-test workflow. That is a useful bootstrap seed, but it is not yet a complete Malgo implementation and two reviewed defects make the new workflow misleading:

- `runtime/malgo/compiler/Eval.mlg` handles `getContents` by returning the constant string `"Hello\n"`, while `runtime/malgo/compiler/Main.mlg` and `scripts/selfhost-golden.sh` expect source text to arrive on stdin. The self-hosted golden job therefore evaluates a constant input instead of each testcase.
- `runtime/malgo/compiler/AST.mlg` represents every import as `DeclImport String`; `runtime/malgo/compiler/Parser.mlg` skips the import target, and `runtime/malgo/compiler/Eval.mlg` imports everything unqualified. This erases the difference between `module {..} = import ...`, `module Prelude = import ...`, and `module {foo, (<|)} = import ...`, breaking existing cases such as `UseModule.mlg` and `TestExplicitModule.mlg`.

The host Haskell implementation is the compatibility target. Its pipeline is:

```text
ParserPass
  -> RenamePass
  -> ElaboratePass
  -> InferPass
  -> RefinePass
  -> ToFunPass
  -> ToCorePass
  -> FlatPass
  -> JoinPass
  -> EvalPass or SchemePass
```

The Malgo implementation currently has only:

```text
tokenize : String -> Either String (List Token)
parseModule : List Token -> Either String Module
evalModule : Env -> Module -> Either String Env
runMain : Env -> Either String Value
```

This design defines "complete Malgo in Malgo" as a Malgo-written implementation that can parse, resolve modules/imports, type-check or reject programs consistently with the host compiler, lower to the same semantic core, and evaluate or compile the existing Malgo test suite with matching observable behavior. The Haskell implementation may remain as the stage-0 runner during bootstrap, but not as the source of language semantics.

### Status update after milestone-zero execution

The bootstrap implementation is now materially ahead of the state described above:

- `getContents` now reads real stdin, and the host `malgo eval` path forwards trailing program arguments into the evaluated Malgo program.
- Import targets are preserved in the self-hosted AST/parser/evaluator, including qualified imports and explicit import lists.
- Imported values preserve module scope via `VScoped`, including partial application and closure/cocase creation.
- The self-hosted evaluator now has explicit loader state for normalized relative imports, import caching, and cycle diagnostics.
- The self-hosted golden runner now works correctly on macOS and passes the current 70-case suite.

Those fixes make the bootstrap trustworthy, but they do **not** make the implementation complete. The remaining gap is architectural: the self-hosted code is still a surface-syntax tree-walking interpreter with raw `String` names, while the host compiler is a phase-separated compiler built around renamed IDs, interfaces, optional typing, IR lowering, and backend selection.

## Design Choices

### 1. Treat the two reviewed issues as milestone-zero blockers

The stdin and import bugs must be fixed before expanding the implementation. Otherwise the self-hosted golden job can report progress while exercising the wrong input or wrong namespace semantics.

### 2. Preserve import shape in the AST

Replace `DeclImport String` with a structured import declaration:

```malgo
data ImportTarget
  = ImportAll
  | ImportQualified String
  | ImportOnly(List ImportName)

data ImportName
  = ImportValue String
  | ImportOperator String

data Decl
  = DeclDef(String, Expr)
  | DeclData(String, List String, List ConDecl)
  | DeclImport(ImportTarget, String)
  | DeclForeign(String, String)
```

This lets the evaluator and future renamer distinguish:

- `module {..} = import "path"`: import exported names into the unqualified scope.
- `module Prelude = import "path"`: make exported names available as `Prelude.name` only.
- `module {foo, (<|)} = import "path"`: import only listed names unqualified.

### 3. Introduce an explicit compiler state instead of hiding module state in `Env`

Module loading needs state that is not a user-visible binding environment:

```malgo
data LoadStatus = Loading | Loaded ModuleExports
data CompilerState = CompilerState(
  List ModuleCacheEntry,
  List String,   -- import stack for cycle diagnostics
  String         -- current source directory
)
```

The module cache should store parsed/evaluated exports by normalized module identity. This avoids infinite recursion on import cycles, prevents repeated evaluation of deterministic imports, and keeps internal bookkeeping out of program scope.

### 4. Separate language phases instead of growing the tree-walking evaluator indefinitely

The current evaluator is useful for early bootstrapping, but a complete implementation should mirror the host architecture with Malgo modules for each phase:

```text
runtime/malgo/compiler/
  Diagnostic.mlg
  Source.mlg
  Token.mlg
  Lexer.mlg
  SurfaceAST.mlg
  Parser.mlg
  ModuleLoader.mlg
  Rename.mlg
  Type.mlg
  Infer.mlg
  Elaborate.mlg
  FunIR.mlg
  ToFun.mlg
  CoreIR.mlg
  ToCore.mlg
  Flat.mlg
  Join.mlg
  Eval.mlg
  Scheme.mlg
  Main.mlg
```

The current `AST.mlg`, `Parser.mlg`, and `Eval.mlg` can be migrated incrementally, but phase boundaries should become explicit so work can proceed in parallel.

### 5. Use conformance tests as the implementation contract

The existing Hspec and golden fixtures are the best executable specification. Every task below should add focused Malgo-in-Malgo tests and then graduate to the full `scripts/selfhost-golden.sh` run. The self-hosted workflow should report unsupported features explicitly rather than silently passing through skipped or constant inputs.

### 6. Treat rename/interfaces as the first true semantic phase

The host compiler does not execute surface syntax directly. It first parses into a phase-indexed AST, then performs rename/interface loading/desugaring before any later lowering or evaluation. That rename phase is not a cosmetic pass: it resolves module imports, generates stable identifiers, applies fixity, lowers list/boxed literals, and produces exported interfaces. A complete Malgo-in-Malgo compiler therefore needs a dedicated `Rename.mlg` + `Interface.mlg` layer instead of extending the tree-walking evaluator with more ad-hoc name lookups.

### 7. Preserve the host module identity model

The host compiler distinguishes named modules such as `Builtin` from artifact-path modules loaded from files. That distinction propagates into exported IDs and interface loading. The self-hosted implementation should adopt an explicit module identity type early so path-based bootstrap loading can evolve into host-compatible interface-based imports without rewriting every downstream phase.

### 8. Defer type inference from the first parity target, but preserve its inputs now

Host execution can already skip inference in some paths, so the shortest path to semantic parity is:

```text
parse -> rename/interface -> elaborate -> lower -> join-eval
```

rather than:

```text
parse -> infer -> lower -> eval
```

However, the parser must still stop throwing away type signatures, synonyms, infix declarations, and foreign types, because those are inputs to the eventual rename/infer pipeline.

### 9. Make the foreign-runtime ABI a single contract

Today the self-hosted evaluator duplicates builtin arity and primitive dispatch in large ad-hoc tables. The full implementation should instead treat `foreign import` declarations in `runtime/malgo/Builtin.mlg` as the source-level ABI and keep a single explicit mapping used by the self-hosted evaluator, the host interpreter bridge, and the Scheme backend.

## Implementation Plan

### Task 1: Fix stdin and foreign builtin plumbing

- **Goal**: Ensure `runtime/malgo/compiler/Main.mlg` receives the actual source text piped by `scripts/selfhost-golden.sh`.
- **Scope**: `runtime/malgo/compiler/Eval.mlg`, `runtime/malgo/Builtin.mlg`, `src/Malgo/Sequent/Eval.hs`, `runtime/malgo/compiler/Main.mlg`, stdin-focused tests.
- **Dependencies**: none.
- **Steps**:
  1. Remove the hardcoded `"Hello\n"` implementation of `getContents` from the self-hosted evaluator.
  2. Represent `foreign import malgo_get_contents : () -> String#` as a callable foreign value in the Malgo evaluator.
  3. Route that foreign value to the host primitive already exposed by `src/Malgo/Sequent/Eval.hs`.
  4. Keep the high-level `getContents` wrapper in `runtime/malgo/Builtin.mlg` as the source-level API.
  5. Add a regression case that pipes non-`Hello` source into `runtime/malgo/compiler/Main.mlg` and verifies the output changes with stdin.
- **Verification**: A program piped to `malgo eval runtime/malgo/compiler/Main.mlg` is tokenized, parsed, and evaluated from that exact stdin content.
- **Status**: completed in the current branch.

### Task 2: Preserve and enforce import target semantics

- **Goal**: Make `module {..}`, named modules, and explicit import lists behave like the host implementation for values, operators, and constructors.
- **Scope**: `runtime/malgo/compiler/AST.mlg`, `runtime/malgo/compiler/Parser.mlg`, `runtime/malgo/compiler/Eval.mlg`, `runtime/malgo/compiler/Value.mlg`, parser/evaluator tests.
- **Dependencies**: none.
- **Steps**:
  1. Add `ImportTarget` and `ImportName` to the AST.
  2. Parse `module {..} = import ...`, `module Name = import ...`, and explicit lists including parenthesized operators.
  3. Add a name representation for unqualified and qualified references, or encode qualified references consistently at parse time.
  4. Change module evaluation to return an export set instead of merging all definitions directly into the caller environment.
  5. Apply import targets when extending the caller environment: all exports, qualified-only aliases, or selected unqualified names.
  6. Add negative tests for unimported names and positive tests for `UseModule.mlg` and `TestExplicitModule.mlg`.
- **Verification**: Existing import-sensitive testcases produce the same output through the host evaluator and through the Malgo evaluator.
- **Status**: completed in the current branch, including missing-export validation and re-export-aware module export collection.

### Task 3: Implement module loading, cache, and cycle detection

- **Goal**: Load modules deterministically with clear errors and no repeated work or infinite recursion.
- **Scope**: new `runtime/malgo/compiler/ModuleLoader.mlg`, path helpers in `runtime/malgo/IO.mlg`, integration in `Eval.mlg` and later `Rename.mlg`.
- **Dependencies**: Task 2.
- **Steps**:
  1. Define normalized module identity for named modules such as `Builtin`/`Prelude` and artifact paths such as `../../../runtime/malgo/Either.mlg`.
  2. Track `Loading` vs `Loaded` modules in `CompilerState`.
  3. Detect cycles and report the import stack.
  4. Resolve relative imports against the importing module's directory rather than only stripping leading `../`.
  5. Cache parsed modules, exports, and evaluated module environments separately.
- **Verification**: Duplicate imports are evaluated once; import cycles fail with a diagnostic; relative paths work from nested modules.
- **Status**: partially completed in the current branch. Loader state, cache, cycle diagnostics, and relative-path normalization now exist in `Eval.mlg`, but the functionality still lives inside the evaluator instead of a dedicated `ModuleLoader.mlg` shared with future rename/interface phases.

### Task 4: Align the Malgo lexer and parser with the host surface language

- **Goal**: Parse all supported Malgo source syntax into a lossless surface AST.
- **Scope**: `Token.mlg`, `Lexer.mlg`, new or migrated `SurfaceAST.mlg`, `Parser.mlg`, parser tests.
- **Dependencies**: Task 2 for import syntax.
- **Steps**:
  1. Keep source ranges on tokens and AST nodes for diagnostics.
  2. Represent type signatures, type synonyms, foreign declarations, infix declarations, records, tuples, lists, codata/copatterns, labels/goto, and feature-gated C-style calls.
  3. Replace the current single-precedence operator parser with host-compatible precedence and associativity using parsed/imported infix metadata.
  4. Stop ignoring type annotations; preserve them for the future infer/refine phases.
  5. Add pretty/debug output only as test aids, not as semantic dependencies.
- **Verification**: Parser goldens cover every declaration and expression class used by `runtime/malgo`, `examples/malgo`, and `test/testcases/malgo`.
- **Current gap**: the parser still skips type signatures, type synonyms, and infix declarations, and it treats operators with a simplified left-associative parser rather than host-compatible fixity resolution.

### Task 5: Build a Malgo renamer and namespace model

- **Goal**: Resolve names, scopes, constructors, operators, imported interfaces, and exports consistently with the host `RenamePass`.
- **Scope**: new `Rename.mlg`, new `Interface.mlg`, `ModuleLoader.mlg`, `SurfaceAST.mlg`, renamed AST definitions.
- **Dependencies**: Tasks 2, 3, and 4.
- **Steps**:
  1. Define stable IDs that encode module, local binding, temporal binding, and external names.
  2. Generate a top-level environment before renaming declaration bodies to support recursion.
  3. Rename local patterns and expressions with lexical scope.
  4. Load imported interfaces and merge names according to `ImportTarget`.
  5. Carry infix information into renamed operator applications.
  6. Track exported identifiers and dependencies for downstream phases.
- **Verification**: Rename errors and successful renamed interfaces match host behavior for representative modules.
- **Priority note**: this is the next major semantic milestone after the bootstrap fixes. Without it, the self-hosted implementation remains an interpreter for surface syntax rather than a real host-compatible compiler.

### Task 6: Implement diagnostics and error reporting

- **Goal**: Replace plain `String` errors with structured diagnostics that can be compared and surfaced consistently.
- **Scope**: new `Diagnostic.mlg`, `Source.mlg`, updates across lexer/parser/loader/renamer/evaluator.
- **Dependencies**: can begin after Task 4 defines ranges.
- **Steps**:
  1. Define `Range`, `SourcePos`, `Severity`, and diagnostic messages.
  2. Thread diagnostics through `Either (List Diagnostic) a`.
  3. Preserve simple fail-fast helpers for early phases, but make conversion to structured diagnostics explicit.
  4. Print errors to stderr once stderr support is available; until then, keep stdout behavior testable.
- **Verification**: Missing imports, parse errors, undefined names, type errors, and runtime errors include source context.

### Task 7: Implement types, inference, refinement, and elaboration in Malgo

- **Goal**: Move from a dynamically checked interpreter to a statically checked Malgo implementation.
- **Scope**: new `Type.mlg`, `Infer.mlg`, `Refine.mlg`, `Elaborate.mlg`, typed AST modules.
- **Dependencies**: Tasks 4, 5, and 6.
- **Steps**:
  1. Model the host type language: variables, constructors, arrows, applications, tuples, records/rows, variants/rows, forall, recursive types, and bottom.
  2. Implement substitutions, free-variable analysis, generalization, instantiation, and occurs checks.
  3. Generate and solve equality/subtyping/freshness constraints.
  4. Build type environments from signatures, data declarations, constructors, foreign imports, and imported interfaces.
  5. Implement row-polymorphic record and variant unification.
  6. Port elaboration for codata/copatterns into ordinary records/functions before lowering.
  7. Add compatibility fixtures for host inference successes and failures.
- **Verification**: Programs accepted or rejected by host `InferPass`/`RefinePass` receive matching outcomes in the Malgo implementation for the conformance suite.

### Task 8: Implement IR lowering and evaluators in Malgo

- **Goal**: Make Malgo's implementation execute the same semantics as the host after type checking.
- **Scope**: new `FunIR.mlg`, `ToFun.mlg`, `CoreIR.mlg`, `ToCore.mlg`, `Flat.mlg`, `Join.mlg`, migrated `Eval.mlg`, optional `BigStepEval.mlg`.
- **Dependencies**: Task 7.
- **Steps**:
  1. Port the Fun IR and lower renamed/elaborated expressions to it.
  2. Port Core, Flat, and Join IR data types and transformations.
  3. Implement Join IR evaluation with handlers for stdin, stdout, stderr, and process exit.
  4. Keep the current tree-walking evaluator only as a bootstrap/debug backend until Join evaluation is complete.
  5. Compare intermediate IR output against host golden files where practical.
- **Verification**: Join evaluation from Malgo-produced IR matches host `EvalPass` output for all supported golden testcases.
- **Priority note**: for first full runtime parity, this task can proceed before complete inference if the parser/renamer preserve enough information and the self-hosted path follows the host's "infer optional" execution model.

### Task 9: Implement Scheme backend in Malgo

- **Goal**: Enable a path from Malgo source to generated Scheme without depending on the Haskell backend for code generation semantics.
- **Scope**: new `Scheme.mlg`, runtime support for output files, CLI integration in `Main.mlg`.
- **Dependencies**: Task 8.
- **Steps**:
  1. Port ID mangling, literal escaping, producer/consumer/statement emission, and runtime prelude generation.
  2. Add a target flag to the Malgo CLI once command-line argument support is available.
  3. Generate Scheme from Join IR and compare against host `SchemePass` behavior using runnable output rather than exact formatting.
- **Verification**: Scheme generated by Malgo runs the conformance programs with the same observable output.

### Task 10: Complete runtime, standard library, and host bridges

- **Goal**: Provide the runtime capabilities needed by the compiler without ad-hoc evaluator special cases.
- **Scope**: `runtime/malgo/Builtin.mlg`, `Prelude.mlg`, `Either.mlg`, `Map.mlg`, `IO.mlg`, possible `Set.mlg`, `List.mlg`, `String.mlg`, Haskell primitive bridge in `src/Malgo/Sequent/Eval.hs`.
- **Dependencies**: can run in parallel with Tasks 4 through 8, but final APIs must stabilize before Task 11.
- **Steps**:
  1. Make `Either`, `Map`, and `IO` real library modules with tests.
  2. Add typed wrappers for file I/O, stderr, process exit, command-line arguments, and string-to-number parsing as needed.
  3. Replace duplicated builtin arity and implementation chains with a table-like representation where the language permits it.
  4. Keep primitive names aligned with `foreign import` declarations so source-level wrappers call foreign values rather than evaluator-only magic names.
- **Verification**: Runtime modules can be imported selectively and qualified; compiler modules use library APIs rather than local copies.

### Task 11: Build the self-hosted CLI and bootstrap stages

- **Goal**: Turn the implementation into a usable Malgo-written compiler/interpreter entry point.
- **Scope**: `runtime/malgo/compiler/Main.mlg`, scripts under `scripts/`, CI workflow, docs.
- **Dependencies**: Tasks 1 through 10.
- **Steps**:
  1. Define CLI modes such as eval, check, emit-scheme, and dump-ir.
  2. Support stage-0 execution through the Haskell runner.
  3. Support stage-1 execution where the Malgo implementation evaluates user programs.
  4. Support stage-2 execution where generated Scheme runs the Malgo implementation.
  5. Document which stage is used by each script and CI job.
- **Verification**: Stage-0, stage-1, and stage-2 runs agree on the conformance suite.

### Task 12: Expand conformance tests and CI

- **Goal**: Make regressions visible and prevent unsupported behavior from being reported as success.
- **Scope**: `scripts/selfhost-golden.sh`, `.github/workflows/build.yml`, `test/testcases/malgo`, `.golden`, Hspec tests.
- **Dependencies**: Task 1 for trustworthy stdin, Task 2 for import-sensitive tests.
- **Steps**:
  1. Add focused tests for stdin, qualified imports, explicit import lists, duplicate imports, and missing imports.
  2. Add module-cycle and nested-relative-import tests.
  3. Add parser, rename, infer, and IR golden layers as those phases appear in Malgo.
  4. Make unsupported cases fail with an explicit unsupported-feature diagnostic instead of being skipped silently.
  5. Keep CI jobs split so normal Haskell tests and self-hosted conformance failures are easy to identify.
- **Verification**: `mise run test` and `scripts/selfhost-golden.sh` are required gates, and the self-hosted summary reports all cases as pass/fail/unsupported with no hidden constant-input path.

## Verification

After the tasks are integrated, use these checks as the release gate:

```bash
mise run format
mise run test
scripts/selfhost-golden.sh
```

Additional targeted checks:

```bash
printf 'module {..} = import "../../../runtime/malgo/Builtin.mlg"\ndef main = { _ -> printString "stdin-ok\n" }\n' \
  | cabal run exe:malgo -- eval runtime/malgo/compiler/Main.mlg

cabal run exe:malgo -- eval test/testcases/malgo/UseModule.mlg
cabal run exe:malgo -- eval test/testcases/malgo/TestExplicitModule.mlg
```

And for the loader work specifically:

```bash
printf 'Hello\n' | cabal run exe:malgo -- eval runtime/malgo/compiler/Main.mlg tmp/nested/uses-parent.mlg
printf 'Hello\n' | cabal run exe:malgo -- eval runtime/malgo/compiler/Main.mlg tmp/selfhost-cycle-a.mlg
```

The complete implementation is done when:

- The Malgo-written pipeline handles the existing runtime, examples, and golden testcases with the same observable behavior as the host pipeline.
- Import visibility, qualified access, explicit import lists, and import cycles are handled deliberately.
- `getContents`, file I/O, stderr, and arguments are real runtime bridges, not evaluator-only constants.
- Static checking in Malgo accepts and rejects the same representative programs as the host implementation.
- Malgo-produced Join evaluation and Scheme output are runnable and conformance-tested.

## Risks

| Risk | Mitigation |
|------|------------|
| The current tree-walking evaluator grows into an incompatible second language semantics | Keep it as an early backend only; introduce phase modules matching the host pipeline and move tests to those phases. |
| Import semantics remain ambiguous across dynamic eval and static rename | Define `ImportTarget`, module exports, and interfaces once, then use the same model in evaluator and renamer. |
| Self-hosted tests can pass while exercising the wrong input | Fix stdin first and add regression tests that use non-constant input. |
| Full type inference is large and hard to debug in Malgo | Port the host concepts in small units: substitutions, unification, schemes, rows, then declaration groups. Add focused tests before integrating. |
| Module cycles or duplicate imports cause nontermination | Use explicit `Loading`/`Loaded` cache state and report import stacks. |
| Generated Scheme differs textually from the host backend | Compare runnable behavior and selected structural properties rather than exact formatting. |
| Runtime primitive names drift between `Builtin.mlg`, host evaluator, and self-hosted evaluator | Make `foreign import` declarations the source-level contract and keep primitive dispatch tables tested. |
*** End Patch
