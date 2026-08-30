# Type system error test coverage

Date: 2026-08-30

## Context

A prior coverage sweep of the whole test suite flagged one gap as highest
priority: type/inference errors have essentially no content testing. Parser
and Rename each have exact-message golden coverage for their error paths
(`test/Malgo/ParserSpec/errors/*.mlg`, `test/Malgo/RenameSpec/errors/*.mlg`,
wired via `enumerateErrorCases`/`parserErrorCase`/`renameErrorCase` in
`lean/Test/Main.lean:36-125`), but the `infer` gate has no equivalent.

### The error type

`lean/Malgo/Infer/Constraint.lean:240-246` defines:

```
inductive InferError where
  | unificationError (range : Range) (expected actual : Ty) (msg : String)
  | unboundVariable (range : Range) (name : Id)
  | occursCheckError (range : Range) (varName : Id) (ty : Ty)
  | notImplemented (range : Range) (feature : String)
  | cyclicSynonym (range : Range) (name : Id)
  | synonymArityMismatch (range : Range) (name : Id) (expected got : Nat)
```

rendered by `InferError.render` (`Constraint.lean:255-264`) and converted to
the uniform `CompileError` only at `Malgo.Infer.pass`'s boundary via
`Malgo.wrapError` (`Infer.lean:313-319`).

Four constructors are reachable from real `.mlg` source today:
`unificationError` (constructor mismatch, tuple-length mismatch, generic
fallback, row/record mismatch, variant-arity mismatch — all in
`Infer/Unify.lean`), `unboundVariable` (`Infer.lean:137,150,226`),
`cyclicSynonym` and `synonymArityMismatch` (both in `expandType` /
`expandSynonymApp`, `Infer.lean:43-93`). Two are currently dead code:
`occursCheckError` is never constructed — the unifier resolves a genuine
occurs-check situation into an equirecursive `.tMu` type instead of erroring
(`Unify.lean:108-109`) — and `notImplemented` exists only as the
`Inhabited InferError` default witness (`Constraint.lean:253`).

### Current gate behavior

`runInferGate` (`lean/Test/Main.lean:1415-1443`, driving via `driveInfer` at
line 1266) runs every one of the 90 testcases Parse → Rename → Elaborate →
Infer and asserts each **succeeds**. Its own doc comment is explicit: *"there
is no golden ... every testcase must succeed."* There is no negative-case
list and no fixture directory analogous to `RenameSpec/errors`.

Confirmed empirically against the built binary — `malgo eval --infer
test/testcases/malgo/error/InvalidPattern.mlg` prints a real (if slightly odd
— see risks) type error and exits 1, while the same file without `--infer`
exits 0. That is the only current fixture whose `.mlg.expect` sidecar names
`type` as the expected failure kind; `scripts/cli-gate.sh`'s error mode
checks only the exit code for it, never the message text.

### The feature-flag gap

`driveInfer` (`Test/Main.lean:1266-1281`) already contains the branch needed
to run Infer without Elaborate having run first:

```
let bindGroup ← if (← Malgo.hasFeature .malgo2025)
  then Malgo.Elaborate.pass renamed.moduleName renamed.moduleDefinition
  else pure renamed.moduleDefinition
```

but every caller (`runInferGate`, `runInferCapturing`) fixes the feature set
to `Test.malgo2025 := FeatureFlags.ofList [.malgo2025]` (line 1210), so the
`else` branch never actually runs in CI. `useInfer` (a separate `Flag`
field, not a `Feature`) is likewise hardcoded true via `inferFlag`
(line 1209). `cStyleApply` is unrelated to Infer/Elaborate gating and is out
of scope here.

## Design choices

- **Reuse the existing golden-error-case machinery rather than inventing a
  new harness.** `enumerateErrorCases`/`GoldenCase` is a proven, minimal
  pattern already carrying two groups; a third (`Malgo.Infer`) is the lowest-
  risk way to add this coverage and keeps all three error groups
  discoverable the same way.
- **Build the new golden driver on top of `driveInfer`'s existing pipeline**
  (parse → rename → conditional Elaborate → Infer), not through
  `Query.Engine.fetchInferredModule`'s caching machinery — the query engine
  is designed for multi-module incremental builds and would add complexity
  a single top-level negative fixture doesn't need. `driveInfer` is already
  the test-side idiom for this exact pipeline.
- **Add a constructor-coverage staleness gate** (`InferErrorCoverage`),
  mirroring `PrimitiveCoverage`'s allowlist-with-staleness-check pattern
  (`Test/Main.lean:902-1054`): every `InferError` constructor must either be
  exercised by a golden fixture or explicitly allowlisted with a reason. This
  is what actually prevents the gap from reopening the next time someone adds
  a constructor.
- **Don't manufacture fixtures for `occursCheckError`/`notImplemented`.**
  They're unreachable by design under the current equirecursive-type unifier,
  not by oversight; forcing an artificial trigger would test something that
  can't happen from real source. Allowlist them instead, same as
  `PrimitiveCoverage.knownMissing` allowlists genuinely-missing primitives.
- **Keep `cli-gate.sh` changes to one smoke case.** The bulk of message-
  content coverage belongs in the fast in-process golden suite (Task 1); one
  exact-stderr assertion through the real CLI binary is enough to confirm
  `CompileError` rendering survives the `MalgoM.run` → `IO.eprintln` path
  unmangled end-to-end.
- **Treat the malgo2025-off path as investigative, not a committed
  deliverable.** It's genuinely unclear whether Infer without Elaborate's
  desugaring is a supported configuration or a vestigial branch predating
  Elaborate's introduction. Scope that work to "investigate, then either add
  a small gate or document why not" rather than assuming it's viable.

## Implementation Plan

### Task 1: `Malgo.Infer` error golden group (core deliverable)

- **Goal**: golden-test message text for the four reachable `InferError`
  variants, following the Parser/Rename precedent exactly.
- **Scope**: `lean/Test/Main.lean` (new `inferErrorCaseDir`,
  `inferErrorGolden`, `inferErrorCase`, `enumerateInferErrorCases`,
  `inferErrorCases`, wired into the case list around line 1465-1466); new
  fixtures under `test/Malgo/InferSpec/errors/*.mlg`; generated goldens under
  `.golden/Malgo.Infer/error/<name>/golden`.
- **Dependencies**: none.
- **Steps**:
  1. Confirm the exact golden directory layout by inspecting
     `.golden/Malgo.Rename/error/*/golden` and mirror it precisely.
  2. Write fixtures, at least one per reachable constructor:
     - `unificationError`: a constructor/type mismatch (e.g. `let x : Bool =
       1`), a tuple-length mismatch, and reuse `InvalidPattern.mlg`'s shape
       for the generic-fallback case.
     - `unboundVariable`: **needs investigation before writing** — confirm
       what real source triggers `Infer.lean`'s own `.unboundVariable`
       (lines 137, 150, 226) as distinct from Rename's identically-named
       error, since Rename already catches plain undefined names earlier in
       the pipeline. If nothing reaches this path in practice, fold it into
       the known-unreachable allowlist in Task 2 instead of forcing a
       fixture.
     - `cyclicSynonym`: `type A = A` (or a mutual cycle).
     - `synonymArityMismatch`: apply a two-parameter type synonym with the
       wrong number of type arguments.
  3. Implement `inferErrorGolden` by adapting `driveInfer`'s body
     (`seedParsed` → `fetchRenamedModule` → `buildDepsEnvLenient` →
     conditional `Elaborate.pass` → `Infer.pass`), returning `toString e` on
     catch and `"INFERRED WITHOUT ERROR: ..."` on unexpected success — same
     shape as `renameErrorGolden` (`Test/Main.lean:101-107`).
  4. Wire `enumerateInferErrorCases`/`inferErrorCases` in next to the
     existing parser/rename ones and thread the new cases into whatever
     count the suite reports.
  5. `mise run test -- --update` to generate goldens, then **hand-review
     every golden's text** — a rubber-stamped golden defeats the purpose.
- **Verification**: `mise run test -- --match Infer` passes; each golden
  manually confirmed to read as a correct, useful message; a deliberate typo
  in `InferError.render` locally makes the gate fail, then revert.

### Task 2: `InferErrorCoverage` staleness gate

- **Goal**: a newly-added `InferError` constructor can't go silently
  untested, mirroring `PrimitiveCoverage`'s pattern.
- **Scope**: `lean/Test/Main.lean`, new `namespace InferErrorCoverage`.
- **Dependencies**: Task 1 (needs to know which constructors have fixtures).
- **Steps**:
  1. Capture the raw `InferError` value (before `wrapError` converts it to
     `CompileError`/`String`) for each Task 1 fixture, tagged by constructor
     name — don't reverse-engineer the constructor from rendered text.
  2. Assert every reachable constructor (all but `occursCheckError`,
     `notImplemented`, and `unboundVariable` if Task 1 found it unreachable)
     has at least one fixture.
  3. Allowlist the unreachable ones as `KnownGap`-style entries with a
     `reason` string (citing `Unify.lean`'s equirecursive-mu path for
     `occursCheckError`, the `Inhabited` default-witness role for
     `notImplemented`), same structure as `PrimitiveCoverage.knownMissing`
     (`Test/Main.lean:940-953`).
  4. Wire into `main`'s gate list with `ok`/`FAIL` lines.
- **Verification**: temporarily remove one fixture, confirm the gate fails
  naming the uncovered constructor, then revert.

### Task 3 (stretch, investigative): malgo2025-off path through Infer

- **Goal**: determine whether `driveInfer`'s `else` branch
  (`Test/Main.lean:1275-1277`, Infer running directly on `renamed
  .moduleDefinition` without Elaborate) is a real supported configuration,
  and cover it if so.
- **Scope**: `lean/Test/Main.lean`. Independent of Tasks 1/2; lower priority.
- **Steps**:
  1. Investigate: manually drive 3-5 representative testcases (mixing tagged
     records, pattern matching, plain arithmetic) with `malgo2025` off to see
     whether `Infer.pass` functions correctly against un-elaborated syntax,
     or whether this branch predates Elaborate and is effectively dead.
  2. If viable: add a second success-only `ok`/`FAIL` gate (no golden needed)
     running the corpus, or the viable subset, with `malgo2025` off —
     structurally identical to `runInferGate` but with the flag toggled.
  3. If not viable: don't fabricate coverage. Document the conclusion (a
     one-line comment at `Test/Main.lean:1275-1277` is enough) and stop.
- **Verification**: either a new gate green in CI, or a documented, honest
  "unsupported configuration" conclusion.

### Task 4: `cli-gate.sh` end-to-end smoke assertion

- **Goal**: confirm `CompileError` rendering reaches the real CLI's stderr
  unmangled, as a cheap complement to Task 1's in-process goldens.
- **Scope**: `scripts/cli-gate.sh`. Independent of the other tasks.
- **Steps**:
  1. Add one case running `malgo eval --infer
     test/testcases/malgo/error/InvalidPattern.mlg`, asserting stderr matches
     byte-for-byte, not just exit code.
  2. Keep it to 1-2 cases — this is a wiring smoke check, not a place to
     duplicate Task 1's fixtures.
- **Verification**: `bash scripts/cli-gate.sh` passes; a deliberate message
  change locally produces a clear diff failure.

## Verification (overall)

- `mise run test -- --match Infer` — all new gates green.
- `mise run test` — full suite green, no regression in existing golden
  counts.
- `bash scripts/cli-gate.sh` — green including the new smoke case.
- Every new golden file read and confirmed correct by a human, not just
  "gate passes."

## Risks

| Risk | Mitigation |
|------|------------|
| Golden text is source-range-sensitive; unrelated future parser/pretty-printer changes could cause spurious diffs | Keep fixtures minimal (single-line where possible); same cost every existing error-golden group already carries |
| `occursCheckError`/`notImplemented` might turn out reachable via some untested path, making the allowlist wrong | `InferErrorCoverage`'s staleness check catches this the moment a real fixture appears — no permanent blind spot |
| `unboundVariable` may be unreachable in practice since Rename catches plain undefined names first | Task 1 explicitly investigates the real call sites before writing a fixture; fold into the allowlist if genuinely dead |
| Task 3's malgo2025-off path may be genuinely unsupported, wasting investigation time | Scoped investigative-first with an explicit "stop and document" exit; not a hard deliverable |
| Golden suite size/runtime growth | ~5-10 small single-file cases; negligible next to the existing 90-case corpus run through every stage |
