# Triage follow-up: #424, #425, #426, #429, #440

Date: 2026-08-23

## Context

`/triage` was run over the 10 oldest open issues on 2026-08-22/23. Five were
resolved as part of triage itself (#331 parked, #354 measured and deferred,
#379 labeled `task`, #407's harness restored and merged as #444, #416
closed as already-implemented). The remaining five are concrete code changes,
each already carrying a behavioral agent brief on its GitHub issue from that
triage pass. This document plans how to execute them.

### #424 — field-access `.field` doesn't project through a parenthesized expression

`lean/Malgo/Parser/CStyle.lean`'s `pApply` folds `pApplyPostfix` (call args,
`.field` projection, bare-atom application) left-to-right over one parsed
atom. `pVariableFoldFields` special-cases a bare-identifier atom's
immediately-adjacent dot-chain, consuming it *before* `pApply`'s postfix loop
starts — but no equivalent exists for a parenthesized atom (`pParenTuple`).
So `printString (toStringInt32 (mk 7).exitCode)` attaches `.exitCode` to the
whole `apply(toStringInt32, (mk 7))`, not to `(mk 7)`. Introduced at the
initial Lean port (9b9e1cdd) and untouched since. Fix is isolated to the
parser; the self-hosted parser mirror (`runtime/malgo/compiler/Parser.mlg`)
should be checked for the same gap but is a separate concern.

### #425 — self-hosted evaluator represents literals unboxed

Every source literal is desugared by `lean/Malgo/Rename/Pass.lean`'s
`lookupBox` into an explicit application of its boxing constructor (e.g. `5`
becomes `Int32#(5)`) — upstream of every backend (Eval/Zig/Scheme), so all
three agree on boxed representation. The self-hosted compiler
(`runtime/malgo/compiler/`) has its own independent Rename/ToFun pipeline
with no equivalent step: `ToFun.mlg` maps a literal straight to
`FunLitInt32`, and `Eval.mlg`/`Value.mlg` evaluate/represent it unboxed. Fix
is confined to the self-hosted compiler's own sources — mirror the boxing
desugar there, then adjust its value representation/printer to match.

### #426 — `malgo_panic` unimplemented under the interpreter

`lean/Malgo/Sequent/Eval.lean`'s `fetchPrimitive` match has no case for the
panic primitive, so it falls to the generic
`.primitiveNotImplemented` arm. `runtime/zig/runtime.zig` and
`lean/Malgo/Backend/Scheme.lean` both already implement it correctly
(terminate with the given message) and serve as the reference semantics.
Smallest of the five — one match arm.

### #429 — `buildDepsEnv` false collision on Prelude+Builtin diamond imports

This is bigger than its GitHub brief states, discovered while re-reading
`lean/Malgo/Query/Engine.lean` for this plan. `buildDepsEnv`'s own doc
comment (confirmed against `test/testcases/malgo/error/README.md`) records
that this is a **known, deliberate, oracle-parity decision**, not an
oversight: `ModuleName` has three constructors (`.moduleName` bare-name,
`.artifact` path, `.rawPath` transient), and a module reached via a bare name
*and* via a path resolves to two different `ModuleName` values even when
it's the same file on disk — because `Prelude.mlg` re-exports every
`Builtin.mlg` name via its own bare `import Builtin`, **any** program that
imports both `Builtin` and `Prelude` directly hits this. That's not a corner
case — it's ~most of the corpus (`grep -l runtime/malgo/Builtin.mlg
test/testcases/malgo/**/*.mlg` matches the large majority of the 95
testcases, including the ordinary, non-error `Undefined.mlg`).

It stays latent today for two reasons: (1) the golden suite runs without
`--infer` by default, so `buildDepsEnv` (only reachable through `InferPass`)
is never exercised; (2) the test suite's own infer gate
(`lean/Test/Main.lean`) bypasses the strict `buildDepsEnv` entirely via a
separate lenient left-biased fold, `buildDepsEnvLenient` — inherited from the
Haskell-era test suite, which did the same. Two `error/` fixtures
(`ConstructorArity.mlg`, `StringPatIsNotSupported.mlg`) are deliberately
repurposed to assert this exact crash under `--infer` — their names are
misleading; per the fixture README they are "not actually about constructor
arity"/"not actually about string patterns."

**Consequence for the fix:** correcting `buildDepsEnv`'s dependency identity
to recognize "same file via two resolution routes" as one dependency will
make `--infer` stop crashing on the Prelude+Builtin diamond — which is the
actual point of the fix (`--infer` is currently unusable on most real
programs) — but it also means:
- `ConstructorArity.mlg` and `StringPatIsNotSupported.mlg` will very likely
  stop failing at the `type` stage (their bodies have no visible unrelated
  type error), so their `.expect` sidecars need auditing, not just their
  `Malgo.Query.Engine` counterpart.
- `test/testcases/malgo/error/README.md`'s table and its "real failure mode"
  section need rewriting once the fix lands.
- `buildDepsEnvLenient` in `lean/Test/Main.lean` becomes redundant with a
  correctly-fixed `buildDepsEnv` — worth a follow-up to delete it and call
  the real one, but that's a separate, lower-priority cleanup, not required
  for this fix to be correct.

### #440 — no way to import `.mlg` files from outside the project directory

`lean/Malgo/Module.lean`'s `parseArtifactPathFromPwd`/`parseArtifactPath`
both resolve a path-literal import, then call `lean/Malgo/Path.lean`'s
`stripProperPrefix` against the workspace's parent (pwd) — which throws
`"Path {p} is not inside {dir}"` for anything outside the project tree.
Bare-name imports (`searchAndRegister`) BFS-search the `.malgo-work` mirror
itself, so once *any* route mirrors a file there, bare-name lookups find it
too. Design direction settled during triage: an `MALGO_IMPORT_PATH` env var
(colon-separated extra roots, matching the `MALGO_WORK_DIR` convention
already in place) that `parseArtifactPathFromPwd`/`parseArtifactPath` fall
back to when the resolved origin isn't under pwd, mirroring the result under
a reserved sub-namespace (e.g. `.ext/<root-index>/...`) that can't collide
with any real in-project relative path.

**Why this depends on #429, concretely:** once external roots exist, a file
mirrored from one becomes reachable both by its `.artifact` path *and*,
potentially, by bare name (if something within it or downstream imports it
by name) — the exact aliasing shape #429 is about. Shipping the search-path
feature before #429's identity fix would create new instances of the same
bug in a new location, harder to reason about than in the same-tree case
#429 already documents. #429 must merge (or at least have its identity fix
locked in review) before #440's implementation starts.

## Design Choices

**Five issues, five PRs, not fewer.** #424/#425/#426 touch disjoint files
with no shared concepts; batching them would only save review overhead at
the cost of bisectability, and every merged PR in this repo's recent history
(#409-#414, #417/#420/#428, #444) is single-purpose. #429 and #440 are
sequential (dependency, not independence) — stacking them as two PRs on one
branch chain (matching the repo's own stacked-PR convention, e.g. #409-414's
"PR x/5" series) is more appropriate than merging them into one, since #429
is independently valuable (fixes `--infer` for ~all real programs) even if
#440 never lands.

**#429's fix approach:** key `buildDepsEnv`'s dependency accumulation (and
wherever else `ModuleName` is used as a dependency-set key for this purpose)
by the resolved artifact's underlying identity — realistically, `ArtifactPath.relPath`
already has the right `BEq`/`Ord` instances for this (per its own doc comment,
"so paths reached via different traversal routes compare equal"). The
likely-smallest fix is: before comparing/inserting a `.moduleName` dependency
into the collision-tracked set, resolve it to its `ArtifactPath` (via
`Workspace.getModulePath`, which already does the search-and-register work)
and key on that instead of the raw `ModuleName`. This needs verifying against
the actual current shape of `buildDepsEnv`/`fetchInferredModule` at
implementation time — this plan does not prescribe exact code, per the
agent-brief convention of describing behavior, not implementation.

**#440's fix approach:** env var over CLI flag (see triage discussion on the
issue) — consistent with `MALGO_WORK_DIR`'s precedent, and the primary known
consumer (nix-config) invokes `malgo` non-interactively where an env var is
more natural than a flag. Do not add a CLI flag preemptively (YAGNI) unless
real usage shows the env var isn't enough.

## Implementation Plan

### Task 1: #424 — parenthesized-expression field-access parsing

- **Goal:** `.field` immediately after a parenthesized expression binds to
  that expression, not to the enclosing application.
- **Scope:** `lean/Malgo/Parser/CStyle.lean` (the atom/postfix parsing
  around `pApply`/`pApplyPostfix`/`pVariableFoldFields`/`pParenTuple`).
- **Dependencies:** none.
- **Steps:**
  1. Give a parenthesized atom the same "immediately-adjacent dot-chain binds
     tightly" treatment `pVariableFoldFields` already gives a bare
     identifier, before the enclosing postfix-application fold sees it.
  2. Add a `ParserSurface` case: a parenthesized expression as a function
     argument, immediately followed by `.field` (and a chained
     `.a.b` variant), asserting the field binds to the parenthesized
     expression.
  3. Check whether `runtime/malgo/compiler/Parser.mlg` (self-hosted mirror)
     has the analogous structure and the same gap; fix there too if so, or
     note it as a known follow-up if not trivial.
- **Verification:** `mise run test -- --match Parser`, full `mise run test`,
  `bash scripts/zig-golden.sh`, `bash scripts/selfhost-golden.sh`.

### Task 2: #426 — `malgo_panic` under the interpreter

- **Goal:** the interpreter terminates with the given message on this
  primitive, instead of an "unimplemented" error.
- **Scope:** `lean/Malgo/Sequent/Eval.lean` (`fetchPrimitive`).
- **Dependencies:** none.
- **Steps:**
  1. Add a case for the panic primitive that terminates evaluation and
     surfaces the message, matching `runtime/zig/runtime.zig`'s and
     `lean/Malgo/Backend/Scheme.lean`'s existing semantics.
  2. Add a golden test exercising it under `--target eval`.
- **Verification:** `mise run test`, new golden diffed against Zig/Scheme
  output for the same program.

### Task 3: #425 — self-hosted evaluator literal boxing

- **Goal:** the self-hosted evaluator's literals are boxed the same way the
  Lean interpreter's are.
- **Scope:** `runtime/malgo/compiler/Rename.mlg`, `ToFun.mlg`, `Eval.mlg`,
  `Value.mlg`.
- **Dependencies:** none.
- **Steps:**
  1. Add a boxing-desugar step to the self-hosted compiler's own
     rename/lowering pipeline, mirroring `lean/Malgo/Rename/Pass.lean`'s
     `lookupBox` behavior, for every primitive literal type.
  2. Adjust `Value.mlg`'s representation and printer so a boxed literal
     round-trips/prints the same as the interpreter's.
  3. Add a regression test (print/pattern-match a user-defined wrapper
     around each primitive type) run through both the self-hosted evaluator
     and the interpreter, asserting identical output.
- **Verification:** `bash scripts/selfhost-golden.sh` (Level 1),
  `bash scripts/selfhost-level2.sh` (Level 2), new regression case.

### Task 4: #429 — `buildDepsEnv` module-identity collision

- **Goal:** a module reached via two different resolution routes (bare name
  vs. path) is recognized as one dependency; a genuine same-name collision
  between two different files still fails as today.
- **Scope:** `lean/Malgo/Query/Engine.lean` (`buildDepsEnv`,
  `fetchInferredModule`); `test/testcases/malgo/error/{ConstructorArity,StringPatIsNotSupported}.mlg`
  and their `.expect` sidecars; `test/testcases/malgo/error/README.md`.
- **Dependencies:** none (independent of Tasks 1-3; Task 5 depends on this
  one).
- **Steps:**
  1. Resolve each dependency to its underlying artifact identity before
     collision-checking, instead of comparing raw `ModuleName` values (see
     Design Choices above for the likely mechanism).
  2. Run `malgo eval --infer` on `ConstructorArity.mlg` and
     `StringPatIsNotSupported.mlg` to find their real post-fix behavior.
     Update or remove their `.expect` sidecars accordingly — if either
     surfaces a genuine new error, keep it as an `error/` fixture with an
     accurate `.expect` and a comment on what it now actually tests; if
     neither does, move them out of `error/` (or repurpose/delete) and
     rewrite the README's table and "real failure mode" section to match
     the fixed behavior.
  3. Add a regression test: a program importing the same runtime module
     both directly and transitively succeeds under `--infer`.
  4. Leave `buildDepsEnvLenient` (`lean/Test/Main.lean`) alone for this task
     — noted as a follow-up cleanup once the strict path is trustworthy
     everywhere, not a blocker here.
- **Verification:** `bash scripts/cli-gate.sh error` (the two repurposed
  fixtures' outcome will change — that's the point, not a regression),
  `mise run test`, full golden suite.

### Task 5: #440 — cross-directory `.mlg` imports

- **Goal:** a path-literal import can resolve outside the invoking
  project's directory when explicitly configured to.
- **Scope:** `lean/Malgo/Module.lean` (`Workspace.setup`,
  `parseArtifactPathFromPwd`, `parseArtifactPath`, `searchAndRegister`);
  `lean/Malgo/Path.lean` if a non-throwing containment check is needed;
  `lean/README.md`/`docs/` for the new env var.
- **Dependencies:** Task 4 (#429) must be merged first.
- **Steps:**
  1. Read `MALGO_IMPORT_PATH` (colon-separated absolute directories) at
     workspace setup.
  2. When a path-literal import's resolved origin isn't under pwd, try each
     configured root in order; on a match, mirror it under a reserved
     sub-namespace of the workspace that cannot collide with any in-project
     relative path.
  3. Add a regression test: importing the same external file both by
     explicit path and transitively by bare name (through a module that
     imports it by name) succeeds and is treated as one dependency —
     exercising the Task 4 fix from the new angle this feature opens up.
  4. Document `MALGO_IMPORT_PATH` in `lean/README.md`.
- **Verification:** new regression test above; `mise run test`; a manual
  check against the nix-config consumption pattern described in issue #440
  (vendoring workaround should become unnecessary, though removing it from
  nix-config itself is out of scope here).

## Verification

After all five land: `mise run test`, `bash scripts/zig-golden.sh`,
`bash scripts/selfhost-golden.sh`, `bash scripts/selfhost-level2.sh`,
`bash scripts/scheme-golden.sh`, `bash scripts/cli-gate.sh error` (expect the
two repurposed fixtures' behavior to have changed, per Task 4). No task here
touches `runtime/zig/runtime.zig`'s leak checking, so no leak-check
regression is expected, but every golden run's leak check should still be
watched.

## Risks

| Risk | Mitigation |
|------|------------|
| #429's real scope (test-fixture audit, not just a one-line dedup) wasn't reflected in the agent brief already posted on GitHub | Post a follow-up comment on #429 pointing at this document before an agent picks it up, so the brief's "Out of scope" doesn't undersell the work |
| Fixing #429 changes two `error/` fixtures' pass/fail stage, which could look like a regression to someone skimming CI | Task 4's steps explicitly call for auditing and rewriting the README/`.expect`s in the same PR, not as a follow-up |
| #440 implemented before #429's fix is solid would widen the module-identity bug into a second dimension (external roots) | Explicit Task ordering (Task 4 before Task 5); do not start Task 5 until Task 4 is merged |
| Self-hosted parser mirror (#424) or self-hosted compiler pipeline (#425) drift from the Lean side after this fix, re-diverging later | Both tasks explicitly check the self-hosted mirror for the same gap as part of their own steps, not deferred |
