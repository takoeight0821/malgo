# `error/` testcase directory — ground-truth notes

`scripts/cli-gate.sh`'s `error` mode treats a `.mlg` file here as a genuine
error fixture only if it has a companion `<name>.mlg.expect` sidecar (one
word: `parse`/`rename`/`eval`/`type`, matching the pass that throws first —
see that pass's source for the exact call site if you need to re-derive it).
The binary is only required to exit nonzero on `malgo eval --infer FILE`;
message text is never compared.

**Determined empirically** (2026-07-19) against the then-current Haskell
binary, both with and without `--infer` — since `useInfer` defaults to
`false`, most of these are not actually type errors, and running with
`--infer` uniformly still reproduces every non-type error identically
(parse/rename happen before inference regardless of the flag), which is why
the harness always passes `--infer`. The table survived the Haskell
retirement unchanged: the gate has been green on the Lean binary throughout.

| File | `.expect` | Fails without `--infer`? |
|---|---|---|
| `ErrorInvalidIdent.mlg` | `parse` | yes |
| `ErrorOnlySig.mlg` | `rename` | yes |
| `NeedSpaceDot.mlg` | `rename` | yes |
| `QualifiedImport.mlg` | `rename` | yes |
| `NonExhaustive.mlg` | `eval` | yes (runtime non-exhaustive match) |
| `InvalidPattern.mlg` | `type` | no — succeeds (prints `OK`) without `--infer`; the clause-arity mismatch is only caught by `InferPass` |

**`ConstructorArity.mlg`/`StringPatIsNotSupported.mlg` used to be in this
table too, with `.expect: type`** — both names are misleading holdovers from
when they were repurposed to reproduce a `Query.Engine.buildDepsEnv` bug
(fixed 2026-08-23, #429), not an actual constructor-arity or string-pattern
error. Kept here for anyone who finds a reference to the old behavior:

`buildDepsEnv` folds each dependency's exported `TyEnv` into an accumulator
and throws on any name two dependencies both export — a deliberate,
oracle-matching check for a genuine cross-module `Id` collision (see the
doc comment on `buildDepsEnv` in `lean/Malgo/Query/Engine.lean`). The bug
was that its dependency set, a `Std.TreeSet ModuleName`, could list the
*same file* under two different `ModuleName` aliases — `.moduleName
"Builtin"` for a bare `import Builtin` and the `.artifact` form a
relative-path import resolves to — because `ModuleName`'s derived
`BEq`/`Ord` treats different constructors as always-unequal even when they
name the identical file on disk. Since `Prelude.mlg` re-exports every
`Builtin.mlg` name via its own bare `import Builtin`, any program that
imports both Builtin and Prelude directly (nearly every real program) hit
this: not a genuine collision between two different files, but the same
file folded in twice under two different keys, tripping the check on every
name Builtin exports. It stayed latent because the test suite's own infer
gate never went through the real `buildDepsEnv` — it uses a separate,
deliberately lenient left-biased fold (`buildDepsEnvLenient` in
`lean/Test/Main.lean`, inherited from the Haskell test suite's own
`InferSpec.runInferCapturing`) — so only the CLI binary's `--infer` path
ever hit it (confirmed to crash an *ordinary* testcase too, `Undefined.mlg`,
which has the identical Prelude+Builtin diamond).

The fix resolves each dependency's `ModuleName` to its `ArtifactPath`
(`Workspace.getModulePath`) and dedupes on that resolved identity before
folding, collapsing alias-of-the-same-file entries into one while leaving
the collision check itself unweakened: two entries that resolve to two
*different* files still both survive to the fold and still throw exactly as
before. `ConstructorArity.mlg` and `StringPatIsNotSupported.mlg` no longer
error under `--infer`

**`linkDeps`, 25 lines away in the same file, had the identical
alias-fold defect and was fixed alongside `buildDepsEnv` (#448).** It
folds each dependency's `.sqt` into the linked program rather than into a
`TyEnv`, so the same alias duplication was never fatal there -- no
collision check to trip -- but it silently loaded and concatenated the
aliased dependency's definitions twice per diamond edge. Both functions
now dedupe through one shared helper, `Workspace.dedupeByArtifactPath`
(`lean/Malgo/Module.lean`), so a future third caller with the same
fold-over-`ModuleName`-set shape has one place to reuse rather than a
third copy to write from scratch. — their `.expect` sidecars were removed, and they are
now ordinary `error`-mode-excluded `.mlg` files, like the six below.
`BuiltinPreludeDiamond.mlg` was added alongside them as a purpose-built
regression fixture with the same diamond-import shape; it has no `.expect`
on purpose (it must succeed, not fail). `scripts/cli-gate.sh`'s dedicated
`infer-ok` mode asserts exit 0 under `malgo eval --infer` for all three
files, exercising the fix through the real CLI binary rather than the
lenient Lean-side test gate.

**Six files have NO `.expect` and are excluded from the `error` mode:**
`ErrorKind.mlg`, `ErrorKind2.mlg`, `ErrorKind3.mlg`, `ErrorPatSynRecon.mlg`,
`ErrorRigidType.mlg`, `InferEither.mlg`. Running the binary on each of these
— with `--infer` — **exits 0** (no error at all; several produce no output because they define
no `main`). Malgo's inferencer has no real kind system (arrow-kinded type
constructors like `A a` in `ErrorKind2.mlg`, or `f`-in-`A a -> A a`-style
misuse, are not checked) and does not enforce rigid-variable escape
(`ErrorRigidType.mlg`). These read as aspirational regression fixtures for
features not yet implemented, not currently-failing cases — writing an
`.expect` for them would assert a falsehood. If one of these ever starts
failing (a real fix lands), add its `.expect` sidecar then; until
then they are ordinary (successful, typically no-`main`, no-output)
`eval`-mode cases like any other testcase.
