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
| `ConstructorArity.mlg` | `type` | no — see the `buildDepsEnv` note below; not actually about constructor arity |
| `InvalidPattern.mlg` | `type` | no — succeeds (prints `OK`) without `--infer`; the clause-arity mismatch is only caught by `InferPass` |
| `StringPatIsNotSupported.mlg` | `type` | no — see the `buildDepsEnv` note below; not actually about string patterns |

**`ConstructorArity.mlg`/`StringPatIsNotSupported.mlg`'s real failure mode** is
`Malgo.Query.Engine.buildDepsEnv`'s duplicate-export `error` call — and it is
NOT specific to these two files. Any module that directly imports both
Prelude and Builtin hits it, because Prelude re-exports every Builtin name
(`module {..} = import Builtin`): the direct Builtin import and Prelude's
re-exported copy collide in `buildDepsEnv`'s strict accumulation. Confirmed
empirically that this crashes the CLI (`malgo eval --infer`) on an *ordinary*
testcase too (`Undefined.mlg`, which has the identical Prelude+Builtin
diamond) — so it is a genuine, if latent, CLI defect, not something
particular to the `error/` fixtures.

It stays latent because the test suite's infer gate does not go through
`buildDepsEnv`. It computes a testcase's top-level dependency env with a
separate, lenient left-biased fold (`buildDepsEnvLenient` in
`lean/Test/Main.lean`) — inherited from the Haskell test suite's
`InferSpec.runInferCapturing`, which did the same rather than call the
strict `Query.Engine.buildDepsEnv`. So the infer gate passes on
`Undefined.mlg` and every other Prelude+Builtin-diamond testcase, while the
CLI binary crashes on them under `--infer`. That divergence is exactly what
the `error` mode asserts for these two files.

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
