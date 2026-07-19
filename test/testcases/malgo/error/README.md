# `error/` testcase directory — ground-truth notes

`scripts/lean-parity.sh --mode error` treats a `.mlg` file here as a genuine
error fixture only if it has a companion `<name>.mlg.expect` sidecar (one
word: `parse`/`rename`/`eval`/`type`, matching the pass whose `throwError`
first fires — see each pass's Haskell source for the exact call site if you
need to re-derive it). Both implementations are only required to exit
nonzero on `malgo eval --infer FILE`; message text is never compared.

**Determined empirically** (2026-07-19) by running the Haskell binary
(`cabal build exe:malgo`) against every file here, both with and without
`--infer` — since `useInfer` defaults to `false`, most of these are not
actually type errors, and running with `--infer` uniformly still reproduces
every non-type error identically (parse/rename happen before inference
regardless of the flag), which is why the harness always passes `--infer`.

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
empirically that this crashes the real Haskell CLI (`malgo eval --infer`) on
an *ordinary* testcase too (`Undefined.mlg`, which has the identical
Prelude+Builtin diamond) — so it is a genuine, if latent, Haskell CLI defect,
not something particular to the `error/` fixtures. It stays latent in
Haskell's own test suite only because `InferSpec.runInferCapturing` computes
a testcase's top-level dependency env via its own separate, lenient
left-biased fold (`foldlM (\acc dep -> (depEnv <>)) Map.empty`) instead of
calling the strict `Query.Engine.buildDepsEnv` — the two are different
functions in Haskell, and only the lenient one is exercised by anything that
currently passes. The Lean port mirrors this exact split (see
`lean/Test/Main.lean`'s `buildDepsEnvLenient` vs `Malgo.Query.Engine.
buildDepsEnv`), so: the `lake test` infer gate (mirroring `runInferCapturing`)
passes on `Undefined.mlg` and all other Prelude+Builtin-diamond testcases,
while the real CLI binary (mirroring `Query/Engine.hs`'s `LinkedProgram`
handler literally) crashes on them under `--infer` — on both implementations
identically, which is what `--mode error` checks for these two files.

**Six files have NO `.expect` and are excluded from `--mode error`:**
`ErrorKind.mlg`, `ErrorKind2.mlg`, `ErrorKind3.mlg`, `ErrorPatSynRecon.mlg`,
`ErrorRigidType.mlg`, `InferEither.mlg`. As of the above date, `mise run
test`'s `cabal build exe:malgo` run on each of these — with `--infer` —
**exits 0** (no error at all; several produce no output because they define
no `main`). Malgo's inferencer has no real kind system (arrow-kinded type
constructors like `A a` in `ErrorKind2.mlg`, or `f`-in-`A a -> A a`-style
misuse, are not checked) and does not enforce rigid-variable escape
(`ErrorRigidType.mlg`). These read as aspirational regression fixtures for
features not yet implemented, not currently-failing cases — writing an
`.expect` for them would assert a falsehood. If one of these ever starts
failing in Haskell (a real fix lands), add its `.expect` sidecar then; until
then they are ordinary (successful, typically no-`main`, no-output)
`eval`-mode cases like any other testcase.
