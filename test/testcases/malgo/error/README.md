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
| `ConstructorArity.mlg` | `type` | no — only `Malgo.Query.Engine.buildDepsEnv`'s duplicate-export `error` call, tripped under `--infer` by this file's relative-path imports; not really about constructor arity as the name suggests |
| `InvalidPattern.mlg` | `type` | no — succeeds (prints `OK`) without `--infer`; the clause-arity mismatch is only caught by `InferPass` |
| `StringPatIsNotSupported.mlg` | `type` | no — same as above (prints `OK` without `--infer`) |

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
