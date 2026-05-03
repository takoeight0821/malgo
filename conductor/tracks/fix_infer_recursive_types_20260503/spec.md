# Specification - Fix(infer): Relax occurs check for recursive/codata types

## Problem
Currently, Malgo's Hindley-Milner unification rejects equi-recursive types (like `Stream`) because the occurs check prevents a type variable from being unified with a type that contains it. This causes `InferSpec` tests for codata and recursive types to fail.

## Proposed Solution
Introduce `TMu` (μ-type) representation to handle self-referential types in the inference engine. Update the unification algorithm to allow and represent these cycles safely through equi-recursive unification.

## Requirements
- Add `TMu` constructor to the `Ty` data type in `src/Malgo/Infer/Constraint.hs`.
- Update `freeVars` and `applySubst` in `src/Malgo/Infer/Constraint.hs` to handle `TMu`.
- Modify `unifyTypes` in `src/Malgo/Infer/Unify.hs` to perform equi-recursive unification instead of a strict occurs check failure for codata/recursive types.
- Ensure all existing tests pass and add new tests for recursive types in `InferSpec`.
