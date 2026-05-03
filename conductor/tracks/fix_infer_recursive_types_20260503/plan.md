# Implementation Plan - Fix(infer): Relax occurs check for recursive/codata types

## Phase 1: Type Representation Update [checkpoint: 76188c91]
- [x] Task: Add `TMu` to `Ty` data type
    - [x] Update `Ty` definition in `src/Malgo/Infer/Constraint.hs`
    - [x] Implement `TMu` in `freeVars` and `applySubst` functions
- [x] Task: Conductor - User Manual Verification 'Phase 1: Type Representation Update' (Protocol in workflow.md)

## Phase 2: Unification Engine Refinement [checkpoint: c838fb2]
- [x] Task: Update Unification Logic
    - [x] Modify `unifyTypes` in `src/Malgo/Infer/Unify.hs` to handle recursive unification
    - [x] Implement cycle-safe type processing to avoid infinite loops during unification
- [x] Task: Conductor - User Manual Verification 'Phase 2: Unification Engine Refinement' (Protocol in workflow.md)

## Phase 3: Verification & Testing
- [ ] Task: Fix failing `InferSpec` tests
    - [ ] Run `mise run test -- --match=InferSpec` and ensure codata/recursive tests pass
- [ ] Task: Add new tests for complex recursive types
    - [ ] Create new test cases in `test/testcases/malgo/` for nested recursive types and verify with golden tests
- [ ] Task: Conductor - User Manual Verification 'Phase 3: Verification & Testing' (Protocol in workflow.md)
