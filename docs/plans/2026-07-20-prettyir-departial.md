# De-partialize `Debug/PrettyIR.lean` (issue #359, Tier 3)

Date: 2026-07-20

## Context

Issue #359 tracks introducing real proofs into the Lean 4 port and, separately,
a Tier 3 "mechanical cleanup" bucket: `partial def`s whose recursion is
actually structural but hidden from Lean's termination checker (usually
behind `List.map`/tuple-destructuring lambdas), which should become plain
`def`s with no behavior change and no new theorems.

Three Tier 3 files are already done this session (PRs #364, #365, #366):
`SExpr.lean`, `Rename/Pass.lean`, `Debug/DiffView.lean`. Each surfaced the
same two failure modes, now with an established fix:

1. **Plain `.map`/`.foldl` over a `List` of the recursive type** — Lean's
   automatic structural-recursion checker usually handles this today
   without any help (confirmed repeatedly: `toInter`, most of
   `Rename/Pass.lean`, most of `DiffView.lean` needed *no* changes beyond
   dropping `partial`).
2. **A pattern-matching lambda destructuring a tuple field**
   (`fun (_, v) => ... f v ...` over a `List (String × T)`/similar, or a
   `let (a, b) := f x` destructuring), which loses the connection the
   termination checker needs between the recursed-into value and its
   list-membership proof. Fixed by:
   - Switching to explicit projections (`fun kv => ... f kv.2 ...`, or
     referencing `(f x).2` directly at the call site instead of
     `let`-binding it) so the recursed value stays syntactically tied to
     its enclosing structure.
   - Adding a small reusable helper theorem for the exact shape needed
     (`sizeOf_snd_lt_of_mem`-style: "a pair's/`goFirsts`-result's
     component is never bigger than the list/value it came from").
   - An explicit `termination_by`/`decreasing_by`, closing the remaining
     goals via `simp_wf` then `List.sizeOf_lt_of_mem`/the new helper,
     chained with `Nat.lt_of_lt_of_le`/`omega`.

`Debug/PrettyIR.lean` (~15 functions across 6 groups, 502 lines) is next.
Attempting a blanket `partial` removal exposed **8 separate termination
failures**. Two Explore agents read every group's actual current source
(reported below) and confirmed the failures fall into exactly the two
known categories above — no new failure mode, no evidence of anything
`SaturateCtor`/`Query.Engine`-style non-structural. The only genuinely new
technique needed is **mutual `termination_by`/`decreasing_by`**, since five
of the six groups are `mutual` blocks (2–4 functions each) rather than
single self-recursive functions. This was empirically verified this
session (a throwaway scratch file, not committed) against Lean's actual
core-library convention (`Init/Data/List/Sort/Impl.lean`):

- **Each function in a `mutual` block gets its own `termination_by <arg> =>
  <measure>` clause, placed immediately after its own equations** — not
  clustered at the end of the block, and not using a name-prefixed
  multi-clause syntax. Example (validated form):

  ```lean
  mutual
  def fA : A → Nat
    | .leaf n => n
    | .node b => fB b
  termination_by a => sizeA a
  decreasing_by simp [sizeA]

  def fB : B → Nat
    | .wrap a => fA a
    | .many xs => (xs.map fA).foldl (·+·) 0
  termination_by b => sizeB b
  decreasing_by
    all_goals simp_wf
    all_goals first
      | simp [sizeB]
      | (rename_i h
         exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))
  end
  ```

- Each function needs its own **measure function** (`sizeA`/`sizeB`
  above) — for `Doc`-returning renderers, the natural measure is each
  mutual group's own `sizeOf` on the IR argument itself (no custom
  measure function needed, since we're not computing a `Nat` result the
  way the scratch example did — the target type is `Doc`, and we just
  need `termination_by <arg> => sizeOf <arg>` for the argument being
  matched, which is the default WF measure Lean already tries
  automatically; the value of specifying it explicitly is that it lets
  us follow up with a targeted `decreasing_by`).

## File-by-file findings (from the Explore survey)

All constructor counts from `lean/Malgo/Sequent/Fun.lean`,
`Sequent/Core/{Full,Flat,Join}.lean`, `Syntax.lean`.

### Already fine as plain `def` (no work needed)

- `renderPath` (`Ir.Path`, 2 ctors, single function) — direct sub-term
  recursion only.
- `renderBlock`/`renderTerm` mutual (`Ir.Block` 1 ctor / `Ir.Terminator` 8
  ctors) — only recursive calls are direct fields (`.if`'s `t`/`e`);
  `renderStmt` (called via `stmts.map renderStmt`) is *outside* this
  mutual group already, so it doesn't affect termination here.
- Confirmed via the initial blanket-removal test: these compiled with no
  errors when `partial` was dropped.

### `renderPattern` (`Fun.Pattern`, 4 ctors, single function, line 107)

Every recursive call is `.map`-indirect (`pats.map renderPattern` in
`.destruct`, `renderPattern v` inside a `fun (k, v) => ...` lambda mapped
over `.expand`'s `List (String × Fun.Pattern)` field). The `.expand` case
is the known tuple-lambda failure mode — expect the `.1`/`.2`-projection
fix plus a `termination_by`/`decreasing_by` (single function, so no
mutual-block complication).

### `renderExpr`/`renderBranch` mutual (`Fun.Expr` 12 ctors / `Fun.Branch` 1
ctor, lines 120–145)

Mix of direct sub-term calls (`.let`, `.lambda`, `.apply`'s callee,
`.project`, `.select`'s scrutinee, `.fix`, `renderBranch`'s body) and safe
single-var `.map` calls (`.construct`, `.apply`'s args, `.primitive`,
`.select`'s branches). One tuple-lambda case: `.object`'s
`fun (k, v) => ... renderExpr v` over `List (String × Fun.Expr)`.

### `renderFullStmt`/`Prod`/`Cons`/`Branch` mutual (Full IR, 6/7/7/1 ctors,
lines 152–194)

Same shape as `renderExpr`. Tuple-lambda cases: `.object`'s
`fun (k, ret, stmt) => ...` (a **3-tuple**, `List (String × Id ×
Statement)`) and `.cocase`'s `fun (d, vs, s) => ...` (`List (String ×
List Id × Statement)`, and `vs` itself gets `.map renderName`'d inside —
not a recursive call, so not a termination concern).

### `renderFlatStmt`/`Prod`/`Cons`/`Branch` mutual (Flat IR, 7/6/7/1 ctors,
lines 202–245)

Structurally identical to the Full-IR block (one extra `Statement`
constructor, `.join`, which is direct-recursive; `Producer` has one fewer
constructor, no `.do`). Same two tuple-lambda cases (`.object`, `.cocase`)
as Full.

### `renderJoinStmt`/`Prod`/`Cons`/`Branch` mutual (Join IR, 7/7/7/1 ctors,
lines 253–296)

Same shape again. Tuple-lambda cases: `.object`'s `fun (k, ret, stmt) =>
...` and `.cocase`'s `fun (d, vs, s) => ...`, identical in kind to
Full/Flat.

### `renderPat`/`renderType` (Syntax, 7/9/10 ctors — `renderType` also
covers the mutual family below), lines 419–454

Both already flagged in the original failing-build output (the same
"lost connection" symptom seen in `Rename/Pass.lean`). `renderType`'s
`.record` (`kvs.map fun (k, v) => ...`) and `.variant` (`cases.map fun
(k, ts) => ... ts.map renderType`, a **doubly-nested** case — outer
tuple-lambda, then an inner `.map` over the nested `List (Ty p)`) both
need the fix. `renderPat`'s `.record` (`kps.map fun (k, pt) => ...`) needs
it too. `renderPat` itself has **no direct-subterm recursive call at
all** — every case is `.map`-indirect, but that's fine per the "already
fine" category above except for `.record`.

### `renderExprSyn`/`renderType`/`renderStmtSyn`/`renderClause`/`renderPat`/
`renderCoPat` mutual (Syntax, 16/9/4/1/7/3 ctors, lines 397–459) —
**biggest and most complex group**

This is `renderType`'s actual containing mutual block (so `renderType`'s
fix above must be done *within* this same block, not standalone).
`{p : Syntax.Phase} [RenderId (Syntax.XId p)]` are constant across every
recursive call (never change), so they don't affect the termination
measure — only the final `Syntax.* p` argument does.

- `renderCoPat` (3 ctors): no indirection at all, purely direct fields —
  trivial once the rest of the block's measure exists.
- `renderStmtSyn` (4 ctors): all direct (`renderExprSyn`/`renderPat` on
  literal fields).
- `renderClause` (1 ctor): `pats.toList.map renderPat` (safe, single-var)
  plus a direct `renderExprSyn body`.
- `renderExprSyn` (16 ctors): mostly direct/safe-map; two tuple-lambda
  cases — `.record`'s `kvs.map fun (k, v) => ... renderExprSyn v` and
  `.codata`'s `clauses.map fun (cp, e) => ... renderCoPat cp ...
  renderExprSyn e` (this one recurses into **two different mutual
  types** from inside one lambda — both need fixing together).
- `renderType` (9 ctors, per above): `.record`, `.variant` (doubly
  nested) tuple-lambda cases.
- `renderPat` (7 ctors, per above): `.record` tuple-lambda case.

## Design Choices

**Fix order: easiest/most-isolated first, defer the biggest mutual group
to last.** Each group is independent (no group's fix depends on another
compiling first, since they're separate `mutual` blocks in the same
file), so there's no forced ordering — but doing the single-function and
smaller mutual groups first re-validates the recipe on progressively
larger cases before committing to the ~6-function Syntax block, which is
the one most likely to need iteration.

1. `renderPattern` (single function, 1 tuple-lambda fix) — smallest,
   proves the recipe transfers from `Rename/Pass.lean`/`DiffView.lean`
   to a `Doc`-returning renderer.
2. `renderExpr`/`renderBranch` mutual (2 functions, 1 tuple-lambda fix) —
   first mutual-block test, small.
3. `renderFullStmt`/`Prod`/`Cons`/`Branch` mutual (4 functions, 2
   tuple-lambda fixes, one a 3-tuple).
4. `renderFlatStmt`/`Prod`/`Cons`/`Branch` mutual — structurally identical
   to Full, should be nearly copy-paste once Full's fix pattern is
   proven.
5. `renderJoinStmt`/`Prod`/`Cons`/`Branch` mutual — same shape as
   Full/Flat again.
6. `renderPat`/`renderType`/`renderExprSyn`/`renderStmtSyn`/`renderClause`/
   `renderCoPat` mutual (6 functions, phase-indexed with a typeclass
   param, 4 tuple-lambda fixes including one doubly-nested and one
   crossing two mutual types) — last, biggest, most likely to need real
   iteration.

**Why not a single combined `decreasing_by` tactic reused verbatim
everywhere:** the established `all_goals simp_wf; all_goals first | omega
| (rename_i h; exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by
omega)) | ...` pattern generalizes, but each new tuple-lambda shape needs
its own alternative added to the `first | ... |` chain (as seen going
from Rename/Pass.lean's single-tuple case to DiffView.lean's `let`-based
one) — expect to extend the combinator per group rather than reuse one
verbatim across all 6, and expect at least one extra `sizeOf`-relating
helper lemma for the 3-tuple and doubly-nested cases (a 3-tuple's third
component's `sizeOf` bound follows the same
`cases p with | mk a b c => simp; omega` pattern already used for pairs,
just one more projection deep).

**Why mutual `termination_by`/`decreasing_by` per-function rather than
one shared clause:** confirmed empirically this session against Lean's
own core-library convention — there is no single combined syntax; each
function's clause goes immediately after its own equations, inside the
`mutual...end` block.

## Implementation Plan

Each task is independently assignable (separate mutual blocks, no shared
state) but should land as **one PR** rather than six, matching this
session's existing pattern of one Tier 3 file per PR — splitting further
would just be six small commits inside the same file/PR review, not
independent worktrees.

### Task 1: `renderPattern`

- **Goal**: de-partialize the single-function case.
- **Scope**: `lean/Malgo/Debug/PrettyIR.lean`, `renderPattern` (~line
  107).
- **Dependencies**: none.
- **Steps**: drop `partial`; fix `.expand`'s `fun (k, v) => ...` to `fun
  kv => ... kv.2 ...`; add `termination_by`/`decreasing_by` following the
  `Rename/Pass.lean` recipe (one `sizeOf_snd_lt_of_mem`-shaped helper if
  not already reusable from another file — check whether a shared
  private helper in this file makes sense, since later tasks need the
  identical lemma shape).
- **Verification**: `lake build Malgo.Debug.PrettyIR` in isolation before
  moving on.

### Task 2: `renderExpr`/`renderBranch` mutual

- **Goal**: de-partialize the first mutual block.
- **Scope**: same file, ~lines 120–145.
- **Dependencies**: none (independent of Task 1, though reuses its helper
  lemma if extracted generically).
- **Steps**: drop `partial` from both; fix `.object`'s tuple lambda; add
  per-function `termination_by`/`decreasing_by` inside the `mutual`
  block, one measure per function (`sizeOf` on each function's own IR
  argument).
- **Verification**: isolated build.

### Task 3: `renderFullStmt`/`Prod`/`Cons`/`Branch` mutual

- **Goal**: de-partialize the Full-IR renderer family.
- **Scope**: ~lines 152–194.
- **Dependencies**: none, but structurally previews Tasks 4–5.
- **Steps**: drop `partial` from all four; fix `.object`'s 3-tuple lambda
  (`fun (k, ret, stmt) => ...`) and `.cocase`'s (`fun (d, vs, s) =>
  ...`); one `termination_by`/`decreasing_by` per function (4 clauses
  total, each referencing whichever of the other 3 functions it calls
  mutually).
- **Verification**: isolated build.

### Task 4: `renderFlatStmt`/`Prod`/`Cons`/`Branch` mutual

- **Goal**: de-partialize the Flat-IR renderer family.
- **Scope**: ~lines 202–245.
- **Dependencies**: none functionally, but do after Task 3 — structurally
  near-identical, so the fix should transfer directly with minimal
  adaptation (one extra `Statement` ctor `.join`, direct-recursive, no
  extra work; one fewer `Producer` ctor, nothing to add).
- **Steps**: same as Task 3, applied to Flat's four functions.
- **Verification**: isolated build.

### Task 5: `renderJoinStmt`/`Prod`/`Cons`/`Branch` mutual

- **Goal**: de-partialize the Join-IR renderer family.
- **Scope**: ~lines 253–296.
- **Dependencies**: none functionally, do after Task 3/4 for the same
  "structurally identical, fix transfers" reason.
- **Steps**: same as Task 3, applied to Join's four functions.
- **Verification**: isolated build.

### Task 6: Syntax mutual block (`renderExprSyn`/`renderType`/`renderStmtSyn`/
`renderClause`/`renderPat`/`renderCoPat`)

- **Goal**: de-partialize the last and biggest mutual family.
- **Scope**: ~lines 397–459.
- **Dependencies**: do last — biggest, most likely to need real
  iteration (6 functions, one doubly-nested tuple-lambda in `.variant`,
  one lambda crossing two mutual types in `.codata`).
- **Steps**: drop `partial` from all six; fix `renderExprSyn`'s `.record`
  and `.codata` tuple lambdas, `renderType`'s `.record` and `.variant`
  (doubly-nested — may need an extra helper lemma for "recursing into an
  element of a list that's itself inside a tuple that's itself inside a
  list"), `renderPat`'s `.record`; add 6 `termination_by`/`decreasing_by`
  clauses, one per function, each only measuring the `Syntax.* p`
  argument (the phase `p` and typeclass instance are constant across all
  calls and shouldn't enter the measure).
- **Verification**: isolated build; this is the task most likely to
  reveal an unanticipated 9th/10th failure mode given its size — if it
  does, treat that as new information worth a fresh check-in rather than
  pushing through blindly (consistent with how this session has handled
  every other unexpected-difficulty discovery).

## Verification (whole-file, after all 6 tasks)

1. `grep -n "^partial def" lean/Malgo/Debug/PrettyIR.lean` → empty.
2. `grep -n "sorry" lean/Malgo/Debug/PrettyIR.lean` → empty.
3. `mise run lean-build` (full project) — confirms no downstream callers
   broke (PrettyIR is used by `malgo debug-trace`/`Debug/MetPage.lean`
   and the `Malgo.Debug.PrettyIR` golden tests).
4. `mise run lean-test` — the ~89 `Debug.PrettyIR` golden files (plus any
   MetPage goldens) byte-compare rendered output; this is the real
   behavior-preservation check, not just type-checking.
5. Re-diff the final change to confirm no `render*` function's *output*
   changed — only its internal recursion structure (i.e. no stray `.1`/
   `.2` typo silently reordering a tuple, etc.).

## Risks

| Risk | Mitigation |
|------|------------|
| The doubly-nested `.variant`/`.codata` cases in Task 6 need a genuinely new helper lemma shape (not just a bigger tuple), which could take real iteration | Attempt with the established recipe first; if it doesn't converge in a reasonable number of `lake build` iterations, check in rather than open-endedly iterating (matches this session's established pattern for every prior scope surprise) |
| Six separate `termination_by`/`decreasing_by` clauses in one `mutual` block (Task 6) is more moving parts than anything attempted so far — a measure mismatch in one function could produce confusing errors attributed to a different function | Add clauses one function at a time, rebuilding after each, rather than writing all 6 up front |
| `RenderId (Syntax.XId p)` typeclass parameter might interact unexpectedly with `termination_by`'s measure inference (untested combination) | Verify early (Task 6's first function) rather than assuming it's transparent like the scratch test's non-typeclass case |
| Splitting into 6 tasks but landing as 1 PR risks a large, hard-to-review diff | Keep the PR description structured per-task (mirroring this doc's task list) so review can proceed group-by-group even though it's one changeset |
