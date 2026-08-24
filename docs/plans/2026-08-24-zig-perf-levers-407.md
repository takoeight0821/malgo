# Zig backend perf: remaining levers toward the ~2x target (#407)

Date: 2026-08-24

## Context

#407 tracks the Zig backend's remaining performance gap against its ~2x
target, currently measured at **5.21x slower than Chez Scheme** on the
`l2-ratio` tier (chez 47s / zig 245s, `bash mise run perf-baseline --
--tier=l2-ratio`). Its original brief named four levers: (1) container
allocation combines multiple heap operations, (2) tag/field-name
interning, (3) an object pool / single-threaded allocation mode, (4)
call-dispatch bookkeeping.

This design was chartered by the `/wayfinder` map at
[#465](https://github.com/takoeight0821/malgo/issues/465), whose ticket
[#472](https://github.com/takoeight0821/malgo/issues/472) recorded the
maintainer's explicit decision: pursue #407 now, and execute it as **one
focused, intensive session** rather than split across multiple pickups —
a deliberate exception to this repo's usual single-purpose-PR convention,
justified by all four levers sharing the same allocation/dispatch hot
path and the same before/after baseline.

### What's already done: PR #462

Between #472's ticket being opened and this document, **PR #462** (merged
`eeaec639`, "perf(zig): tighten MAX_ARGS from 4 to 2, shrinking Action by
two words") landed and resolved two of the four levers on its own:

- **Lever 4 (dispatch bookkeeping): done.** `MAX_ARGS` (`runtime/zig/runtime.zig:64`)
  was tightened from 4 to 2 after confirming empirically (220k+ call
  sites across the whole corpus) that no generated call ever carries more
  than 2 arguments. `Action` (`:73-78`) shrank from 7 to 5 words
  (comptime-asserted at `:84-88`), which `run`'s trampoline (`:689-701`)
  copies by value on every dispatch — 1.4e10+ times on selfhost-l2.
  Nothing further looks prunable here without a representation change
  (`argc` could be bit-packed to save one word, but that trades a simple
  comptime-guarded struct for pointer-tagging risk over one word — not
  worth it).
- **Lever 2 (tag/field-name interning): confirmed unnecessary, zero work
  needed.** Independently re-verified during this design's investigation
  (an `Explore` agent traced `tagEq`/`stringEq` at `:790`/`:801` and
  tested a real 24,000-line generated file): Zig 0.16's `std.mem.eql`
  already takes a pointer-equality fast path (`.../lib/std/mem.zig:732-746`,
  before any byte scan) and `emitProgram` (`Emit.lean:250`) compiles the
  entire runtime plus every generated function as **one Zig compilation
  unit**, so identical tag/field-name string literals — always
  compile-time literals here, stored by reference and uncopied at both
  construction and use sites — get deduplicated to the same address by
  Zig itself. Every tag/field-name comparison against a matching literal
  is already an O(1) pointer compare. "Interning" in the sense #407's
  brief meant is already happening for free.
  - A related but **materially different and more invasive** lever
    surfaced during this investigation: pattern-match dispatch is still a
    linear if/else chain (`emitTest`'s `.tagEq` case, `Emit.lean:94-96`),
    not a jump table, and record/codata field lookup is a linear scan.
    For closed-set ADT constructors (not row-polymorphic records/codata,
    where the field layout genuinely isn't statically known), assigning
    each constructor a dense integer discriminant at compile time and
    emitting a Zig `switch` would be a real structural change. This is
    **out of scope for this document** — it's not what #407's brief asked
    for, and deserves its own issue with its own risk/benefefit case
    rather than being folded in here.

That leaves two levers for this document to plan: **lever 1** (container
allocation) and **lever 3** (object pool / single-threaded allocation).

### Lever 1: container allocation combining — investigated, staying deferred

Every `mk*` constructor does 2-3 separate heap operations: a `g_value.dupe`
of its variable-size array (fields/captures/branches) followed by
`alloc`'s `g_value.create(Object)` (`runtime.zig:472-493`, `210-216`).
`mkStruct`/`mkClosure` are 2 operations; `mkRecord`/`mkCodata` are 3.
(`mkCodata` is dead code from the Zig backend's actual output today — no
`.mkCodata` IR variant exists and no generated Zig calls it; only a
runtime unit test does. Struct/closure/record are the real-world targets.)

Both PR #462's own investigation and this document's independent `Explore`
agent reached the same conclusion: **this lever is genuinely invasive**,
for two compounding reasons:

1. **A real correctness hazard.** `dropReuse`/`mkStructReuse`
   (`runtime.zig:318-376`) recycle a uniquely-referenced Object *in place*
   — same pointer, same identity — when the compiler's static `Reuse`
   pass proves a `Drop` and a later same-arity `MkStruct` pair in one
   block. Both plausible "combine into one allocation" shapes threaten
   this: an inline small-array field (`Object` grows a `[N]Value`) still
   touches every construction/free/reuse site across both `.strukt` and
   `.closure` (they share one reuse-token protocol, so they can't be
   changed independently — it's the whole hazard surface or none of it);
   a single over-allocated block (`Object` + trailing fields) would move
   the Object's address whenever a reuse pairs mismatched arities,
   breaking `dropReuse`/`mkStructReuse`'s same-pointer identity contract
   outright.
2. **The evidence gate can't see the win.** `g_total_allocs`
   (`runtime.zig:143`) counts `Object` allocations only, not the `dupe`
   calls alongside them — the exact malloc traffic this lever would
   reduce is invisible to the counter this repo's perf-baseline ratchet
   gates on. Landing this lever today would be an ungated change: no
   counter in `bench/perf-baseline.json` would move, so there'd be no
   before/after evidence a CI regression check could ever catch if a
   future change silently reverted it.

**Decision: stay deferred**, unchanged from PR #462's assessment. Fixing
(2) — adding a real malloc-call counter alongside `g_total_allocs` — is a
prerequisite for ever picking this lever up rebustly, but is itself
out of scope here: it's test/measurement infrastructure with no
performance effect of its own, and doesn't fit in the single focused
session this document is scoping (see Design Choices).

## Design Choices

**Scope this session to lever 3 only.** Of the two remaining levers,
lever 3 is the one with a contained, well-understood change surface and a
gate that can actually see its effect:

- `initHeap` (`runtime.zig:188-192`) selects `std.heap.smp_allocator` for
  `.ReleaseFast`/`.ReleaseSmall` — the modes `perf-baseline` actually
  measures. Zig 0.16's `SmpAllocator` is built for multithreading: every
  alloc/free does a thread-local thread-index lookup plus a per-thread
  `mutex.tryLock()` (with contention retry across up to 128 slots) before
  touching a size-class freelist. Malgo's runtime never spawns a thread
  (confirmed: no `std.Thread`/`spawn(` anywhere in `runtime.zig`) and
  dispatch is one single-threaded trampoline loop — so all of that
  locking and thread-local-storage machinery is pure overhead on every
  single allocation and free, exactly as #407's brief framed it.
- `Object` (`runtime.zig:115`) is one fixed-size struct
  (`{rc: u32, kind: Kind, payload: Payload}`, `Payload` sized to its
  largest variant) regardless of `Kind`. `alloc` (`:210-216`) is the sole
  Object allocation site. A pool for Objects specifically needs **no
  size-class bucketing** — a single freelist of Object-sized chunks
  covers every allocation this pool serves.
- This doesn't touch `dropReuse`/`mkStructReuse`'s existing in-place
  recycling (`:318-376`) at all — that stays exactly as-is, a narrower,
  compile-time-proven single-slot optimization that already avoids
  `alloc`/`free` entirely on the paths it covers. A general Object pool is
  additive: it catches drop-then-alloc pairs the static `Reuse` pass
  can't prove adjacent (non-adjacent code, `.record`/`.codata`/shared
  values, arity mismatches — all of which fall back to plain
  `alloc`/`free` today), and it removes `smp_allocator`'s locking
  overhead even on `Reuse`'s own fallback path.
- **Debug/ReleaseSafe are untouched.** They stay on `DebugAllocator`
  exactly as today, preserving the leak/UAF/double-free detection the
  golden sweep and leak-check gate (every compiled binary self-checks at
  exit) depend on. The pool is a new arm under the existing
  `.ReleaseFast, .ReleaseSmall` branch of `initHeap`'s switch
  (`runtime.zig:189-192`), not a change to the switch's other arm.
- Variable-size arrays (`dupe`'d fields/captures/branches, string bytes)
  keep using `smp_allocator` as today. Pooling them would need real
  size-class bucketing (per the value-construction investigation) — a
  separate, additive lever, and adding it now would push this outside a
  single focused session for a much smaller expected win (those arrays
  are typically small and short-lived compared to Object churn).

**Alternative considered: swap the whole allocator to `std.heap.c_allocator`.**
A one-line change instead of a custom pool. Rejected as the primary plan:
`c_allocator` still goes through libc `malloc`/`free`, which does its own
internal bookkeeping (and, depending on platform libc, its own locking) —
there's no guarantee it's actually cheaper than `smp_allocator` for this
runtime's allocation pattern, and it doesn't specifically target the one
site (`alloc`, fixed-size, highest churn) known to matter most. Worth
trying as a **quick comparison data point** before committing to the
custom pool (see Task 1), since it costs one line to measure.

**Measure via wall-clock, not the allocation-count ratchet.** A pure
allocator swap changes *how expensive* an allocation is, not *how many*
happen — `total_allocs`/`dispatches`/`force_depth_max` are expected to be
byte-identical before and after (same op count, same reduction steps),
mirroring what PR #462 found for its own counter-neutral change. The
actual evidence for this lever is `l2-ratio`'s wall-clock comparison
(`mise run perf-baseline -- --tier=l2-ratio`, currently chez 47s / zig
245s / 5.21x — the exact number #407 exists to move) plus a plain
`--timing` sample on `fib-deep`/`selfhost-l2` for a sanity cross-check.
This repo's established culture for exactly this kind of "does it
actually help" question is measure-first, keep-if-real (see #354's own
precedent: a plausible-sounding optimization, measured, and shelved
because the measured win was smaller than the implementation cost
justified). Apply the same discipline here: if the pool's measured
`l2-ratio` win is not clearly outside the tier's existing 15% noise band,
don't merge it — revert and record the negative result on #407, the same
way #354 recorded its negative result.

## Implementation Plan

Sequenced for **one agent, one session**, not split across parallel
teammates — per the maintainer's explicit instruction on ticket #472.
Every task below touches the same file (`runtime/zig/runtime.zig`) and
the same measurement artifact (`bench/perf-baseline.json`), so parallel
worktrees would only create merge conflicts, not save time.

### Task 1: baseline + quick `c_allocator` comparison point

- **Goal:** Establish the exact pre-change baseline, and get a one-line
  data point on whether `c_allocator` alone moves the needle, before
  investing in the custom pool.
- **Scope:** `runtime/zig/runtime.zig:191` (temporary, reverted at the end
  of this task), `bench/perf-baseline.json` (read-only in this task).
- **Dependencies:** none.
- **Steps:**
  1. `mise run perf-baseline -- --tier=all` on current `master` (do not
     `--update`); record the `l2-ratio` wall-clock numbers alongside the
     committed `total_allocs`/`dispatches`/`force_depth_max` for all
     tiers as this task's "before" reference. Note: the triage-followup
     history records the committed `selfhost-l2` row already drifting
     from `origin/master` HEAD before any of this work — that drift is
     pre-existing and unrelated; don't try to fix it here, just don't
     mistake it for a regression this work introduced.
  2. Temporarily change line 191's `.ReleaseFast, .ReleaseSmall =>
     std.heap.smp_allocator` to `std.heap.c_allocator`, rebuild
     (`mise run bust-runtime` first — required after any
     `runtime/zig/runtime.zig` edit), rerun `--tier=l2-ratio` three times
     for noise-averaging.
  3. Record the result as a data point in this document's Verification
     section (not committed to `bench/perf-baseline.json`). Revert the
     line before Task 2.
- **Verification:** the "before" numbers are captured; the `c_allocator`
  data point is recorded either way (a null result is still useful
  context for Task 2's writeup).

### Task 2: single-threaded Object pool

- **Goal:** A freelist pool for `Object`-sized allocations, active only
  under `.ReleaseFast`/`.ReleaseSmall`, with zero effect on
  `.Debug`/`.ReleaseSafe`.
- **Scope:** `runtime/zig/runtime.zig` (`initHeap`, `alloc`, and a new
  pool implementation near them; no other file needs to change —
  `dropReuse`/`mkStructReuse` and every `mk*` constructor stay untouched,
  since the pool sits entirely inside `alloc`'s existing
  `g_value.create(Object)` call, not at the constructors' call sites).
- **Dependencies:** Task 1 (baseline).
- **Steps:**
  1. Implement a simple intrusive freelist: a global (single-threaded,
     no lock — malgo never spawns a thread) singly-linked list of
     Object-sized free chunks. On `alloc`: pop the freelist if non-empty,
     else fall back to `g_value.create(Object)` (still `smp_allocator`
     underneath, for the case a chunk isn't available). On `free`
     (`drainFreeWorklist`, `runtime.zig:267-...`): after an Object's
     `.strukt`/`.closure`/etc-specific teardown, push its slot onto the
     freelist instead of `g_value.destroy(obj)`.
  2. Gate the whole pool behind `builtin.mode == .ReleaseFast or
     builtin.mode == .ReleaseSmall` at the same points `initHeap`
     branches today (`:189-192`) — `.Debug`/`.ReleaseSafe` must allocate
     and free through `DebugAllocator` exactly as before, unconditionally,
     so leak/UAF/double-free detection is untouched.
  3. Decide up front whether the freelist itself needs an upper bound
     (unbounded growth risk if a long-lived program frees far more than
     it currently holds live) — a simple cap (e.g. only retain up to some
     small multiple of `g_live_objects`'s high-water mark, freeing the
     rest back to `smp_allocator`) is enough; don't over-engineer this
     without a concrete case showing it matters.
  4. `mise run bust-runtime`, then `mise run test`, `bash
     scripts/zig-golden.sh` (81/81, zero leaks — the leak-check gate is
     the sharpest check that pool bookkeeping doesn't double-free or
     leak), `bash scripts/zig-deep-recursion.sh` (counters unchanged).
- **Verification:** all of the above green; `mise run perf-baseline --
  --tier=all` shows `total_allocs`/`dispatches`/`force_depth_max`
  byte-identical to Task 1's "before" numbers (this change must be
  counter-neutral — same allocation count, just cheaper allocations).

### Task 3: measure, decide keep-or-revert, record the result

- **Goal:** An evidenced go/no-go, recorded on #407 regardless of outcome
  — mirroring #354's own precedent of recording a negative result rather
  than silently dropping the idea.
- **Scope:** `bench/perf-baseline.json` (only if keeping the change);
  a comment on #407.
- **Dependencies:** Task 2.
- **Steps:**
  1. Run `mise run perf-baseline -- --tier=l2-ratio` at least 3 times
     post-change; compare against Task 1's baseline and the tier's
     existing 15% noise band.
  2. **If the win is clear and outside the noise band:** keep the change,
     `mise run perf-baseline -- --tier=all --update` to reseed
     `bench/perf-baseline.json` (the counter fields should be unchanged
     per Task 2's verification; only wall-clock-adjacent fields, if any
     are tracked, would move), open the PR, and comment on #407 with the
     before/after numbers.
  3. **If the win is within noise or negative:** revert the pool
     entirely (do not leave partially-gated dead code behind), and post
     a comment on #407 recording the measured result and that lever 3 is
     shelved for the reason #462 already predicted (a modest win over
     `smp_allocator`'s already-thread-local free list) — closing out
     #407's brief as "levers 2 and 4 done (#462), lever 1 deferred
     (invasive, needs new accounting first), lever 3 measured and not
     worth it." Either way, #407 reaches a decided, evidenced state.
- **Verification:** #407 carries a dated comment with the actual
  measured numbers, whichever way it went.

## Verification

After Task 3, regardless of outcome: `mise run test`, `bash
scripts/zig-golden.sh`, `bash scripts/zig-deep-recursion.sh`, `bash
scripts/selfhost-golden.sh` all green, zero leaks. If the pool is kept,
`bench/perf-baseline.json`'s counter fields (`total_allocs`, `dispatches`,
`force_depth_max`) must be unchanged from pre-change values — this lever
is expected to be counter-neutral by design (see Design Choices).

## Risks

| Risk | Mitigation |
|------|------------|
| Freelist pool masks a genuine leak or double-free that `DebugAllocator` would have caught in Debug/ReleaseSafe | Pool is gated to `.ReleaseFast`/`.ReleaseSmall` only; `.Debug`/`.ReleaseSafe` paths are untouched, so the existing leak-check gate still runs on the same code shape it always has |
| The measured win is smaller than #462 already predicted, making this a wasted session | Task 1's cheap `c_allocator` comparison point surfaces this early; Task 3 explicitly allows reverting and recording a negative result rather than forcing a merge |
| Unbounded freelist growth under a long-lived or allocation-heavy program | Cap the freelist size in Task 2 step 3; fall back to the underlying allocator once at the cap |
| Conflating this work with the pre-existing `selfhost-l2` baseline drift noted in #462's commit message | Task 1 explicitly measures and notes this drift as pre-existing before touching anything; not fixed here |
| Scope creep into lever 1 (container allocation) or the closed-set-ADT switch-dispatch idea found during investigation | Both are explicitly out of scope for this document (see Context); lever 1 stays deferred, the switch-dispatch idea is noted as a candidate for a future, separate issue |
