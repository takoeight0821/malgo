# Perceus Reference Counting

The Zig backend's memory model is Perceus (PLDI 2021), as adapted for compiled
closures by Lean 4's "Counting Immutable Beans": a pure `Ir.Program -> Ir.Program`
rewrite (`Malgo.Backend.Zig.Perceus`) that inserts `dup`/`drop` operations so that, on
every non-panic control-flow path, every owned reference is consumed exactly once.
No tracing garbage collector runs at any point — every allocation's lifetime is
determined statically, at compile time, by counting occurrences in the IR.

This document assumes familiarity with the Zig backend's pipeline; see
[`zig-backend.md`](zig-backend.md) for where Perceus sits relative to `ClosureConv`,
`Peephole`, `Reuse`, `RcCheck`, and `Emit`.

## Ownership discipline

Ownership rules, matching the runtime's actual behavior (`runtime/zig/runtime.zig`):

- A function owns its parameters, and — for closure/field functions — its `self`
  closure object.
- Captures are borrowed reads out of `self` (`ReadCapture`), promoted to owned by a
  `Dup` if still live afterwards; `self` is dropped as soon as it is dead (right
  after the last capture read — the required dup-captures-before-drop-self ordering
  falls out of plain liveness, not a special case).
- `MkStruct`/`MkClosure`/`MkRecord` move one reference per operand into the new
  object; `Force` moves one reference of the record (the field function drops it as
  its own `self`); every operand of a consuming terminator moves into the call.
- `Prim` operands, `ReadPath`/`ReadCapture` sources, and guard tests only borrow —
  they read a value without taking ownership of it.

At every insertion point, all `Dup`s precede any `Drop` of the same statement
position (garbage-free ordering): a struct's live fields are duplicated by their
borrowed-let bindings before liveness kills the struct itself, so a value is never
observably at refcount zero while a sibling binding still needs to read it.

## How `dup`/`drop` get inserted

`Perceus.insertBlock` walks a `Block`'s statement list with a set Δ of owned
variables currently in scope (the invariant: each Δ variable holds exactly one
owned reference at that point). Two rules combine at each statement:

- **(D) Eager drop.** Before processing a statement, drop every Δ variable that is
  dead from this point on (not in `freeVarsBlock` of the remaining statements and
  terminator).
- **(Let) Owning-let / borrowed-let.** A binding that borrows (`ReadPath`,
  `ReadCapture`) is promoted to owned by a `Dup` if it survives past this point, or
  simply elided if it's immediately dead. A binding that consumes operands
  (`MkStruct`, `MkClosure`, `MkRecord`, `Force`, most `Prim`s) needs one `Dup` per
  operand occurrence that is still live afterwards (an operand occurring twice needs
  one dup for the second occurrence, since one reference already moves with the
  first) — an operand that dies exactly here needs no dup at all, since its own
  reference moves into the new binding.
- **(T) Consuming terminators** apply the same "one dup per live-afterwards repeat
  occurrence" rule to a terminator's operands, after (D) has already reduced Δ down
  to exactly that operand set.

`reuseHint` (inserted by `Malgo.Sequent.ReuseSpecialize`, see
[`zig-backend.md`](zig-backend.md)) is the one primitive that *consumes* its operand
rather than borrowing it — marking that operand's last use immediately before a
reconstruction, for the `Reuse` pass below to recognize.

### Why this can't be quadratic by accident

Both (D) and (Let) need "the free variables of everything from here on" at every
statement position. Computing that with a fresh `freeVarsBlock` call on each
shrinking suffix, at every recursive step, would make one pass over an
`n`-statement block cost `O(n²)`. `Ir.suffixFreeVars` instead computes every
suffix's free-variable set in a single bottom-up `scanr`, and `Perceus`/`Emit`
thread the resulting list alongside their recursion instead of recomputing.

## Reuse tokens (FBIP)

`Malgo.Backend.Zig.Reuse` runs after Perceus (it needs Perceus's `Drop` placement)
and before `RcCheck`. Within one straight-line statement list (never crossing an
`if`'s branches — those are separate `Block`s, recursed into independently), it
pairs the nearest preceding `Drop` with a later `MkStruct`, LIFO — the same pairing
order Koka's own reuse analysis uses — rewriting both into `DropReuse`/
`MkStructReuse`.

At runtime, `rt.dropReuse` recycles the dropped Object in place when it was
uniquely referenced (struct, closure, and scalar payloads are all eligible — not
just literal cell reuse of the same shape), falling back to an ordinary drop and a
null token otherwise. `rt.mkStructReuse` then either overwrites that recycled Object
or allocates fresh from a null token. No static arity check is needed at this
level: every pairing is only a candidate, verified dynamically per call, and a
rewritten-but-never-taken fallback path is exactly as correct (if less cheap) as the
original `Drop`/`MkStruct` pair would have been.

Object reuse only ever recycles a *single* backing array per Object — a `record`'s
separate fields array, a `codata`'s separate branches array, and a `string`'s bytes
all fall back to an ordinary drop/allocate pair rather than adding another
bookkeeping case to `dropReuse`.

`MALGO_RC_STATS=1` on a compiled binary reports `reuse_hits` alongside
`total_allocs` at exit, to measure this pass's effect.

## `RcCheck`: static verification, not just documentation

`Malgo.Backend.Zig.RcCheck.checkProgram` symbolically executes the RC-annotated
program Perceus + Reuse produced, counting each variable's owned references along
every control-flow path, and reports any path where a reference is:

- consumed twice, or used after being consumed (`UseAfterConsume`),
- never consumed (`UnconsumedAtExit`),
- `Dup`'d or `Drop`'d while already dead (`DupOfDead`/`DropOfDead`), or
- a reuse token (`MkStructReuse`) referencing a token that was never produced by a
  `DropReuse`, or was already consumed (`TokenUnavailable`/`TokenUnconsumed`).

`Malgo.Backend.Zig`'s `runPassImpl` runs this check unconditionally — on every
compile, not just in the test suite or behind a debug flag — turning any
Perceus/Reuse bug into a *compile-time* error (`ZigError`) rather than a
use-after-free or a leak in the produced binary. The golden-test corpus also runs it
directly on every testcase with no Zig toolchain needed at all, since it is a
pure function over the IR.

`RcCheck`'s model is deliberately stricter than heap reachability: a borrowed alias
(a `ReadPath`/`ReadCapture` result) is only accessible while its root still holds a
reference *in this scope*, or the alias was itself promoted by a `Dup` — exactly
Perceus's own local ownership discipline, which is what makes it a useful oracle for
Perceus's output specifically, rather than a general-purpose verifier.

## Runtime implementation

`runtime/zig/runtime.zig` backs all of the above:

- Every `Value` is a `*Object { rc: u32, kind: Kind, payload: Payload }`. `IMMORTAL`
  (`0xFFFF_FFFF`) marks a value that lives for the whole process (`rt.no_self`);
  `dup`/`drop` are no-ops on it.
- `dup`/`drop` are the primitive ops the compiler inserts directly.
  `drop` decrements and, at zero, queues the Object onto a deferred free worklist
  (`g_free_worklist`) rather than recursing immediately — so dropping a deep
  structure (a 100k-element cons list) never recurses on the native stack.
- Two heaps: `g_value` (Objects, their fields/captures backing arrays, string
  payload bytes — individually freed, and what the leak check below covers) and a
  scratch arena (transient non-`Value` bytes: print formatting, parse/encode
  buffers, the free worklist's own storage) — bulk-freed at process exit, exempt
  from the leak check.
- `g_value` is `std.heap.DebugAllocator` in `Debug`/`ReleaseSafe` (catching
  non-`Object` leaks, use-after-free, and double-frees with per-allocation stack
  traces) and `std.heap.smp_allocator` in `ReleaseFast`/`ReleaseSmall`, where
  `g_live_objects` alone still provides the zero-leak invariant, build-mode
  independent.
- `exitWithLeakCheck` (the generated `main`'s last statement) checks
  `g_live_objects != 0` — any leftover live Object means Perceus under-dropped
  somewhere — printing `MALGO-LEAK: <N> objects` and exiting 83 (the zig-golden
  harness's dedicated leak bucket) rather than exiting 0 through a corrupted or
  simply-wrong program.

### A note on `std.debug.assert` vs. explicit checks

`std.debug.assert` is compiled out entirely in `ReleaseFast`/`ReleaseSmall` — it is
the right tool for internal invariants where a violation always indicates a compiler
bug and a full stack trace during development is the priority (e.g. `drop`'s
underflow check). But `mkStructReuse`'s token-safety check
(`obj.rc == 1 and obj.kind == .strukt`) is the *last* line of defense behind
`RcCheck`'s static verification — if RcCheck ever has a false negative, this is what
stands between a malformed token and silently overwriting memory that was never
actually vacated. It therefore runs both ways: `std.debug.assert` first, for a full
stack trace in `Debug`/`ReleaseSafe`, followed by an explicit `if`/`panic` that is
never compiled out, as the actual guard in `ReleaseFast`/`ReleaseSmall` (at the cost
of only a one-line message there, not a trace).

## Debugging: `MALGO_RC_TRACE` and `scripts/rctrace.py`

Every `dup`/`drop`/`dropReuse`/`mkStruct`/`mkClosure`/`mkStructReuse`/`mkRecord` the compiler
emits actually calls a `*Named` wrapper (`dupNamed`, `dropNamed`, ...) that performs
the exact same RC operation, then — only when the `MALGO_RC_TRACE` environment
variable is set — emits one JSON-lines event to stderr carrying the compile-time
symbolic name and enclosing function `Malgo.Backend.Zig.Emit` threads through from
the IR. Tracing is purely additive: the plain, untraced operations are what every
`zig test` block in `runtime.zig` exercises directly, so tracing can never perturb
the RC decisions those tests check.

```
MALGO_RC_TRACE=1 ./some_compiled_binary 2>trace.jsonl
python3 scripts/rctrace.py trace.jsonl                    # first dropReuse_miss
python3 scripts/rctrace.py trace.jsonl --target 0x104d604c0
python3 scripts/rctrace.py trace.jsonl --target 0x104d604c0 --at-line 812
```

`rctrace.py` replays the log to answer "who still holds a reference to this object
right now" — resolving an address to a *generation* (the heap reuses freed
addresses, so a `dropReuse_miss` needs scoping to the construction event at or
before the line in question, not every event that ever touched that address), then
listing the (container, slot) pairs that captured it and were never released by a
matching `decChild`.

Because `name`/`func` are compiler-generated, theoretically unbounded-length
strings formatted into fixed-size stack buffers in the runtime's tracing code, an
unusually long identifier can overflow the buffer; rather than silently dropping
that event, the runtime emits a `{"ev":"trace_overflow"}` marker in its place, and
`rctrace.py` surfaces a warning (both globally and at the specific
live-referrers lookup) whenever such a marker falls within the range it's
analyzing — a bit of trace loss is visible, not a silently wrong conclusion.
