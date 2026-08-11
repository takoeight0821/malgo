//! Malgo Zig backend runtime.
//!
//! Memory model (M9): every Value carries a reference count; `dup`/`drop`
//! calls are inserted by the compiler's Perceus pass. Two heaps:
//!
//!   * `g_value` -- Objects, their fields/captures arrays, and string
//!     payload bytes. Individually freed when a count reaches zero;
//!     covered by the leak check.
//!   * scratch (an arena) -- transient non-Value bytes (print formatting,
//!     parse/encode buffers, the free worklist). Bulk-freed at process
//!     exit; exempt from the leak check.
//!
//! I/O deliberately bypasses Zig 0.16's new async `std.Io` interface (which
//! churned significantly between 0.15 and 0.16) in favor of direct libc/
//! POSIX calls (`std.c.write`, `std.posix.read`), which are a thin, stable
//! layer unlikely to move under us across Zig point releases. All writes
//! are unbuffered, so there is nothing to flush and no "flush before exit /
//! before stdin read" ordering bug to get wrong.
const std = @import("std");
const builtin = @import("builtin");

// ===== Value representation =====

pub const Kind = enum(u8) { int32, int64, float, double, char, string, strukt, closure, record, codata, unit };

pub const Tag = union(enum) { tuple: void, named: []const u8 };

/// Self-passing calling convention (Koka/Lean style): a closure/record
/// field/codata branch receives the closure object itself as `self` and
/// reads its captures via `capturesOf(self)`. Under Perceus RC (M9), this
/// is what makes "a call consumes one reference of the callee" possible:
/// the callee dups the captures it needs and then drops `self` -- the
/// caller has no post-call point to do either, since every call is a tail
/// call. Top-level definitions are called directly with the `no_self`
/// sentinel and ignore it.
///
/// Generated code never *performs* a call: it returns an `Action` naming
/// the call it wants, and `run` below dispatches it in a loop. Zig does
/// not guarantee tail-call optimization, so emitting these tail calls as
/// native `return f(...)` grew the stack by one frame per reduction step
/// -- ~98.6 bytes each, never popped until the program exited, which
/// SIGSEGV'd any program of more than ~150k steps (issue #360). Same
/// reasoning as `g_free_worklist`'s iterative free below: a structurally
/// recursive process gets an explicit loop rather than the native stack.
pub const CodeFn = *const fn (self: Value, args: []const Value) Action;

/// Maximum number of arguments at any call site. The front end cannot
/// currently produce more than 2 (`ToFun` builds only single-parameter
/// lambdas and singleton applies; `ToCore` appends exactly one consumer,
/// giving `TCallClosure f [arg, kont]`), but nothing in the IR enforces
/// that, so this is generous. `Malgo.Backend.Zig.Emit` mirrors the number
/// to reject an over-arity call site with a readable message; `mkAction`'s
/// `rcInvariant` below is the authoritative backstop if the two drift.
pub const MAX_ARGS: usize = 4;

/// What a generated function returns instead of calling: either the next
/// call to perform (`code != null`) or the finished value (`code == null`,
/// result in `argv[0]`).
///
/// An Action is a **move, not a borrow**. It carries exactly the references
/// a direct call would have transferred: one of the callee into `self`, one
/// of each operand into `argv[0..argc]`. See `run` for the full contract.
pub const Action = struct {
    code: ?CodeFn,
    self: Value,
    argv: [MAX_ARGS]Value,
    argc: usize,
};

pub const Struct = struct { tag: Tag, fields: []const Value };
pub const Closure = struct { code: CodeFn, captures: []const Value };
pub const NamedField = struct { name: []const u8, code: CodeFn };
pub const Record = struct { fields: []const NamedField, captures: []const Value };
pub const NamedBranch = struct { name: []const u8, code: CodeFn };
pub const Codata = struct { branches: []const NamedBranch, captures: []const Value };

pub const Payload = union(Kind) {
    int32: i32,
    int64: i64,
    float: f32,
    double: f64,
    char: u21,
    string: []const u8,
    strukt: Struct,
    closure: Closure,
    record: Record,
    codata: Codata,
    unit: void,
};

/// Reference count sentinel for values that live for the whole process
/// (`no_self`, the identity continuation): `dup`/`drop` are no-ops on them.
pub const IMMORTAL: u32 = 0xFFFF_FFFF;

pub const Object = struct { rc: u32, kind: Kind, payload: Payload };
pub const Value = *Object;

var NO_SELF_OBJ: Object = .{ .rc = IMMORTAL, .kind = .unit, .payload = .{ .unit = {} } };

/// Sentinel `self` for calling a top-level definition (which has no
/// captures and ignores it).
pub const no_self: Value = &NO_SELF_OBJ;

// ===== Heaps =====

var g_debug: std.heap.DebugAllocator(.{}) = .init;

/// The Value heap: Objects, their fields/captures backing arrays, and
/// string payload bytes. `DebugAllocator` in safe build modes (leak, UAF
/// and double-free detection); `smp_allocator` in release-fast, where the
/// `g_live_objects` counter still provides the zero-leak invariant.
pub var g_value: std.mem.Allocator = undefined;

var g_scratch_state: std.heap.ArenaAllocator = undefined;

/// Live Objects in the Value heap (allocated minus freed), independent of
/// build mode. Zero at exit == no leaked Values.
pub var g_live_objects: usize = 0;

/// Total Objects ever allocated. Reported on stderr at exit when the
/// MALGO_RC_STATS environment variable is set; purely an optimization
/// instrument, invisible otherwise.
pub var g_total_allocs: usize = 0;

/// Times 'dropReuse' recycled an Object in place instead of freeing it.
/// Reported alongside `g_total_allocs`.
pub var g_reuse_hits: usize = 0;

/// Actions dispatched by `run`. Reported alongside `g_total_allocs`: a
/// deterministic, machine-independent reduction-step count, so a pass that
/// accidentally doubles the work shows up here even when wall-clock noise
/// hides it.
pub var g_dispatches: usize = 0;

/// Current and high-water nesting depth of `forceField`'s nested `run`.
/// The trampoline makes native stack O(this), not O(reduction steps), so
/// the high-water mark is the quantity worth watching.
pub var g_force_depth: usize = 0;
pub var g_force_depth_max: usize = 0;

/// Whether RC tracing is compiled in at all. ReleaseFast excludes it, which is
/// what makes it free rather than nearly-free (#385).
///
/// The `*Named` wrappers are the only RC entry points generated code calls, and
/// every allocation site passes them a stack-materialized
/// `&[_][]const u8{"x","y"}` array of per-slot symbolic names plus a `func`
/// string literal -- two words per slot, on every single allocation. A runtime
/// `if (!g_trace_enabled) return` cannot remove that: `g_trace_enabled` is a
/// mutable global, so the branch is not foldable and the arguments must still be
/// materialized before the call. Making the trace bodies *comptime*-dead makes
/// the wrappers empty, which lets LLVM inline them away and only then does the
/// names array become genuinely dead code.
///
/// Debug and ReleaseSafe keep tracing, which is where RC is actually debugged --
/// `rcInvariant` fires there, and #354's reuse-token investigations run there.
/// Flip this to `true` if a release-only RC bug ever needs `scripts/rctrace.py`.
pub const rc_trace_supported = builtin.mode != .ReleaseFast;

/// Set from the MALGO_RC_TRACE environment variable at startup. When true,
/// every `*Named` wrapper below (see "Named RC tracing" further down) emits
/// one JSON-lines record per RC event to stderr, for offline correlation by
/// `scripts/rctrace.py`. The plain (untraced) dup/drop/mkStruct/... below
/// are never touched by this -- tracing is purely additive and cannot
/// change any RC decision. Has no effect unless `rc_trace_supported`.
pub var g_trace_enabled: bool = false;

/// Called first thing in the generated `main` (and in `zig test` blocks).
pub fn initHeap() void {
    g_value = switch (builtin.mode) {
        .Debug, .ReleaseSafe => g_debug.allocator(),
        .ReleaseFast, .ReleaseSmall => std.heap.smp_allocator,
    };
    g_scratch_state = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // The worklist's backing storage came from the previous scratch arena
    // (if any); growing it under the fresh arena would be undefined. Only
    // matters for `zig test`, where initHeap runs once per test block.
    g_free_worklist = .empty;
    // std.c.getenv, not std.posix (removed in Zig 0.16); libc is always
    // linked (see Toolchain.hs's -lc).
    g_trace_enabled = rc_trace_supported and std.c.getenv("MALGO_RC_TRACE") != null;
}

/// Transient non-Value bytes: print formatting, encode/parse buffers, the
/// free worklist. Never individually freed; bulk-released at process exit;
/// exempt from the leak check.
fn scratch() std.mem.Allocator {
    return g_scratch_state.allocator();
}

fn alloc(kind: Kind, payload: Payload) Value {
    const obj = g_value.create(Object) catch @panic("Malgo: out of memory");
    obj.* = Object{ .rc = 1, .kind = kind, .payload = payload };
    g_live_objects += 1;
    g_total_allocs += 1;
    return obj;
}

// ===== Reference counting =====

/// A heap-corruption invariant shared by every RC bookkeeping site that
/// guards against over-drop / reuse-token misuse: fail at the exact
/// faulty call rather than silently corrupting the heap. In
/// Debug/ReleaseSafe, `std.debug.assert` gives a full stack trace via
/// Zig's safety-checked `unreachable`. In ReleaseFast/ReleaseSmall,
/// `assert` compiles to `unreachable` as a pure optimizer hint with no
/// runtime check -- worse, if a plain `if (!cond) panic(...)` followed
/// it, LLVM can use the assert to "prove" that condition dead and elide
/// the panic entirely, even though `cond` was actually violated at
/// runtime (verified empirically: an assert immediately followed by the
/// same check as a plain `if` silently drops the panic branch under
/// `-O ReleaseFast`). So the two build-mode families get entirely
/// separate code paths, never both an assert and a check on the same
/// condition in the same build.
inline fn rcInvariant(cond: bool, comptime msg: []const u8) void {
    if (builtin.mode == .Debug or builtin.mode == .ReleaseSafe) {
        std.debug.assert(cond);
    } else if (!cond) {
        panic(msg);
    }
}

pub inline fn dup(v: Value) void {
    if (v.rc != IMMORTAL) v.rc += 1;
}

pub fn drop(v: Value) void {
    if (v.rc == IMMORTAL) return;
    rcInvariant(v.rc > 0, "drop: over-drop of an already-dead Object");
    v.rc -= 1;
    if (v.rc == 0) free(v);
}

/// Deferred stack of dead (rc == 0) Objects awaiting destruction, so that
/// dropping a deep structure (a 100k-element cons list) never recurses on
/// the native stack. Backed by scratch: its storage is reused across
/// frees and reclaimed only at exit.
var g_free_worklist: std.ArrayList(Value) = .empty;

fn free(root: Value) void {
    g_free_worklist.append(scratch(), root) catch @panic("Malgo: out of memory");
    drainFreeWorklist();
}

/// Destroys every dead Object currently queued. Shared by `free` (which
/// queues `root` itself first) and `dropReuse` (which queues only a
/// recycled Object's former children, never the Object itself).
fn drainFreeWorklist() void {
    while (g_free_worklist.pop()) |obj| {
        switch (obj.kind) {
            .strukt => {
                decChildren(obj, obj.payload.strukt.fields);
                g_value.free(obj.payload.strukt.fields);
            },
            .closure => {
                decChildren(obj, obj.payload.closure.captures);
                g_value.free(obj.payload.closure.captures);
            },
            .record => {
                decChildren(obj, obj.payload.record.captures);
                g_value.free(obj.payload.record.captures);
                // NamedField holds no Values -- only a name (a string
                // literal in the generated code's .rodata, never freed)
                // and a code pointer.
                g_value.free(obj.payload.record.fields);
            },
            .codata => {
                decChildren(obj, obj.payload.codata.captures);
                g_value.free(obj.payload.codata.captures);
                g_value.free(obj.payload.codata.branches);
            },
            // String payload bytes are unconditionally owned by the Value
            // heap: `mkString` copies and `mkStringOwned` documents the
            // transfer, so there is no static-vs-heap ambiguity here.
            .string => g_value.free(obj.payload.string),
            .int32, .int64, .float, .double, .char, .unit => {},
        }
        g_live_objects -= 1;
        g_value.destroy(obj);
    }
}

/// FBIP-style reuse (Koka's "reuse tokens"): when `v` is uniquely
/// referenced, releases its children exactly as `drop` would but keeps the
/// Object itself alive (as a `.strukt`, since `mkStructReuse` is the only
/// consumer of a token) for the paired `mkStructReuse` to overwrite --
/// recycling one allocation instead of freeing-then-reallocating. The
/// backing array survives too when its length already matches `arity`,
/// otherwise it is freed here and `mkStructReuse` allocates a fresh one.
///
/// Falls back to an ordinary `drop` (returning `null`) for a shared value,
/// an immortal, or a kind with a second owned buffer beyond one children
/// array (`record`'s separate fields array, `codata`'s separate branches
/// array, `string`'s bytes) -- reuse only ever recycles a single array, so
/// these fall back rather than adding another bookkeeping case. Every
/// caller of `mkStructReuse` already handles a `null` token by allocating
/// fresh, so a fallback here is exactly as correct as the original
/// `drop`\/`mkStruct` pair, only less cheap.
pub fn dropReuse(v: Value, arity: usize) ?Value {
    if (v.rc == IMMORTAL) return null;
    if (v.rc != 1) {
        drop(v);
        return null;
    }
    switch (v.kind) {
        .strukt => {
            const fields = v.payload.strukt.fields;
            decChildren(v, fields);
            drainFreeWorklist();
            if (fields.len != arity) {
                g_value.free(fields);
                v.payload.strukt.fields = &.{};
            }
        },
        .closure => {
            const captures = v.payload.closure.captures;
            decChildren(v, captures);
            drainFreeWorklist();
            const kept = if (captures.len == arity) captures else blk: {
                g_value.free(captures);
                break :blk &.{};
            };
            v.payload = .{ .strukt = .{ .tag = .{ .tuple = {} }, .fields = kept } };
        },
        .int32, .int64, .float, .double, .char, .unit => {
            v.payload = .{ .strukt = .{ .tag = .{ .tuple = {} }, .fields = &.{} } };
        },
        .record, .codata, .string => {
            drop(v);
            return null;
        },
    }
    v.kind = .strukt;
    g_reuse_hits += 1;
    return v;
}

/// Overwrites a token produced by `dropReuse` in place when possible, else
/// allocates fresh via `mkStruct`. `tag` is a value (a `[]const u8` name
/// pointing at the generated code's own `.rodata`, or a bare tuple
/// marker), so overwriting it is just a copy with no ownership
/// implications.
pub fn mkStructReuse(tok: ?Value, tag: Tag, fields: []const Value) Value {
    const obj = tok orelse return mkStruct(tag, fields);
    // A malformed token here would mean overwriting memory `dropReuse` never
    // vacated -- corruption, not a recoverable error, and a last line of
    // defense behind RcCheck's static token-linearity verification.
    rcInvariant(obj.rc == 1 and obj.kind == .strukt, "mkStructReuse: token does not own a unique strukt Object");
    if (obj.payload.strukt.fields.len == fields.len) {
        @memcpy(@constCast(obj.payload.strukt.fields), fields);
        obj.payload.strukt.tag = tag;
    } else {
        rcInvariant(obj.payload.strukt.fields.len == 0, "mkStructReuse: arity-mismatched token was not cleared by dropReuse");
        obj.payload.strukt = .{ .tag = tag, .fields = g_value.dupe(Value, fields) catch @panic("Malgo: out of memory") };
    }
    return obj;
}

/// `container` is only used for tracing (see "Named RC tracing" below); it
/// carries no RC obligation of its own here (the caller already dropped its
/// own reference to `container` by the time its children are decremented).
fn decChildren(container: Value, children: []const Value) void {
    for (children, 0..) |c, i| {
        traceDecChild(container, i, c);
        if (c.rc == IMMORTAL) continue;
        rcInvariant(c.rc > 0, "decChildren: over-drop of an already-dead child Object");
        c.rc -= 1;
        if (c.rc == 0) g_free_worklist.append(scratch(), c) catch @panic("Malgo: out of memory");
    }
}

// ===== Interned small integers (#385) =====
//
// Every scalar is otherwise a separate heap Object: `mkInt32` allocates, and so
// every arithmetic primitive allocates one Object per intermediate result (see
// `malgo_add_int32_t`). On the self-hosting workload that dominates the profile
// -- 4.57M allocations to compute `fib 5` through an interpreter written in
// Malgo -- and #385 names unboxed small scalars as its single biggest lever.
//
// Full unboxing (tagged pointers / NaN boxing) would change `Value = *Object`
// everywhere, in the compiler as well as here. Interning is the part of that win
// available without touching the representation: values in this range become
// statically allocated Objects, so they cost no allocation and no RC traffic.
//
// Correctness rests on `IMMORTAL`, which already existed for `NO_SELF_OBJ` and
// `IDENTITY_KONT_OBJ`:
//   * `dup` (:222) and `drop` (:226) both short-circuit on it, so an interned
//     int is never freed and never counted by the leak gate;
//   * `dropReuse` (:299) returns null on it, so an interned int can never be
//     handed to `mkStructReuse` as a recycling token. Payloads are otherwise
//     never mutated in place, so sharing one Object per value is sound.
//
// The range is deliberately modest. It covers loop counters, small list indices,
// character codes and the 0/1 that dominate `fib`-shaped recursion, at a fixed
// cost of (INT32_INTERN_MAX - INT32_INTERN_MIN + 1) Objects in .data.
const INT32_INTERN_MIN: i32 = -128;
const INT32_INTERN_MAX: i32 = 1024;

const int32_intern_table = blk: {
    const n = INT32_INTERN_MAX - INT32_INTERN_MIN + 1;
    // One backwards branch per entry, against a default quota of 1000.
    @setEvalBranchQuota(4 * n);
    var table: [n]Object = undefined;
    for (&table, 0..) |*slot, i| {
        slot.* = .{
            .rc = IMMORTAL,
            .kind = .int32,
            .payload = .{ .int32 = INT32_INTERN_MIN + @as(i32, @intCast(i)) },
        };
    }
    break :blk table;
};

/// Mutable so the interned Objects live in `.data` and yield a stable `Value`
/// (`*Object`) per entry. Never written after initialization -- `IMMORTAL`
/// keeps every RC path off them.
var int32_interned: [int32_intern_table.len]Object = int32_intern_table;

pub fn mkInt32(n: i32) Value {
    if (n >= INT32_INTERN_MIN and n <= INT32_INTERN_MAX) {
        return &int32_interned[@intCast(n - INT32_INTERN_MIN)];
    }
    return alloc(.int32, .{ .int32 = n });
}
pub fn mkInt64(n: i64) Value {
    return alloc(.int64, .{ .int64 = n });
}
pub fn mkFloat(f: f32) Value {
    return alloc(.float, .{ .float = f });
}
pub fn mkDouble(d: f64) Value {
    return alloc(.double, .{ .double = d });
}
pub fn mkChar(codepoint: u21) Value {
    return alloc(.char, .{ .char = codepoint });
}
/// Copies `bytes` into the Value heap, so a string's payload is always
/// owned by its Object (a caller may pass a .rodata literal, a stack
/// buffer, or scratch bytes -- `free` never has to guess).
pub fn mkString(bytes: []const u8) Value {
    const owned = g_value.dupe(u8, bytes) catch @panic("Malgo: out of memory");
    return alloc(.string, .{ .string = owned });
}

/// Takes ownership of a buffer the caller already allocated on `g_value`
/// (exact-sized: it is later freed with this very slice).
pub fn mkStringOwned(bytes: []const u8) Value {
    return alloc(.string, .{ .string = bytes });
}

/// Copies `fields`: the caller typically passes a stack-temporary slice
/// literal (`&[_]Value{...}`) that does not outlive this call.
pub fn mkStruct(tag: Tag, fields: []const Value) Value {
    const owned = g_value.dupe(Value, fields) catch @panic("Malgo: out of memory");
    return alloc(.strukt, .{ .strukt = .{ .tag = tag, .fields = owned } });
}

/// Copies `captures`, for the same reason as 'mkStruct'.
pub fn mkClosure(code: CodeFn, captures: []const Value) Value {
    const owned = g_value.dupe(Value, captures) catch @panic("Malgo: out of memory");
    return alloc(.closure, .{ .closure = .{ .code = code, .captures = owned } });
}

pub fn mkRecord(fields: []const NamedField, captures: []const Value) Value {
    const ownedFields = g_value.dupe(NamedField, fields) catch @panic("Malgo: out of memory");
    const ownedCaptures = g_value.dupe(Value, captures) catch @panic("Malgo: out of memory");
    return alloc(.record, .{ .record = .{ .fields = ownedFields, .captures = ownedCaptures } });
}

pub fn mkCodata(branches: []const NamedBranch, captures: []const Value) Value {
    const ownedBranches = g_value.dupe(NamedBranch, branches) catch @panic("Malgo: out of memory");
    const ownedCaptures = g_value.dupe(Value, captures) catch @panic("Malgo: out of memory");
    return alloc(.codata, .{ .codata = .{ .branches = ownedBranches, .captures = ownedCaptures } });
}

// ===== Named RC tracing (scripts/rctrace.py) =====
//
// Every wrapper here performs the exact same RC operation as its untraced
// counterpart above (by calling straight through to it), then -- only when
// `g_trace_enabled` -- emits one JSON-lines event to stderr carrying the
// compile-time-known symbolic name and enclosing function that
// 'Malgo.Backend.Zig.Emit' threads through from the Zig-IR 'Name'/'Func'
// each operation came from. `scripts/rctrace.py` replays this log to
// answer "who still holds a reference to this object right now", instead
// of the throwaway raw-pointer tracing this replaces (see the M13 plan
// entry in the project's Zig-backend notes).
//
// These wrappers are the only thing 'Emit.hs' ever calls; the plain
// dup/drop/dropReuse/mkStruct/mkClosure/mkStructReuse/mkRecord above are
// otherwise only exercised directly by this file's own `test` blocks, so
// tracing cannot perturb the RC decisions those tests already exercise.

/// `name`\/`func` come from compiler-generated (unbounded-length) Zig-IR
/// identifiers, so formatting into a fixed stack buffer can in principle
/// overflow. Writes a valid one-line JSON marker in that case instead of
/// silently dropping the event -- `scripts/rctrace.py` can then tell a
/// missing event from one that was never emitted.
fn emitTraced(result: std.fmt.BufPrintError![]const u8) void {
    const line = result catch {
        writeStderr("{\"ev\":\"trace_overflow\"}\n");
        return;
    };
    writeStderr(line);
}

fn traceLine(event: []const u8, v: Value, name: []const u8, func: []const u8) void {
    if (!rc_trace_supported) return;
    if (!g_trace_enabled) return;
    const rc: i64 = if (v.rc == IMMORTAL) -1 else @intCast(v.rc);
    var buf: [512]u8 = undefined;
    emitTraced(std.fmt.bufPrint(
        &buf,
        "{{\"ev\":\"{s}\",\"ptr\":\"0x{x}\",\"rc\":{d},\"name\":\"{s}\",\"func\":\"{s}\"}}\n",
        .{ event, @intFromPtr(v), rc, name, func },
    ));
}

/// Like 'traceLine', but identifies the object by an address captured
/// earlier instead of dereferencing `v` -- for events reported after a call
/// (like 'dropReuseNamed') that may already have freed the object.
fn traceAddr(event: []const u8, addr: usize, name: []const u8, func: []const u8) void {
    if (!rc_trace_supported) return;
    if (!g_trace_enabled) return;
    var buf: [512]u8 = undefined;
    emitTraced(std.fmt.bufPrint(
        &buf,
        "{{\"ev\":\"{s}\",\"ptr\":\"0x{x}\",\"name\":\"{s}\",\"func\":\"{s}\"}}\n",
        .{ event, addr, name, func },
    ));
}

/// Traces a construction event
/// (`mkStruct`/`mkClosure`/`mkStructReuse`/`mkRecord`):
/// records the fresh container's address plus, per slot, the symbolic name
/// and child address that went into it -- so a later 'traceDecChild' event
/// against the same (container, slot) pair can be resolved back to a name.
fn traceSlots(event: []const u8, v: Value, fields: []const Value, names: []const []const u8, func: []const u8) void {
    if (!rc_trace_supported) return;
    if (!g_trace_enabled) return;
    var head: [512]u8 = undefined;
    const headLine = std.fmt.bufPrint(
        &head,
        "{{\"ev\":\"{s}\",\"ptr\":\"0x{x}\",\"func\":\"{s}\",\"slots\":[",
        .{ event, @intFromPtr(v), func },
    ) catch {
        writeStderr("{\"ev\":\"trace_overflow\"}\n");
        return;
    };
    writeStderr(headLine);
    for (fields, 0..) |child, i| {
        if (i != 0) writeStderr(",");
        const name = if (i < names.len) names[i] else "?";
        var slot: [512]u8 = undefined;
        // A fallback placeholder, not `catch continue`: skipping the slot
        // outright would also break the enclosing JSON array (a comma was
        // already written above for every non-first slot).
        const slotLine = std.fmt.bufPrint(
            &slot,
            "{{\"i\":{d},\"name\":\"{s}\",\"child\":\"0x{x}\"}}",
            .{ i, name, @intFromPtr(child) },
        ) catch "{\"i\":-1,\"name\":\"?\",\"child\":\"0x0\"}";
        writeStderr(slotLine);
    }
    writeStderr("]}\n");
}

/// Traces a `decChildren` release: `container`'s slot `slot` (a former
/// field/capture) is about to have its reference decremented. `rc_before`
/// is `child`'s refcount at this moment.
fn traceDecChild(container: Value, slot: usize, child: Value) void {
    if (!rc_trace_supported) return;
    if (!g_trace_enabled) return;
    const rc: i64 = if (child.rc == IMMORTAL) -1 else @intCast(child.rc);
    var buf: [160]u8 = undefined;
    const line = std.fmt.bufPrint(
        &buf,
        "{{\"ev\":\"decChild\",\"container\":\"0x{x}\",\"slot\":{d},\"child\":\"0x{x}\",\"rc_before\":{d}}}\n",
        .{ @intFromPtr(container), slot, @intFromPtr(child), rc },
    ) catch return;
    writeStderr(line);
}

pub fn dupNamed(v: Value, name: []const u8, func: []const u8) void {
    dup(v);
    traceLine("dup", v, name, func);
}

pub fn dropNamed(v: Value, name: []const u8, func: []const u8) void {
    // Trace before dropping: `v` may be destroyed (and its memory reused or
    // poisoned) by `drop` if this was its last reference.
    traceLine("drop", v, name, func);
    drop(v);
}

pub fn dropReuseNamed(v: Value, arity: usize, name: []const u8, func: []const u8) ?Value {
    const addr = @intFromPtr(v);
    traceLine("dropReuse_attempt", v, name, func);
    const result = dropReuse(v, arity);
    // Only the pre-captured address is used past this point: on a miss
    // that fell through to a plain `drop`, `v` itself may already be freed.
    traceAddr(if (result != null) "dropReuse_hit" else "dropReuse_miss", addr, name, func);
    return result;
}

pub fn mkStructNamed(tag: Tag, fields: []const Value, names: []const []const u8, func: []const u8) Value {
    const v = mkStruct(tag, fields);
    traceSlots("mkStruct", v, fields, names, func);
    return v;
}

pub fn mkClosureNamed(code: CodeFn, captures: []const Value, names: []const []const u8, func: []const u8) Value {
    const v = mkClosure(code, captures);
    traceSlots("mkClosure", v, captures, names, func);
    return v;
}

pub fn mkStructReuseNamed(tok: ?Value, tag: Tag, fields: []const Value, names: []const []const u8, func: []const u8) Value {
    const v = mkStructReuse(tok, tag, fields);
    traceSlots("mkStructReuse", v, fields, names, func);
    return v;
}

pub fn mkRecordNamed(fields: []const NamedField, captures: []const Value, names: []const []const u8, func: []const u8) Value {
    const v = mkRecord(fields, captures);
    traceSlots("mkRecord", v, captures, names, func);
    return v;
}

// ===== Dispatch =====

/// Package a call as an `Action` for `run` to dispatch. Takes ownership of
/// `self` and every element of `args` on the caller's behalf -- see `run`.
fn mkAction(code: CodeFn, self: Value, args: []const Value) Action {
    rcInvariant(args.len <= MAX_ARGS, "call arity exceeds MAX_ARGS");
    var action = Action{ .code = code, .self = self, .argv = undefined, .argc = args.len };
    for (args, 0..) |a, i| action.argv[i] = a;
    return action;
}

/// The finished value: what a generated function returns for Join IR's
/// `Finish` (`Ir.TReturn`).
pub fn done(value: Value) Action {
    var action = Action{ .code = null, .self = no_self, .argv = undefined, .argc = 1 };
    action.argv[0] = value;
    return action;
}

/// A direct call to a lifted top-level function (`Ir.TStaticCall`), whose
/// `self` is the immortal `no_self` sentinel.
pub fn staticCall(code: CodeFn, args: []const Value) Action {
    return mkAction(code, no_self, args);
}

/// The trampoline. Dispatches actions until one is `done`, and returns the
/// single owned reference that one carries.
///
/// **`run` is strictly RC-neutral.** Between receiving an Action and
/// dispatching it, it performs no `dup`, no `drop`, and no read of any
/// `Value`'s payload; it never discards an Action without dispatching it
/// (that would leak every reference the Action carries). Ownership passes
/// straight through: the references a generated function moved into the
/// Action are the ones the callee receives as `self`/`args`. This is what
/// keeps `Malgo.Backend.Zig.Perceus` and `RcCheck` sound unchanged -- both
/// model only a single frame, and "these references leave this frame here"
/// is still true when they leave into an Action.
///
/// Nesting `run` is legal and costs one native frame per nesting level;
/// `forceField` is the only nesting site today, and any future synchronous
/// helper counts against the same budget.
pub fn run(code: CodeFn, self: Value, args: []const Value) Value {
    var cur = mkAction(code, self, args);
    while (cur.code) |c| {
        // Deliberately not `cur = c(...)`: Zig's result-location semantics
        // would let the callee build its returned Action directly into
        // `cur` while `args` still points into `cur.argv`. A separate slot
        // makes that aliasing impossible.
        const next = c(cur.self, cur.argv[0..cur.argc]);
        cur = next;
        g_dispatches += 1;
    }
    return cur.argv[0];
}

pub fn applyCovalue(covalue: Value, value: Value) Action {
    return callClosure(covalue, &[_]Value{value});
}

pub fn callClosure(closure: Value, args: []const Value) Action {
    if (closure.kind != .closure) panic("callClosure: value is not a function");
    // Resolved eagerly, while the caller's reference to `closure` is still
    // being moved in: the code pointer must be read before ownership of the
    // closure object passes into the Action.
    return mkAction(closure.payload.closure.code, closure, args);
}

/// The captures slice of a closure/record/codata value, read by the
/// callee out of its own `self` argument.
pub fn capturesOf(v: Value) []const Value {
    return switch (v.kind) {
        .closure => v.payload.closure.captures,
        .record => v.payload.record.captures,
        .codata => v.payload.codata.captures,
        else => panic("capturesOf: value has no captures"),
    };
}

pub fn applyDestructor(codata: Value, name: []const u8, args: []const Value) Action {
    if (codata.kind != .codata) panic("applyDestructor: value is not codata");
    for (codata.payload.codata.branches) |branch| {
        if (stringEq(branch.name, name)) return mkAction(branch.code, codata, args);
    }
    panic("applyDestructor: no matching destructor");
}

pub fn projectField(record: Value, name: []const u8, k: Value) Action {
    if (record.kind != .record) panic("projectField: value is not a record");
    for (record.payload.record.fields) |field| {
        if (stringEq(field.name, name)) return mkAction(field.code, record, &[_]Value{k});
    }
    panic("projectField: no such field");
}

fn identityCode(self: Value, args: []const Value) Action {
    // The uniform closure protocol is "dup used captures, then drop self";
    // with no captures and an immortal self this is a no-op, kept for
    // uniformity with generated closure bodies.
    drop(self);
    return done(args[0]);
}

var IDENTITY_KONT_OBJ: Object = .{
    .rc = IMMORTAL,
    .kind = .closure,
    .payload = .{ .closure = .{ .code = &identityCode, .captures = &[_]Value{} } },
};

/// A covalue that, once invoked, finishes with its argument unchanged.
/// Running a record field's code with this as its continuation makes the
/// field's (call-by-name) computation synchronous from the caller's point of
/// view -- the nested `run` in `forceField` dispatches until this kont turns
/// the value into a `done` Action. Used by `Expand` pattern matching to force
/// a field into a plain value it can immediately test and bind against.
/// Immortal: forcing a field allocates nothing and creates no RC obligation.
pub fn identityKont() Value {
    return &IDENTITY_KONT_OBJ;
}

/// Force a record field by name without checking `record`'s kind first
/// (callers that already know it is a record, e.g. `Expand` pattern
/// matching after its own `.kind == .record` guard, can skip that check).
///
/// `Ir.Force` is an expression in the middle of a block, so this has to hand
/// back a plain `?Value` -- it runs a *nested* trampoline to completion
/// rather than returning an Action. That is the one place native stack
/// still grows: depth is bounded by the maximum dynamic nesting of `Force`
/// (three frames per level), not by the total number of reduction steps.
/// `g_force_depth_max` reports the high-water mark under MALGO_RC_STATS.
pub fn forceField(record: Value, name: []const u8) ?Value {
    if (record.kind != .record) return null;
    for (record.payload.record.fields) |field| {
        if (stringEq(field.name, name)) {
            g_force_depth += 1;
            if (g_force_depth > g_force_depth_max) g_force_depth_max = g_force_depth;
            defer g_force_depth -= 1;
            return run(field.code, record, &[_]Value{identityKont()});
        }
    }
    return null;
}

pub fn tagEq(a: Tag, b: Tag) bool {
    return switch (a) {
        .tuple => b == .tuple,
        .named => |an| switch (b) {
            .tuple => false,
            .named => |bn| stringEq(an, bn),
        },
    };
}

pub fn stringEq(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

pub fn isZero(v: Value) bool {
    return switch (v.kind) {
        .int32 => v.payload.int32 == 0,
        .int64 => v.payload.int64 == 0,
        .float => v.payload.float == 0,
        .double => v.payload.double == 0,
        else => false,
    };
}

pub fn panic(msg: []const u8) noreturn {
    writeStderr("Malgo: ");
    writeStderr(msg);
    writeStderr("\n");
    std.process.exit(1);
}

pub fn panicUnimplemented(feature: []const u8) noreturn {
    writeStderr("Malgo: not yet implemented in the Zig backend: ");
    writeStderr(feature);
    writeStderr("\n");
    std.process.exit(1);
}

// ===== Unbuffered direct I/O (see module doc) =====

fn writeAllFd(fd: c_int, bytes: []const u8) void {
    var off: usize = 0;
    while (off < bytes.len) {
        const n = std.c.write(fd, bytes.ptr + off, bytes.len - off);
        if (n < 0) {
            if (std.c.errno(n) == .INTR) continue;
            return;
        }
        if (n == 0) return;
        off += @intCast(n);
    }
}

/// `writeAllFd` returns silently on a short or failed write, which is the
/// right call for stdout/stderr but not for a file the program believes it
/// wrote. Fatal instead.
fn writeAllFdChecked(fd: c_int, bytes: []const u8, comptime what: []const u8) void {
    var off: usize = 0;
    while (off < bytes.len) {
        const n = std.c.write(fd, bytes.ptr + off, bytes.len - off);
        if (n < 0) {
            if (std.c.errno(n) == .INTR) continue;
            panic(what);
        }
        if (n == 0) panic(what);
        off += @intCast(n);
    }
}

fn writeStdout(bytes: []const u8) void {
    writeAllFd(1, bytes);
}

fn writeStderr(bytes: []const u8) void {
    writeAllFd(2, bytes);
}

/// No-op: writes are unbuffered.
pub fn flushStdout() void {}

/// The generated `main`'s last statement. A leaked Value means the Perceus
/// pass under-dropped somewhere: report and exit 83 (the zig-golden
/// harness's leak bucket). Early-exit paths (`panic`, `malgo_exit_*`)
/// deliberately bypass this -- the process is going down anyway, and every
/// golden case exits 0 through here, so the corpus is fully gated.
pub fn exitWithLeakCheck() void {
    // std.c.getenv, not std.posix (removed in Zig 0.16); libc is always
    // linked (see Toolchain.hs's -lc).
    if (std.c.getenv("MALGO_RC_STATS") != null) {
        var buf: [160]u8 = undefined;
        const line = std.fmt.bufPrint(
            &buf,
            "MALGO-STATS: total_allocs={d} reuse_hits={d} dispatches={d} force_depth_max={d}\n",
            .{ g_total_allocs, g_reuse_hits, g_dispatches, g_force_depth_max },
        ) catch "MALGO-STATS: ?\n";
        writeStderr(line);
    }
    var leaked = g_live_objects != 0;
    if (builtin.mode == .Debug or builtin.mode == .ReleaseSafe) {
        // Also ask DebugAllocator, which catches non-Object leaks in the
        // Value heap (fields/captures arrays, string bytes) and prints
        // per-allocation stack traces to stderr.
        if (g_debug.deinit() == .leak) leaked = true;
    }
    if (leaked) {
        var buf: [32]u8 = undefined;
        const count = std.fmt.bufPrint(&buf, "{d}", .{g_live_objects}) catch "?";
        writeStderr("MALGO-LEAK: ");
        writeStderr(count);
        writeStderr(" objects\n");
        std.process.exit(83);
    }
}

/// Reads one byte from stdin, or null at EOF.
fn readStdinByte() ?u8 {
    var buf: [1]u8 = undefined;
    const n = std.posix.read(0, &buf) catch panic("stdin read error");
    if (n == 0) return null;
    return buf[0];
}

// ===== Value accessors =====

fn asI32(v: Value) i32 {
    return v.payload.int32;
}
fn asI64(v: Value) i64 {
    return v.payload.int64;
}
fn asF32(v: Value) f32 {
    return v.payload.float;
}
fn asF64(v: Value) f64 {
    return v.payload.double;
}
fn asChar(v: Value) u21 {
    return v.payload.char;
}
fn asStr(v: Value) []const u8 {
    return v.payload.string;
}
fn boolValue(b: bool) Value {
    return mkInt32(if (b) 1 else 0);
}
fn unitValue() Value {
    return alloc(.strukt, .{ .strukt = .{ .tag = .{ .tuple = {} }, .fields = &[_]Value{} } });
}

// ===== Unicode-scalar string helpers (Malgo strings index by codepoint,
// matching Haskell Text semantics, not by byte) =====

fn utf8ByteOffsetOfScalar(s: []const u8, scalarIndex: usize) usize {
    var it: std.unicode.Utf8Iterator = .{ .bytes = s, .i = 0 };
    var i: usize = 0;
    while (i < scalarIndex) : (i += 1) {
        _ = it.nextCodepointSlice() orelse panic("string index out of range");
    }
    return it.i;
}

fn utf8EncodeAlloc(a: std.mem.Allocator, codepoint: u21) []const u8 {
    var buf: [4]u8 = undefined;
    const len = std.unicode.utf8Encode(codepoint, &buf) catch panic("invalid Unicode codepoint");
    return a.dupe(u8, buf[0..len]) catch @panic("Malgo: out of memory");
}

// ===== Primitives (rt.<foreign import name>, called with a single args slice) =====
// Authoritative semantics: src/Malgo/Sequent/Eval.hs's `fetchPrimitive`.

pub fn malgo_unsafe_cast(args: []const Value) Value {
    // Primitives borrow their arguments but return an owned result; this
    // is the one primitive that returns an argument itself, so it must
    // mint the reference it hands back.
    dup(args[0]);
    return args[0];
}

/// Inserted by Malgo.Sequent.ReuseSpecialize: keeps a matched-and-about-to-
/// be-discarded scrutinee (args[0]) mentioned right before the
/// reconstruction meant to reuse it, so Perceus's static last-use analysis
/// places the scrutinee's real Drop here instead of immediately after
/// pattern-match projection. Must not touch args[0]'s refcount itself --
/// Perceus already accounts for and inserts that Drop statically -- so this
/// returns the immortal `no_self` sentinel, whose (always-unused) result
/// needs no Drop either.
pub fn reuseHint(args: []const Value) Value {
    _ = args;
    return no_self;
}

pub fn malgo_add_int32_t(args: []const Value) Value {
    return mkInt32(asI32(args[0]) +% asI32(args[1]));
}
pub fn malgo_sub_int32_t(args: []const Value) Value {
    return mkInt32(asI32(args[0]) -% asI32(args[1]));
}
pub fn malgo_mul_int32_t(args: []const Value) Value {
    return mkInt32(asI32(args[0]) *% asI32(args[1]));
}
pub fn malgo_div_int32_t(args: []const Value) Value {
    const divisor = asI32(args[1]);
    if (divisor == 0) panic("divide by zero");
    return mkInt32(@divFloor(asI32(args[0]), divisor));
}
pub fn malgo_mod_int32_t(args: []const Value) Value {
    const divisor = asI32(args[1]);
    if (divisor == 0) panic("divide by zero");
    return mkInt32(@mod(asI32(args[0]), divisor));
}
pub fn malgo_neg_int32_t(args: []const Value) Value {
    return mkInt32(-%asI32(args[0]));
}

pub fn malgo_add_int64_t(args: []const Value) Value {
    return mkInt64(asI64(args[0]) +% asI64(args[1]));
}
pub fn malgo_sub_int64_t(args: []const Value) Value {
    return mkInt64(asI64(args[0]) -% asI64(args[1]));
}
pub fn malgo_mul_int64_t(args: []const Value) Value {
    return mkInt64(asI64(args[0]) *% asI64(args[1]));
}
pub fn malgo_div_int64_t(args: []const Value) Value {
    const divisor = asI64(args[1]);
    if (divisor == 0) panic("divide by zero");
    return mkInt64(@divFloor(asI64(args[0]), divisor));
}
pub fn malgo_mod_int64_t(args: []const Value) Value {
    const divisor = asI64(args[1]);
    if (divisor == 0) panic("divide by zero");
    return mkInt64(@mod(asI64(args[0]), divisor));
}
pub fn malgo_neg_int64_t(args: []const Value) Value {
    return mkInt64(-%asI64(args[0]));
}

pub fn malgo_add_float(args: []const Value) Value {
    return mkFloat(asF32(args[0]) + asF32(args[1]));
}
pub fn malgo_sub_float(args: []const Value) Value {
    return mkFloat(asF32(args[0]) - asF32(args[1]));
}
pub fn malgo_mul_float(args: []const Value) Value {
    return mkFloat(asF32(args[0]) * asF32(args[1]));
}
pub fn malgo_div_float(args: []const Value) Value {
    return mkFloat(asF32(args[0]) / asF32(args[1]));
}
pub fn malgo_neg_float(args: []const Value) Value {
    return mkFloat(-asF32(args[0]));
}

pub fn malgo_add_double(args: []const Value) Value {
    return mkDouble(asF64(args[0]) + asF64(args[1]));
}
pub fn malgo_sub_double(args: []const Value) Value {
    return mkDouble(asF64(args[0]) - asF64(args[1]));
}
pub fn malgo_mul_double(args: []const Value) Value {
    return mkDouble(asF64(args[0]) * asF64(args[1]));
}
pub fn malgo_div_double(args: []const Value) Value {
    return mkDouble(asF64(args[0]) / asF64(args[1]));
}
pub fn malgo_neg_double(args: []const Value) Value {
    return mkDouble(-asF64(args[0]));
}

pub fn sqrtf(args: []const Value) Value {
    return mkFloat(@sqrt(asF32(args[0])));
}
pub fn sqrt(args: []const Value) Value {
    return mkDouble(@sqrt(asF64(args[0])));
}

pub fn malgo_eq_int32_t(args: []const Value) Value {
    return boolValue(asI32(args[0]) == asI32(args[1]));
}
pub fn malgo_ne_int32_t(args: []const Value) Value {
    return boolValue(asI32(args[0]) != asI32(args[1]));
}
pub fn malgo_lt_int32_t(args: []const Value) Value {
    return boolValue(asI32(args[0]) < asI32(args[1]));
}
pub fn malgo_gt_int32_t(args: []const Value) Value {
    return boolValue(asI32(args[0]) > asI32(args[1]));
}
pub fn malgo_le_int32_t(args: []const Value) Value {
    return boolValue(asI32(args[0]) <= asI32(args[1]));
}
pub fn malgo_ge_int32_t(args: []const Value) Value {
    return boolValue(asI32(args[0]) >= asI32(args[1]));
}

pub fn malgo_eq_int64_t(args: []const Value) Value {
    return boolValue(asI64(args[0]) == asI64(args[1]));
}
pub fn malgo_ne_int64_t(args: []const Value) Value {
    return boolValue(asI64(args[0]) != asI64(args[1]));
}
pub fn malgo_lt_int64_t(args: []const Value) Value {
    return boolValue(asI64(args[0]) < asI64(args[1]));
}
pub fn malgo_gt_int64_t(args: []const Value) Value {
    return boolValue(asI64(args[0]) > asI64(args[1]));
}
pub fn malgo_le_int64_t(args: []const Value) Value {
    return boolValue(asI64(args[0]) <= asI64(args[1]));
}
pub fn malgo_ge_int64_t(args: []const Value) Value {
    return boolValue(asI64(args[0]) >= asI64(args[1]));
}

pub fn malgo_eq_float(args: []const Value) Value {
    return boolValue(asF32(args[0]) == asF32(args[1]));
}
pub fn malgo_ne_float(args: []const Value) Value {
    return boolValue(asF32(args[0]) != asF32(args[1]));
}
pub fn malgo_lt_float(args: []const Value) Value {
    return boolValue(asF32(args[0]) < asF32(args[1]));
}
pub fn malgo_gt_float(args: []const Value) Value {
    return boolValue(asF32(args[0]) > asF32(args[1]));
}
pub fn malgo_le_float(args: []const Value) Value {
    return boolValue(asF32(args[0]) <= asF32(args[1]));
}
pub fn malgo_ge_float(args: []const Value) Value {
    return boolValue(asF32(args[0]) >= asF32(args[1]));
}

pub fn malgo_eq_double(args: []const Value) Value {
    return boolValue(asF64(args[0]) == asF64(args[1]));
}
pub fn malgo_ne_double(args: []const Value) Value {
    return boolValue(asF64(args[0]) != asF64(args[1]));
}
pub fn malgo_lt_double(args: []const Value) Value {
    return boolValue(asF64(args[0]) < asF64(args[1]));
}
pub fn malgo_gt_double(args: []const Value) Value {
    return boolValue(asF64(args[0]) > asF64(args[1]));
}
pub fn malgo_le_double(args: []const Value) Value {
    return boolValue(asF64(args[0]) <= asF64(args[1]));
}
pub fn malgo_ge_double(args: []const Value) Value {
    return boolValue(asF64(args[0]) >= asF64(args[1]));
}

pub fn malgo_eq_char(args: []const Value) Value {
    return boolValue(asChar(args[0]) == asChar(args[1]));
}
pub fn malgo_ne_char(args: []const Value) Value {
    return boolValue(asChar(args[0]) != asChar(args[1]));
}
pub fn malgo_lt_char(args: []const Value) Value {
    return boolValue(asChar(args[0]) < asChar(args[1]));
}
pub fn malgo_gt_char(args: []const Value) Value {
    return boolValue(asChar(args[0]) > asChar(args[1]));
}
pub fn malgo_le_char(args: []const Value) Value {
    return boolValue(asChar(args[0]) <= asChar(args[1]));
}
pub fn malgo_ge_char(args: []const Value) Value {
    return boolValue(asChar(args[0]) >= asChar(args[1]));
}

pub fn malgo_eq_string(args: []const Value) Value {
    return boolValue(std.mem.eql(u8, asStr(args[0]), asStr(args[1])));
}
pub fn malgo_ne_string(args: []const Value) Value {
    return boolValue(!std.mem.eql(u8, asStr(args[0]), asStr(args[1])));
}
pub fn malgo_lt_string(args: []const Value) Value {
    return boolValue(std.mem.order(u8, asStr(args[0]), asStr(args[1])) == .lt);
}
pub fn malgo_gt_string(args: []const Value) Value {
    return boolValue(std.mem.order(u8, asStr(args[0]), asStr(args[1])) == .gt);
}
pub fn malgo_le_string(args: []const Value) Value {
    return boolValue(std.mem.order(u8, asStr(args[0]), asStr(args[1])) != .gt);
}
pub fn malgo_ge_string(args: []const Value) Value {
    return boolValue(std.mem.order(u8, asStr(args[0]), asStr(args[1])) != .lt);
}

fn isAscii(c: u21) bool {
    return c < 128;
}

/// Haskell's `Data.Char.isUpper`/`isLower`/`isAlpha` are full-Unicode, but
/// Zig's stdlib only classifies ASCII. As a bounded, honest partial match to
/// the oracle we also cover the Latin-1 Supplement block (the next most
/// common range after ASCII); Greek/Cyrillic/CJK/etc. remain unimplemented.
fn isLatin1Upper(c: u21) bool {
    return (c >= 0xC0 and c <= 0xD6) or (c >= 0xD8 and c <= 0xDE);
}

fn isLatin1Lower(c: u21) bool {
    return (c >= 0xDF and c <= 0xF6) or (c >= 0xF8 and c <= 0xFF);
}

pub fn malgo_char_ord(args: []const Value) Value {
    return mkInt32(@intCast(asChar(args[0])));
}
pub fn malgo_int32_t_to_char(args: []const Value) Value {
    const n = asI32(args[0]);
    if (n < 0 or n > std.math.maxInt(u21)) panic("Prelude.chr: bad argument");
    return mkChar(@intCast(n));
}
pub fn malgo_char_to_string(args: []const Value) Value {
    return mkStringOwned(utf8EncodeAlloc(g_value, asChar(args[0])));
}
pub fn malgo_is_digit(args: []const Value) Value {
    // Data.Char.isDigit is itself ASCII-only ('0'..'9'), so no Latin-1 case applies here.
    const c = asChar(args[0]);
    return boolValue(isAscii(c) and std.ascii.isDigit(@intCast(c)));
}
pub fn malgo_is_lower(args: []const Value) Value {
    const c = asChar(args[0]);
    return boolValue((isAscii(c) and std.ascii.isLower(@intCast(c))) or isLatin1Lower(c));
}
pub fn malgo_is_upper(args: []const Value) Value {
    const c = asChar(args[0]);
    return boolValue((isAscii(c) and std.ascii.isUpper(@intCast(c))) or isLatin1Upper(c));
}
pub fn malgo_is_alphanum(args: []const Value) Value {
    const c = asChar(args[0]);
    return boolValue((isAscii(c) and std.ascii.isAlphanumeric(@intCast(c))) or isLatin1Lower(c) or isLatin1Upper(c));
}

pub fn malgo_string_length(args: []const Value) Value {
    const n = std.unicode.utf8CountCodepoints(asStr(args[0])) catch panic("malformed UTF-8 string");
    return mkInt64(@intCast(n));
}

pub fn malgo_string_at(args: []const Value) Value {
    const i = asI64(args[0]);
    const s = asStr(args[1]);
    if (i < 0) panic("malgo_string_at: negative index");
    const byteOff = utf8ByteOffsetOfScalar(s, @intCast(i));
    var it: std.unicode.Utf8Iterator = .{ .bytes = s, .i = byteOff };
    const slice = it.nextCodepointSlice() orelse panic("malgo_string_at: index out of range");
    const cp = std.unicode.utf8Decode(slice) catch panic("malformed UTF-8 string");
    return mkChar(cp);
}

pub fn malgo_string_cons(args: []const Value) Value {
    const c = asChar(args[0]);
    const s = asStr(args[1]);
    var buf: [4]u8 = undefined;
    const len = std.unicode.utf8Encode(c, &buf) catch panic("invalid Unicode codepoint");
    const owned = g_value.alloc(u8, len + s.len) catch @panic("Malgo: out of memory");
    @memcpy(owned[0..len], buf[0..len]);
    @memcpy(owned[len..], s);
    return mkStringOwned(owned);
}

pub fn malgo_string_append(args: []const Value) Value {
    const a = asStr(args[0]);
    const b = asStr(args[1]);
    const owned = g_value.alloc(u8, a.len + b.len) catch @panic("Malgo: out of memory");
    @memcpy(owned[0..a.len], a);
    @memcpy(owned[a.len..], b);
    return mkStringOwned(owned);
}

/// Mirrors Eval.hs's `malgo_substring`, which never fails: `T.take (end -
/// start) (T.drop start s)`. `T.drop` clamps a negative or overlong `start`
/// to `[0, len]`; `T.take` then clamps its own (unclamped, sign-preserved)
/// count against the remaining length. Indices past the string's bounds or
/// a negative `start` are ordinary, non-panicking inputs -- only malformed
/// UTF-8 is a genuine error.
pub fn malgo_substring(args: []const Value) Value {
    const s = asStr(args[0]);
    const rawStart = asI64(args[1]);
    const rawEnd = asI64(args[2]);
    const len: i64 = @intCast(std.unicode.utf8CountCodepoints(s) catch panic("malformed UTF-8 string"));
    const clampedStart: i64 = clampI64(rawStart, 0, len);
    const takeCount = rawEnd -% rawStart;
    if (takeCount <= 0) return mkString("");
    const clampedEnd: i64 = clampI64(clampedStart + takeCount, clampedStart, len);
    const startByte = utf8ByteOffsetOfScalar(s, @intCast(clampedStart));
    const endByte = utf8ByteOffsetOfScalar(s, @intCast(clampedEnd));
    const owned = g_value.dupe(u8, s[startByte..endByte]) catch @panic("Malgo: out of memory");
    return mkStringOwned(owned);
}

fn clampI64(x: i64, lo: i64, hi: i64) i64 {
    if (x < lo) return lo;
    if (x > hi) return hi;
    return x;
}

pub fn malgo_string_reverse(args: []const Value) Value {
    const s = asStr(args[0]);
    var codepoints: std.ArrayList(u21) = .empty;
    var it: std.unicode.Utf8Iterator = .{ .bytes = s, .i = 0 };
    while (it.nextCodepoint()) |cp| codepoints.append(scratch(), cp) catch @panic("Malgo: out of memory");
    var out: std.ArrayList(u8) = .empty;
    var i: usize = codepoints.items.len;
    while (i > 0) {
        i -= 1;
        var buf: [4]u8 = undefined;
        const len = std.unicode.utf8Encode(codepoints.items[i], &buf) catch panic("invalid Unicode codepoint");
        out.appendSlice(scratch(), buf[0..len]) catch @panic("Malgo: out of memory");
    }
    return mkString(out.items);
}

pub fn malgo_int32_t_to_string(args: []const Value) Value {
    const s = std.fmt.allocPrint(scratch(), "{d}", .{asI32(args[0])}) catch @panic("Malgo: out of memory");
    return mkString(s);
}
pub fn malgo_int64_t_to_string(args: []const Value) Value {
    const s = std.fmt.allocPrint(scratch(), "{d}", .{asI64(args[0])}) catch @panic("Malgo: out of memory");
    return mkString(s);
}
/// Mirrors Haskell's `show :: RealFloat a => a -> String` (fixed-point
/// notation for `x == 0 || 0.1 <= |x| < 10_000_000`, scientific notation
/// otherwise, always at least one digit on both sides of the decimal
/// point). `{d}`/`{e}` already produce the shortest round-tripping digit
/// string, matching `floatToDigits`; only the notation choice and the
/// "always show a fractional part" rule need reproducing by hand.
fn formatHaskellFloat(comptime T: type, x: T) []const u8 {
    const a = scratch();
    if (std.math.isNan(x)) return "NaN";
    if (std.math.isInf(x)) return if (x < 0) "-Infinity" else "Infinity";
    const absX = @abs(x);
    if (x == 0 or (absX >= 0.1 and absX < 10_000_000.0)) {
        const s = std.fmt.allocPrint(a, "{d}", .{x}) catch @panic("Malgo: out of memory");
        if (std.mem.indexOfScalar(u8, s, '.') != null) return s;
        return std.fmt.allocPrint(a, "{s}.0", .{s}) catch @panic("Malgo: out of memory");
    }
    const s = std.fmt.allocPrint(a, "{e}", .{x}) catch @panic("Malgo: out of memory");
    const eIdx = std.mem.indexOfScalar(u8, s, 'e') orelse unreachable;
    const mantissa = s[0..eIdx];
    if (std.mem.indexOfScalar(u8, mantissa, '.') != null) return s;
    return std.fmt.allocPrint(a, "{s}.0{s}", .{ mantissa, s[eIdx..] }) catch @panic("Malgo: out of memory");
}

pub fn malgo_float_to_string(args: []const Value) Value {
    return mkString(formatHaskellFloat(f32, asF32(args[0])));
}
pub fn malgo_double_to_string(args: []const Value) Value {
    return mkString(formatHaskellFloat(f64, asF64(args[0])));
}

pub fn malgo_exit_failure(args: []const Value) Value {
    _ = args;
    std.process.exit(1);
}
pub fn malgo_exit_success(args: []const Value) Value {
    _ = args;
    std.process.exit(0);
}
pub fn malgo_exit_with_code(args: []const Value) Value {
    const bits: u32 = @bitCast(asI32(args[0]));
    std.process.exit(@truncate(bits));
}

pub fn malgo_newline(args: []const Value) Value {
    _ = args;
    writeStdout("\n");
    return unitValue();
}
pub fn malgo_print_char(args: []const Value) Value {
    var buf: [4]u8 = undefined;
    const len = std.unicode.utf8Encode(asChar(args[0]), &buf) catch panic("invalid Unicode codepoint");
    writeStdout(buf[0..len]);
    return unitValue();
}
pub fn malgo_print_string(args: []const Value) Value {
    writeStdout(asStr(args[0]));
    return unitValue();
}
pub fn malgo_print(args: []const Value) Value {
    writeStdout(valueToText(args[0]));
    return unitValue();
}
pub fn malgo_flush(args: []const Value) Value {
    _ = args;
    flushStdout();
    return unitValue();
}

/// Decodes one full Unicode scalar from stdin (which may be a multi-byte
/// UTF-8 sequence), unlike a single raw byte read.
pub fn malgo_get_char(args: []const Value) Value {
    _ = args;
    const lead = readStdinByte() orelse return mkChar(0);
    const seqLen = std.unicode.utf8ByteSequenceLength(lead) catch return mkChar(lead);
    if (seqLen == 1) return mkChar(lead);
    var buf: [4]u8 = undefined;
    buf[0] = lead;
    var i: u3 = 1;
    while (i < seqLen) : (i += 1) {
        buf[i] = readStdinByte() orelse panic("malformed UTF-8 on stdin");
    }
    const cp = std.unicode.utf8Decode(buf[0..seqLen]) catch panic("malformed UTF-8 on stdin");
    return mkChar(cp);
}

pub fn malgo_get_contents(args: []const Value) Value {
    _ = args;
    var out: std.ArrayList(u8) = .empty;
    while (readStdinByte()) |b| out.append(scratch(), b) catch @panic("Malgo: out of memory");
    return mkString(out.items);
}

pub fn malgo_get_line(args: []const Value) Value {
    _ = args;
    var out: std.ArrayList(u8) = .empty;
    while (readStdinByte()) |b| {
        if (b == '\n') break;
        out.append(scratch(), b) catch @panic("Malgo: out of memory");
    }
    return mkString(out.items);
}

/// The real argv, captured once from `main`'s `std.process.Init.Minimal`
/// (see the generated `main` in Malgo.Backend.Zig.Emit); empty until then
/// (e.g. inside `zig test`, which never calls `setArgv`).
var g_argv: []const [*:0]const u8 = &.{};

pub fn setArgv(vector: []const [*:0]const u8) void {
    g_argv = vector;
}

pub fn malgo_get_args(args: []const Value) Value {
    _ = args;
    // g_argv[0] is the program's own path, which System.Environment.getArgs
    // (and this project's Handlers.arguments) never includes.
    if (g_argv.len <= 1) return mkString("");
    var out: std.ArrayList(u8) = .empty;
    for (g_argv[1..], 0..) |arg, i| {
        if (i != 0) out.append(scratch(), '\n') catch @panic("Malgo: out of memory");
        out.appendSlice(scratch(), std.mem.sliceTo(arg, 0)) catch @panic("Malgo: out of memory");
    }
    return mkString(out.items);
}

pub fn malgo_stderr_string(args: []const Value) Value {
    writeStderr(asStr(args[0]));
    return unitValue();
}

// std.c.getenv, not std.posix (removed in Zig 0.16); libc is always linked
// (see the runtime unit tests' `-lc` requirement in AGENTS.md). Distinct
// has/get primitives (rather than treating "" as absent) so a variable
// explicitly set to the empty string is distinguishable from an unset one --
// matches the interpreter (`IO.getEnv`) and Scheme (`getenv`) backends.
pub fn malgo_has_env(args: []const Value) Value {
    const name = scratch().dupeZ(u8, asStr(args[0])) catch @panic("Malgo: out of memory");
    return boolValue(std.c.getenv(name) != null);
}
pub fn malgo_get_env(args: []const Value) Value {
    const name = scratch().dupeZ(u8, asStr(args[0])) catch @panic("Malgo: out of memory");
    const value = std.c.getenv(name) orelse return mkString("");
    return mkString(std.mem.sliceTo(value, 0));
}

pub fn malgo_panic(args: []const Value) Value {
    panic(asStr(args[0]));
}

/// The interpreter oracle (`Malgo.Sequent.Eval`'s `malgo_read_file`) calls
/// `BS.readFile` with no error handling, so a missing file kills the process
/// with an uncaught IOException. Panicking here matches that: both die
/// nonzero with a message on stderr, and `zig-golden.sh` compares stdout.
pub fn malgo_read_file(args: []const Value) Value {
    // `asStr` yields a non-NUL-terminated slice; open(2) needs a sentinel.
    // The scratch arena is bulk-freed at exit and exempt from the leak check.
    const path = scratch().dupeZ(u8, asStr(args[0])) catch @panic("Malgo: out of memory");
    const fd = std.c.open(path, .{ .ACCMODE = .RDONLY });
    if (fd < 0) panic("readFile: cannot open file");
    defer _ = std.c.close(fd);
    var out: std.ArrayList(u8) = .empty;
    var buf: [8192]u8 = undefined;
    while (true) {
        const n = std.c.read(fd, &buf, buf.len);
        if (n < 0) {
            if (std.c.errno(n) == .INTR) continue;
            panic("readFile: read error");
        }
        if (n == 0) break;
        out.appendSlice(scratch(), buf[0..@intCast(n)]) catch @panic("Malgo: out of memory");
    }
    return mkString(out.items);
}

pub fn malgo_write_file(args: []const Value) Value {
    const path = scratch().dupeZ(u8, asStr(args[0])) catch @panic("Malgo: out of memory");
    const fd = std.c.open(
        path,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true },
        @as(std.c.mode_t, 0o644),
    );
    if (fd < 0) panic("writeFile: cannot open file");
    defer _ = std.c.close(fd);
    writeAllFdChecked(fd, asStr(args[1]), "writeFile: write error");
    return unitValue();
}

fn isReadsSpace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == 11 or c == 12 or c == '\r';
}

fn parseIntLiteral(comptime T: type, s: []const u8) T {
    // Mirrors Haskell's `reads` for integers, which the oracle (Eval.hs)
    // requires to consume the whole string (`[(n, "")]`): skip leading
    // whitespace (via `lex`), then an optional '-' (no '+'), then one or
    // more digits, with nothing left over. `std.fmt.parseInt` differs from
    // this in both directions -- it accepts '_' separators and a leading
    // '+' that `reads` rejects, and it rejects leading whitespace that
    // `reads` accepts -- so the accepted digit run is sliced out by hand
    // and only that slice is handed to `parseInt`.
    var i: usize = 0;
    while (i < s.len and isReadsSpace(s[i])) : (i += 1) {}
    const digitsStart = i;
    if (i < s.len and s[i] == '-') i += 1;
    const signEnd = i;
    while (i < s.len and s[i] >= '0' and s[i] <= '9') : (i += 1) {}
    if (i == signEnd or i != s.len) panic("malformed integer literal in string");
    return std.fmt.parseInt(T, s[digitsStart..], 10) catch panic("malformed integer literal in string");
}

pub fn malgo_string_to_int32(args: []const Value) Value {
    return mkInt32(parseIntLiteral(i32, asStr(args[0])));
}
pub fn malgo_string_to_int64(args: []const Value) Value {
    return mkInt64(parseIntLiteral(i64, asStr(args[0])));
}

// ===== Generic `malgo_print`/error-message formatting =====
// Mirrors `Malgo.Sequent.Eval.valueToText`.

fn valueToText(v: Value) []const u8 {
    const a = scratch();
    return switch (v.kind) {
        .int32 => std.fmt.allocPrint(a, "{d}", .{v.payload.int32}) catch @panic("Malgo: out of memory"),
        .int64 => std.fmt.allocPrint(a, "{d}", .{v.payload.int64}) catch @panic("Malgo: out of memory"),
        .float => formatHaskellFloat(f32, v.payload.float),
        .double => formatHaskellFloat(f64, v.payload.double),
        .char => utf8EncodeAlloc(a, v.payload.char),
        .string => v.payload.string,
        .strukt => structToText(v.payload.strukt),
        .closure => "<function>",
        .record => "<record>",
        .codata => "<codata>",
        .unit => "()",
    };
}

// ===== Runtime unit tests (`zig test runtime/zig/runtime.zig`) =====
// The Haskell test suite never runs these; CI's zig-golden job and local
// development do.

/// An `int32` Object that is genuinely heap-allocated, for the RC and
/// allocation-accounting tests below. `mkInt32` interns
/// `INT32_INTERN_MIN..INT32_INTERN_MAX` into immortal statics (#385), which are
/// deliberately invisible to `g_live_objects`/`g_total_allocs` and inert under
/// dup/drop -- so a test that means to exercise refcounting has to ask for a
/// value outside that range. `nth` just keeps distinct payloads distinct.
fn mkHeapInt(nth: i32) Value {
    return mkInt32(INT32_INTERN_MAX + 1 + nth);
}

test "small ints are interned: no allocation, no RC traffic, still correct" {
    initHeap();
    const before = g_live_objects;
    const beforeAllocs = g_total_allocs;

    const a = mkInt32(7);
    const b = mkInt32(7);
    // Same value means the very same Object, and no allocation happened.
    try std.testing.expectEqual(a, b);
    try std.testing.expectEqual(beforeAllocs, g_total_allocs);
    try std.testing.expectEqual(before, g_live_objects);
    try std.testing.expectEqual(@as(i32, 7), asI32(a));

    // Immortal, so dup/drop cannot free it and the leak gate ignores it.
    try std.testing.expectEqual(IMMORTAL, a.rc);
    dup(a);
    drop(a);
    drop(a);
    try std.testing.expectEqual(IMMORTAL, a.rc);
    try std.testing.expectEqual(before, g_live_objects);

    // An interned int can never be offered as a reuse token.
    try std.testing.expectEqual(@as(?Value, null), dropReuse(a, 2));

    // Both ends of the range are interned; just past either end is not.
    try std.testing.expectEqual(IMMORTAL, mkInt32(INT32_INTERN_MIN).rc);
    try std.testing.expectEqual(IMMORTAL, mkInt32(INT32_INTERN_MAX).rc);
    const lo = mkInt32(INT32_INTERN_MIN - 1);
    const hi = mkInt32(INT32_INTERN_MAX + 1);
    try std.testing.expectEqual(@as(u32, 1), lo.rc);
    try std.testing.expectEqual(@as(u32, 1), hi.rc);
    try std.testing.expectEqual(INT32_INTERN_MIN - 1, asI32(lo));
    try std.testing.expectEqual(INT32_INTERN_MAX + 1, asI32(hi));
    drop(lo);
    drop(hi);
    try std.testing.expectEqual(before, g_live_objects);
}

test "dup/drop: an extra reference delays the free by exactly one drop" {
    initHeap();
    const before = g_live_objects;
    const v = mkHeapInt(0);
    try std.testing.expectEqual(before + 1, g_live_objects);
    dup(v);
    drop(v);
    try std.testing.expectEqual(before + 1, g_live_objects);
    drop(v);
    try std.testing.expectEqual(before, g_live_objects);
}

test "g_total_allocs counts every Object ever allocated, freed or not" {
    initHeap();
    const before = g_total_allocs;
    const a = mkHeapInt(0);
    const pair = mkStruct(.{ .tuple = {} }, &[_]Value{a});
    try std.testing.expectEqual(before + 2, g_total_allocs);
    drop(pair);
    try std.testing.expectEqual(before + 2, g_total_allocs);
}

test "drop releases a struct's children transitively" {
    initHeap();
    const before = g_live_objects;
    const a = mkHeapInt(0);
    const b = mkHeapInt(1);
    const pair = mkStruct(.{ .tuple = {} }, &[_]Value{ a, b });
    try std.testing.expectEqual(before + 3, g_live_objects);
    drop(pair);
    try std.testing.expectEqual(before, g_live_objects);
}

test "a shared child survives its first parent's drop" {
    initHeap();
    const before = g_live_objects;
    const shared = mkInt32(7);
    dup(shared);
    const p1 = mkStruct(.{ .tuple = {} }, &[_]Value{shared});
    const p2 = mkStruct(.{ .tuple = {} }, &[_]Value{shared});
    drop(p1);
    try std.testing.expectEqual(@as(i32, 7), shared.payload.int32);
    drop(p2);
    try std.testing.expectEqual(before, g_live_objects);
}

test "deep chain frees iteratively without native-stack recursion" {
    initHeap();
    const before = g_live_objects;
    var chain = mkStruct(.{ .named = "Nil" }, &[_]Value{});
    var i: usize = 0;
    while (i < 100_000) : (i += 1) {
        chain = mkStruct(.{ .named = "Cons" }, &[_]Value{ mkInt32(0), chain });
    }
    drop(chain);
    try std.testing.expectEqual(before, g_live_objects);
}

test "immortals ignore rc traffic" {
    initHeap();
    dup(no_self);
    drop(no_self);
    drop(no_self);
    try std.testing.expectEqual(IMMORTAL, no_self.rc);
    const k = identityKont();
    dup(k);
    drop(k);
    drop(k);
    try std.testing.expectEqual(IMMORTAL, k.rc);
}

test "string payloads are owned and freed with the object" {
    initHeap();
    const before = g_live_objects;
    const s = mkString("hello");
    const t = malgo_string_append(&[_]Value{ s, s });
    try std.testing.expect(stringEq(t.payload.string, "hellohello"));
    drop(t);
    drop(s);
    try std.testing.expectEqual(before, g_live_objects);
}

test "unsafe_cast returns an owned reference" {
    initHeap();
    const before = g_live_objects;
    const v = mkHeapInt(0);
    const w = malgo_unsafe_cast(&[_]Value{v});
    drop(w);
    try std.testing.expectEqual(before + 1, g_live_objects);
    drop(v);
    try std.testing.expectEqual(before, g_live_objects);
}

test "dropReuse recycles a uniquely-referenced same-arity struct in place" {
    initHeap();
    const before = g_live_objects;
    const beforeAllocs = g_total_allocs;
    const beforeHits = g_reuse_hits;
    const a = mkHeapInt(0);
    const b = mkHeapInt(1);
    const pair = mkStruct(.{ .tuple = {} }, &[_]Value{ a, b });
    const tok = dropReuse(pair, 2);
    try std.testing.expect(tok != null);
    try std.testing.expectEqual(pair, tok.?);
    try std.testing.expectEqual(beforeHits + 1, g_reuse_hits);
    // a/b were released transitively, just as an ordinary drop would.
    try std.testing.expectEqual(before + 1, g_live_objects);
    const c = mkHeapInt(2);
    const d = mkHeapInt(3);
    const rebuilt = mkStructReuse(tok, .{ .named = "Cons" }, &[_]Value{ c, d });
    try std.testing.expectEqual(pair, rebuilt);
    // 5 Objects allocated total (a, b, pair, c, d); the recycled pair
    // Object + its array were reused for `rebuilt`, not freed-and-reallocated.
    try std.testing.expectEqual(beforeAllocs + 5, g_total_allocs);
    drop(rebuilt);
    try std.testing.expectEqual(before, g_live_objects);
}

test "dropReuse falls back to an ordinary drop for a shared value" {
    initHeap();
    const before = g_live_objects;
    const a = mkInt32(1);
    const pair = mkStruct(.{ .tuple = {} }, &[_]Value{a});
    dup(pair);
    const tok = dropReuse(pair, 1);
    try std.testing.expect(tok == null);
    try std.testing.expectEqual(@as(u32, 1), pair.rc);
    drop(pair);
    try std.testing.expectEqual(before, g_live_objects);
}

test "dropReuse falls back for an immortal value" {
    initHeap();
    try std.testing.expectEqual(@as(?Value, null), dropReuse(no_self, 0));
    try std.testing.expectEqual(IMMORTAL, no_self.rc);
}

test "dropReuse reallocates the backing array on an arity mismatch" {
    initHeap();
    const before = g_live_objects;
    const a = mkInt32(1);
    const pair = mkStruct(.{ .tuple = {} }, &[_]Value{a});
    const tok = dropReuse(pair, 3);
    try std.testing.expect(tok != null);
    const c = mkInt32(2);
    const d = mkInt32(3);
    const e = mkInt32(4);
    const rebuilt = mkStructReuse(tok, .{ .tuple = {} }, &[_]Value{ c, d, e });
    try std.testing.expectEqual(pair, rebuilt);
    try std.testing.expectEqual(@as(usize, 3), rebuilt.payload.strukt.fields.len);
    drop(rebuilt);
    try std.testing.expectEqual(before, g_live_objects);
}

test "dropReuse recycles a closure's Object into a struct" {
    initHeap();
    const before = g_live_objects;
    const a = mkInt32(1);
    const b = mkInt32(2);
    const closure = mkClosure(&identityCode, &[_]Value{ a, b });
    const tok = dropReuse(closure, 2);
    try std.testing.expect(tok != null);
    try std.testing.expectEqual(closure, tok.?);
    const c = mkInt32(3);
    const d = mkInt32(4);
    const rebuilt = mkStructReuse(tok, .{ .tuple = {} }, &[_]Value{ c, d });
    try std.testing.expectEqual(closure, rebuilt);
    drop(rebuilt);
    try std.testing.expectEqual(before, g_live_objects);
}

test "mkStructReuse allocates fresh on a null token" {
    initHeap();
    const before = g_live_objects;
    const a = mkInt32(1);
    const built = mkStructReuse(null, .{ .tuple = {} }, &[_]Value{a});
    try std.testing.expectEqual(Kind.strukt, built.kind);
    drop(built);
    try std.testing.expectEqual(before, g_live_objects);
}

// --- Trampoline dispatch (see `CodeFn`/`run`) ---

var t_remaining: usize = 0;
var t_first_frame: usize = 0;
var t_last_frame: usize = 0;

/// Passes its `self` and argument straight through for `t_remaining`
/// dispatches, then finishes -- the minimal shape of a generated function
/// under the Action protocol, and a probe of the frame address `run`
/// dispatches from.
fn chainCode(self: Value, args: []const Value) Action {
    var probe: u8 = 0;
    std.mem.doNotOptimizeAway(&probe);
    const addr = @intFromPtr(&probe);
    if (t_first_frame == 0) t_first_frame = addr;
    t_last_frame = addr;
    if (t_remaining == 0) {
        drop(self);
        return done(args[0]);
    }
    t_remaining -= 1;
    return mkAction(&chainCode, self, args);
}

test "run dispatches in constant native stack and is RC-neutral" {
    initHeap();
    const before = g_live_objects;
    t_remaining = 100_000;
    t_first_frame = 0;
    t_last_frame = 0;

    const callee = mkClosure(&chainCode, &[_]Value{});
    const result = run(&chainCode, callee, &[_]Value{mkInt32(7)});

    // The property issue #360 is about: 100k dispatches, one frame. Before
    // the trampoline this chain cost ~98.6 bytes of never-popped stack each.
    try std.testing.expect(t_first_frame != 0);
    try std.testing.expectEqual(t_first_frame, t_last_frame);

    try std.testing.expectEqual(@as(i32, 7), result.payload.int32);
    drop(result);
    // `run` neither dups nor drops: the one reference of `callee` and the one
    // of the argument that went in are the ones that came back out.
    try std.testing.expectEqual(before, g_live_objects);
}

fn forcedFieldCode(self: Value, args: []const Value) Action {
    drop(self);
    return applyCovalue(args[0], mkInt32(99));
}

test "forceField runs a nested trampoline down to a plain value" {
    initHeap();
    const before = g_live_objects;
    const rec = mkRecord(&[_]NamedField{.{ .name = "f", .code = &forcedFieldCode }}, &[_]Value{});

    const forced = forceField(rec, "f") orelse return error.TestUnexpectedResult;

    try std.testing.expectEqual(@as(i32, 99), forced.payload.int32);
    try std.testing.expectEqual(@as(usize, 0), g_force_depth);
    try std.testing.expect(g_force_depth_max >= 1);
    drop(forced);
    // forceField consumed the record's reference as the field code's `self`.
    try std.testing.expectEqual(before, g_live_objects);
}

fn dropRestCode(self: Value, args: []const Value) Action {
    drop(self);
    for (args[1..]) |a| drop(a);
    return done(args[0]);
}

test "an action carrying exactly MAX_ARGS arguments round-trips" {
    initHeap();
    const before = g_live_objects;
    var argv: [MAX_ARGS]Value = undefined;
    for (&argv, 0..) |*slot, i| slot.* = mkInt32(@intCast(i));

    const callee = mkClosure(&dropRestCode, &[_]Value{});
    const result = run(&dropRestCode, callee, argv[0..MAX_ARGS]);

    try std.testing.expectEqual(@as(i32, 0), result.payload.int32);
    drop(result);
    try std.testing.expectEqual(before, g_live_objects);
}

test "applyDestructor dispatches to the matching branch" {
    initHeap();
    const before = g_live_objects;
    // Nothing in the compiler constructs codata yet (Cocase lowers to a
    // panic stub), so this path has no corpus coverage at all -- build one
    // by hand rather than leave the branch untested.
    const cd = mkCodata(&[_]NamedBranch{
        .{ .name = "other", .code = &forcedFieldCode },
        .{ .name = "wanted", .code = &forcedFieldCode },
    }, &[_]Value{});

    const action = applyDestructor(cd, "wanted", &[_]Value{identityKont()});
    try std.testing.expectEqual(cd, action.self);
    const result = run(action.code.?, action.self, action.argv[0..action.argc]);

    try std.testing.expectEqual(@as(i32, 99), result.payload.int32);
    drop(result);
    try std.testing.expectEqual(before, g_live_objects);
}

// Zig only semantically analyses what is reachable, so a `pub fn` that no
// test and no generated program happens to reference is never typechecked --
// `zig test` passes and the breakage surfaces later as a `zig build-exe`
// failure inside `malgo compile`. That is not hypothetical: the first draft
// of `malgo_read_file` below used `std.posix.open`, which does not exist in
// Zig 0.16, and the suite stayed green until a test referenced it. This
// forces every declaration in the file to be analysed.
test "every declaration typechecks" {
    std.testing.refAllDecls(@This());
}

// --- File I/O ---

test "writeFile then readFile round-trips, owning only the returned string" {
    initHeap();
    const before = g_live_objects;

    const path = mkString("malgo-runtime-test-roundtrip.txt");
    const body = mkString("hello\nfile\n");
    const unit = malgo_write_file(&[_]Value{ path, body });
    const read = malgo_read_file(&[_]Value{path});

    try std.testing.expect(stringEq(asStr(read), "hello\nfile\n"));

    // Primitives borrow their arguments and return an owned value
    // (`Ir.Prim`: "borrows its arguments, returns owned"), so the caller
    // still owns `path`/`body` here and nothing was consumed under us.
    drop(read);
    drop(unit);
    drop(body);
    drop(path);
    try std.testing.expectEqual(before, g_live_objects);

    _ = std.c.unlink("malgo-runtime-test-roundtrip.txt");
}

test "readFile of an empty file yields an empty string" {
    initHeap();
    const before = g_live_objects;

    const path = mkString("malgo-runtime-test-empty.txt");
    const empty = mkString("");
    const unit = malgo_write_file(&[_]Value{ path, empty });
    const read = malgo_read_file(&[_]Value{path});

    try std.testing.expectEqual(@as(usize, 0), asStr(read).len);

    drop(read);
    drop(unit);
    drop(empty);
    drop(path);
    try std.testing.expectEqual(before, g_live_objects);

    _ = std.c.unlink("malgo-runtime-test-empty.txt");
}

test "readFile reads back a payload larger than one read buffer" {
    initHeap();
    const before = g_live_objects;

    // 8192 is the read chunk size; go past it so the accumulate loop runs
    // more than once.
    var big: [20000]u8 = undefined;
    for (&big, 0..) |*b, i| b.* = @intCast('a' + (i % 26));

    const path = mkString("malgo-runtime-test-big.txt");
    const body = mkString(&big);
    const unit = malgo_write_file(&[_]Value{ path, body });
    const read = malgo_read_file(&[_]Value{path});

    try std.testing.expectEqual(@as(usize, big.len), asStr(read).len);
    try std.testing.expect(stringEq(asStr(read), &big));

    drop(read);
    drop(unit);
    drop(body);
    drop(path);
    try std.testing.expectEqual(before, g_live_objects);

    _ = std.c.unlink("malgo-runtime-test-big.txt");
}

fn structToText(s: Struct) []const u8 {
    const a = scratch();
    return switch (s.tag) {
        .tuple => blk: {
            if (s.fields.len == 0) break :blk "{}";
            var out: std.ArrayList(u8) = .empty;
            out.appendSlice(a, "{") catch @panic("Malgo: out of memory");
            for (s.fields, 0..) |f, i| {
                if (i != 0) out.appendSlice(a, ", ") catch @panic("Malgo: out of memory");
                out.appendSlice(a, valueToText(f)) catch @panic("Malgo: out of memory");
            }
            out.appendSlice(a, "}") catch @panic("Malgo: out of memory");
            break :blk out.items;
        },
        .named => |name| blk: {
            if (s.fields.len == 0) break :blk name;
            var out: std.ArrayList(u8) = .empty;
            out.appendSlice(a, name) catch @panic("Malgo: out of memory");
            out.appendSlice(a, "(") catch @panic("Malgo: out of memory");
            for (s.fields, 0..) |f, i| {
                if (i != 0) out.appendSlice(a, ", ") catch @panic("Malgo: out of memory");
                out.appendSlice(a, valueToText(f)) catch @panic("Malgo: out of memory");
            }
            out.appendSlice(a, ")") catch @panic("Malgo: out of memory");
            break :blk out.items;
        },
    };
}
