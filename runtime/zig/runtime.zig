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
pub const CodeFn = *const fn (self: Value, args: []const Value) Value;

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

/// Set from the MALGO_RC_TRACE environment variable at startup. When true,
/// every `*Named` wrapper below (see "Named RC tracing" further down) emits
/// one JSON-lines record per RC event to stderr, for offline correlation by
/// `scripts/rctrace.py`. The plain (untraced) dup/drop/mkStruct/... below
/// are never touched by this -- tracing is purely additive and cannot
/// change any RC decision.
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
    g_trace_enabled = std.c.getenv("MALGO_RC_TRACE") != null;
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

pub inline fn dup(v: Value) void {
    if (v.rc != IMMORTAL) v.rc += 1;
}

pub fn drop(v: Value) void {
    if (v.rc == IMMORTAL) return;
    // An underflow here is an over-drop bug in the Perceus pass; fail at
    // the exact faulty drop rather than corrupting the heap.
    std.debug.assert(v.rc > 0);
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
    std.debug.assert(obj.rc == 1 and obj.kind == .strukt);
    if (obj.payload.strukt.fields.len == fields.len) {
        @memcpy(@constCast(obj.payload.strukt.fields), fields);
        obj.payload.strukt.tag = tag;
    } else {
        std.debug.assert(obj.payload.strukt.fields.len == 0);
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
        std.debug.assert(c.rc > 0);
        c.rc -= 1;
        if (c.rc == 0) g_free_worklist.append(scratch(), c) catch @panic("Malgo: out of memory");
    }
}

pub fn mkInt32(n: i32) Value {
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
// dup/drop/dropReuse/mkStruct/mkClosure/mkStructReuse above are otherwise
// only exercised directly by this file's own `test` blocks, so tracing
// cannot perturb the RC decisions those tests already exercise.

fn traceLine(event: []const u8, v: Value, name: []const u8, func: []const u8) void {
    if (!g_trace_enabled) return;
    const rc: i64 = if (v.rc == IMMORTAL) -1 else @intCast(v.rc);
    var buf: [256]u8 = undefined;
    const line = std.fmt.bufPrint(
        &buf,
        "{{\"ev\":\"{s}\",\"ptr\":\"0x{x}\",\"rc\":{d},\"name\":\"{s}\",\"func\":\"{s}\"}}\n",
        .{ event, @intFromPtr(v), rc, name, func },
    ) catch return;
    writeStderr(line);
}

/// Like 'traceLine', but identifies the object by an address captured
/// earlier instead of dereferencing `v` -- for events reported after a call
/// (like 'dropReuseNamed') that may already have freed the object.
fn traceAddr(event: []const u8, addr: usize, name: []const u8, func: []const u8) void {
    if (!g_trace_enabled) return;
    var buf: [256]u8 = undefined;
    const line = std.fmt.bufPrint(
        &buf,
        "{{\"ev\":\"{s}\",\"ptr\":\"0x{x}\",\"name\":\"{s}\",\"func\":\"{s}\"}}\n",
        .{ event, addr, name, func },
    ) catch return;
    writeStderr(line);
}

/// Traces a construction event (`mkStruct`/`mkClosure`/`mkStructReuse`):
/// records the fresh container's address plus, per slot, the symbolic name
/// and child address that went into it -- so a later 'traceDecChild' event
/// against the same (container, slot) pair can be resolved back to a name.
fn traceSlots(event: []const u8, v: Value, fields: []const Value, names: []const []const u8, func: []const u8) void {
    if (!g_trace_enabled) return;
    var head: [256]u8 = undefined;
    const headLine = std.fmt.bufPrint(
        &head,
        "{{\"ev\":\"{s}\",\"ptr\":\"0x{x}\",\"func\":\"{s}\",\"slots\":[",
        .{ event, @intFromPtr(v), func },
    ) catch return;
    writeStderr(headLine);
    for (fields, 0..) |child, i| {
        if (i != 0) writeStderr(",");
        const name = if (i < names.len) names[i] else "?";
        var slot: [256]u8 = undefined;
        const slotLine = std.fmt.bufPrint(
            &slot,
            "{{\"i\":{d},\"name\":\"{s}\",\"child\":\"0x{x}\"}}",
            .{ i, name, @intFromPtr(child) },
        ) catch continue;
        writeStderr(slotLine);
    }
    writeStderr("]}\n");
}

/// Traces a `decChildren` release: `container`'s slot `slot` (a former
/// field/capture) is about to have its reference decremented. `rc_before`
/// is `child`'s refcount at this moment.
fn traceDecChild(container: Value, slot: usize, child: Value) void {
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

// ===== Dispatch =====

pub fn applyCovalue(covalue: Value, value: Value) Value {
    return callClosure(covalue, &[_]Value{value});
}

pub fn callClosure(closure: Value, args: []const Value) Value {
    if (closure.kind != .closure) panic("callClosure: value is not a function");
    return closure.payload.closure.code(closure, args);
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

pub fn applyDestructor(codata: Value, name: []const u8, args: []const Value) Value {
    if (codata.kind != .codata) panic("applyDestructor: value is not codata");
    for (codata.payload.codata.branches) |branch| {
        if (stringEq(branch.name, name)) return branch.code(codata, args);
    }
    panic("applyDestructor: no matching destructor");
}

pub fn projectField(record: Value, name: []const u8, k: Value) Value {
    if (record.kind != .record) panic("projectField: value is not a record");
    for (record.payload.record.fields) |field| {
        if (stringEq(field.name, name)) return field.code(record, &[_]Value{k});
    }
    panic("projectField: no such field");
}

fn identityCode(self: Value, args: []const Value) Value {
    // The uniform closure protocol is "dup used captures, then drop self";
    // with no captures and an immortal self this is a no-op, kept for
    // uniformity with generated closure bodies.
    drop(self);
    return args[0];
}

var IDENTITY_KONT_OBJ: Object = .{
    .rc = IMMORTAL,
    .kind = .closure,
    .payload = .{ .closure = .{ .code = &identityCode, .captures = &[_]Value{} } },
};

/// A covalue that, once invoked, returns its argument unchanged. Since every
/// generated function tail-returns whatever the covalue it invokes returns,
/// calling a record field's code with this as its continuation makes the
/// field's (call-by-name) computation synchronous from the caller's point of
/// view -- used by `Expand` pattern matching to force a field into a plain
/// value it can immediately test and bind against. Immortal: forcing a
/// field allocates nothing and creates no RC obligation.
pub fn identityKont() Value {
    return &IDENTITY_KONT_OBJ;
}

/// Force a record field by name without checking `record`'s kind first
/// (callers that already know it is a record, e.g. `Expand` pattern
/// matching after its own `.kind == .record` guard, can skip that check).
pub fn forceField(record: Value, name: []const u8) ?Value {
    if (record.kind != .record) return null;
    for (record.payload.record.fields) |field| {
        if (stringEq(field.name, name)) return field.code(record, &[_]Value{identityKont()});
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
        var buf: [96]u8 = undefined;
        const line = std.fmt.bufPrint(&buf, "MALGO-STATS: total_allocs={d} reuse_hits={d}\n", .{ g_total_allocs, g_reuse_hits }) catch "MALGO-STATS: ?\n";
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
    const n = std.posix.read(0, &buf) catch return null;
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
    const takeCount = rawEnd - rawStart;
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

pub fn malgo_panic(args: []const Value) Value {
    panic(asStr(args[0]));
}

pub fn malgo_read_file(args: []const Value) Value {
    _ = args;
    panicUnimplemented("malgo_read_file");
}
pub fn malgo_write_file(args: []const Value) Value {
    _ = args;
    panicUnimplemented("malgo_write_file");
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

test "dup/drop: an extra reference delays the free by exactly one drop" {
    initHeap();
    const before = g_live_objects;
    const v = mkInt32(42);
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
    const a = mkInt32(1);
    const pair = mkStruct(.{ .tuple = {} }, &[_]Value{a});
    try std.testing.expectEqual(before + 2, g_total_allocs);
    drop(pair);
    try std.testing.expectEqual(before + 2, g_total_allocs);
}

test "drop releases a struct's children transitively" {
    initHeap();
    const before = g_live_objects;
    const a = mkInt32(1);
    const b = mkInt32(2);
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
    const v = mkInt32(1);
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
    const a = mkInt32(1);
    const b = mkInt32(2);
    const pair = mkStruct(.{ .tuple = {} }, &[_]Value{ a, b });
    const tok = dropReuse(pair, 2);
    try std.testing.expect(tok != null);
    try std.testing.expectEqual(pair, tok.?);
    try std.testing.expectEqual(beforeHits + 1, g_reuse_hits);
    // a/b were released transitively, just as an ordinary drop would.
    try std.testing.expectEqual(before + 1, g_live_objects);
    const c = mkInt32(3);
    const d = mkInt32(4);
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
