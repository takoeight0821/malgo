//! Malgo Zig backend runtime.
//!
//! v1 (M1-M8): arena allocator, never frees. `dup`/`drop` land with the
//! Perceus RC pass (M9); until then nothing in the emitted code calls them.
//!
//! I/O deliberately bypasses Zig 0.16's new async `std.Io` interface (which
//! churned significantly between 0.15 and 0.16) in favor of direct libc/
//! POSIX calls (`std.c.write`, `std.posix.read`), which are a thin, stable
//! layer unlikely to move under us across Zig point releases. All writes
//! are unbuffered, so there is nothing to flush and no "flush before exit /
//! before stdin read" ordering bug to get wrong.
const std = @import("std");

// ===== Value representation =====

pub const Kind = enum(u8) { int32, int64, float, double, char, string, strukt, closure, record, codata, unit };

pub const Tag = union(enum) { tuple: void, named: []const u8 };

pub const CodeFn = *const fn (cap: []const Value, args: []const Value) Value;

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

pub const Object = struct { kind: Kind, payload: Payload };
pub const Value = *const Object;

// ===== Allocation (arena, v1) =====

pub var g_arena: *std.heap.ArenaAllocator = undefined;

pub fn newArena() std.heap.ArenaAllocator {
    return std.heap.ArenaAllocator.init(std.heap.page_allocator);
}

fn alloc(kind: Kind, payload: Payload) Value {
    const obj = g_arena.allocator().create(Object) catch @panic("Malgo: out of memory");
    obj.* = Object{ .kind = kind, .payload = payload };
    return obj;
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
pub fn mkString(bytes: []const u8) Value {
    return alloc(.string, .{ .string = bytes });
}

/// Copies `fields`: the caller typically passes a stack-temporary slice
/// literal (`&[_]Value{...}`) that does not outlive this call.
pub fn mkStruct(tag: Tag, fields: []const Value) Value {
    const owned = g_arena.allocator().dupe(Value, fields) catch @panic("Malgo: out of memory");
    return alloc(.strukt, .{ .strukt = .{ .tag = tag, .fields = owned } });
}

/// Copies `captures`, for the same reason as 'mkStruct'.
pub fn mkClosure(code: CodeFn, captures: []const Value) Value {
    const owned = g_arena.allocator().dupe(Value, captures) catch @panic("Malgo: out of memory");
    return alloc(.closure, .{ .closure = .{ .code = code, .captures = owned } });
}

pub fn mkRecord(fields: []const NamedField, captures: []const Value) Value {
    const ownedFields = g_arena.allocator().dupe(NamedField, fields) catch @panic("Malgo: out of memory");
    const ownedCaptures = g_arena.allocator().dupe(Value, captures) catch @panic("Malgo: out of memory");
    return alloc(.record, .{ .record = .{ .fields = ownedFields, .captures = ownedCaptures } });
}

pub fn mkCodata(branches: []const NamedBranch, captures: []const Value) Value {
    const ownedBranches = g_arena.allocator().dupe(NamedBranch, branches) catch @panic("Malgo: out of memory");
    const ownedCaptures = g_arena.allocator().dupe(Value, captures) catch @panic("Malgo: out of memory");
    return alloc(.codata, .{ .codata = .{ .branches = ownedBranches, .captures = ownedCaptures } });
}

// ===== Dispatch =====

pub fn applyCovalue(covalue: Value, value: Value) Value {
    return callClosure(covalue, &[_]Value{value});
}

pub fn callClosure(closure: Value, args: []const Value) Value {
    if (closure.kind != .closure) panic("callClosure: value is not a function");
    return closure.payload.closure.code(closure.payload.closure.captures, args);
}

pub fn applyDestructor(codata: Value, name: []const u8, args: []const Value) Value {
    if (codata.kind != .codata) panic("applyDestructor: value is not codata");
    for (codata.payload.codata.branches) |branch| {
        if (stringEq(branch.name, name)) return branch.code(codata.payload.codata.captures, args);
    }
    panic("applyDestructor: no matching destructor");
}

pub fn projectField(record: Value, name: []const u8, k: Value) Value {
    if (record.kind != .record) panic("projectField: value is not a record");
    for (record.payload.record.fields) |field| {
        if (stringEq(field.name, name)) return field.code(record.payload.record.captures, &[_]Value{k});
    }
    panic("projectField: no such field");
}

fn identityCode(cap: []const Value, args: []const Value) Value {
    _ = cap;
    return args[0];
}

/// A covalue that, once invoked, returns its argument unchanged. Since every
/// generated function tail-returns whatever the covalue it invokes returns,
/// calling a record field's code with this as its continuation makes the
/// field's (call-by-name) computation synchronous from the caller's point of
/// view -- used by `Expand` pattern matching to force a field into a plain
/// value it can immediately test and bind against.
pub fn identityKont() Value {
    return mkClosure(&identityCode, &[_]Value{});
}

/// Force a record field by name without checking `record`'s kind first
/// (callers that already know it is a record, e.g. `Expand` pattern
/// matching after its own `.kind == .record` guard, can skip that check).
pub fn forceField(record: Value, name: []const u8) ?Value {
    if (record.kind != .record) return null;
    for (record.payload.record.fields) |field| {
        if (stringEq(field.name, name)) return field.code(record.payload.record.captures, &[_]Value{identityKont()});
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
        if (n <= 0) return;
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

fn utf8EncodeAlloc(codepoint: u21) []const u8 {
    var buf: [4]u8 = undefined;
    const len = std.unicode.utf8Encode(codepoint, &buf) catch panic("invalid Unicode codepoint");
    return g_arena.allocator().dupe(u8, buf[0..len]) catch @panic("Malgo: out of memory");
}

// ===== Primitives (rt.<foreign import name>, called with a single args slice) =====
// Authoritative semantics: src/Malgo/Sequent/Eval.hs's `fetchPrimitive`.

pub fn malgo_unsafe_cast(args: []const Value) Value {
    return args[0];
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
    return mkInt32(@divFloor(asI32(args[0]), asI32(args[1])));
}
pub fn malgo_mod_int32_t(args: []const Value) Value {
    return mkInt32(@mod(asI32(args[0]), asI32(args[1])));
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
    return mkInt64(@divFloor(asI64(args[0]), asI64(args[1])));
}
pub fn malgo_mod_int64_t(args: []const Value) Value {
    return mkInt64(@mod(asI64(args[0]), asI64(args[1])));
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
    return mkChar(@intCast(asI32(args[0])));
}
pub fn malgo_char_to_string(args: []const Value) Value {
    return mkString(utf8EncodeAlloc(asChar(args[0])));
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
    const owned = g_arena.allocator().alloc(u8, len + s.len) catch @panic("Malgo: out of memory");
    @memcpy(owned[0..len], buf[0..len]);
    @memcpy(owned[len..], s);
    return mkString(owned);
}

pub fn malgo_string_append(args: []const Value) Value {
    const a = asStr(args[0]);
    const b = asStr(args[1]);
    const owned = g_arena.allocator().alloc(u8, a.len + b.len) catch @panic("Malgo: out of memory");
    @memcpy(owned[0..a.len], a);
    @memcpy(owned[a.len..], b);
    return mkString(owned);
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
    const owned = g_arena.allocator().dupe(u8, s[startByte..endByte]) catch @panic("Malgo: out of memory");
    return mkString(owned);
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
    while (it.nextCodepoint()) |cp| codepoints.append(g_arena.allocator(), cp) catch @panic("Malgo: out of memory");
    var out: std.ArrayList(u8) = .empty;
    var i: usize = codepoints.items.len;
    while (i > 0) {
        i -= 1;
        var buf: [4]u8 = undefined;
        const len = std.unicode.utf8Encode(codepoints.items[i], &buf) catch panic("invalid Unicode codepoint");
        out.appendSlice(g_arena.allocator(), buf[0..len]) catch @panic("Malgo: out of memory");
    }
    return mkString(out.items);
}

pub fn malgo_int32_t_to_string(args: []const Value) Value {
    const s = std.fmt.allocPrint(g_arena.allocator(), "{d}", .{asI32(args[0])}) catch @panic("Malgo: out of memory");
    return mkString(s);
}
pub fn malgo_int64_t_to_string(args: []const Value) Value {
    const s = std.fmt.allocPrint(g_arena.allocator(), "{d}", .{asI64(args[0])}) catch @panic("Malgo: out of memory");
    return mkString(s);
}
pub fn malgo_float_to_string(args: []const Value) Value {
    // Placeholder pending the Haskell-`show`-compatible formatter (M6);
    // ArithInt32/HelloBoxed-class M1 tests never call this.
    const s = std.fmt.allocPrint(g_arena.allocator(), "{d}", .{asF32(args[0])}) catch @panic("Malgo: out of memory");
    return mkString(s);
}
pub fn malgo_double_to_string(args: []const Value) Value {
    const s = std.fmt.allocPrint(g_arena.allocator(), "{d}", .{asF64(args[0])}) catch @panic("Malgo: out of memory");
    return mkString(s);
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
    writeStdout(utf8EncodeAlloc(asChar(args[0])));
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

pub fn malgo_get_char(args: []const Value) Value {
    _ = args;
    const b = readStdinByte() orelse return mkChar(0);
    return mkChar(b);
}

pub fn malgo_get_contents(args: []const Value) Value {
    _ = args;
    var out: std.ArrayList(u8) = .empty;
    while (readStdinByte()) |b| out.append(g_arena.allocator(), b) catch @panic("Malgo: out of memory");
    return mkString(out.items);
}

pub fn malgo_get_line(args: []const Value) Value {
    _ = args;
    var out: std.ArrayList(u8) = .empty;
    while (readStdinByte()) |b| {
        if (b == '\n') break;
        out.append(g_arena.allocator(), b) catch @panic("Malgo: out of memory");
    }
    return mkString(out.items);
}

pub fn malgo_get_args(args: []const Value) Value {
    _ = args;
    // No test case passes program arguments (see plan); an empty argv is
    // observably identical to the interpreter under every golden case.
    return mkString("");
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
    const a = g_arena.allocator();
    return switch (v.kind) {
        .int32 => std.fmt.allocPrint(a, "{d}", .{v.payload.int32}) catch @panic("Malgo: out of memory"),
        .int64 => std.fmt.allocPrint(a, "{d}", .{v.payload.int64}) catch @panic("Malgo: out of memory"),
        .float => std.fmt.allocPrint(a, "{d}", .{v.payload.float}) catch @panic("Malgo: out of memory"),
        .double => std.fmt.allocPrint(a, "{d}", .{v.payload.double}) catch @panic("Malgo: out of memory"),
        .char => utf8EncodeAlloc(v.payload.char),
        .string => v.payload.string,
        .strukt => structToText(v.payload.strukt),
        .closure => "<function>",
        .record => "<record>",
        .codata => "<codata>",
        .unit => "()",
    };
}

fn structToText(s: Struct) []const u8 {
    const a = g_arena.allocator();
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
