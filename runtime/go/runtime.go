// Malgo's Go runtime. Embedded verbatim into every generated program by
// Malgo.Backend.Go.Runtime, so this file is a fragment: it declares no
// package clause and no imports (Emit supplies both, plus `main`).
//
// Memory is Go's GC. There is no reference counting, no reuse token, no
// leak check -- the three Zig passes that exist for those (Perceus, Reuse,
// RcCheck) have no counterpart here, and neither does the self-passing
// calling convention that let a callee release its own closure.
//
// What does carry over from the Zig runtime is the trampoline. Go, like
// Zig, does not guarantee tail calls, and this IR is CPS: every call is a
// tail call. Emitting them as native calls grows the stack by a frame per
// reduction step and never pops, which dies well before `debug.SetMaxStack`'s
// 1GB default. So a generated function never performs a call; it returns an
// Action naming the call it wants, and `run` dispatches in a loop.
//
// The semantic oracle is Malgo.Sequent.Eval. Any observable divergence from
// the interpreter is a bug here, not there.

// ===== Values =====
//
// Every concrete type stored in a Value is one machine word -- a pointer or
// a func -- so putting one into the interface never allocates. A two-word
// type like a bare `string` would allocate on every conversion, which is
// why Str is a struct behind a pointer rather than a defined string type.

type Value interface{ malgoValue() }

type Int32 struct{ V int32 }
type Int64 struct{ V int64 }
type Float struct{ V float32 }
type Double struct{ V float64 }
type Char struct{ V rune }

// Str caches what a codepoint-indexed operation would otherwise rescan.
// Both fields are scalars, so a Str still costs exactly one allocation --
// caching the decoded `[]rune` instead was measured at twice the total
// runtime, because the evaluator makes far more short-lived strings than it
// indexes into, and 4 bytes per character is more work than the scan it
// saves. See wiki/2026-09-12-go-backend-performance-investigation.md.
type Str struct {
	V string
	// Codepoint length, or -1 before the first scan.
	nRunes int
	ascii  bool
	// Most recently resolved codepoint index and its byte offset, so a walk
	// that moves forward -- which is what a lexer does -- resumes instead of
	// restarting. lastIdx is -1 before the first resolution.
	lastIdx int64
	lastOff int
}

// scan fills nRunes and ascii in one pass. The ASCII test is a byte loop
// that stops at the first non-ASCII byte, and the count is left to
// `utf8.RuneCountInString`, which has its own word-at-a-time fast path --
// ranging over the string to count runes here instead was measurably slower,
// since that decodes every character.
func (x *Str) scan() {
	s := x.V
	ascii := true
	for i := 0; i < len(s); i++ {
		if s[i] >= utf8.RuneSelf {
			ascii = false
			break
		}
	}
	x.ascii = ascii
	if ascii {
		x.nRunes = len(s)
	} else {
		x.nRunes = utf8.RuneCountInString(s)
	}
}

// byteOffset maps a codepoint index to a byte offset.
//
// The ASCII case is the identity. Otherwise this resumes from the last index
// resolved on this same string when the request is at or past it, which turns
// a left-to-right walk from quadratic into linear. A single non-ASCII byte
// anywhere -- one comment in the source being lexed is enough -- disables the
// ASCII path for the whole string, so the cursor is what actually carries
// this workload.
func (x *Str) byteOffset(index int64) int {
	if index < 0 {
		malgoPanic("string index out of range")
	}
	if x.isASCII() {
		if index > int64(len(x.V)) {
			malgoPanic("string index out of range")
		}
		return int(index)
	}
	i, off := int64(0), 0
	if x.lastIdx >= 0 && index >= x.lastIdx {
		i, off = x.lastIdx, x.lastOff
	}
	for i < index && off < len(x.V) {
		_, size := utf8.DecodeRuneInString(x.V[off:])
		off += size
		i++
	}
	if i != index {
		malgoPanic("string index out of range")
	}
	x.lastIdx, x.lastOff = index, off
	return off
}

func (x *Str) count() int {
	if x.nRunes < 0 {
		x.scan()
	}
	return x.nRunes
}

// isASCII reports whether byte offsets and codepoint indices coincide, which
// is what lets the indexing primitives skip their scan entirely. Malgo source
// text and the tokens the self-hosted lexer cuts out of it are almost all
// ASCII, so this is the common case rather than a lucky one.
func (x *Str) isASCII() bool {
	if x.nRunes < 0 {
		x.scan()
	}
	return x.ascii
}

// Struct covers both tagged constructors and tuples; Tag == tupleTag means
// a tuple. Unit is the empty tuple, matching Eval's `Value.struct .tuple []`.
type Struct struct {
	Tag    string
	Fields []Value
}

// NamedField is one call-by-name record field. Fields are kept in ascending
// Name order in a slice, never a map: Go randomizes map iteration order, so
// any traversal of a record's fields would make output nondeterministic.
type NamedField struct {
	Name string
	Code Fn
}

type Record struct{ Fields []NamedField }

// Fn is both a Malgo closure and a continuation. Unlike the Zig runtime
// there is no separate Closure object carrying a captures array: a Go
// closure captures its environment directly, and the GC keeps it alive.
type Fn func(a, b Value) Action

func (*Int32) malgoValue()  {}
func (*Int64) malgoValue()  {}
func (*Float) malgoValue()  {}
func (*Double) malgoValue() {}
func (*Char) malgoValue()   {}
func (*Str) malgoValue()    {}
func (*Struct) malgoValue() {}
func (*Record) malgoValue() {}
func (Fn) malgoValue()      {}

const tupleTag = "tuple"

// ===== Constructors =====

// Small int32s are interned. Every scalar is otherwise a separate heap
// object, so every arithmetic intermediate would allocate; the Zig runtime
// measured 4.57M allocations to compute `fib 5` through a Malgo-written
// interpreter before interning the same range.
const int32InternMin = -128
const int32InternMax = 1024

var int32Interned [int32InternMax - int32InternMin + 1]Int32

var theUnit = &Struct{Tag: tupleTag}

var gTotalAllocs uint64
var gDispatches uint64

func initRuntime() {
	for i := range int32Interned {
		int32Interned[i].V = int32(i + int32InternMin)
	}
}

func mkInt32(n int32) Value {
	if n >= int32InternMin && n <= int32InternMax {
		return &int32Interned[n-int32InternMin]
	}
	gTotalAllocs++
	return &Int32{V: n}
}

func mkInt64(n int64) Value    { gTotalAllocs++; return &Int64{V: n} }
func mkFloat(f float32) Value  { gTotalAllocs++; return &Float{V: f} }
func mkDouble(f float64) Value { gTotalAllocs++; return &Double{V: f} }
func mkChar(c rune) Value      { gTotalAllocs++; return &Char{V: c} }
func mkString(s string) Value  { gTotalAllocs++; return &Str{V: s, nRunes: -1, lastIdx: -1} }

func mkStruct(tag string, fields ...Value) Value {
	gTotalAllocs++
	return &Struct{Tag: tag, Fields: fields}
}

func mkRecord(fields []NamedField) Value {
	gTotalAllocs++
	return &Record{Fields: fields}
}

func unit() Value { return theUnit }

func boolValue(b bool) Value {
	if b {
		return mkInt32(1)
	}
	return mkInt32(0)
}

// ===== Accessors =====
//
// A failed assertion means the emitter produced a type-incorrect program,
// which is a compiler bug rather than a user error, so these abort.

func asI32(v Value) int32 {
	x, ok := v.(*Int32)
	if !ok {
		malgoPanic("expected Int32")
	}
	return x.V
}

func asI64(v Value) int64 {
	x, ok := v.(*Int64)
	if !ok {
		malgoPanic("expected Int64")
	}
	return x.V
}

func asF32(v Value) float32 {
	x, ok := v.(*Float)
	if !ok {
		malgoPanic("expected Float")
	}
	return x.V
}

func asF64(v Value) float64 {
	x, ok := v.(*Double)
	if !ok {
		malgoPanic("expected Double")
	}
	return x.V
}

func asChar(v Value) rune {
	x, ok := v.(*Char)
	if !ok {
		malgoPanic("expected Char")
	}
	return x.V
}

func asStr(v Value) string {
	return asStrObj(v).V
}

// asStrObj is asStr for the primitives that also want the cached fields.
func asStrObj(v Value) *Str {
	x, ok := v.(*Str)
	if !ok {
		malgoPanic("expected String")
	}
	return x
}

func asFn(v Value) Fn {
	x, ok := v.(Fn)
	if !ok {
		malgoPanic("expected function")
	}
	return x
}

// isZero backs the `ifz` terminator. Only int32 scrutinees reach it.
func isZero(v Value) bool {
	x, ok := v.(*Int32)
	return ok && x.V == 0
}

// ===== Trampoline =====

// Action names the call a generated function wants performed. Code == nil
// means finished, with the result in A. It is a plain value: five words,
// returned in registers, never heap-allocated.
//
// Two argument slots, because the front end cannot produce more: ToFun
// builds single-parameter lambdas and singleton applies, and ToCore appends
// exactly one consumer. A generated function knows its own arity, so the
// slot it does not use is simply nil -- no count travels with the Action.
//
// The slots are named parameters rather than a `[2]Value` array handed over
// as a slice. That costs nothing to build (no slice header per dispatch) and
// nothing to read (no bounds check per argument), which is worth about a
// quarter of the dispatch cost.
type Action struct {
	Code Fn
	A, B Value
}

func done(v Value) Action { return Action{A: v} }

func tail0(f Fn) Action { return Action{Code: f} }

func tail1(f Fn, a Value) Action { return Action{Code: f, A: a} }

func tail2(f Fn, a, b Value) Action { return Action{Code: f, A: a, B: b} }

// applyCo passes a value to a consumer. The consumer is a Value because
// continuations flow through the same positions as data.
func applyCo(k Value, v Value) Action { return tail1(asFn(k), v) }

// run dispatches until an Action says it is finished. Arguments are passed
// by value, so the callee cannot observe `cur` while building its result --
// which is what lets the loop assign straight back into `cur`.
func run(code Fn, a, b Value) Value {
	cur := Action{Code: code, A: a, B: b}
	for cur.Code != nil {
		cur = cur.Code(cur.A, cur.B)
		gDispatches++
	}
	return cur.A
}

// ===== Records =====
//
// Record fields are call-by-name: Eval re-runs the field's statement on
// every projection, so forcing twice legitimately does the work twice.

func fieldCode(v Value, name string) Fn {
	r, ok := v.(*Record)
	if !ok {
		malgoPanic("projection on a non-record")
	}
	for i := range r.Fields {
		if r.Fields[i].Name == name {
			return r.Fields[i].Code
		}
	}
	malgoPanic("no such field: " + name)
	return nil
}

// projectField is a terminator: the field's continuation is the caller's,
// so no nested `run` is needed.
func projectField(v Value, name string, k Value) Action {
	return tail1(fieldCode(v, name), k)
}

// forceField is the one place the native stack still grows, and only by
// the dynamic nesting depth of record forcing inside a pattern match --
// not by reduction steps. `Pattern.expand` binds several fields mid-block
// and needs each as a plain value, which a terminator cannot supply.
var gForceDepth int
var gForceDepthMax int

func forceField(v Value, name string) Value {
	gForceDepth++
	if gForceDepth > gForceDepthMax {
		gForceDepthMax = gForceDepth
	}
	result := run(fieldCode(v, name), identityKont, nil)
	gForceDepth--
	return result
}

var identityKont Fn = func(a, b Value) Action { return done(a) }

// ===== Panic and exit =====

func malgoPanic(msg string) {
	writeStderr("Malgo: " + msg + "\n")
	exitProcess(1)
}

// ===== Value printing (mirrors Eval.valueToText) =====

func valueToText(v Value) string {
	switch x := v.(type) {
	case *Int32:
		return strconv.FormatInt(int64(x.V), 10)
	case *Int64:
		return strconv.FormatInt(x.V, 10)
	case *Float:
		return formatHaskellFloat32(x.V)
	case *Double:
		return formatHaskellFloat64(x.V)
	case *Char:
		return string(x.V)
	case *Str:
		return x.V
	case *Struct:
		return structToText(x)
	case *Record:
		return "<record>"
	case Fn:
		return "<function>"
	}
	return "<unknown>"
}

func structToText(s *Struct) string {
	if s.Tag == tupleTag {
		if len(s.Fields) == 0 {
			return "{}"
		}
		return "{" + joinFields(s.Fields) + "}"
	}
	if len(s.Fields) == 0 {
		return s.Tag
	}
	return s.Tag + "(" + joinFields(s.Fields) + ")"
}

func joinFields(fields []Value) string {
	var b strings.Builder
	for i, f := range fields {
		if i != 0 {
			b.WriteString(", ")
		}
		b.WriteString(valueToText(f))
	}
	return b.String()
}

// formatHaskellFloat64 mirrors Haskell's `show :: RealFloat a => a -> String`:
// fixed-point for `x == 0 || 0.1 <= |x| < 1e7`, scientific otherwise, always
// with at least one digit on each side of the point. Go's 'g' format already
// gives the shortest round-tripping digits; only the notation choice and the
// mandatory fractional part need reproducing.
func formatHaskellFloat64(x float64) string {
	return formatHaskellFloat(x, 64)
}

func formatHaskellFloat32(x float32) string {
	return formatHaskellFloat(float64(x), 32)
}

func formatHaskellFloat(x float64, bitSize int) string {
	if math.IsNaN(x) {
		return "NaN"
	}
	if math.IsInf(x, 1) {
		return "Infinity"
	}
	if math.IsInf(x, -1) {
		return "-Infinity"
	}
	abs := math.Abs(x)
	if x == 0 || (abs >= 0.1 && abs < 1e7) {
		s := strconv.FormatFloat(x, 'f', -1, bitSize)
		if !strings.ContainsRune(s, '.') {
			s += ".0"
		}
		return s
	}
	s := strconv.FormatFloat(x, 'e', -1, bitSize)
	// Haskell writes `1.0e-2`, Go writes `1e-02`: force a fractional part
	// and strip the exponent's zero padding and explicit '+'.
	eIdx := strings.IndexByte(s, 'e')
	mantissa := s[:eIdx]
	exponent := s[eIdx+1:]
	if !strings.ContainsRune(mantissa, '.') {
		mantissa += ".0"
	}
	sign := ""
	if exponent[0] == '+' {
		exponent = exponent[1:]
	} else if exponent[0] == '-' {
		sign = "-"
		exponent = exponent[1:]
	}
	exponent = strings.TrimLeft(exponent, "0")
	if exponent == "" {
		exponent = "0"
	}
	return mantissa + "e" + sign + exponent
}

// ===== Unbuffered I/O =====
//
// Writes go straight to the fd, matching the Zig runtime, so stdout and
// stderr interleave in the order the program produced them and there is no
// flush ordering to get wrong. Stdin is read one byte at a time for the
// same reason: a buffered reader would consume input a spawned subprocess
// is entitled to.

func writeStdout(s string) {
	if len(s) == 0 {
		return
	}
	if _, err := os.Stdout.WriteString(s); err != nil {
		exitProcess(1)
	}
}

func writeStderr(s string) {
	if len(s) == 0 {
		return
	}
	os.Stderr.WriteString(s)
}

var stdinByteBuf [1]byte

func readStdinByte() (byte, bool) {
	n, err := os.Stdin.Read(stdinByteBuf[:])
	if n == 1 {
		return stdinByteBuf[0], true
	}
	_ = err
	return 0, false
}

func exitProcess(code int) {
	os.Exit(code)
}


// ===== Primitives =====
//
// One Go function per `foreign import` in runtime/malgo/Builtin.mlg, named
// identically, with the primitive's natural arity. Arity comes from the
// call site rather than a length check, so a miscount is a Go compile error
// in the generated program rather than a runtime surprise.

// --- Control ---

func malgo_panic(a0 Value) Value {
	malgoPanic(asStr(a0))
	return nil
}

func malgo_unsafe_cast(a0 Value) Value { return a0 }

// reuseHint is inserted by Malgo.Sequent.ReuseSpecialize for the Zig
// backend's reuse analysis. With a GC there is nothing to recycle, so it
// is the identity.
func reuseHint(a0 Value) Value { return a0 }

// --- Int32 arithmetic ---

func malgo_add_int32_t(a0, a1 Value) Value { return mkInt32(asI32(a0) + asI32(a1)) }
func malgo_sub_int32_t(a0, a1 Value) Value { return mkInt32(asI32(a0) - asI32(a1)) }
func malgo_mul_int32_t(a0, a1 Value) Value { return mkInt32(asI32(a0) * asI32(a1)) }

func malgo_div_int32_t(a0, a1 Value) Value {
	d := asI32(a1)
	if d == 0 {
		malgoPanic("divide by zero")
	}
	return mkInt32(asI32(a0) / d)
}

func malgo_mod_int32_t(a0, a1 Value) Value {
	d := asI32(a1)
	if d == 0 {
		malgoPanic("divide by zero")
	}
	return mkInt32(asI32(a0) % d)
}

func malgo_neg_int32_t(a0 Value) Value { return mkInt32(-asI32(a0)) }

// --- Int64 arithmetic ---

func malgo_add_int64_t(a0, a1 Value) Value { return mkInt64(asI64(a0) + asI64(a1)) }
func malgo_sub_int64_t(a0, a1 Value) Value { return mkInt64(asI64(a0) - asI64(a1)) }
func malgo_mul_int64_t(a0, a1 Value) Value { return mkInt64(asI64(a0) * asI64(a1)) }

func malgo_div_int64_t(a0, a1 Value) Value {
	d := asI64(a1)
	if d == 0 {
		malgoPanic("divide by zero")
	}
	return mkInt64(asI64(a0) / d)
}

func malgo_mod_int64_t(a0, a1 Value) Value {
	d := asI64(a1)
	if d == 0 {
		malgoPanic("divide by zero")
	}
	return mkInt64(asI64(a0) % d)
}

func malgo_neg_int64_t(a0 Value) Value { return mkInt64(-asI64(a0)) }

// --- Float / Double arithmetic ---

func malgo_add_float(a0, a1 Value) Value { return mkFloat(asF32(a0) + asF32(a1)) }
func malgo_sub_float(a0, a1 Value) Value { return mkFloat(asF32(a0) - asF32(a1)) }
func malgo_mul_float(a0, a1 Value) Value { return mkFloat(asF32(a0) * asF32(a1)) }
func malgo_div_float(a0, a1 Value) Value { return mkFloat(asF32(a0) / asF32(a1)) }
func malgo_neg_float(a0 Value) Value     { return mkFloat(-asF32(a0)) }

func malgo_add_double(a0, a1 Value) Value { return mkDouble(asF64(a0) + asF64(a1)) }
func malgo_sub_double(a0, a1 Value) Value { return mkDouble(asF64(a0) - asF64(a1)) }
func malgo_mul_double(a0, a1 Value) Value { return mkDouble(asF64(a0) * asF64(a1)) }
func malgo_div_double(a0, a1 Value) Value { return mkDouble(asF64(a0) / asF64(a1)) }
func malgo_neg_double(a0 Value) Value     { return mkDouble(-asF64(a0)) }

func sqrt(a0 Value) Value  { return mkDouble(math.Sqrt(asF64(a0))) }
func sqrtf(a0 Value) Value { return mkFloat(float32(math.Sqrt(float64(asF32(a0))))) }

// --- Comparisons. All return Int32 1/0, which `isTrue#` pattern-matches. ---

func malgo_eq_int32_t(a0, a1 Value) Value { return boolValue(asI32(a0) == asI32(a1)) }
func malgo_ne_int32_t(a0, a1 Value) Value { return boolValue(asI32(a0) != asI32(a1)) }
func malgo_lt_int32_t(a0, a1 Value) Value { return boolValue(asI32(a0) < asI32(a1)) }
func malgo_le_int32_t(a0, a1 Value) Value { return boolValue(asI32(a0) <= asI32(a1)) }
func malgo_gt_int32_t(a0, a1 Value) Value { return boolValue(asI32(a0) > asI32(a1)) }
func malgo_ge_int32_t(a0, a1 Value) Value { return boolValue(asI32(a0) >= asI32(a1)) }

func malgo_eq_int64_t(a0, a1 Value) Value { return boolValue(asI64(a0) == asI64(a1)) }
func malgo_ne_int64_t(a0, a1 Value) Value { return boolValue(asI64(a0) != asI64(a1)) }
func malgo_lt_int64_t(a0, a1 Value) Value { return boolValue(asI64(a0) < asI64(a1)) }
func malgo_le_int64_t(a0, a1 Value) Value { return boolValue(asI64(a0) <= asI64(a1)) }
func malgo_gt_int64_t(a0, a1 Value) Value { return boolValue(asI64(a0) > asI64(a1)) }
func malgo_ge_int64_t(a0, a1 Value) Value { return boolValue(asI64(a0) >= asI64(a1)) }

func malgo_eq_float(a0, a1 Value) Value { return boolValue(asF32(a0) == asF32(a1)) }
func malgo_ne_float(a0, a1 Value) Value { return boolValue(asF32(a0) != asF32(a1)) }
func malgo_lt_float(a0, a1 Value) Value { return boolValue(asF32(a0) < asF32(a1)) }
func malgo_le_float(a0, a1 Value) Value { return boolValue(asF32(a0) <= asF32(a1)) }
func malgo_gt_float(a0, a1 Value) Value { return boolValue(asF32(a0) > asF32(a1)) }
func malgo_ge_float(a0, a1 Value) Value { return boolValue(asF32(a0) >= asF32(a1)) }

func malgo_eq_double(a0, a1 Value) Value { return boolValue(asF64(a0) == asF64(a1)) }
func malgo_ne_double(a0, a1 Value) Value { return boolValue(asF64(a0) != asF64(a1)) }
func malgo_lt_double(a0, a1 Value) Value { return boolValue(asF64(a0) < asF64(a1)) }
func malgo_le_double(a0, a1 Value) Value { return boolValue(asF64(a0) <= asF64(a1)) }
func malgo_gt_double(a0, a1 Value) Value { return boolValue(asF64(a0) > asF64(a1)) }
func malgo_ge_double(a0, a1 Value) Value { return boolValue(asF64(a0) >= asF64(a1)) }

func malgo_eq_char(a0, a1 Value) Value { return boolValue(asChar(a0) == asChar(a1)) }
func malgo_ne_char(a0, a1 Value) Value { return boolValue(asChar(a0) != asChar(a1)) }
func malgo_lt_char(a0, a1 Value) Value { return boolValue(asChar(a0) < asChar(a1)) }
func malgo_le_char(a0, a1 Value) Value { return boolValue(asChar(a0) <= asChar(a1)) }
func malgo_gt_char(a0, a1 Value) Value { return boolValue(asChar(a0) > asChar(a1)) }
func malgo_ge_char(a0, a1 Value) Value { return boolValue(asChar(a0) >= asChar(a1)) }

func malgo_eq_string(a0, a1 Value) Value { return boolValue(asStr(a0) == asStr(a1)) }
func malgo_ne_string(a0, a1 Value) Value { return boolValue(asStr(a0) != asStr(a1)) }
func malgo_lt_string(a0, a1 Value) Value { return boolValue(asStr(a0) < asStr(a1)) }
func malgo_le_string(a0, a1 Value) Value { return boolValue(asStr(a0) <= asStr(a1)) }
func malgo_gt_string(a0, a1 Value) Value { return boolValue(asStr(a0) > asStr(a1)) }
func malgo_ge_string(a0, a1 Value) Value { return boolValue(asStr(a0) >= asStr(a1)) }

// --- Char ---
//
// The classification predicates are ASCII-only, matching Lean's
// `Char.isDigit`/`isLower`/`isUpper`/`isAlphanum` in the oracle. Go's
// `unicode` package would accept more.

func malgo_char_ord(a0 Value) Value { return mkInt32(int32(asChar(a0))) }

func malgo_int32_t_to_char(a0 Value) Value {
	n := asI32(a0)
	if n < 0 || n > 0x10FFFF || (n >= 0xD800 && n <= 0xDFFF) {
		malgoPanic("invalid Unicode codepoint")
	}
	return mkChar(rune(n))
}

func malgo_is_digit(a0 Value) Value {
	c := asChar(a0)
	return boolValue(c >= '0' && c <= '9')
}

func malgo_is_lower(a0 Value) Value {
	c := asChar(a0)
	return boolValue(c >= 'a' && c <= 'z')
}

func malgo_is_upper(a0 Value) Value {
	c := asChar(a0)
	return boolValue(c >= 'A' && c <= 'Z')
}

func malgo_is_alphanum(a0 Value) Value {
	c := asChar(a0)
	return boolValue((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
}

// --- String ---

func malgo_string_length(a0 Value) Value {
	return mkInt64(int64(asStrObj(a0).count()))
}

func malgo_string_at(a0, a1 Value) Value {
	x := asStrObj(a1)
	off := x.byteOffset(asI64(a0))
	if off >= len(x.V) {
		malgoPanic("string index out of range")
	}
	if x.ascii {
		return mkChar(rune(x.V[off]))
	}
	r, _ := utf8.DecodeRuneInString(x.V[off:])
	return mkChar(r)
}

func malgo_string_cons(a0, a1 Value) Value {
	return mkString(string(asChar(a0)) + asStr(a1))
}

func malgo_string_append(a0, a1 Value) Value {
	return mkString(asStr(a0) + asStr(a1))
}

func malgo_substring(a0, a1, a2 Value) Value {
	x := asStrObj(a0)
	start := asI64(a1)
	end := asI64(a2)
	if end < start {
		malgoPanic("substring: end before start")
	}
	from := x.byteOffset(start)
	to := x.byteOffset(end)
	return mkString(x.V[from:to])
}

func malgo_string_reverse(a0 Value) Value {
	runes := []rune(asStr(a0))
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return mkString(string(runes))
}

// parseIntLiteral mirrors Haskell's `reads` for integers, which the oracle
// requires to consume the whole string: optional leading whitespace, an
// optional '-' (never '+'), one or more digits, nothing left over. Go's
// ParseInt differs in both directions -- it takes '+' and '_' that `reads`
// rejects, and rejects the leading space `reads` skips -- so the accepted
// run is sliced out first.
func parseIntLiteral(s string, bitSize int) int64 {
	i := 0
	for i < len(s) && isReadsSpace(s[i]) {
		i++
	}
	digitsStart := i
	if i < len(s) && s[i] == '-' {
		i++
	}
	signEnd := i
	for i < len(s) && s[i] >= '0' && s[i] <= '9' {
		i++
	}
	if i == signEnd || i != len(s) {
		malgoPanic("malformed integer literal in string")
	}
	n, err := strconv.ParseInt(s[digitsStart:], 10, bitSize)
	if err != nil {
		malgoPanic("malformed integer literal in string")
	}
	return n
}

func isReadsSpace(c byte) bool {
	return c == ' ' || c == '\t' || c == '\n' || c == 11 || c == 12 || c == '\r'
}

func malgo_string_to_int32(a0 Value) Value {
	return mkInt32(int32(parseIntLiteral(asStr(a0), 32)))
}

func malgo_string_to_int64(a0 Value) Value {
	return mkInt64(parseIntLiteral(asStr(a0), 64))
}

// --- Conversions to string ---

func malgo_int32_t_to_string(a0 Value) Value {
	return mkString(strconv.FormatInt(int64(asI32(a0)), 10))
}

func malgo_int64_t_to_string(a0 Value) Value {
	return mkString(strconv.FormatInt(asI64(a0), 10))
}

func malgo_float_to_string(a0 Value) Value {
	return mkString(formatHaskellFloat32(asF32(a0)))
}

func malgo_double_to_string(a0 Value) Value {
	return mkString(formatHaskellFloat64(asF64(a0)))
}

func malgo_char_to_string(a0 Value) Value {
	return mkString(string(asChar(a0)))
}

// --- Output ---

func malgo_newline(a0 Value) Value {
	_ = a0
	writeStdout("\n")
	return unit()
}

func malgo_print_char(a0 Value) Value {
	writeStdout(string(asChar(a0)))
	return unit()
}

func malgo_print_string(a0 Value) Value {
	writeStdout(asStr(a0))
	return unit()
}

func malgo_print(a0 Value) Value {
	writeStdout(valueToText(a0))
	return unit()
}

func malgo_flush(a0 Value) Value {
	_ = a0
	return unit()
}

func malgo_stderr_string(a0 Value) Value {
	writeStderr(asStr(a0))
	return unit()
}

// --- Input ---

// malgo_get_char decodes one full Unicode scalar, which may span several
// bytes on stdin. EOF yields NUL, matching the Zig runtime.
func malgo_get_char(a0 Value) Value {
	_ = a0
	lead, ok := readStdinByte()
	if !ok {
		return mkChar(0)
	}
	seqLen := utf8SequenceLength(lead)
	if seqLen == 1 {
		return mkChar(rune(lead))
	}
	buf := make([]byte, 0, 4)
	buf = append(buf, lead)
	for i := 1; i < seqLen; i++ {
		b, ok := readStdinByte()
		if !ok {
			malgoPanic("malformed UTF-8 on stdin")
		}
		buf = append(buf, b)
	}
	r, size := utf8.DecodeRune(buf)
	if r == utf8.RuneError && size <= 1 {
		malgoPanic("malformed UTF-8 on stdin")
	}
	return mkChar(r)
}

func utf8SequenceLength(b byte) int {
	switch {
	case b < 0x80:
		return 1
	case b&0xE0 == 0xC0:
		return 2
	case b&0xF0 == 0xE0:
		return 3
	case b&0xF8 == 0xF0:
		return 4
	}
	return 1
}

func malgo_get_contents(a0 Value) Value {
	_ = a0
	var b strings.Builder
	for {
		c, ok := readStdinByte()
		if !ok {
			break
		}
		b.WriteByte(c)
	}
	return mkString(b.String())
}

func malgo_get_line(a0 Value) Value {
	_ = a0
	var b strings.Builder
	for {
		c, ok := readStdinByte()
		if !ok || c == '\n' {
			break
		}
		b.WriteByte(c)
	}
	return mkString(b.String())
}

// --- Files ---
//
// The oracle calls readFile with no error handling, so a missing file kills
// the process. Panicking matches that: both die nonzero with a message on
// stderr, and the golden gate compares stdout.

func malgo_read_file(a0 Value) Value {
	data, err := os.ReadFile(asStr(a0))
	if err != nil {
		malgoPanic("readFile: cannot open file")
	}
	return mkString(string(data))
}

func malgo_write_file(a0, a1 Value) Value {
	if err := os.WriteFile(asStr(a0), []byte(asStr(a1)), 0o644); err != nil {
		malgoPanic("writeFile: cannot open file")
	}
	return unit()
}

// --- Process and environment ---

var gArgv []string

func setArgv(args []string) { gArgv = args }

// malgo_get_args returns the arguments newline-separated, excluding the
// program's own path, matching System.Environment.getArgs.
func malgo_get_args(a0 Value) Value {
	_ = a0
	if len(gArgv) <= 1 {
		return mkString("")
	}
	return mkString(strings.Join(gArgv[1:], "\n"))
}

// See Builtin.mlg for why presence and value are two primitives: a variable
// set to the empty string must stay distinguishable from an unset one.
func malgo_has_env(a0 Value) Value {
	_, ok := os.LookupEnv(asStr(a0))
	return boolValue(ok)
}

func malgo_get_env(a0 Value) Value {
	v, ok := os.LookupEnv(asStr(a0))
	if !ok {
		return mkString("")
	}
	return mkString(v)
}

// malgo_run_process takes argv elements each followed by a NUL terminator
// (a trailing terminator per element, not a separator between them, so the
// empty list and a one-element list holding "" encode differently). No
// shell is involved: the argv array goes straight to the OS, as in the
// interpreter. Returns the boxed triple (Int32, String, String).
func malgo_run_process(a0, a1 Value) Value {
	cmd := asStr(a0)
	args := splitNulTerminated(asStr(a1))
	c := exec.Command(cmd, args...)
	var stdout, stderr strings.Builder
	c.Stdout = &stdout
	c.Stderr = &stderr
	exitCode := 0
	if err := c.Run(); err != nil {
		var ee *exec.ExitError
		if errors.As(err, &ee) {
			exitCode = ee.ExitCode()
		} else {
			exitCode = 1
		}
	}
	return mkStruct(tupleTag,
		mkStruct("Int32#", mkInt32(int32(exitCode))),
		mkStruct("String#", mkString(stdout.String())),
		mkStruct("String#", mkString(stderr.String())))
}

func splitNulTerminated(blob string) []string {
	var out []string
	start := 0
	for i := 0; i < len(blob); i++ {
		if blob[i] == 0 {
			out = append(out, blob[start:i])
			start = i + 1
		}
	}
	return out
}

// --- Exit ---

func malgo_exit_failure(a0 Value) Value {
	_ = a0
	exitProcess(1)
	return nil
}

func malgo_exit_success(a0 Value) Value {
	_ = a0
	exitProcess(0)
	return nil
}

func malgo_exit_with_code(a0 Value) Value {
	exitProcess(int(uint8(uint32(asI32(a0)))))
	return nil
}

// ===== Exit accounting =====
//
// Counters are always collected and only the reporting is env-gated, so an
// instrumented run and a timed run measure the same binary. `dispatches` is
// the deterministic, machine-independent reduction-step count the perf gate
// ratchets on; allocations are reported but not gated, since Go's own
// allocations are invisible here.

func reportStats() {
	if os.Getenv("MALGO_RC_STATS") == "" {
		return
	}
	writeStderr("MALGO-STATS: total_allocs=" + strconv.FormatUint(gTotalAllocs, 10) +
		" dispatches=" + strconv.FormatUint(gDispatches, 10) +
		" force_depth_max=" + strconv.Itoa(gForceDepthMax) + "\n")
}
