import Malgo.Module
import Malgo.Data.ShowFloat
import Malgo.Backend.Zig.Ir
import Malgo.Backend.Zig.Runtime

/-! Port of `src/Malgo/Backend/Zig/Emit.hs`: prints the backend `Ir.Program`
(produced by `ClosureConv.convertProgram`) as Zig source text.

Every `Func` becomes a Zig function with the uniform self-passing signature
`fn (self: rt.Value, args: []const rt.Value) rt.Action`: a closure/record field
receives the closure object itself as `self` and reads its captures out of it
(`ReadCapture` → `rt.capturesOf(self)`), while a top-level definition is called
directly with the `rt.no_self` sentinel and ignores it.

A generated function never performs a call: it returns an `rt.Action` naming the
call it wants, and the runtime's `rt.run` trampoline dispatches in a loop. Zig
does not guarantee tail-call optimization, so emitting these tail calls natively
grew the stack by one frame per reduction step (issue #360). The RC discipline is
unchanged — an Action carries exactly the references a direct call moved.

Zig errors on both an unused local `const` and a pointless `_ = x;` discard of a
used one, so the printer discards exactly the bindings (and `self`/`args`) that
the rest of the function never references — read off `Ir.suffixFreeVars` so the
walk stays linear.

Byte-parity with the Haskell printer is NOT required (Zig text is never
golden-compared, only the compiled binary's runtime behavior is); the
`rt.*` call names and shapes must match `runtime/zig/runtime.zig`'s public API. -/

namespace Malgo.Backend.Zig.Emit

open Malgo.Sequent.Fun (Name Literal Tag)
open Malgo.Backend.Zig.Ir

/-- Two-digit lowercase hex, for escaping control bytes in a Zig string. -/
private def toHex2 (k : Nat) : String :=
  let digits := "0123456789abcdef".toList
  String.ofList [digits[(k / 16) % 16]!, digits[k % 16]!]

private def zigEscapeChar (c : Char) : String :=
  if c == '\\' then "\\\\"
  else if c == '"' then "\\\""
  else if c == '\n' then "\\n"
  else if c == '\r' then "\\r"
  else if c == '\t' then "\\t"
  else if c == '\x00' then "\\x00"
  else if c.toNat < 0x20 then "\\x" ++ toHex2 c.toNat
  else String.singleton c

/-- Zig string literal, escaped. Non-ASCII bytes pass through raw (Zig source
is UTF-8 and accepts literal UTF-8 in string literals). -/
def zigStringLit (t : String) : String :=
  "\"" ++ String.join (t.toList.map zigEscapeChar) ++ "\""

/-- Mangle a Malgo `Id` into a Zig identifier using Zig's raw-identifier syntax
(`@"..."`), which accepts almost any string, sidestepping keyword collisions;
reuses `Id.toText` for a globally-unique canonical name. -/
def mangleId (ident : Name) : String :=
  let esc := fun (c : Char) =>
    if c == '\\' then "\\\\" else if c == '"' then "\\\"" else String.singleton c
  "@\"" ++ String.join ((Malgo.Id.toText ident).toList.map esc) ++ "\""

/-- A single value's symbolic name as a Zig string literal (as opposed to
`mangleId`, which renders it as a raw identifier). -/
def nameLit (x : Name) : String := zigStringLit (Malgo.Id.toText x)

def compileTag : Tag → String
  | .tuple => "rt.Tag{ .tuple = {} }"
  | .tag t => "rt.Tag{ .named = " ++ zigStringLit t ++ " }"

def compileLiteral : Literal → String
  | .int32 n => "rt.mkInt32(" ++ toString n.toInt ++ ")"
  | .int64 n => "rt.mkInt64(" ++ toString n.toInt ++ ")"
  | .float f => "rt.mkFloat(" ++ Malgo.haskellShowFloat32 f ++ ")"
  | .double d => "rt.mkDouble(" ++ Malgo.haskellShowFloat d ++ ")"
  | .char c => "rt.mkChar(" ++ toString c.toNat ++ ")"
  | .string s => "rt.mkString(" ++ zigStringLit s ++ ")"

def literalEqExpr (scrutinee : String) : Literal → String
  | .int32 n => scrutinee ++ ".kind == .int32 and " ++ scrutinee ++ ".payload.int32 == " ++ toString n.toInt
  | .int64 n => scrutinee ++ ".kind == .int64 and " ++ scrutinee ++ ".payload.int64 == " ++ toString n.toInt
  | .float f => scrutinee ++ ".kind == .float and " ++ scrutinee ++ ".payload.float == " ++ Malgo.haskellShowFloat32 f
  | .double d => scrutinee ++ ".kind == .double and " ++ scrutinee ++ ".payload.double == " ++ Malgo.haskellShowFloat d
  | .char c => scrutinee ++ ".kind == .char and " ++ scrutinee ++ ".payload.char == " ++ toString c.toNat
  | .string s => scrutinee ++ ".kind == .string and rt.stringEq(" ++ scrutinee ++ ".payload.string, " ++ zigStringLit s ++ ")"

def emitPath (pv : Name → String) : Ir.Path → String
  | .root v => pv v
  | .field p i => emitPath pv p ++ ".payload.strukt.fields[" ++ toString i ++ "]"

def emitTest (pv : Name → String) : Test → String
  | .kindIs p kindName => emitPath pv p ++ ".kind == ." ++ kindName
  | .tagEq p tag =>
    let s := emitPath pv p
    s ++ ".kind == .strukt and rt.tagEq(" ++ s ++ ".payload.strukt.tag, " ++ compileTag tag ++ ")"
  | .litEq p lit => literalEqExpr (emitPath pv p) lit

def emitGuard (pv : Name → String) : Guard → String
  | .and [] => "true"
  | .and tests => " and ".intercalate (tests.map (emitTest pv))
  | .isZero v => "rt.isZero(" ++ pv v ++ ")"

def valueSlice (pv : Name → String) (vs : List Name) : String :=
  "&[_]rt.Value{" ++ ", ".intercalate (vs.map pv) ++ "}"

/-- Mirrors `MAX_ARGS` in `runtime/zig/runtime.zig`, the fixed argument capacity
of an `rt.Action`. The runtime's own `rcInvariant` in `mkAction` is the
authoritative backstop if the two ever drift. -/
def maxCallArgs : Nat := 4

/-- `valueSlice` for a call site, rejecting an arity the runtime's Action cannot
carry. The front end tops out at 2 (`callClosure f [arg, kont]`), so exceeding
this is a compiler bug rather than a user error. -/
def callArgs (pv : Name → String) (what : String) (vs : List Name) : String :=
  if vs.length > maxCallArgs then
    panic! s!"Malgo.Backend.Zig.Emit: {what} call site has {vs.length} arguments, \
             exceeding MAX_ARGS ({maxCallArgs}) in runtime/zig/runtime.zig"
  else
    valueSlice pv vs

/-- A Zig string-literal slice of `vs`'s symbolic (compile-time) names, in the
same order as the matching `valueSlice` — passed alongside it to an `rt.*Named`
RC-tracing wrapper so a construction event's slots can be attributed back to
source names. -/
def nameSlice (vs : List Name) : String :=
  "&[_][]const u8{" ++ ", ".intercalate (vs.map nameLit) ++ "}"

/-- `funcName` is the already-quoted Zig string literal naming the enclosing
`Func`, threaded into every `rt.*Named` RC-tracing call in its body. -/
def emitExpr (pv : Name → String) (funcName : String) : Expr → String
  | .lit lit => compileLiteral lit
  | .mkStruct tag vs =>
    "rt.mkStructNamed(" ++ compileTag tag ++ ", " ++ valueSlice pv vs ++ ", " ++ nameSlice vs ++ ", " ++ funcName ++ ")"
  | .mkStructReuse tok tag vs =>
    "rt.mkStructReuseNamed(" ++ pv tok ++ ", " ++ compileTag tag ++ ", " ++ valueSlice pv vs ++ ", " ++ nameSlice vs ++ ", " ++ funcName ++ ")"
  | .mkClosure fn vs =>
    "rt.mkClosureNamed(&" ++ mangleId fn ++ ", " ++ valueSlice pv vs ++ ", " ++ nameSlice vs ++ ", " ++ funcName ++ ")"
  | .mkRecord fields vs =>
    "rt.mkRecordNamed(&[_]rt.NamedField{"
      ++ ", ".intercalate (fields.map (fun (fieldName, fn) =>
            ".{ .name = " ++ zigStringLit fieldName ++ ", .code = &" ++ mangleId fn ++ " }"))
      ++ "}, " ++ valueSlice pv vs ++ ", " ++ nameSlice vs ++ ", " ++ funcName ++ ")"
  | .prim name vs => "rt." ++ name ++ "(" ++ valueSlice pv vs ++ ")"
  | .readPath p => emitPath pv p
  | .readCapture self i => "rt.capturesOf(" ++ pv self ++ ")[" ++ toString i ++ "]"
  | .force v field =>
    "rt.forceField(" ++ pv v ++ ", " ++ zigStringLit field
      ++ ") orelse rt.panic(\"Expand: missing field " ++ field ++ "\")"
  | .panicExpr what => "rt.panicUnimplemented(" ++ zigStringLit what ++ ")"

/-- `_ = name;` when `isUsed` is false, else nothing — an unused binding must be
explicitly discarded or Zig errors. -/
def discardUnless (name : String) (isUsed : Bool) : String :=
  if isUsed then "" else "_ = " ++ name ++ ";\n"

/-- `const name = rhs;` followed by `discardUnless` for that same name. -/
def declareConst (name rhs : String) (isUsed : Bool) : String :=
  "const " ++ name ++ " = " ++ rhs ++ ";\n" ++ discardUnless name isUsed

mutual

partial def emitBlock (pv : Name → String) (funcName : String) : Block → String
  | .mk stmts term => emitStmts pv funcName (suffixFreeVars stmts term) stmts term

/-- `lives` is `suffixFreeVars stmts term`: `lives.head` is the free vars of the
current suffix, `lives[1]` (`liveAfter`) the free vars of everything after the
head statement — used to decide whether a binding is discarded. -/
partial def emitStmts (pv : Name → String) (funcName : String)
    (lives : List (Std.TreeSet Name)) : List Stmt → Terminator → String
  | [], term => emitTerminator pv funcName term
  -- `panicExpr` is noreturn: nothing after it is reachable, and Zig rejects
  -- unreachable statements, so printing truncates here.
  | .let _ (.panicExpr what) :: _, _ => "rt.panicUnimplemented(" ++ zigStringLit what ++ ");\n"
  | .let x e :: rest, term =>
    match lives with
    | _ :: liveAfter :: rest_lives =>
      declareConst (pv x) (emitExpr pv funcName e) (liveAfter.contains x)
        ++ emitStmts pv funcName (liveAfter :: rest_lives) rest term
    | _ => panic! "Malgo.Backend.Zig.Emit: suffixFreeVars shorter than stmts (invariant violation)"
  | .dup x :: rest, term =>
    "rt.dupNamed(" ++ pv x ++ ", " ++ nameLit x ++ ", " ++ funcName ++ ");\n"
      ++ emitStmts pv funcName lives.tail rest term
  | .drop x :: rest, term =>
    "rt.dropNamed(" ++ pv x ++ ", " ++ nameLit x ++ ", " ++ funcName ++ ");\n"
      ++ emitStmts pv funcName lives.tail rest term
  | .dropReuse tok x arity :: rest, term =>
    match lives with
    | _ :: liveAfter :: rest_lives =>
      declareConst (pv tok)
        ("rt.dropReuseNamed(" ++ pv x ++ ", " ++ toString arity ++ ", " ++ nameLit x ++ ", " ++ funcName ++ ")")
        (liveAfter.contains tok)
        ++ emitStmts pv funcName (liveAfter :: rest_lives) rest term
    | _ => panic! "Malgo.Backend.Zig.Emit: suffixFreeVars shorter than stmts (invariant violation)"

partial def emitTerminator (pv : Name → String) (funcName : String) : Terminator → String
  | .applyCo k v => "return rt.applyCovalue(" ++ pv k ++ ", " ++ pv v ++ ");\n"
  | .callClosure f args => "return rt.callClosure(" ++ pv f ++ ", " ++ callArgs pv "closure" args ++ ");\n"
  | .staticCall fn args => "return rt.staticCall(&" ++ mangleId fn ++ ", " ++ callArgs pv "static" args ++ ");\n"
  | .project v field k => "return rt.projectField(" ++ pv v ++ ", " ++ zigStringLit field ++ ", " ++ pv k ++ ");\n"
  | .«return» v => "return rt.done(" ++ pv v ++ ");\n"
  | .«if» guard t e =>
    "if (" ++ emitGuard pv guard ++ ") {\n"
      ++ emitBlock pv funcName t
      ++ "} else {\n"
      ++ emitBlock pv funcName e
      ++ "}\n"
  | .panic msg => "rt.panic(" ++ zigStringLit msg ++ ");\n"

end

def emitFunc (fn : Func) : String :=
  let pv := fun (nm : Name) => if nm == fn.selfVar then "self" else mangleId nm
  let funcNameLit := zigStringLit (Malgo.Id.toText fn.name)
  let bodyFree := freeVarsBlock fn.body
  let discardSelf := discardUnless "self" (bodyFree.contains fn.selfVar)
  let discardArgs := if fn.params.isEmpty then "_ = args;\n" else ""
  let paramBinds := String.join (fn.params.zipIdx.map (fun (p, i) =>
    declareConst (pv p) ("args[" ++ toString i ++ "]") (bodyFree.contains p)))
  "fn " ++ mangleId fn.name ++ "(self: rt.Value, args: []const rt.Value) rt.Action {\n"
    ++ discardSelf ++ discardArgs ++ paramBinds
    ++ emitBlock pv funcNameLit fn.body ++ "}"

/-- `T.unlines`: each line followed by a newline. -/
private def unlines (xs : List String) : String :=
  String.join (xs.map (· ++ "\n"))

/-- Print the whole program to Zig source. The Haskell pass runs under
`Reader ModuleName` only to name the generated-source comment; the port takes
the `ModuleName` directly and is otherwise pure. -/
def emitProgram (modName : ModuleName) (program : Program) : String :=
  let entryCall := match program.entry with
    | none => ""
    -- The Finish value comes back out of the trampoline here; dropping it is
    -- the last consumption the leak check relies on.
    | some name => "    rt.drop(rt.run(&" ++ mangleId name ++ ", rt.no_self, &[_]rt.Value{}));"
  unlines
    [ "// Generated by the Malgo Zig backend from module " ++ modName.toStr ++ ".",
      "// Memory: Perceus reference counting (dup/drop inserted by the compiler);",
      "// a leaked Value at exit prints MALGO-LEAK to stderr and exits with 83.",
      "const rt = struct {",
      zigRuntime,
      "};",
      "",
      "\n\n".intercalate (program.funcs.map emitFunc),
      "",
      "pub fn main(rt_init: rt.std.process.Init.Minimal) void {",
      "    rt.setArgv(rt_init.args.vector);",
      "    rt.initHeap();",
      entryCall,
      "    rt.flushStdout();",
      "    rt.exitWithLeakCheck();",
      "}" ]

/-! ## `mangleId` (port of Haskell `test/Malgo/Backend/ZigSpec.hs`)

Zig's raw-identifier syntax `@"..."` accepts almost any string, which is what
lets a Malgo `Id` become a Zig identifier without a keyword-substitution
table. These pin the three properties the Haskell spec pins. `Id.toText`
renders the same three sorts identically on both sides, so the expected
strings are the Haskell ones verbatim. -/

private def ext (s : String) : Name :=
  { name := s, moduleName := .moduleName "Test", sort := .external }

private def intern (s : String) (uniq : Nat) : Name :=
  { name := s, moduleName := .moduleName "Test", sort := .internal uniq }

-- Zig keywords survive the wrapping unscathed.
#guard mangleId (ext "error") == "@\"Test.error\""
#guard mangleId (ext "fn") == "@\"Test.fn\""
#guard mangleId (ext "test") == "@\"Test.test\""

-- Same surface name, different uniq: must not collide.
#guard mangleId (intern "x" 1) != mangleId (intern "x" 2)

-- Quotes and backslashes would otherwise close or escape the raw identifier.
#guard mangleId (ext "a\"b\\c") == "@\"Test.a\\\"b\\\\c\""

end Malgo.Backend.Zig.Emit
