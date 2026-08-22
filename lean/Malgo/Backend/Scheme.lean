import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.Sequent.Fun
import Malgo.Sequent.Core.Join

/-! Port of `src/Malgo/Backend/Scheme.hs`: the Scheme backend that lowers
the Join IR to Chez Scheme source text.

The Haskell pass runs under `Reader ModuleName` and uses no fresh names
(`State Uniq` is absent), so the whole port is a pure function
`compileToScheme : ModuleName → Join.Program → String` (`Text → String`).

`mangleId`/`mangleText`/`escapeString`/`escapeChar` and the embedded
`schemeRuntime` prelude are ported byte-for-byte: mangling fixes the
generated symbol names, and the runtime string is fed to Chez Scheme by
the self-host gate.

One deviation: Haskell's `mangleChar` fall-through uses `Data.Char.isAlphaNum`
(Unicode-aware), whereas Lean's `Char.isAlphanum` is ASCII-only. The Greek
letters that actually occur in Malgo identifiers are handled by explicit
cases before the fall-through, and any remaining non-ASCII alphanumeric is
mangled to `_u<codepoint>_` deterministically, so generated programs stay
self-consistent (the self-host gate diffs program output, not source
text). -/

namespace Malgo.Backend.Scheme

open Malgo.Sequent.Fun (Name Literal Tag Pattern)
open Malgo.Sequent.Core

/-- ASCII punctuation Chez Scheme's reader accepts directly as a symbol
constituent (embedded or trailing) -- verified against Chez 10.4.1 with
each character mid- and end-of-symbol (`Foo.bar`, `Foo-`, `Foo?`, etc., all
parse as plain symbols). Only `#` and `'` are genuinely unsafe (`#` is a
reader-syntax prefix -- "invalid sharp-sign prefix" if it appears bare
mid-symbol; `'` is the quote shorthand), so those two are the only ASCII
punctuation `mangleChar` still escapes below. -/
def isSafeSchemeSymbolChar (c : Char) : Bool :=
  c == '.' || c == '-' || c == '?' || c == '!' || c == '/' ||
  c == '+' || c == '*' || c == '<' || c == '>' || c == '='

/-- Mangle a single character to a valid-Scheme-identifier fragment. -/
def mangleChar (c : Char) : String :=
  match c with
  | '#' => "_hash_"
  | '\'' => "_prime_"
  -- Greek letters that occur in Malgo identifiers: Chez's reader accepts
  -- them directly as symbol constituents (verified), so they pass through
  -- unmangled. Lean's `Char.isAlphanum` is ASCII-only and would otherwise
  -- route these into the numeric fallback below, unlike the original
  -- Haskell backend's Unicode-aware `isAlphaNum`, which already treated
  -- them as safe -- this restores that parity instead of introducing a
  -- second, ASCII-only rendering for the same identifiers.
  | 'α' | 'β' | 'γ' | 'δ' | 'λ' | 'μ' => String.singleton c
  | c =>
    if c.isAlphanum || c == '_' || isSafeSchemeSymbolChar c
    then String.singleton c
    else "_u" ++ toString c.toNat ++ "_"

/-- Mangle a text string to be a valid Scheme identifier. -/
def mangleText (s : String) : String :=
  String.join (s.toList.map mangleChar)

/-- Mangle a Malgo `Id` into a valid Scheme identifier. -/
def mangleId (x : Malgo.Id) : String :=
  match x.sort with
  | .external => mangleText (x.moduleName.toStr ++ "." ++ x.name)
  | .internal uniq => mangleText x.name ++ "_" ++ toString uniq
  | .temporal uniq => "_t_" ++ mangleText x.name ++ "_" ++ toString uniq

def escapeChar (c : Char) : String :=
  match c with
  | ' ' => "space"
  | '\n' => "newline"
  | '\t' => "tab"
  | '\r' => "return"
  | '\x00' => "nul"
  | '\x7f' => "delete"
  | c => String.singleton c

/-- Escape special characters in a string for Scheme output. -/
def escapeString (s : String) : String :=
  String.join <| s.toList.map fun c =>
    match c with
    | '\\' => "\\\\"
    | '"' => "\\\""
    | '\n' => "\\n"
    | '\r' => "\\r"
    | '\t' => "\\t"
    | c => String.singleton c

def compileTag : Tag → String
  | .tuple => "tuple"
  | .tag t => mangleText t

/-- Compile a literal to a Scheme expression. Floats render via
`haskellShowFloat`/`haskellShowFloat32` (the Haskell backend emits `show`'s
output; Chez reads both fixed and `e`-notation). -/
def compileLiteral : Literal → String
  | .int32 n => toString n.toInt
  | .int64 n => toString n.toInt
  | .float f => Malgo.haskellShowFloat32 f
  | .double d => Malgo.haskellShowFloat d
  | .char c => "#\\" ++ escapeChar c
  | .string t => "\"" ++ escapeString t ++ "\""

/-- Concatenate arguments with spaces, prepending a space if non-empty. -/
def concatArgs : List String → String
  | [] => ""
  | xs => " " ++ " ".intercalate xs

private def binop (op : String) (args : List String) (r : String) : String :=
  match args with
  | [a, b] => "(" ++ r ++ " (" ++ op ++ " " ++ a ++ " " ++ b ++ "))"
  | _ => "(" ++ r ++ " (error 'prim \"" ++ op ++ ": wrong number of arguments\"))"

private def unaryop (op : String) (args : List String) (r : String) : String :=
  match args with
  | [a] => "(" ++ r ++ " (" ++ op ++ " " ++ a ++ "))"
  | _ => "(" ++ r ++ " (error 'prim \"" ++ op ++ ": wrong number of arguments\"))"

/-- Comparison returning 1/0 instead of `#t`/`#f`. -/
private def cmpop (op : String) (args : List String) (r : String) : String :=
  match args with
  | [a, b] => "(" ++ r ++ " (if (" ++ op ++ " " ++ a ++ " " ++ b ++ ") 1 0))"
  | _ => "(" ++ r ++ " (error 'prim \"" ++ op ++ ": wrong number of arguments\"))"

/-- Negated comparison returning 1/0. -/
private def cmpopNeg (op : String) (args : List String) (r : String) : String :=
  match args with
  | [a, b] => "(" ++ r ++ " (if (not (" ++ op ++ " " ++ a ++ " " ++ b ++ ")) 1 0))"
  | _ => "(" ++ r ++ " (error 'prim \"not-" ++ op ++ ": wrong number of arguments\"))"

/-- Unary predicate returning 1/0. -/
private def cmpBool (op : String) (args : List String) (r : String) : String :=
  match args with
  | [a] => "(" ++ r ++ " (if (" ++ op ++ " " ++ a ++ ") 1 0))"
  | _ => "(" ++ r ++ " (error 'prim \"" ++ op ++ ": wrong number of arguments\"))"

/-- Compile a primitive operation. -/
def compilePrimitive (name : String) (args : List String) (ret : String) : String :=
  match name with
  | "add_i32" => binop "+" args ret
  | "sub_i32" => binop "-" args ret
  | "mul_i32" => binop "*" args ret
  | "div_i32" => binop "quotient" args ret
  | "mod_i32" => binop "modulo" args ret
  | "add_i64" => binop "+" args ret
  | "sub_i64" => binop "-" args ret
  | "mul_i64" => binop "*" args ret
  | "div_i64" => binop "quotient" args ret
  | "mod_i64" => binop "modulo" args ret
  | "add_f64" => binop "+" args ret
  | "sub_f64" => binop "-" args ret
  | "mul_f64" => binop "*" args ret
  | "div_f64" => binop "/" args ret
  | "eq_i32" => binop "equal?" args ret
  | "ne_i32" => binop "malgo-ne" args ret
  | "lt_i32" => binop "<" args ret
  | "le_i32" => binop "<=" args ret
  | "gt_i32" => binop ">" args ret
  | "ge_i32" => binop ">=" args ret
  | "eq_i64" => binop "equal?" args ret
  | "ne_i64" => binop "malgo-ne" args ret
  | "lt_i64" => binop "<" args ret
  | "le_i64" => binop "<=" args ret
  | "gt_i64" => binop ">" args ret
  | "ge_i64" => binop ">=" args ret
  | "eq_f64" => binop "equal?" args ret
  | "ne_f64" => binop "malgo-ne" args ret
  | "lt_f64" => binop "<" args ret
  | "le_f64" => binop "<=" args ret
  | "gt_f64" => binop ">" args ret
  | "ge_f64" => binop ">=" args ret
  | "eq_char" => binop "char=?" args ret
  | "ne_char" => binop "malgo-ne" args ret
  | "string_append" => binop "string-append" args ret
  | "int32_to_string" => unaryop "number->string" args ret
  | "int64_to_string" => unaryop "number->string" args ret
  | "float_to_string" => unaryop "number->string" args ret
  | "double_to_string" => unaryop "number->string" args ret
  | "char_to_string" => unaryop "string" args ret
  | "string_to_int32" => unaryop "string->number" args ret
  | "string_to_int64" => unaryop "string->number" args ret
  | "string_length" => unaryop "string-length" args ret
  | "string_at" => binop "string-ref" args ret
  | "string_substring" =>
    match args with
    | [s, start, len] => "(" ++ ret ++ " (substring " ++ s ++ " " ++ start ++ " (+ " ++ start ++ " " ++ len ++ ")))"
    | _ => "(error 'prim \"string_substring: wrong number of arguments\")"
  | "putchar" =>
    match args with
    | [c] => "(begin (display " ++ c ++ ") (" ++ ret ++ " '()))"
    | _ => "(error 'prim \"putchar: wrong number of arguments\")"
  | "getchar" =>
    "(" ++ ret ++ " (let ((c (read-char))) (if (eof-object? c) #f c)))"
  | "putstr" =>
    match args with
    | [s] => "(begin (display " ++ s ++ ") (" ++ ret ++ " '()))"
    | _ => "(error 'prim \"putstr: wrong number of arguments\")"
  | "println" =>
    match args with
    | [s] => "(begin (display " ++ s ++ ") (newline) (" ++ ret ++ " '()))"
    | _ => "(error 'prim \"println: wrong number of arguments\")"
  | "error" =>
    match args with
    | [msg] => "(error 'malgo " ++ msg ++ ")"
    | _ => "(error 'malgo \"error\")"
  | "negate_i32" => unaryop "-" args ret
  | "negate_i64" => unaryop "-" args ret
  | "negate_f64" => unaryop "-" args ret
  -- malgo_* foreign import names
  | "malgo_read_file" =>
    match args with
    | [path] => "(" ++ ret ++ " (call-with-input-file " ++ path ++ " (lambda (p) (let ((s (get-string-all p))) (if (eof-object? s) \"\" s)))))"
    | _ => "(error 'prim \"malgo_read_file: wrong number of arguments\")"
  | "malgo_write_file" =>
    match args with
    -- `call-with-output-file` opens in Chez's default exclusive-create
    -- mode, erroring "file exists" the moment the target already has
    -- content -- unlike the interpreter's IO.FS.writeFile, which always
    -- overwrites. `'replace` matches that: creates if absent, truncates
    -- if present.
    | [path, content] => "(begin (call-with-port (open-output-file " ++ path ++ " 'replace) (lambda (p) (put-string p " ++ content ++ "))) (" ++ ret ++ " '()))"
    | _ => "(error 'prim \"malgo_write_file: wrong number of arguments\")"
  | "malgo_get_line" =>
    "(" ++ ret ++ " (let ((line (read-line))) (if (eof-object? line) \"\" line)))"
  | "malgo_get_args" =>
    "(" ++ ret ++ " (malgo-string-join (cdr (command-line)) \"\\n\"))"
  | "malgo_exit_success" => "(exit 0)"
  | "malgo_exit_with_code" =>
    match args with
    | [code] => "(exit " ++ code ++ ")"
    | _ => "(error 'prim \"malgo_exit_with_code: wrong number of arguments\")"
  | "malgo_stderr_string" =>
    match args with
    | [s] => "(begin (put-string (current-error-port) " ++ s ++ ") (" ++ ret ++ " '()))"
    | _ => "(error 'prim \"malgo_stderr_string: wrong number of arguments\")"
  -- See Builtin.mlg's `malgo_has_env`/`malgo_get_env` for why this is two
  -- primitives rather than one that treats "" as absent.
  | "malgo_has_env" =>
    match args with
    | [name] => "(" ++ ret ++ " (if (getenv " ++ name ++ ") 1 0))"
    | _ => "(error 'prim \"malgo_has_env: wrong number of arguments\")"
  | "malgo_get_env" =>
    match args with
    | [name] => "(" ++ ret ++ " (or (getenv " ++ name ++ ") \"\"))"
    | _ => "(error 'prim \"malgo_get_env: wrong number of arguments\")"
  | "malgo_run_process" =>
    match args with
    | [cmd, argsBlob] => "(" ++ ret ++ " (malgo-run-process " ++ cmd ++ " " ++ argsBlob ++ "))"
    | _ => "(error 'prim \"malgo_run_process: wrong number of arguments\")"
  | "malgo_string_to_int32" => unaryop "string->number" args ret
  | "malgo_string_to_int64" => unaryop "string->number" args ret
  -- Arithmetic (malgo_* foreign import names from Builtin.mlg)
  | "malgo_add_int32_t" => binop "+" args ret
  | "malgo_sub_int32_t" => binop "-" args ret
  | "malgo_mul_int32_t" => binop "*" args ret
  | "malgo_div_int32_t" => binop "quotient" args ret
  | "malgo_mod_int32_t" => binop "modulo" args ret
  | "malgo_neg_int32_t" => unaryop "-" args ret
  | "malgo_add_int64_t" => binop "+" args ret
  | "malgo_sub_int64_t" => binop "-" args ret
  | "malgo_mul_int64_t" => binop "*" args ret
  | "malgo_div_int64_t" => binop "quotient" args ret
  | "malgo_mod_int64_t" => binop "modulo" args ret
  | "malgo_neg_int64_t" => unaryop "-" args ret
  | "malgo_add_float" => binop "+" args ret
  | "malgo_sub_float" => binop "-" args ret
  | "malgo_mul_float" => binop "*" args ret
  | "malgo_div_float" => binop "/" args ret
  | "malgo_neg_float" => unaryop "-" args ret
  | "malgo_add_double" => binop "+" args ret
  | "malgo_sub_double" => binop "-" args ret
  | "malgo_mul_double" => binop "*" args ret
  | "malgo_div_double" => binop "/" args ret
  | "malgo_neg_double" => unaryop "-" args ret
  -- Comparisons return Int32# (1=true, 0=false) to match isTrue# pattern matching
  | "malgo_eq_int32_t" => cmpop "equal?" args ret
  | "malgo_ne_int32_t" => cmpopNeg "equal?" args ret
  | "malgo_lt_int32_t" => cmpop "<" args ret
  | "malgo_le_int32_t" => cmpop "<=" args ret
  | "malgo_gt_int32_t" => cmpop ">" args ret
  | "malgo_ge_int32_t" => cmpop ">=" args ret
  | "malgo_eq_int64_t" => cmpop "equal?" args ret
  | "malgo_ne_int64_t" => cmpopNeg "equal?" args ret
  | "malgo_lt_int64_t" => cmpop "<" args ret
  | "malgo_le_int64_t" => cmpop "<=" args ret
  | "malgo_gt_int64_t" => cmpop ">" args ret
  | "malgo_ge_int64_t" => cmpop ">=" args ret
  | "malgo_eq_float" => cmpop "equal?" args ret
  | "malgo_ne_float" => cmpopNeg "equal?" args ret
  | "malgo_lt_float" => cmpop "<" args ret
  | "malgo_le_float" => cmpop "<=" args ret
  | "malgo_gt_float" => cmpop ">" args ret
  | "malgo_ge_float" => cmpop ">=" args ret
  | "malgo_eq_double" => cmpop "equal?" args ret
  | "malgo_ne_double" => cmpopNeg "equal?" args ret
  | "malgo_lt_double" => cmpop "<" args ret
  | "malgo_le_double" => cmpop "<=" args ret
  | "malgo_gt_double" => cmpop ">" args ret
  | "malgo_ge_double" => cmpop ">=" args ret
  | "malgo_eq_char" => cmpop "char=?" args ret
  | "malgo_ne_char" => cmpopNeg "char=?" args ret
  | "malgo_lt_char" => cmpop "char<?" args ret
  | "malgo_le_char" => cmpop "char<=?" args ret
  | "malgo_gt_char" => cmpop "char>?" args ret
  | "malgo_ge_char" => cmpop "char>=?" args ret
  | "malgo_eq_string" => cmpop "string=?" args ret
  | "malgo_ne_string" => cmpopNeg "string=?" args ret
  | "malgo_lt_string" => cmpop "string<?" args ret
  | "malgo_le_string" => cmpop "string<=?" args ret
  | "malgo_gt_string" => cmpop "string>?" args ret
  | "malgo_ge_string" => cmpop "string>=?" args ret
  -- Char/string operations
  | "malgo_char_ord" => unaryop "char->integer" args ret
  | "malgo_int32_t_to_char" => unaryop "integer->char" args ret
  | "malgo_char_to_string" => unaryop "string" args ret
  | "malgo_is_digit" => cmpBool "char-numeric?" args ret
  | "malgo_is_lower" => cmpBool "char-lower-case?" args ret
  | "malgo_is_upper" => cmpBool "char-upper-case?" args ret
  | "malgo_is_alphanum" =>
    match args with
    | [c] => "(" ++ ret ++ " (if (or (char-alphabetic? " ++ c ++ ") (char-numeric? " ++ c ++ ")) 1 0))"
    | _ => "(error 'prim \"malgo_is_alphanum: wrong number of arguments\")"
  | "malgo_string_append" => binop "string-append" args ret
  | "malgo_string_length" => unaryop "string-length" args ret
  | "malgo_string_at" =>
    match args with
    | [i, s] => "(" ++ ret ++ " (string-ref " ++ s ++ " " ++ i ++ "))"
    | _ => "(error 'prim \"malgo_string_at: wrong number of arguments\")"
  | "malgo_string_cons" =>
    match args with
    | [c, s] => "(" ++ ret ++ " (string-append (string " ++ c ++ ") " ++ s ++ "))"
    | _ => "(error 'prim \"malgo_string_cons: wrong number of arguments\")"
  | "malgo_substring" =>
    match args with
    | [s, start, end_] => "(" ++ ret ++ " (substring " ++ s ++ " " ++ start ++ " " ++ end_ ++ "))"
    | _ => "(error 'prim \"malgo_substring: wrong number of arguments\")"
  | "malgo_string_reverse" =>
    match args with
    | [s] => "(" ++ ret ++ " (list->string (reverse (string->list " ++ s ++ "))))"
    | _ => "(error 'prim \"malgo_string_reverse: wrong number of arguments\")"
  -- Conversion
  | "malgo_int32_t_to_string" => unaryop "number->string" args ret
  | "malgo_int64_t_to_string" => unaryop "number->string" args ret
  | "malgo_float_to_string" => unaryop "number->string" args ret
  | "malgo_double_to_string" => unaryop "number->string" args ret
  -- IO
  | "malgo_print_string" =>
    match args with
    | [s] => "(begin (display " ++ s ++ ") (" ++ ret ++ " (list 'tuple)))"
    | _ => "(error 'prim \"malgo_print_string: wrong number of arguments\")"
  | "malgo_print_char" =>
    match args with
    | [c] => "(begin (display " ++ c ++ ") (" ++ ret ++ " (list 'tuple)))"
    | _ => "(error 'prim \"malgo_print_char: wrong number of arguments\")"
  | "malgo_print" =>
    match args with
    | [v] => "(begin (malgo-print-value " ++ v ++ ") (" ++ ret ++ " (list 'tuple)))"
    | _ => "(error 'prim \"malgo_print: wrong number of arguments\")"
  | "malgo_newline" => "(begin (newline) (" ++ ret ++ " (list 'tuple)))"
  | "malgo_flush" => "(begin (flush-output-port) (" ++ ret ++ " (list 'tuple)))"
  | "malgo_get_char" =>
    "(" ++ ret ++ " (let ((c (read-char))) (if (eof-object? c) #\\nul c)))"
  | "malgo_get_contents" =>
    "(" ++ ret ++ " (let ((s (get-string-all (current-input-port)))) (if (eof-object? s) \"\" s)))"
  -- Error / control
  | "malgo_panic" =>
    match args with
    | [msg] => "(error 'panic " ++ msg ++ ")"
    | _ => "(error 'panic \"panic\")"
  | "malgo_unsafe_cast" =>
    match args with
    | [x] => "(" ++ ret ++ " " ++ x ++ ")"
    | _ => "(error 'prim \"malgo_unsafe_cast: wrong number of arguments\")"
  | "malgo_exit_failure" => "(exit 1)"
  -- Math
  | "sqrt" => unaryop "sqrt" args ret
  | "sqrtf" => unaryop "sqrt" args ret
  -- Inserted by Malgo.Sequent.ReuseSpecialize for the Zig backend's
  -- reference-counting reuse analysis; a no-op everywhere else.
  | "reuseHint" =>
    match args with
    | [x] => "(" ++ ret ++ " " ++ x ++ ")"
    | _ => "(error 'prim \"reuseHint: wrong number of arguments\")"
  | _ => "(error 'prim \"unknown primitive: " ++ name ++ "\")"

/-- Compile a pattern against a scrutinee expression, returning
`(guard, let*-bindings)`: a Scheme boolean guard and sequential `let*`
entries. -/
partial def compilePattern (scrutinee : String) : Pattern → String × List (String × String)
  | .pvar _ n => ("#t", [(mangleId n, scrutinee)])
  | .pliteral _ lit => ("(equal? " ++ scrutinee ++ " " ++ compileLiteral lit ++ ")", [])
  | .destruct _ tag pats =>
    let tagStr := compileTag tag
    let tagCheck := "(and (pair? " ++ scrutinee ++ ") (eq? (car " ++ scrutinee ++ ") '" ++ tagStr ++ "))"
    let subResults := pats.mapIdx fun i p =>
      compilePattern ("(list-ref " ++ scrutinee ++ " " ++ toString (i + 1) ++ ")") p
    let subGuards := (subResults.map (·.1)).filter (· != "#t")
    let subBindings := (subResults.map (·.2)).flatten
    let fullGuard := match subGuards with
      | [] => tagCheck
      | gs => "(and " ++ " ".intercalate (tagCheck :: gs) ++ ")"
    (fullGuard, subBindings)
  | .expand _ fieldPats =>
    let subResults := (sortAssocAscending fieldPats).map fun (fname, p) =>
      let mangledFname := mangleText fname
      let raw := "(cdr (assq '" ++ mangledFname ++ " " ++ scrutinee ++ "))"
      let tmpName := "%fv_" ++ mangledFname
      let tmpName2 := "%fvr_" ++ mangledFname
      -- Object fields are stored as thunks; force them before pattern matching
      let forced :=
        "(let ((" ++ tmpName ++ " " ++ raw ++ ")) " ++
        "(if (procedure? " ++ tmpName ++ ") " ++
        "(" ++ tmpName ++ " (lambda (" ++ tmpName2 ++ ") " ++ tmpName2 ++ ")) " ++
        tmpName ++ "))"
      compilePattern forced p
    let subGuards := (subResults.map (·.1)).filter (· != "#t")
    let subBindings := (subResults.map (·.2)).flatten
    let fullGuard := match subGuards with
      | [] => "#t"
      | [g] => g
      | gs => "(and " ++ " ".intercalate gs ++ ")"
    (fullGuard, subBindings)

mutual

/-- Compile a Producer to a Scheme expression. -/
partial def compileProducer : Join.Producer → String
  | .var _ name => mangleId name
  | .literal _ lit => compileLiteral lit
  | .construct _ tag producers returns =>
    let tagStr := compileTag tag
    let prodStrs := producers.map compileProducer
    let retStrs := returns.map mangleId
    "(list '" ++ tagStr ++ concatArgs prodStrs ++ concatArgs retStrs ++ ")"
  | .lambda _ names body =>
    let nameStrs := names.map mangleId
    let bodyStr := compileStatement body
    match nameStrs with
    | [] => "(lambda () " ++ bodyStr ++ ")"
    | [x] => "(lambda (" ++ x ++ ") " ++ bodyStr ++ ")"
    | _ => "(lambda (" ++ " ".intercalate nameStrs ++ ") " ++ bodyStr ++ ")"
  | .mu .. =>
    -- `Producer.mu` should only ever appear directly as the producer of a
    -- `Statement.cut`, handled by `compileStatement`'s dedicated `.cut (.mu
    -- ..) ..` case above (which substitutes, matching `cut (mu a. c) b ==
    -- c[a := b]`) -- never reached as a plain producer. Enforced by
    -- `IrInvariants` in `Test/Main.lean` (checked over every golden case's
    -- Flat IR, which this pass's `Join.lean` input preserves unchanged) and
    -- mirrored by the Zig backend's `ClosureConv.lean`, which raises an
    -- error for the same "should be impossible" case -- `compileProducer`
    -- is pure and can't raise, so `panic!` is this codebase's convention
    -- for the equivalent situation in a pure function (see e.g.
    -- `Zig/Peephole.lean:152,159`). Returning some plausible-looking string
    -- here instead would be silently wrong, not caught, if that invariant
    -- is ever broken by a future pass change.
    panic! "Malgo.Backend.Scheme: Mu in producer position should have been eliminated (see IrInvariants)"
  | .object _ fields =>
    let fieldStrs := (sortAssocAscending fields).map compileField
    "(list " ++ " ".intercalate fieldStrs ++ ")"

partial def compileField : String × Name × Join.Statement → String
  | (fieldName, retName, body) =>
    let retStr := mangleId retName
    let bodyStr := compileStatement body
    "(cons '" ++ mangleText fieldName ++ " (lambda (" ++ retStr ++ ") " ++ bodyStr ++ "))"

/-- Compile a Consumer to a Scheme expression. -/
partial def compileConsumer : Join.Consumer → String
  | .label _ name => mangleId name
  | .apply _ producers returns =>
    let prodStrs := producers.map compileProducer
    let retStrs := returns.map mangleId
    "(lambda (%fn) (%fn" ++ concatArgs (prodStrs ++ retStrs) ++ "))"
  | .project _ field returnName =>
    let retStr := mangleId returnName
    "(lambda (%rec) (let ((%v (cdr (assq '" ++ mangleText field ++ " %rec)))) (if (procedure? %v) (%v " ++ retStr ++ ") (" ++ retStr ++ " %v))))"
  | .«then» _ name body =>
    let nameStr := mangleId name
    let bodyStr := compileStatement body
    "(lambda (" ++ nameStr ++ ") " ++ bodyStr ++ ")"
  | .finish _ => "malgo-finish"
  | .select _ branches =>
    let branchStrs := branches.map compileBranch
    "(lambda (%v) (cond " ++ " ".intercalate branchStrs ++ " (else (error 'select \"no matching branch\" %v))))"

partial def compileBranch : Join.Branch → String
  | .branch _ pat body =>
    let bodyStr := compileStatement body
    let (guard, bindings) := compilePattern "%v" pat
    let withBindings :=
      if bindings.isEmpty then bodyStr
      else "(let* (" ++ " ".intercalate (bindings.map fun (n, e) => "(" ++ n ++ " " ++ e ++ ")") ++ ") " ++ bodyStr ++ ")"
    "(" ++ guard ++ " " ++ withBindings ++ ")"

/-- Compile a Statement to a Scheme expression. -/
partial def compileStatement : Join.Statement → String
  | .cut (.mu _ name stmt) returnName =>
    -- Mu-reduction (`cut (mu a. c) b == c[a := b]`): `a` is a consumer
    -- variable standing for "whatever this computation is cut against", not
    -- a plain value, so it must be bound to the ambient consumer and the
    -- body run directly, not handed to that consumer as an argument (which
    -- is what the generic `.cut` case below does for ordinary producers).
    let nameStr := mangleId name
    let bodyStr := compileStatement stmt
    let retStr := mangleId returnName
    "(let ((" ++ nameStr ++ " " ++ retStr ++ ")) " ++ bodyStr ++ ")"
  | .cut producer returnName =>
    let prodStr := compileProducer producer
    let retStr := mangleId returnName
    "(" ++ retStr ++ " " ++ prodStr ++ ")"
  | .join _ name consumer body =>
    let nameStr := mangleId name
    let consStr := compileConsumer consumer
    let bodyStr := compileStatement body
    "(let ((" ++ nameStr ++ " " ++ consStr ++ ")) " ++ bodyStr ++ ")"
  | .primitive _ primName producers returnName =>
    let prodStrs := producers.map compileProducer
    let retStr := mangleId returnName
    compilePrimitive primName prodStrs retStr
  | .invoke _ name returnName =>
    let nameStr := mangleId name
    let retStr := mangleId returnName
    "(" ++ nameStr ++ " " ++ retStr ++ ")"
  | .externalCall _ name producers returnName =>
    let prodStrs := producers.map compileProducer
    let retStr := mangleId returnName
    compilePrimitive name prodStrs retStr
  | .binOp _ op lhs rhs returnName =>
    let lhsStr := compileProducer lhs
    let rhsStr := compileProducer rhs
    let retStr := mangleId returnName
    compilePrimitive op [lhsStr, rhsStr] retStr
  | .ifz _ cond thenBranch elseBranch =>
    let condStr := compileProducer cond
    let thenStr := compileStatement thenBranch
    let elseStr := compileStatement elseBranch
    "(if (eqv? " ++ condStr ++ " 0) " ++ thenStr ++ " " ++ elseStr ++ ")"

end

def compileDefinition (d : Join.Definition) : String :=
  let nameStr := mangleId d.name
  let returnStr := mangleId d.ret
  let bodyStr := compileStatement d.body
  "(define (" ++ nameStr ++ " " ++ returnStr ++ ")\n  " ++ bodyStr ++ ")\n"

/-- Minimal Chez Scheme runtime for Malgo. Ported byte-for-byte from
`schemeRuntime` in `Scheme.hs` (`T.unlines` = each line followed by a
newline, including a trailing blank line). -/
def schemeRuntime : String :=
  -- `Int32#`/`String#` constructor tags must be mangled the same way
  -- `compileTag` mangles every other Malgo constructor name reaching
  -- Scheme (`#` -> `_hash_`), since the real compiled `(Int32# x)`/
  -- `(String# x)` patterns `runProcess`'s callers pattern-match against
  -- check for the mangled symbol, not the literal source name.
  let int32Tag := mangleText "Int32#"
  let stringTag := mangleText "String#"
  "\n".intercalate
    [ ";; Malgo Runtime Support",
      ";; Generated by Malgo Scheme Backend",
      "",
      ";; Result printer",
      "(define (malgo-print-result v)",
      "  (malgo-print-value v)",
      "  (newline))",
      "",
      ";; Reverses the two remaining named escapes `mangleChar` applies (`#`",
      ";; and `'`, the only ASCII punctuation Chez's reader can't take bare",
      ";; mid-symbol -- see Backend/Scheme.lean's `isSafeSchemeSymbolChar`).",
      ";; Everything else `compileTag` emits is already the source-level",
      ";; name verbatim, so this is a small, exact fixup, not a general",
      ";; unmangler -- a `_u<N>_` numeric-escape tag (non-Greek Unicode)",
      ";; still displays in its escaped form.",
      "(define (malgo-replace-all str from to)",
      "  (let ((flen (string-length from)))",
      "    (let loop ((s str))",
      "      (let ((idx (let find ((i 0))",
      "                   (cond",
      "                     ((> (+ i flen) (string-length s)) #f)",
      "                     ((string=? (substring s i (+ i flen)) from) i)",
      "                     (else (find (+ i 1)))))))",
      "        (if idx",
      "            (loop (string-append (substring s 0 idx) to (substring s (+ idx flen) (string-length s))))",
      "            s)))))",
      "(define (malgo-demangle-tag sym)",
      "  (malgo-replace-all (malgo-replace-all (symbol->string sym) \"_hash_\" \"#\") \"_prime_\" \"'\"))",
      "",
      "(define (malgo-print-value v)",
      "  (cond",
      "    ((boolean? v) (display (if v \"true\" \"false\")))",
      "    ((null? v) (display \"()\"))",
      "    ((string? v) (display v))",
      "    ((char? v) (display v))",
      "    ((pair? v)",
      "     (if (and (pair? (car v)) (symbol? (caar v)))",
      "         ;; Record (valueToText: always \"<record>\", fields are never",
      "         ;; pretty-printed -- also sidesteps that object fields compile",
      "         ;; to thunks here, which would need forcing to print at all)",
      "         (display \"<record>\")",
      "         ;; Tagged data (valueToText: tuples print as {a, b}, other",
      "         ;; constructors as Name or Name(a, b) -- never S-expression style)",
      "         (if (symbol? (car v))",
      "             (if (eq? (car v) 'tuple)",
      "                 (begin",
      "                   (display \"{\")",
      "                   (let loop ((args (cdr v)))",
      "                     (unless (null? args)",
      "                       (malgo-print-value (car args))",
      "                       (unless (null? (cdr args)) (display \", \"))",
      "                       (loop (cdr args))))",
      "                   (display \"}\"))",
      "                 (if (null? (cdr v))",
      "                     (display (malgo-demangle-tag (car v)))",
      "                     (begin",
      "                       (display (malgo-demangle-tag (car v)))",
      "                       (display \"(\")",
      "                       (let loop ((args (cdr v)))",
      "                         (unless (null? args)",
      "                           (malgo-print-value (car args))",
      "                           (unless (null? (cdr args)) (display \", \"))",
      "                           (loop (cdr args))))",
      "                       (display \")\"))))",
      "             ;; Regular pair",
      "             (begin",
      "               (display \"(\")",
      "               (malgo-print-value (car v))",
      "               (display \" . \")",
      "               (malgo-print-value (cdr v))",
      "               (display \")\")))))",
      "    ((procedure? v) (display \"<function>\"))",
      "    (else (display v))))",
      "",
      ";; Not-equal operator",
      "(define (malgo-ne a b) (not (equal? a b)))",
      "",
      ";; String join (SRFI-13 not available in Chez Scheme)",
      "(define (malgo-string-join lst sep)",
      "  (if (null? lst) \"\"",
      "    (let loop ((rest (cdr lst)) (acc (car lst)))",
      "      (if (null? rest) acc",
      "        (loop (cdr rest) (string-append acc sep (car rest)))))))",
      "",
      ";; Subprocess execution. Chez has no shell-free spawn (confirmed against",
      ";; 10.4.1 and cisco/ChezScheme#604): process/system/open-process-ports",
      ";; all run their argument through one /bin/sh -c. Safety comes from",
      ";; shell-quoting every argv element (single-quote wrapping), not from",
      ";; avoiding the shell.",
      "(define (malgo-shell-quote s)",
      "  (string-append \"'\"",
      "    (apply string-append",
      "      (map (lambda (c) (if (char=? c #\\') \"'\\\\''\" (string c)))",
      "           (string->list s)))",
      "    \"'\"))",
      "",
      ";; Undo Prelude.mlg's joinWithNul: each argv element is followed by a",
      ";; NUL terminator rather than separated by one, so an empty blob is",
      ";; zero elements and a single embedded NUL is one empty-string element",
      ";; -- a plain separator-join can't tell those two cases apart.",
      "(define (malgo-split-nul-terminated s)",
      "  (let ([len (string-length s)])",
      "    (let loop ([start 0] [i 0] [acc '()])",
      "      (cond",
      "        [(= i len) (reverse acc)]",
      "        [(char=? (string-ref s i) #\\nul)",
      "         (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]",
      "        [else (loop start (+ i 1) acc)]))))",
      "",
      "(define (malgo-find-last-nul s)",
      "  (let loop ([i (- (string-length s) 1)])",
      "    (cond [(< i 0) #f]",
      "          [(char=? (string-ref s i) #\\nul) i]",
      "          [else (loop (- i 1))])))",
      "",
      ";; Extract the exit code from stderr's trailing marker, or -1 (never a",
      ";; real POSIX exit code, which is always 0..255) if the marker is",
      ";; missing, truncated, or non-numeric. Distinguishing \"confirmed exit",
      ";; code N\" from \"couldn't determine\" matters: the marker can go",
      ";; missing (a shell builtin like `exit`/`exec` used to truncate the",
      ";; script before it ran -- fixed below by running the quoted command",
      ";; in a `( ... )` subshell instead, so `exit`/`exec` only end the",
      ";; subshell -- or the shell being killed by a signal first, which",
      ";; the subshell wrap can't fix). Silently reporting 0 (success) for",
      ";; \"unknown\" would be worse than not trying to distinguish it.",
      "(define (malgo-process-exit-code err-full marker-idx)",
      "  (if (and marker-idx (<= (+ marker-idx 12) (string-length err-full)))",
      "      (let ([n (string->number (substring err-full (+ marker-idx 12) (string-length err-full)))])",
      "        (if (number? n) n -1))",
      "      -1))",
      "",
      ";; Run `cmd` with the NUL-terminated `args-blob`, returning",
      ";; (exit code, stdout, stderr). The exit code travels as a",
      ";; NUL-prefixed marker appended to STDERR only (matching the Zig",
      ";; runtime's own MALGO-LEAK stderr marker) -- never interleaved into",
      ";; stdout, since nix-config's task scripts need byte-exact stdout for",
      ";; jq/gh --json consumers -- as long as it's valid UTF-8: like the",
      ";; interpreter's own `IO.Process.output`-based capture, an invalid",
      ";; byte sequence is lossily replaced (U+FFFD), not passed through",
      ";; raw, since `get-string-all` decodes text, not bytes. Wrapping in",
      ";; `( ... )` means a shell builtin like `exit`/`exec` in `cmd` only",
      ";; ends the subshell, not the whole script -- so the trailing marker",
      ";; print still runs and still reports that subshell's own status via",
      ";; `$?`. open-process-ports already runs its argument through one",
      ";; /bin/sh -c layer, so the quoted command line needs no further",
      ";; shell wrapping beyond that subshell grouping.",
      "(define (malgo-run-process cmd args-blob)",
      "  (let* ([args (malgo-split-nul-terminated args-blob)]",
      "         [quoted (map malgo-shell-quote (cons cmd args))]",
      "         [cmdline (malgo-string-join quoted \" \")]",
      "         [full (string-append \"( \" cmdline \" ); printf '\\\\000MALGO_EXIT:%d' \\\"$?\\\" 1>&2\")])",
      "    (call-with-values",
      "      (lambda () (open-process-ports full (buffer-mode line) (native-transcoder)))",
      "      (lambda (to-stdin from-stdout from-stderr pid)",
      "        (close-port to-stdin)",
      "        (let* ([raw-out (get-string-all from-stdout)]",
      "               [raw-err (get-string-all from-stderr)]",
      "               [out (if (eof-object? raw-out) \"\" raw-out)]",
      "               [err-full (if (eof-object? raw-err) \"\" raw-err)]",
      "               [marker-idx (malgo-find-last-nul err-full)]",
      "               [err (if marker-idx (substring err-full 0 marker-idx) err-full)]",
      "               [code (malgo-process-exit-code err-full marker-idx)])",
      "          (close-port from-stdout)",
      "          (close-port from-stderr)",
      "          (list 'tuple (list '" ++ int32Tag ++ " code) (list '" ++ stringTag ++ " out) (list '" ++ stringTag ++ " err)))))))",
      "",
      ";; Finish continuation (halt)",
      "(define (malgo-finish v) v)",
      "" ]
    ++ "\n"

/-- Compile a Join IR program to Scheme source code. -/
def compileToScheme (modName : ModuleName) (program : Join.Program) : String :=
  let mainName := mangleText (modName.toStr ++ ".main")
  schemeRuntime
    ++ "\n;; Definitions\n"
    ++ "\n".intercalate (program.definitions.map compileDefinition)
    ++ "\n\n;; Run main\n"
    ++ "(" ++ mainName ++ " (lambda (fn) (fn (list 'tuple) malgo-finish)))\n"

private def r0 : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
private def nm (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }

-- Mangling / literal / small-tree checks.
-- `.`, `-`, and `?` are all valid Chez Scheme symbol constituents (verified
-- against 10.4.1: `Foo.bar`/`Foo-`/`Foo?` all parse and `define`/reference
-- correctly as plain symbols) -- only `#` and `'` still get mangled.
#guard mangleText "foo-bar?" == "foo-bar?"
#guard mangleText "Int32#" == "Int32_hash_"
#guard mangleId (nm "map") == "t.map"
#guard mangleId { name := "x", moduleName := .moduleName "t", sort := .temporal 3 } == "_t_x_3"
#guard mangleId { name := "y", moduleName := .moduleName "t", sort := .internal 7 } == "y_7"
#guard escapeString "a\"b\\c" == "a\\\"b\\\\c"
#guard compileLiteral (.int32 (-5)) == "-5"
#guard compileLiteral (.float 3.14) == "3.14"
#guard compileLiteral (.string "hi\n") == "\"hi\\n\""
#guard compileLiteral (.char ' ') == "#\\space"
#guard compileStatement (.invoke r0 (nm "f") (nm "k")) == "(t.f t.k)"
#guard compileStatement (.cut (.literal r0 (.int32 1)) (nm "k")) == "(t.k 1)"
#guard compileStatement (.binOp r0 "add_i32" (.var r0 (nm "a")) (.var r0 (nm "b")) (nm "k"))
  == "(t.k (+ t.a t.b))"

end Malgo.Backend.Scheme
