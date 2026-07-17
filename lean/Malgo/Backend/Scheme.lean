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

/-- Mangle a single character to a valid-Scheme-identifier fragment. -/
def mangleChar (c : Char) : String :=
  match c with
  | '#' => "_hash_"
  | '.' => "_dot_"
  | '\'' => "_prime_"
  | '-' => "_dash_"
  | '?' => "_q_"
  | '!' => "_bang_"
  | '/' => "_slash_"
  | '+' => "_plus_"
  | '*' => "_star_"
  | '<' => "_lt_"
  | '>' => "_gt_"
  | '=' => "_eq_"
  -- Greek letters
  | 'α' => "alpha"
  | 'β' => "beta"
  | 'γ' => "gamma"
  | 'δ' => "delta"
  | 'λ' => "lambda_"
  | 'μ' => "mu"
  | c => if c.isAlphanum || c == '_' then String.singleton c else "_u" ++ toString c.toNat ++ "_"

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
`haskellShowFloat` (the Haskell backend emits `show`'s output; Chez reads
both fixed and `e`-notation). -/
def compileLiteral : Literal → String
  | .int32 n => toString n.toInt
  | .int64 n => toString n.toInt
  | .float f => Malgo.haskellShowFloat f.toFloat
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
    | [path] => "(" ++ ret ++ " (call-with-input-file " ++ path ++ " (lambda (p) (get-string-all p))))"
    | _ => "(error 'prim \"malgo_read_file: wrong number of arguments\")"
  | "malgo_write_file" =>
    match args with
    | [path, content] => "(begin (call-with-output-file " ++ path ++ " (lambda (p) (put-string p " ++ content ++ "))) (" ++ ret ++ " '()))"
    | _ => "(error 'prim \"malgo_write_file: wrong number of arguments\")"
  | "malgo_get_line" =>
    "(" ++ ret ++ " (let ((line (read-line))) (if (eof-object? line) \"\" line)))"
  | "malgo_get_args" =>
    "(" ++ ret ++ " (malgo-string-join (cdr (command-line)) \"\\n\"))"
  | "malgo_exit_success" => "(exit 0)"
  | "malgo_stderr_string" =>
    match args with
    | [s] => "(begin (put-string (current-error-port) " ++ s ++ ") (" ++ ret ++ " '()))"
    | _ => "(error 'prim \"malgo_stderr_string: wrong number of arguments\")"
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
    "(" ++ ret ++ " (get-string-all (current-input-port)))"
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
  | .mu _ name stmt =>
    let nameStr := mangleId name
    let bodyStr := compileStatement stmt
    "(lambda (" ++ nameStr ++ ") " ++ bodyStr ++ ")"
  | .cocase _ branches =>
    let branchStrs := branches.map compileCocaseBranch
    "(lambda (%dtor . %args) (cond " ++ " ".intercalate branchStrs ++ " (else (error 'cocase \"no matching destructor\"))))"
  | .object _ fields =>
    let fieldStrs := (sortAssocAscending fields).map compileField
    "(list " ++ " ".intercalate fieldStrs ++ ")"

partial def compileCocaseBranch : String × List Name × Join.Statement → String
  | (dName, vars, body) =>
    let mangledName := mangleText dName
    let bodyStr := compileStatement body
    let bindings := vars.mapIdx fun i v =>
      "(" ++ mangleId v ++ " (list-ref %args " ++ toString i ++ "))"
    "((eq? %dtor '" ++ mangledName ++ ") " ++
      (if bindings.isEmpty then bodyStr ++ ")"
       else "(let* (" ++ " ".intercalate bindings ++ ") " ++ bodyStr ++ "))")

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
  | .destructor _ dName producers returnName =>
    let mangledName := mangleText dName
    let prodStrs := producers.map compileProducer
    let retStr := mangleId returnName
    let argList := concatArgs prodStrs ++ " " ++ retStr
    "(lambda (%cocase) (%cocase '" ++ mangledName ++ argList ++ "))"
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

def compileDefinition : Range × Name × Name × Join.Statement → String
  | (_, name, returnName, body) =>
    let nameStr := mangleId name
    let returnStr := mangleId returnName
    let bodyStr := compileStatement body
    "(define (" ++ nameStr ++ " " ++ returnStr ++ ")\n  " ++ bodyStr ++ ")\n"

/-- Minimal Chez Scheme runtime for Malgo. Ported byte-for-byte from
`schemeRuntime` in `Scheme.hs` (`T.unlines` = each line followed by a
newline, including a trailing blank line). -/
def schemeRuntime : String :=
  "\n".intercalate
    [ ";; Malgo Runtime Support",
      ";; Generated by Malgo Scheme Backend",
      "",
      ";; Result printer",
      "(define (malgo-print-result v)",
      "  (malgo-print-value v)",
      "  (newline))",
      "",
      "(define (malgo-print-value v)",
      "  (cond",
      "    ((boolean? v) (display (if v \"true\" \"false\")))",
      "    ((null? v) (display \"()\"))",
      "    ((string? v)",
      "     (display \"\\\"\")",
      "     (display v)",
      "     (display \"\\\"\"))",
      "    ((char? v)",
      "     (display \"'\")",
      "     (display v)",
      "     (display \"'\"))",
      "    ((pair? v)",
      "     (if (and (pair? (car v)) (symbol? (caar v)))",
      "         ;; Record",
      "         (begin",
      "           (display \"{ \")",
      "           (let loop ((fields v))",
      "             (unless (null? fields)",
      "               (display (caar fields))",
      "               (display \" = \")",
      "               (malgo-print-value (cdar fields))",
      "               (unless (null? (cdr fields))",
      "                 (display \", \"))",
      "               (loop (cdr fields))))",
      "           (display \" }\"))",
      "         ;; Tagged data",
      "         (if (symbol? (car v))",
      "             (if (null? (cdr v))",
      "                 (display (car v))",
      "                 (begin",
      "                   (display \"(\")",
      "                   (display (car v))",
      "                   (let loop ((args (cdr v)))",
      "                     (unless (null? args)",
      "                       (display \" \")",
      "                       (malgo-print-value (car args))",
      "                       (loop (cdr args))))",
      "                   (display \")\")))",
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
#guard mangleText "foo-bar?" == "foo_dash_bar_q_"
-- External ids mangle "<module>.<name>", so the dot becomes "_dot_".
#guard mangleId (nm "map") == "t_dot_map"
#guard mangleId { name := "x", moduleName := .moduleName "t", sort := .temporal 3 } == "_t_x_3"
#guard mangleId { name := "y", moduleName := .moduleName "t", sort := .internal 7 } == "y_7"
#guard escapeString "a\"b\\c" == "a\\\"b\\\\c"
#guard compileLiteral (.int32 (-5)) == "-5"
#guard compileLiteral (.string "hi\n") == "\"hi\\n\""
#guard compileLiteral (.char ' ') == "#\\space"
#guard compileStatement (.invoke r0 (nm "f") (nm "k")) == "(t_dot_f t_dot_k)"
#guard compileStatement (.cut (.literal r0 (.int32 1)) (nm "k")) == "(t_dot_k 1)"
#guard compileStatement (.binOp r0 "add_i32" (.var r0 (nm "a")) (.var r0 (nm "b")) (nm "k"))
  == "(t_dot_k (+ t_dot_a t_dot_b))"

end Malgo.Backend.Scheme
