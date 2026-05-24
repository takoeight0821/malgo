module Malgo.Backend.Scheme
  ( SchemePass (..),
    compileToScheme,
    mangleId,
    escapeString,
  )
where

import Control.Exception (Exception (..))
import Data.Map qualified as Map
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static ()
import Effectful.Reader.Static (Reader, ask)
import Malgo.Id
import Malgo.Module (ModuleName, moduleNameToString)
import Malgo.Pass
import Malgo.Prelude
import Malgo.Sequent.Core.Join qualified as Join
import Malgo.Sequent.Fun (Literal (..), Name, Pattern (..), Tag (..))

-- | SchemePass translates Join IR to Scheme source code.
data SchemePass = SchemePass

instance Pass SchemePass where
  type Input SchemePass = Join.Program
  type Output SchemePass = Text
  type ErrorType SchemePass = SchemeError
  type Effects SchemePass es = Reader ModuleName :> es

  runPassImpl _ program = do
    modName <- ask @ModuleName
    pure $ compileToScheme modName program

data SchemeError = SchemeError Text
  deriving stock (Show)

instance Exception SchemeError where
  displayException (SchemeError msg) = "Scheme backend error: " <> convertString msg

-- | Compile a Join IR program to Scheme source code.
compileToScheme :: ModuleName -> Join.Program -> Text
compileToScheme modName program =
  schemeRuntime
    <> "\n;; Definitions\n"
    <> T.intercalate "\n" (map compileDefinition program.definitions)
    <> "\n\n;; Run main\n"
    <> "("
    <> mainName
    <> " (lambda (fn) (fn (list 'tuple) malgo-finish)))\n"
  where
    mainName = mangleText (moduleNameToString modName <> ".main")

compileDefinition :: (Range, Name, Name, Join.Statement) -> Text
compileDefinition (_, name, returnName, body) =
  let nameStr = mangleId name
      returnStr = mangleId returnName
      bodyStr = compileStatement body
   in "(define ("
        <> nameStr
        <> " "
        <> returnStr
        <> ")\n  "
        <> bodyStr
        <> ")\n"

-- | Compile a Producer to a Scheme expression.
compileProducer :: Join.Producer -> Text
compileProducer (Join.Var _ name) = mangleId name
compileProducer (Join.Literal _ lit) = compileLiteral lit
compileProducer (Join.Construct _ tag producers returns) =
  let tagStr = compileTag tag
      prodStrs = map compileProducer producers
      retStrs = map mangleId returns
   in "(list '"
        <> tagStr
        <> concatArgs prodStrs
        <> concatArgs retStrs
        <> ")"
compileProducer (Join.Lambda _ names body) =
  let nameStrs = map mangleId names
      bodyStr = compileStatement body
   in case nameStrs of
        [] -> "(lambda () " <> bodyStr <> ")"
        [x] -> "(lambda (" <> x <> ") " <> bodyStr <> ")"
        _ -> "(lambda (" <> T.intercalate " " nameStrs <> ") " <> bodyStr <> ")"
compileProducer (Join.Mu _ name stmt) =
  let nameStr = mangleId name
      bodyStr = compileStatement stmt
   in "(lambda (" <> nameStr <> ") " <> bodyStr <> ")"
compileProducer (Join.Cocase _ branches) =
  let branchStrs = map compileCocaseBranch branches
   in "(lambda (%dtor . %args) (cond "
        <> T.intercalate " " branchStrs
        <> " (else (error 'cocase \"no matching destructor\"))))"
  where
    compileCocaseBranch (dName, vars, body) =
      let mangledName = mangleText dName
          bodyStr = compileStatement body
          bindings = zipWith (\v i -> "(" <> mangleId v <> " (list-ref %args " <> convertString (show i) <> "))") vars [0 :: Int ..]
       in "((eq? %dtor '"
            <> mangledName
            <> ") "
            <> if null bindings
              then bodyStr <> ")"
              else "(let* (" <> T.intercalate " " bindings <> ") " <> bodyStr <> "))"
compileProducer (Join.Object _ fields) =
  let fieldStrs = map compileField (Map.toList fields)
   in "(list " <> T.intercalate " " fieldStrs <> ")"
  where
    compileField :: (Text, (Name, Join.Statement)) -> Text
    compileField (fieldName, (retName, body)) =
      let retStr = mangleId retName
          bodyStr = compileStatement body
       in "(cons '"
            <> mangleText fieldName
            <> " (lambda ("
            <> retStr
            <> ") "
            <> bodyStr
            <> "))"

-- | Compile a Consumer to a Scheme expression.
compileConsumer :: Join.Consumer -> Text
compileConsumer (Join.Label _ name) = mangleId name
compileConsumer (Join.Apply _ producers returns) =
  let prodStrs = map compileProducer producers
      retStrs = map mangleId returns
   in "(lambda (%fn) (%fn" <> concatArgs (prodStrs <> retStrs) <> "))"
compileConsumer (Join.Project _ field returnName) =
  let retStr = mangleId returnName
   in "(lambda (%rec) (let ((%v (cdr (assq '"
        <> mangleText field
        <> " %rec)))) (if (procedure? %v) (%v "
        <> retStr
        <> ") ("
        <> retStr
        <> " %v))))"
compileConsumer (Join.Then _ name body) =
  let nameStr = mangleId name
      bodyStr = compileStatement body
   in "(lambda (" <> nameStr <> ") " <> bodyStr <> ")"
compileConsumer (Join.Finish _) = "malgo-finish"
compileConsumer (Join.Destructor _ dName producers returnName) =
  let mangledName = mangleText dName
      prodStrs = map compileProducer producers
      retStr = mangleId returnName
      argList = concatArgs prodStrs <> " " <> retStr
   in "(lambda (%cocase) (%cocase '" <> mangledName <> argList <> "))"
compileConsumer (Join.Select _ branches) =
  let branchStrs = map compileBranch branches
   in "(lambda (%v) (cond " <> T.intercalate " " branchStrs <> " (else (error 'select \"no matching branch\" %v))))"

-- | Compile a pattern against a scrutinee expression.
-- Returns (guard_condition, let*_bindings) where guard is a Scheme boolean
-- expression and bindings are sequential let* entries.
compilePattern :: Text -> Pattern -> (Text, [(Text, Text)])
compilePattern scrutinee (PVar _ n) =
  ("#t", [(mangleId n, scrutinee)])
compilePattern scrutinee (PLiteral _ lit) =
  ("(equal? " <> scrutinee <> " " <> compileLiteral lit <> ")", [])
compilePattern scrutinee (Destruct _ tag pats) =
  let tagStr = compileTag tag
      tagCheck = "(and (pair? " <> scrutinee <> ") (eq? (car " <> scrutinee <> ") '" <> tagStr <> "))"
      subResults = zipWith (\i p -> compilePattern ("(list-ref " <> scrutinee <> " " <> convertString (show (i :: Int)) <> ")") p) [1 ..] pats
      subGuards = filter (/= "#t") (map fst subResults)
      subBindings = concatMap snd subResults
      fullGuard = case subGuards of
        [] -> tagCheck
        gs -> "(and " <> T.intercalate " " (tagCheck : gs) <> ")"
   in (fullGuard, subBindings)
compilePattern scrutinee (Expand _ fieldPats) =
  let subResults =
        map
          ( \(fname, p) ->
              let mangledFname = mangleText fname
                  raw = "(cdr (assq '" <> mangledFname <> " " <> scrutinee <> "))"
                  tmpName = "%fv_" <> mangledFname
                  tmpName2 = "%fvr_" <> mangledFname
                  -- Object fields are stored as thunks; force them before pattern matching
                  forced =
                    "(let (("
                      <> tmpName
                      <> " "
                      <> raw
                      <> ")) "
                      <> "(if (procedure? "
                      <> tmpName
                      <> ") "
                      <> "("
                      <> tmpName
                      <> " (lambda ("
                      <> tmpName2
                      <> ") "
                      <> tmpName2
                      <> ")) "
                      <> tmpName
                      <> "))"
               in compilePattern forced p
          )
          (Map.toList fieldPats)
      subGuards = filter (/= "#t") (map fst subResults)
      subBindings = concatMap snd subResults
      fullGuard = case subGuards of
        [] -> "#t"
        [g] -> g
        gs -> "(and " <> T.intercalate " " gs <> ")"
   in (fullGuard, subBindings)

compileBranch :: Join.Branch -> Text
compileBranch (Join.Branch _ pat body) =
  let bodyStr = compileStatement body
      (guard, bindings) = compilePattern "%v" pat
      withBindings =
        if null bindings
          then bodyStr
          else "(let* (" <> T.intercalate " " (map (\(n, e) -> "(" <> n <> " " <> e <> ")") bindings) <> ") " <> bodyStr <> ")"
   in "(" <> guard <> " " <> withBindings <> ")"

-- | Compile a Statement to a Scheme expression.
compileStatement :: Join.Statement -> Text
compileStatement (Join.Cut producer returnName) =
  let prodStr = compileProducer producer
      retStr = mangleId returnName
   in "(" <> retStr <> " " <> prodStr <> ")"
compileStatement (Join.Join _ name consumer body) =
  let nameStr = mangleId name
      consStr = compileConsumer consumer
      bodyStr = compileStatement body
   in "(let ((" <> nameStr <> " " <> consStr <> ")) " <> bodyStr <> ")"
compileStatement (Join.Primitive _ primName producers returnName) =
  let prodStrs = map compileProducer producers
      retStr = mangleId returnName
   in compilePrimitive primName prodStrs retStr
compileStatement (Join.Invoke _ name returnName) =
  let nameStr = mangleId name
      retStr = mangleId returnName
   in "(" <> nameStr <> " " <> retStr <> ")"
compileStatement (Join.ExternalCall _ name producers returnName) =
  let prodStrs = map compileProducer producers
      retStr = mangleId returnName
   in compilePrimitive name prodStrs retStr
compileStatement (Join.BinOp _ op lhs rhs returnName) =
  let lhsStr = compileProducer lhs
      rhsStr = compileProducer rhs
      retStr = mangleId returnName
   in compilePrimitive op [lhsStr, rhsStr] retStr
compileStatement (Join.Ifz _ cond thenBranch elseBranch) =
  let condStr = compileProducer cond
      thenStr = compileStatement thenBranch
      elseStr = compileStatement elseBranch
   in "(if (eqv? " <> condStr <> " 0) " <> thenStr <> " " <> elseStr <> ")"

-- | Compile a primitive operation.
compilePrimitive :: Text -> [Text] -> Text -> Text
compilePrimitive name args ret = case name of
  "add_i32" -> binop "+" args ret
  "sub_i32" -> binop "-" args ret
  "mul_i32" -> binop "*" args ret
  "div_i32" -> binop "quotient" args ret
  "mod_i32" -> binop "modulo" args ret
  "add_i64" -> binop "+" args ret
  "sub_i64" -> binop "-" args ret
  "mul_i64" -> binop "*" args ret
  "div_i64" -> binop "quotient" args ret
  "mod_i64" -> binop "modulo" args ret
  "add_f64" -> binop "+" args ret
  "sub_f64" -> binop "-" args ret
  "mul_f64" -> binop "*" args ret
  "div_f64" -> binop "/" args ret
  "eq_i32" -> binop "equal?" args ret
  "ne_i32" -> binop "malgo-ne" args ret
  "lt_i32" -> binop "<" args ret
  "le_i32" -> binop "<=" args ret
  "gt_i32" -> binop ">" args ret
  "ge_i32" -> binop ">=" args ret
  "eq_i64" -> binop "equal?" args ret
  "ne_i64" -> binop "malgo-ne" args ret
  "lt_i64" -> binop "<" args ret
  "le_i64" -> binop "<=" args ret
  "gt_i64" -> binop ">" args ret
  "ge_i64" -> binop ">=" args ret
  "eq_f64" -> binop "equal?" args ret
  "ne_f64" -> binop "malgo-ne" args ret
  "lt_f64" -> binop "<" args ret
  "le_f64" -> binop "<=" args ret
  "gt_f64" -> binop ">" args ret
  "ge_f64" -> binop ">=" args ret
  "eq_char" -> binop "char=?" args ret
  "ne_char" -> binop "malgo-ne" args ret
  "string_append" -> binop "string-append" args ret
  "int32_to_string" -> unaryop "number->string" args ret
  "int64_to_string" -> unaryop "number->string" args ret
  "float_to_string" -> unaryop "number->string" args ret
  "double_to_string" -> unaryop "number->string" args ret
  "char_to_string" -> unaryop "string" args ret
  "string_to_int32" -> unaryop "string->number" args ret
  "string_to_int64" -> unaryop "string->number" args ret
  "string_length" -> unaryop "string-length" args ret
  "string_at" -> binop "string-ref" args ret
  "string_substring" ->
    case args of
      [s, start, len] -> "(" <> ret <> " (substring " <> s <> " " <> start <> " (+ " <> start <> " " <> len <> ")))"
      _ -> "(error 'prim \"string_substring: wrong number of arguments\")"
  "putchar" ->
    case args of
      [c] -> "(begin (display " <> c <> ") (" <> ret <> " '()))"
      _ -> "(error 'prim \"putchar: wrong number of arguments\")"
  "getchar" ->
    "(" <> ret <> " (let ((c (read-char))) (if (eof-object? c) #f c)))"
  "putstr" ->
    case args of
      [s] -> "(begin (display " <> s <> ") (" <> ret <> " '()))"
      _ -> "(error 'prim \"putstr: wrong number of arguments\")"
  "println" ->
    case args of
      [s] -> "(begin (display " <> s <> ") (newline) (" <> ret <> " '()))"
      _ -> "(error 'prim \"println: wrong number of arguments\")"
  "error" ->
    case args of
      [msg] -> "(error 'malgo " <> msg <> ")"
      _ -> "(error 'malgo \"error\")"
  "negate_i32" -> unaryop "-" args ret
  "negate_i64" -> unaryop "-" args ret
  "negate_f64" -> unaryop "-" args ret
  -- malgo_* foreign import names
  "malgo_read_file" ->
    case args of
      [path] -> "(" <> ret <> " (call-with-input-file " <> path <> " (lambda (p) (get-string-all p))))"
      _ -> "(error 'prim \"malgo_read_file: wrong number of arguments\")"
  "malgo_write_file" ->
    case args of
      [path, content] -> "(begin (call-with-output-file " <> path <> " (lambda (p) (put-string p " <> content <> "))) (" <> ret <> " '()))"
      _ -> "(error 'prim \"malgo_write_file: wrong number of arguments\")"
  "malgo_get_line" ->
    "(" <> ret <> " (let ((line (read-line))) (if (eof-object? line) \"\" line)))"
  "malgo_get_args" ->
    "(" <> ret <> " (malgo-string-join (cdr (command-line)) \"\\n\"))"
  "malgo_exit_success" -> "(exit 0)"
  "malgo_stderr_string" ->
    case args of
      [s] -> "(begin (put-string (current-error-port) " <> s <> ") (" <> ret <> " '()))"
      _ -> "(error 'prim \"malgo_stderr_string: wrong number of arguments\")"
  "malgo_string_to_int32" -> unaryop "string->number" args ret
  "malgo_string_to_int64" -> unaryop "string->number" args ret
  -- Arithmetic (malgo_* foreign import names from Builtin.mlg)
  "malgo_add_int32_t" -> binop "+" args ret
  "malgo_sub_int32_t" -> binop "-" args ret
  "malgo_mul_int32_t" -> binop "*" args ret
  "malgo_div_int32_t" -> binop "quotient" args ret
  "malgo_mod_int32_t" -> binop "modulo" args ret
  "malgo_neg_int32_t" -> unaryop "-" args ret
  "malgo_add_int64_t" -> binop "+" args ret
  "malgo_sub_int64_t" -> binop "-" args ret
  "malgo_mul_int64_t" -> binop "*" args ret
  "malgo_div_int64_t" -> binop "quotient" args ret
  "malgo_mod_int64_t" -> binop "modulo" args ret
  "malgo_neg_int64_t" -> unaryop "-" args ret
  "malgo_add_float" -> binop "+" args ret
  "malgo_sub_float" -> binop "-" args ret
  "malgo_mul_float" -> binop "*" args ret
  "malgo_div_float" -> binop "/" args ret
  "malgo_neg_float" -> unaryop "-" args ret
  "malgo_add_double" -> binop "+" args ret
  "malgo_sub_double" -> binop "-" args ret
  "malgo_mul_double" -> binop "*" args ret
  "malgo_div_double" -> binop "/" args ret
  "malgo_neg_double" -> unaryop "-" args ret
  -- Comparisons return Int32# (1=true, 0=false) to match isTrue# pattern matching
  "malgo_eq_int32_t" -> cmpop "equal?" args ret
  "malgo_ne_int32_t" -> cmpopNeg "equal?" args ret
  "malgo_lt_int32_t" -> cmpop "<" args ret
  "malgo_le_int32_t" -> cmpop "<=" args ret
  "malgo_gt_int32_t" -> cmpop ">" args ret
  "malgo_ge_int32_t" -> cmpop ">=" args ret
  "malgo_eq_int64_t" -> cmpop "equal?" args ret
  "malgo_ne_int64_t" -> cmpopNeg "equal?" args ret
  "malgo_lt_int64_t" -> cmpop "<" args ret
  "malgo_le_int64_t" -> cmpop "<=" args ret
  "malgo_gt_int64_t" -> cmpop ">" args ret
  "malgo_ge_int64_t" -> cmpop ">=" args ret
  "malgo_eq_float" -> cmpop "equal?" args ret
  "malgo_ne_float" -> cmpopNeg "equal?" args ret
  "malgo_lt_float" -> cmpop "<" args ret
  "malgo_le_float" -> cmpop "<=" args ret
  "malgo_gt_float" -> cmpop ">" args ret
  "malgo_ge_float" -> cmpop ">=" args ret
  "malgo_eq_double" -> cmpop "equal?" args ret
  "malgo_ne_double" -> cmpopNeg "equal?" args ret
  "malgo_lt_double" -> cmpop "<" args ret
  "malgo_le_double" -> cmpop "<=" args ret
  "malgo_gt_double" -> cmpop ">" args ret
  "malgo_ge_double" -> cmpop ">=" args ret
  "malgo_eq_char" -> cmpop "char=?" args ret
  "malgo_ne_char" -> cmpopNeg "char=?" args ret
  "malgo_lt_char" -> cmpop "char<?" args ret
  "malgo_le_char" -> cmpop "char<=?" args ret
  "malgo_gt_char" -> cmpop "char>?" args ret
  "malgo_ge_char" -> cmpop "char>=?" args ret
  "malgo_eq_string" -> cmpop "string=?" args ret
  "malgo_ne_string" -> cmpopNeg "string=?" args ret
  "malgo_lt_string" -> cmpop "string<?" args ret
  "malgo_le_string" -> cmpop "string<=?" args ret
  "malgo_gt_string" -> cmpop "string>?" args ret
  "malgo_ge_string" -> cmpop "string>=?" args ret
  -- Char/string operations
  "malgo_char_ord" -> unaryop "char->integer" args ret
  "malgo_int32_t_to_char" -> unaryop "integer->char" args ret
  "malgo_char_to_string" -> unaryop "string" args ret
  "malgo_is_digit" -> cmpBool "char-numeric?" args ret
  "malgo_is_lower" -> cmpBool "char-lower-case?" args ret
  "malgo_is_upper" -> cmpBool "char-upper-case?" args ret
  "malgo_is_alphanum" ->
    case args of
      [c] -> "(" <> ret <> " (if (or (char-alphabetic? " <> c <> ") (char-numeric? " <> c <> ")) 1 0))"
      _ -> "(error 'prim \"malgo_is_alphanum: wrong number of arguments\")"
  "malgo_string_append" -> binop "string-append" args ret
  "malgo_string_length" -> unaryop "string-length" args ret
  "malgo_string_at" ->
    case args of
      [i, s] -> "(" <> ret <> " (string-ref " <> s <> " " <> i <> "))"
      _ -> "(error 'prim \"malgo_string_at: wrong number of arguments\")"
  "malgo_string_cons" ->
    case args of
      [c, s] -> "(" <> ret <> " (string-append (string " <> c <> ") " <> s <> "))"
      _ -> "(error 'prim \"malgo_string_cons: wrong number of arguments\")"
  "malgo_substring" ->
    case args of
      [s, start, end_] -> "(" <> ret <> " (substring " <> s <> " " <> start <> " " <> end_ <> "))"
      _ -> "(error 'prim \"malgo_substring: wrong number of arguments\")"
  "malgo_string_reverse" ->
    case args of
      [s] -> "(" <> ret <> " (list->string (reverse (string->list " <> s <> "))))"
      _ -> "(error 'prim \"malgo_string_reverse: wrong number of arguments\")"
  -- Conversion
  "malgo_int32_t_to_string" -> unaryop "number->string" args ret
  "malgo_int64_t_to_string" -> unaryop "number->string" args ret
  "malgo_float_to_string" -> unaryop "number->string" args ret
  "malgo_double_to_string" -> unaryop "number->string" args ret
  -- IO
  "malgo_print_string" ->
    case args of
      [s] -> "(begin (display " <> s <> ") (" <> ret <> " (list 'tuple)))"
      _ -> "(error 'prim \"malgo_print_string: wrong number of arguments\")"
  "malgo_print_char" ->
    case args of
      [c] -> "(begin (display " <> c <> ") (" <> ret <> " (list 'tuple)))"
      _ -> "(error 'prim \"malgo_print_char: wrong number of arguments\")"
  "malgo_print" ->
    case args of
      [v] -> "(begin (malgo-print-value " <> v <> ") (" <> ret <> " (list 'tuple)))"
      _ -> "(error 'prim \"malgo_print: wrong number of arguments\")"
  "malgo_newline" -> "(begin (newline) (" <> ret <> " (list 'tuple)))"
  "malgo_flush" -> "(begin (flush-output-port) (" <> ret <> " (list 'tuple)))"
  "malgo_get_char" ->
    "(" <> ret <> " (let ((c (read-char))) (if (eof-object? c) #\\nul c)))"
  "malgo_get_contents" ->
    "(" <> ret <> " (get-string-all (current-input-port)))"
  -- Error / control
  "malgo_panic" ->
    case args of
      [msg] -> "(error 'panic " <> msg <> ")"
      _ -> "(error 'panic \"panic\")"
  "malgo_unsafe_cast" ->
    case args of
      [x] -> "(" <> ret <> " " <> x <> ")"
      _ -> "(error 'prim \"malgo_unsafe_cast: wrong number of arguments\")"
  "malgo_exit_failure" -> "(exit 1)"
  -- Math
  "sqrt" -> unaryop "sqrt" args ret
  "sqrtf" -> unaryop "sqrt" args ret
  _ -> "(error 'prim \"unknown primitive: " <> name <> "\")"
  where
    binop :: Text -> [Text] -> Text -> Text
    binop op [a, b] r = "(" <> r <> " (" <> op <> " " <> a <> " " <> b <> "))"
    binop op _ r = "(" <> r <> " (error 'prim \"" <> op <> ": wrong number of arguments\"))"

    unaryop :: Text -> [Text] -> Text -> Text
    unaryop op [a] r = "(" <> r <> " (" <> op <> " " <> a <> "))"
    unaryop op _ r = "(" <> r <> " (error 'prim \"" <> op <> ": wrong number of arguments\"))"

    -- comparison returning 1/0 instead of #t/#f
    cmpop :: Text -> [Text] -> Text -> Text
    cmpop op [a, b] r = "(" <> r <> " (if (" <> op <> " " <> a <> " " <> b <> ") 1 0))"
    cmpop op _ r = "(" <> r <> " (error 'prim \"" <> op <> ": wrong number of arguments\"))"

    -- negated comparison returning 1/0
    cmpopNeg :: Text -> [Text] -> Text -> Text
    cmpopNeg op [a, b] r = "(" <> r <> " (if (not (" <> op <> " " <> a <> " " <> b <> ")) 1 0))"
    cmpopNeg op _ r = "(" <> r <> " (error 'prim \"not-" <> op <> ": wrong number of arguments\"))"

    -- unary predicate returning 1/0
    cmpBool :: Text -> [Text] -> Text -> Text
    cmpBool op [a] r = "(" <> r <> " (if (" <> op <> " " <> a <> ") 1 0))"
    cmpBool op _ r = "(" <> r <> " (error 'prim \"" <> op <> ": wrong number of arguments\"))"

-- | Compile a literal to a Scheme expression.
compileLiteral :: Literal -> Text
compileLiteral (Int32 n) = convertString (show n)
compileLiteral (Int64 n) = convertString (show n)
compileLiteral (Float f) = convertString (show f)
compileLiteral (Double d) = convertString (show d)
compileLiteral (Char c) = "#\\" <> escapeChar c
compileLiteral (String t) = "\"" <> escapeString t <> "\""

escapeChar :: Char -> Text
escapeChar ' ' = "space"
escapeChar '\n' = "newline"
escapeChar '\t' = "tab"
escapeChar '\r' = "return"
escapeChar '\0' = "nul"
escapeChar '\DEL' = "delete"
escapeChar c = T.singleton c

-- | Escape special characters in a string for Scheme output.
escapeString :: Text -> Text
escapeString =
  T.concatMap
    ( \case
        '\\' -> "\\\\"
        '"' -> "\\\""
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        c -> T.singleton c
    )

compileTag :: Tag -> Text
compileTag Tuple = "tuple"
compileTag (Tag t) = mangleText t

-- | Mangle a Malgo Id into a valid Scheme identifier.
mangleId :: Id -> Text
mangleId id =
  let base = case id.sort of
        External -> mangleText (moduleNameToString @Text id.moduleName <> "." <> id.name)
        Internal uniq -> mangleText id.name <> "_" <> convertString (show uniq)
        Temporal uniq -> "_t_" <> mangleText id.name <> "_" <> convertString (show uniq)
   in base

-- | Mangle a text string to be a valid Scheme identifier.
mangleText :: Text -> Text
mangleText = T.concatMap mangleChar
  where
    mangleChar :: Char -> Text
    mangleChar '#' = "_hash_"
    mangleChar '.' = "_dot_"
    mangleChar '\'' = "_prime_"
    mangleChar '-' = "_dash_"
    mangleChar '?' = "_q_"
    mangleChar '!' = "_bang_"
    mangleChar '/' = "_slash_"
    mangleChar '+' = "_plus_"
    mangleChar '*' = "_star_"
    mangleChar '<' = "_lt_"
    mangleChar '>' = "_gt_"
    mangleChar '=' = "_eq_"
    -- Greek letters
    mangleChar '\x03B1' = "alpha"
    mangleChar '\x03B2' = "beta"
    mangleChar '\x03B3' = "gamma"
    mangleChar '\x03B4' = "delta"
    mangleChar '\x03BB' = "lambda_"
    mangleChar '\x03BC' = "mu"
    mangleChar c
      | isAlphaNum c || c == '_' = T.singleton c
      | otherwise = "_u" <> convertString (show (ord c)) <> "_"

-- | Concatenate arguments with spaces, prepending a space if non-empty.
concatArgs :: [Text] -> Text
concatArgs [] = ""
concatArgs xs = " " <> T.intercalate " " xs

-- | Minimal Chez Scheme runtime for Malgo.
schemeRuntime :: Text
schemeRuntime =
  T.unlines
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
      ""
    ]
