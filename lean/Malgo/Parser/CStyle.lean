import Malgo.Parser.Prim
import Malgo.Syntax
import Malgo.Syntax.Extension
import Malgo.Module
import Malgo.Features

/-! Port of `src/Malgo/Parser/CStyle.hs`: the whole surface grammar.

Fidelity: the same `attempt`/`choice` order, `captureRange` placement, and
desugarings as the Haskell source. `makeExprParser` single levels become
Prim combinators: `pType`'s `InfixR ->` is `chainr1`, `pOpApp`'s
`InfixL` is `chainl1`, `pApply`'s `Postfix manyUnaryOp` is a
`many` of postfix operators folded left.

Two deviations from CStyle.hs, both forced by the pure-parser design:
- The `Features` effect constraint threaded through every Haskell
  production is dropped. The grammar never branches on a feature flag
  (`hasFeature`/`isMalgo2025Enabled` live only in `Query/Engine.hs`), so
  there is nothing to thread; pragma flags are still extracted in
  `Malgo.Parser.pass`.
- Path imports and the module's own name are emitted as
  `ModuleName.rawPath`; `Malgo.Parser.pass` resolves them to `.artifact`
  afterwards (Haskell resolves them mid-parse via IO). -/

namespace Malgo.Parser

open Malgo
open Malgo.Syntax

/-! ## Literal parsers -/

/-- `pStringLiteral`: `"` charLiteral* `"`, lexeme. -/
def pStringLiteral : P String := P.lexeme do
  _ ← P.char '"'
  let str ← P.manyTill P.charLiteral (P.char '"')
  pure (String.ofList str)

/-- `pReal`: `L.float` with an optional `f32`/`f64` suffix. -/
def pReal : P (Literal .boxed) := P.lexeme do
  let f ← P.float
  let tail ← P.optional (P.string "f32" <|> P.string "f64")
  match tail with
  | some "f32" => pure (Literal.float f.toFloat32)
  | _ => pure (Literal.double f)

/-- `pInt`: `L.decimal` with an optional `i32`/`i64` suffix. -/
def pInt : P (Literal .boxed) := P.lexeme do
  let ds ← P.some P.digitChar
  let i := ds.toList.foldl (fun acc d => acc * 10 + (d.toNat - '0'.toNat)) 0
  let tail ← P.optional (P.string "i32" <|> P.string "i64")
  match tail with
  | some "i64" => pure (Literal.int64 (Int64.ofNat i))
  | _ => pure (Literal.int32 (Int32.ofNat i))

/-- `pChar`: `'` charLiteral `'`. -/
def pChar : P (Literal .boxed) := P.lexeme do
  _ ← P.char '\''
  let c ← P.charLiteral
  _ ← P.char '\''
  pure (Literal.char c)

def pString : P (Literal .boxed) := Literal.str <$> pStringLiteral

def pBoxed : P (Literal .boxed) :=
  P.choice [P.attempt pReal, pInt, pChar, pString]

/-- `pLiteral`: a boxed literal, unboxed if immediately followed by `#`. -/
def pLiteral : P (Expr .parse) := P.captureRange do
  let boxed ← pBoxed
  let sharp ← P.optional (P.symbol "#")
  pure fun range => match sharp with
    | some _ => Expr.unboxed range boxed.toUnboxed
    | none => Expr.boxed range boxed

def pLiteralP : P (Pat .parse) := P.captureRange do
  let boxed ← pBoxed
  let sharp ← P.optional (P.symbol "#")
  pure fun range => match sharp with
    | some _ => Pat.unboxed range boxed.toUnboxed
    | none => Pat.boxed range boxed

/-- `pVarP`: a variable pattern. -/
def pVarP : P (Pat .parse) := P.captureRange do
  let name ← P.ident
  pure fun range => Pat.var range name

/-- Accumulate `.field` projections for `pVariable`; each `Project`
spans from the whole expression's start to the end of the latest field. -/
private partial def pVariableFoldFields
    (projStart : SourcePos) (expr : Expr .parse) : P (Expr .parse) := do
  let mField ← P.optional (P.attempt (P.char '.' *> P.rawIdent))
  match mField with
  | none => pure expr
  | some field =>
    let fieldEndPos ← P.getSourcePos
    pVariableFoldFields projStart
      (Expr.project { start := projStart, stop := fieldEndPos } expr field)

/-- `pVariable`: an identifier, greedily consuming immediately-adjacent
`.field` access chains (no whitespace before the `.`). -/
def pVariable : P (Expr .parse) := do
  let startPos ← P.getSourcePos
  P.notFollowedBy P.anyReserved
  let name ← P.rawIdent
  let identEndPos ← P.getSourcePos
  let hasDot ← P.option false (P.lookAhead (P.char '.') *> pure true)
  if hasDot then
    let result ← pVariableFoldFields startPos
      (Expr.var { start := startPos, stop := identEndPos } name)
    P.space
    pure result
  else
    P.space
    let endPos ← P.getSourcePos
    pure (Expr.var { start := startPos, stop := endPos } name)

/-! ## Type parsers -/

mutual

/-- `pType`: `tyapp ("->" type)?`, right-associative. -/
partial def pType : P (Ty .parse) :=
  P.chainr1 pTyApp do
    let start ← P.getSourcePos
    P.reservedOperator "->"
    let stop ← P.getSourcePos
    pure fun dom cod => Ty.arr { start, stop } dom cod

/-- `pTyApp`: `atomType atomType*`. -/
partial def pTyApp : P (Ty .parse) := P.captureRange do
  let ty ← pAtomType
  let tys ← P.many pAtomType
  pure fun range => match tys with
    | [] => ty
    | _ => Ty.app range ty tys

partial def pAtomType : P (Ty .parse) :=
  P.choice [P.attempt pTyBottom, pTyTilde, pTyVar, pTyTuple,
    P.attempt pTyRecord, pTyVariant, P.attempt pTyBlock, pTyCStyleTuple]

partial def pTyVar : P (Ty .parse) := P.captureRange do
  let name ← P.ident
  pure fun range => Ty.var range name

partial def pTyTuple : P (Ty .parse) := P.captureRange do
  let tys ← P.between (P.symbol "(") (P.symbol ")") (P.sepBy pType (P.symbol ","))
  pure fun range => match tys with
    | [ty] => ty
    | _ => Ty.tuple range tys

partial def pTyRecord : P (Ty .parse) := P.captureRange do
  let (fields, rowTail) ← P.between (P.symbol "{") (P.symbol "}") do
    let fields ← P.sepEndBy1 pTyRecordField (P.symbol ",")
    let rowTail ← P.optional (P.reservedOperator "|" *> pType)
    pure (fields, rowTail)
  pure fun range => Ty.record range fields.toList rowTail

partial def pTyRecordField : P (String × Ty .parse) := do
  let field ← P.ident
  P.reservedOperator ":"
  let value ← pType
  pure (field, value)

partial def pTyBottom : P (Ty .parse) := P.captureRange do
  _ ← P.lexeme do
    let r ← P.string "_|_"
    P.notFollowedBy P.identContinue
    pure r
  pure fun range => Ty.bottom range

partial def pTyTilde : P (Ty .parse) := P.captureRange do
  P.reservedOperator "~"
  let ty ← pAtomType
  pure fun range => Ty.tilde range ty

partial def pTyVariant : P (Ty .parse) :=
  P.captureRange <| P.between (P.symbol "[") (P.symbol "]") do
    let first ← pVariantCase
    let rest ← P.many (P.attempt (P.reservedOperator "|" *> pVariantCase))
    let rowTail ← P.optional (P.reservedOperator "|" *> pType)
    pure fun range => Ty.variant range (first :: rest) rowTail

partial def pVariantCase : P (String × List (Ty .parse)) := do
  let name ← P.attempt do
    let n ← P.ident
    if (match n.toList with | c :: _ => c.isUpper | [] => false) then
      pure n
    else
      P.fail "variant case must start with an uppercase letter"
  let args ← P.many pAtomType
  pure (name, args)

partial def pTyCStyleTuple : P (Ty .parse) := P.captureRange do
  let tys ← P.between (P.symbol "{") (P.symbol "}") (P.sepBy pType (P.symbol ","))
  pure fun range => match tys with
    | [ty] => ty
    | _ => Ty.tuple range tys

partial def pTyBlock : P (Ty .parse) := P.captureRange do
  let ty ← P.between (P.symbol "{") (P.symbol "}") pType
  pure fun range => Ty.block range ty

end

/-! ## Expression, statement, pattern, clause, and copattern parsers -/

/-- Apply the C-style copattern arguments, innermost first (port of the
Haskell `applyPats` local). -/
def applyPats : List (Pat .parse) → (Range → CoPat .parse) → (Range → CoPat .parse)
  | [], copat => copat
  | p :: ps, copat => applyPats ps (fun range => CoPat.apply range (copat range) p)

mutual

/-- `pExpr`: an operator application, optionally with a type annotation. -/
partial def pExpr : P (Expr .parse) := do
  let expr ← pOpApp
  (P.attempt (P.captureRange do
      P.reservedOperator ":"
      let ty ← pType
      pure fun range => Expr.ann range expr ty))
  <|> pure expr

/-- `pOpApp`: left-associative application of any operator. -/
partial def pOpApp : P (Expr .parse) :=
  P.chainl1 pApply (P.captureRange do
    let op ← P.operator
    pure fun range (lhs rhs : Expr .parse) => Expr.opApp range op lhs rhs)

/-- `pApply`: C-style call, field projection, and regular-style
application, chained left. -/
partial def pApply : P (Expr .parse) := do
  let x ← pAtom
  let fs ← P.many pApplyPostfix
  pure (fs.foldl (fun acc f => f acc) x)

partial def pApplyPostfix : P (Expr .parse → Expr .parse) :=
  P.choice [
    -- Function application: expr(arg1, ...); () is treated as ({})
    P.captureRange do
      let args ← P.between (P.symbol "(") (P.symbol ")") (P.sepBy pExpr (P.symbol ","))
      pure fun range fn =>
        let actualArgs := if args.isEmpty then [Expr.tuple range []] else args
        actualArgs.foldl (fun f a => Expr.apply range f a) fn,
    -- Field projection: expr.field
    P.captureRange do
      P.reservedOperator "."
      let field ← P.ident
      pure fun range record => Expr.project range record field,
    -- Regular-style application: expr arg
    P.captureRange do
      let argument ← pAtom
      pure fun range fn => Expr.apply range fn argument ]

partial def pAtom : P (Expr .parse) :=
  P.choice [
    pLabel, pGoto, pLiteral, pVariable,
    P.attempt pParenTuple, P.attempt pTuple, P.attempt pRecord,
    pBrace, pList, pSeq ]

partial def pLabel : P (Expr .parse) := P.captureRange do
  P.reserved "label"
  let name ← P.ident
  let body ← pExpr
  pure fun range => Expr.label range name body

partial def pGoto : P (Expr .parse) := P.captureRange do
  P.reserved "goto"
  let (value, lbl) ← P.between (P.symbol "(") (P.symbol ")") do
    let value ← pExpr
    _ ← P.symbol ","
    let lbl ← pExpr
    pure (value, lbl)
  pure fun range => Expr.goto range value lbl

partial def pParenTuple : P (Expr .parse) := P.captureRange do
  let exprs ← P.between (P.symbol "(") (P.symbol ")") (P.sepBy pExpr (P.symbol ","))
  match exprs with
  | [expr] => pure fun range => Expr.parens range expr
  | _ => pure fun range => Expr.tuple range exprs

partial def pTuple : P (Expr .parse) := P.captureRange do
  let exprs ← P.between (P.symbol "{") (P.symbol "}") (P.sepBy pExpr (P.symbol ","))
  match exprs with
  | [_] => P.fail "c-style tuple must have at least two expressions or be empty"
  | _ => pure fun range => Expr.tuple range exprs

partial def pRecord : P (Expr .parse) := P.captureRange do
  let fields ← P.between (P.symbol "{") (P.symbol "}") (P.sepEndBy1 pRecordField (P.symbol ","))
  pure fun range => Expr.record range fields.toList

partial def pRecordField : P (String × Expr .parse) := do
  P.reservedOperator "."
  let field ← P.ident
  P.reservedOperator "->"
  let value ← pExpr
  pure (field, value)

partial def pBrace : P (Expr .parse) := P.captureRange do
  let content ← P.between (P.symbol "{") (P.symbol "}") (P.choice [P.attempt pCodata, pFn])
  pure fun range => content range

partial def pFn : P (Range → Expr .parse) := do
  let clauses ← P.sepEndBy1 pClause (P.symbol ",")
  pure fun range => Expr.fn range clauses

partial def pCodata : P (Range → Expr .parse) := do
  let clauses ← P.sepEndBy1 pCodataClause (P.symbol ",")
  pure fun range => Expr.codata range clauses.toList

partial def pCodataClause : P (CoPat .parse × Expr .parse) := do
  let cp ← pCopattern
  P.reservedOperator "->"
  let e ← pExpr
  pure (cp, e)

partial def pCopattern : P (CoPat .parse) := P.captureRange do
  _ ← P.symbol "#"
  pCopatternSuffix (fun range => CoPat.hole range)

partial def pCopatternSuffix (cp : Range → CoPat .parse) : P (Range → CoPat .parse) :=
  P.choice [
    P.attempt do
      P.reservedOperator "."
      let field ← P.ident
      pCopatternSuffix (fun range => CoPat.project range (cp range) field),
    P.attempt do
      let parenStart ← P.getSourcePos
      _ ← P.symbol "("
      let pats ← P.sepBy pPat (P.symbol ",")
      _ ← P.symbol ")"
      let parenEnd ← P.getSourcePos
      let parenRange : Range := { start := parenStart, stop := parenEnd }
      let actualPats := if pats.isEmpty then [Pat.tuple parenRange []] else pats
      pCopatternSuffix (applyPats actualPats cp),
    pure cp ]

partial def pList : P (Expr .parse) :=
  P.between (P.symbol "[") (P.symbol "]") <| P.captureRange do
    let elements ← P.sepEndBy pExpr (P.symbol ",")
    pure fun range => Expr.list range elements

partial def pSeq : P (Expr .parse) :=
  P.between (P.symbol "(") (P.symbol ")") pStmts

partial def pStmts : P (Expr .parse) := P.captureRange do
  let stmts ← P.sepEndBy1 pStmt (P.symbol ";")
  pure fun range => Expr.seq range stmts

partial def pStmt : P (Stmt .parse) :=
  pLet <|> pWith <|> pNoBind

partial def pLet : P (Stmt .parse) := P.captureRange do
  P.reserved "let"
  let pat ← pPat
  P.reservedOperator "="
  let body ← pExpr
  pure fun range => match pat with
    | .var _ name => Stmt.letS range name body
    | _ => Stmt.letPS range pat body

partial def pWith : P (Stmt .parse) := P.captureRange do
  P.reserved "with"
  P.choice [
    P.attempt do
      let name ← P.ident
      P.reservedOperator "="
      let body ← pExpr
      pure fun range => Stmt.withS range (some name) body,
    do
      let body ← pExpr
      pure fun range => Stmt.withS range none body ]

partial def pNoBind : P (Stmt .parse) := P.captureRange do
  let body ← pExpr
  pure fun range => Stmt.noBind range body

partial def pPat : P (Pat .parse) :=
  P.attempt pConP <|> pAtomPat

partial def pAtomPat : P (Pat .parse) :=
  P.choice [pVarP, pLiteralP, P.attempt pTupleP, P.attempt pParensTupleP, pRecordP, pListP]

partial def pConP : P (Pat .parse) := P.captureRange do
  let constructor ← P.ident
  let patterns ← (P.attempt do
      let cStylePatterns ← P.between (P.symbol "(") (P.symbol ")") (P.sepEndBy pPat (P.symbol ","))
      P.notFollowedBy pAtomPat
      pure cStylePatterns)
    <|> (NEList.toList <$> P.some pAtomPat)
  pure fun range => Pat.con range constructor patterns

partial def pTupleP : P (Pat .parse) := P.captureRange do
  let patterns ← P.between (P.symbol "{") (P.symbol "}") (P.sepBy pPat (P.symbol ","))
  match patterns with
  | [_] => P.fail "c-style tuple must have at least two patterns or be empty"
  | _ => pure fun range => Pat.tuple range patterns

partial def pParensTupleP : P (Pat .parse) := P.captureRange do
  let patterns ← P.between (P.symbol "(") (P.symbol ")") (P.sepBy pPat (P.symbol ","))
  match patterns with
  | [pattern] => pure fun _ => pattern
  | _ => pure fun range => Pat.tuple range patterns

partial def pRecordP : P (Pat .parse) := P.captureRange do
  let fields ← P.between (P.symbol "{") (P.symbol "}") (P.sepEndBy1 pRecordPField (P.symbol ","))
  pure fun range => Pat.record range fields.toList

partial def pRecordPField : P (String × Pat .parse) := do
  P.reservedOperator "."
  let field ← P.ident
  P.reservedOperator "->"
  let value ← pPat
  pure (field, value)

partial def pListP : P (Pat .parse) := P.captureRange do
  let pats ← P.between (P.symbol "[") (P.symbol "]") (P.sepEndBy pPat (P.symbol ","))
  pure fun range => Pat.list range pats

/-- `pClause`: optional C-style/regular patterns, `->`, then a statement
block. An empty pattern list desugars to a single `_` pattern. -/
partial def pClause : P (Clause .parse) := P.captureRange do
  let patterns ←
    (P.attempt (P.between (P.symbol "(") (P.symbol ")") (P.sepEndBy pPat (P.symbol ","))
      <* P.reservedOperator "->"))
    <|> (P.attempt ((NEList.toList <$> P.some pAtomPat) <* P.reservedOperator "->"))
    <|> pure []
  let body ← pStmts
  pure fun range => match patterns with
    | [] => Clause.mk range (NEList.singleton (Pat.var range "_")) body
    | p :: ps => Clause.mk range ⟨p, ps⟩ body

end

/-! ## Declaration parsers -/

private def pParameter : P (Range × String) := P.captureRange do
  let param ← P.ident
  pure fun range => (range, param)

/-- `pParameterList`: comma-separated parenthesized parameters. -/
def pParameterList : P (List (Range × String)) :=
  P.between (P.symbol "(") (P.symbol ")") (P.sepBy pParameter (P.symbol ","))

def pConstructor : P (Range × String × List (Ty .parse)) := P.captureRange do
  let name ← P.ident
  let cStyleParameters ← (Option.getD · []) <$>
    P.optional (P.attempt (P.between (P.symbol "(") (P.symbol ")") (P.sepBy pType (P.symbol ","))))
  let parameters ← match cStyleParameters with
    | [] => P.many pAtomType
    | [field] => (fun rest => field :: rest) <$> P.many pAtomType
    | fields => pure fields
  pure fun range => (range, name, parameters)

def pDataDef : P (Decl .parse) := P.captureRange do
  P.reserved "data"
  let name ← P.ident
  let parameters ← pDataParameters
  P.reservedOperator "="
  let constructors ← P.sepBy1 pConstructor (P.reservedOperator "|")
  pure fun range => Decl.dataDef range name parameters constructors.toList
where
  pDataParameters : P (List (Range × String)) := do
    let cStyleParameters ← P.optional (P.attempt pParameterList)
    match cStyleParameters with
    | some ps => pure ps
    | none => P.many (P.captureRange do let parameter ← P.ident; pure fun range => (range, parameter))

def pTypeSynonym : P (Decl .parse) := P.captureRange do
  P.reserved "type"
  let name ← P.ident
  let parameters ← pTypeSynonymParameters
  P.reservedOperator "="
  let ty ← pType
  pure fun range => Decl.typeSynonym range name parameters ty
where
  pTypeSynonymParameters : P (List String) := do
    let cStyleParameters ← P.optional (P.attempt pParameterList)
    match cStyleParameters with
    | some ps => pure (ps.map (·.2))
    | none => P.many P.ident

def pScSig : P (Decl .parse) := P.captureRange do
  P.reserved "def"
  let name ← P.choice [P.ident, P.between (P.symbol "(") (P.symbol ")") P.operator]
  P.reservedOperator ":"
  let ty ← pType
  pure fun range => Decl.scSig range name ty

def pScDef : P (Decl .parse) := P.captureRange do
  P.reserved "def"
  let name ← P.choice [P.ident, P.between (P.symbol "(") (P.symbol ")") P.operator]
  P.reservedOperator "="
  let body ← pExpr
  pure fun range => Decl.scDef range name body

def pInfix : P (Decl .parse) := P.captureRange do
  P.choice [
    do
      P.reserved "infixl"
      let prec ← P.decimal
      let op ← P.between (P.symbol "(") (P.symbol ")") P.operator
      pure fun range => Decl.«infix» range .leftA prec op,
    do
      P.reserved "infixr"
      let prec ← P.decimal
      let op ← P.between (P.symbol "(") (P.symbol ")") P.operator
      pure fun range => Decl.«infix» range .rightA prec op,
    do
      P.reserved "infix"
      let prec ← P.decimal
      let op ← P.between (P.symbol "(") (P.symbol ")") P.operator
      pure fun range => Decl.«infix» range .neutralA prec op ]

def pForeign : P (Decl .parse) := P.captureRange do
  P.reserved "foreign"
  P.reserved "import"
  let name ← P.ident
  P.reservedOperator ":"
  let ty ← pType
  pure fun range => Decl.foreign range name ty

def pImport : P (Decl .parse) := P.captureRange do
  P.reserved "module"
  let importList ← pImportList
  P.reservedOperator "="
  P.reserved "import"
  let moduleName ← pModuleName
  pure fun range => Decl.«import» range moduleName importList
where
  pImportList : P ImportList := P.choice [P.attempt pAll, pSelected, pAs]
  pAll : P ImportList := P.between (P.symbol "{") (P.symbol "}") do
    _ ← P.symbol ".."
    pure ImportList.all
  pSelected : P ImportList := P.between (P.symbol "{") (P.symbol "}") do
    let items ← P.sepBy pImportItem (P.symbol ",")
    pure (ImportList.selected items)
  pAs : P ImportList := ImportList.«as» <$> pModuleName
  pImportItem : P String := P.choice [P.ident, P.between (P.symbol "(") (P.symbol ")") P.operator]
  pModuleName : P ModuleName := asIdent <|> asPath
  asIdent : P ModuleName := ModuleName.moduleName <$> P.ident
  asPath : P ModuleName := ModuleName.rawPath <$> pStringLiteral

/-! ## Module -/

def pDecl : P (Decl .parse) := do
  _ ← P.many P.skipPragma
  P.choice [pDataDef, pTypeSynonym, pInfix, pForeign, pImport, P.attempt pScSig, pScDef]

/-- `pModule`: leading space, then declarations. The module's own name is
emitted as `rawPath` (the source name) and resolved in `Malgo.Parser.pass`. -/
def pModule : P (Module .parse) := do
  P.space
  let pos ← P.getSourcePos
  let decls ← P.many pDecl
  pure {
    moduleName := ModuleName.rawPath pos.sourceName,
    moduleDefinition := (⟨decls⟩ : ParsedDefinitions .parse) }

/-- Entry point: `pModule <* eof`. -/
def parseCStyle (sourceName : String) (input : String) : Except PError (Module .parse) :=
  P.run (do let m ← pModule; P.eof; pure m) sourceName input

end Malgo.Parser
