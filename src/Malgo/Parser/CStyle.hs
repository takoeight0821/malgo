module Malgo.Parser.CStyle (parseCStyle) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Trans (lift)
import Data.Char (isUpper)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effectful (Eff, IOE, type (:>))
import Malgo.Features (Features)
import Malgo.Module (ModuleName (..), Workspace, parseArtifactPath, pwdPath)
import Malgo.Parser.Core
  ( Parser,
    captureRange,
    decimal,
    ident,
    identContinue,
    lexeme,
    manyUnaryOp,
    operator,
    optional,
    reserved,
    reservedOperator,
    skipPragma,
    space,
    symbol,
  )
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec hiding (Label, optional, parse)
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L

-- * Entry Point

-- | parseCStyle parses C-style apply syntax with parentheses and braces
parseCStyle :: (IOE :> es, Workspace :> es, Features :> es) => FilePath -> TL.Text -> Eff es (Either (ParseErrorBundle TL.Text Void) (Module (Malgo Parse)))
parseCStyle srcPath text = do
  runParserT (pModule <* eof) srcPath text

-- * Module and Declaration Parsing

-- | pModule parses a complete module using C-style syntax
pModule :: (IOE :> es, Workspace :> es, Features :> es) => Parser es (Module (Malgo Parse))
pModule = do
  space -- consume leading whitespace and comments
  sourcePath <- (.sourceName) <$> getSourcePos
  pwd <- lift pwdPath
  sourcePath <- lift $ parseArtifactPath pwd sourcePath
  decls <- many pDecl
  pure
    Module
      { moduleName = Artifact sourcePath,
        moduleDefinition = ParsedDefinitions decls
      }

-- | pDecl parses declarations with C-style expression syntax
pDecl :: (IOE :> es, Workspace :> es, Features :> es) => Parser es (Decl (Malgo Parse))
pDecl = do
  _ <- many skipPragma
  choice
    [ pDataDef,
      pTypeSynonym,
      pInfix,
      pForeign,
      pImport,
      try pScSig, -- must be before pScDef
      pScDef
    ]

-- * Declaration Parsers

-- | pDataDef parses C-style data definitions with parenthesized parameters
-- > cStyleDataDef = "data" ident "(" (ident ("," ident)*)? ")" "=" constructor ("|" constructor)* ;
-- > constructor = ident "(" (atomType ("," atomType)*)? ")" ;
pDataDef :: Parser es (Decl (Malgo Parse))
pDataDef = captureRange do
  reserved "data"
  name <- ident
  parameters <- fromMaybe [] <$> optional pParameterList
  reservedOperator "="
  constructors <- sepBy1 pConstructor (reservedOperator "|")
  pure $ \range -> DataDef range name parameters constructors
  where
    pConstructor = captureRange do
      name <- ident
      parameters <- fromMaybe [] <$> optional (try $ between (symbol "(") (symbol ")") (sepBy pType (symbol ",")))
      pure (,name,parameters)

-- | pTypeSynonym parses C-style type synonyms with parenthesized parameters
-- > cStyleTypeSynonym = "type" ident "(" (ident ("," ident)*)? ")" "=" type ;
pTypeSynonym :: Parser es (Decl (Malgo Parse))
pTypeSynonym = captureRange do
  reserved "type"
  name <- ident
  parameters <- map snd <$> pParameterList
  reservedOperator "="
  ty <- pType
  pure $ \range -> TypeSynonym range name parameters ty

-- | pParameterList parses comma-separated parameters in parentheses
pParameterList :: Parser es [(Range, Text)]
pParameterList = between (symbol "(") (symbol ")") (sepBy pParameter (symbol ","))
  where
    pParameter = captureRange do
      param <- ident
      pure (,param)

-- | pScSig parses a value signature.
--
-- > scSig = "def" (ident | "(" operator")")":" type ;
pScSig :: Parser es (Decl (Malgo Parse))
pScSig = captureRange do
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator ":"
  ty <- pType
  pure $ \range -> ScSig range name ty

-- | pScDef parses value definitions with C-style expression syntax
pScDef :: (Features :> es) => Parser es (Decl (Malgo Parse))
pScDef = captureRange do
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator "="
  body <- pExpr
  pure $ \range -> ScDef range name body

-- | pInfix parses an infix declaration.
--
-- > infix = "infixl" decimal operator | "infixr" decimal operator | "infix" decimal operator ;
pInfix :: Parser es (Decl (Malgo Parse))
pInfix = captureRange do
  choice
    [ do
        reserved "infixl"
        precedence <- decimal
        operator <- between (symbol "(") (symbol ")") operator
        pure $ \range -> Infix range LeftA precedence operator,
      do
        reserved "infixr"
        precedence <- decimal
        operator <- between (symbol "(") (symbol ")") operator
        pure $ \range -> Infix range RightA precedence operator,
      do
        reserved "infix"
        precedence <- decimal
        operator <- between (symbol "(") (symbol ")") operator
        pure $ \range -> Infix range NeutralA precedence operator
    ]

-- | pForeign parses a foreign declaration.
--
-- > foreign = "foreign" "import" ident ":" type ;
pForeign :: Parser es (Decl (Malgo Parse))
pForeign = captureRange do
  reserved "foreign"
  reserved "import"
  name <- ident
  reservedOperator ":"
  ty <- pType
  pure $ \range -> Foreign range name ty

-- | pImport parses an import declaration.
--
-- > import = "module" importList "=" "import" moduleName
-- > importList = "{" ".." "}"
-- >            | "{" importItem ("," importItem)* "}"
-- >            | moduleName ;
-- > importItem = ident | "(" operator ")" ;
pImport :: (IOE :> es, Workspace :> es) => Parser es (Decl (Malgo Parse))
pImport = captureRange do
  reserved "module"
  importList <- pImportList
  reservedOperator "="
  reserved "import"
  moduleName <- pModuleName
  pure $ \range -> Import range moduleName importList
  where
    pImportList = choice [try pAll, pSelected, pAs]
    pAll = between (symbol "{") (symbol "}") do
      symbol ".."
      pure Malgo.Syntax.Extension.All
    pSelected = between (symbol "{") (symbol "}") do
      items <- sepBy pImportItem (symbol ",")
      pure $ Selected items
    pAs = As <$> pModuleName
    pImportItem = choice [ident, between (symbol "(") (symbol ")") operator]
    pModuleName = asIdent <|> asPath
    asIdent = ModuleName <$> ident
    asPath = do
      path <- pStringLiteral
      sourcePath <- (.sourceName) <$> getSourcePos
      pwd <- lift pwdPath
      sourcePath <- lift $ parseArtifactPath pwd sourcePath
      path' <- lift $ parseArtifactPath sourcePath path
      pure $ Artifact path'

-- * Type Parsers

-- | pType parses a type.
--
-- > type = tyapp "->" type
-- >      | tyapp ;
pType :: Parser es (Type (Malgo Parse))
pType = makeExprParser pTyApp table
  where
    table =
      [ [ InfixR do
            start <- getSourcePos
            reservedOperator "->"
            TyArr . Range start <$> getSourcePos
        ]
      ]

-- | pTyApp parses a type application.
--
-- > tyapp = atomType atomType* ;
pTyApp :: Parser es (Type (Malgo Parse))
pTyApp = captureRange do
  ty <- pAtomType
  tys <- many pAtomType
  pure $ \range -> case tys of
    [] -> ty
    _ -> TyApp range ty tys

-- | pAtomType parses an atomic type.
--
-- > atomType = tyBottom | tyTilde | tyVar | tyTuple | tyRecord | tyVariant | tyBlock | tyCStyleTuple
pAtomType :: Parser es (Type (Malgo Parse))
pAtomType = choice [try pTyBottom, pTyTilde, pTyVar, pTyTuple, try pTyRecord, pTyVariant, try pTyBlock, pTyCStyleTuple]

-- | pTyVar parses a type variable.
--
-- > tyVar = ident ;
pTyVar :: Parser es (Type (Malgo Parse))
pTyVar = captureRange do
  name <- ident
  pure $ \range -> TyVar range name

-- | pTyTuple parses a tuple type or parenthesized type.
--
-- > tyTuple = "(" type ("," type)* ")"
-- >         | "(" ")" ;
pTyTuple :: Parser es (Type (Malgo Parse))
pTyTuple = captureRange do
  tys <- between (symbol "(") (symbol ")") (sepBy pType (symbol ","))
  pure $ \range -> case tys of
    [ty] -> ty
    _ -> TyTuple range tys

-- | pTyRecord parses a record type with optional row tail.
--
-- > tyRecord = "{" field ("," field)* ["|" type] "}" ;
-- > field = ident ":" type ;
pTyRecord :: Parser es (Type (Malgo Parse))
pTyRecord = captureRange do
  (fields, rowTail) <- between (symbol "{") (symbol "}") do
    fields <- sepEndBy1 pField (symbol ",")
    rowTail <- optional (reservedOperator "|" >> pType)
    pure (fields, rowTail)
  pure $ \range -> TyRecord range fields rowTail
  where
    pField = do
      field <- ident
      reservedOperator ":"
      value <- pType
      pure (field, value)

-- | pTyBottom parses the bottom type.
--
-- > tyBottom = "_|_" ;
pTyBottom :: Parser es (Type (Malgo Parse))
pTyBottom = captureRange do
  lexeme (string "_|_" <* notFollowedBy identContinue)
  pure $ \range -> TyBottom range

-- | pTyTilde parses a tilde (covalue) type.
--
-- > tyTilde = "~" atomType ;
pTyTilde :: Parser es (Type (Malgo Parse))
pTyTilde = captureRange do
  reservedOperator "~"
  ty <- pAtomType
  pure $ \range -> TyTilde range ty

-- | pTyVariant parses a variant type with optional row tail.
--
-- > tyVariant = "[" case ("|" case)* ["|" type] "]" ;
-- > case = UPPER_IDENT atomType* ;
pTyVariant :: Parser es (Type (Malgo Parse))
pTyVariant = captureRange do
  between (symbol "[") (symbol "]") do
    first <- pVariantCase
    rest <- many (try (reservedOperator "|" *> pVariantCase))
    rowTail <- optional (reservedOperator "|" *> pType)
    pure $ \range -> TyVariant range (first : rest) rowTail
  where
    pVariantCase = do
      name <- try do
        n <- ident
        guard (not (T.null n) && isUpper (T.head n))
        pure n
      args <- many pAtomType
      pure (name, args)

-- | pTyCStyleTuple parses C-style tuple types with braces
-- > tyCStyleTuple = "{" type ("," type)* "}"
-- >               | "{" "}" ;
pTyCStyleTuple :: Parser es (Type (Malgo Parse))
pTyCStyleTuple = captureRange do
  tys <- between (symbol "{") (symbol "}") (sepBy pType (symbol ","))
  pure $ \range -> case tys of
    [ty] -> ty
    _ -> TyTuple range tys

-- | pTyBlock parses a block type.
--
-- > tyBlock = "{" type "}" ;
pTyBlock :: Parser es (Type (Malgo Parse))
pTyBlock = captureRange do
  ty <- between (symbol "{") (symbol "}") pType
  pure $ \range -> TyBlock range ty

-- * Expression Parsers

-- | pExpr parses expressions with C-style syntax
pExpr :: (Features :> es) => Parser es (Expr (Malgo Parse))
pExpr = do
  expr <- pOpApp
  -- try parse a type annotation
  try (pAnn expr) <|> pure expr
  where
    pAnn expr = captureRange do
      reservedOperator ":"
      ty <- pType
      pure $ \range -> Ann range expr ty

-- | pOpApp parses operator application with C-style syntax
pOpApp :: (Features :> es) => Parser es (Expr (Malgo Parse))
pOpApp = makeExprParser pApply table
  where
    table =
      [ [ InfixL do
            captureRange do
              op <- operator
              pure $ \range -> OpApp range op
        ]
      ]

-- | pApply parses C-style function application and field projection with chaining
pApply :: (Features :> es) => Parser es (Expr (Malgo Parse))
pApply =
  makeExprParser
    pAtom
    [ [ Postfix $ manyUnaryOp do
          choice
            [ -- Function application: expr(arg1, arg2, ...)
              -- Empty argument list () is treated as ({})
              captureRange do
                args <- between (symbol "(") (symbol ")") (sepBy pExpr (symbol ","))
                pure $ \range fn ->
                  let actualArgs = if null args then [Tuple range []] else args
                   in foldl (Apply range) fn actualArgs,
              -- Field projection: expr.field
              captureRange do
                reservedOperator "."
                field <- ident
                pure $ \range record -> Project range record field
            ]
      ]
    ]

-- | pAtom parses atoms with C-style syntax
pAtom :: (Features :> es) => Parser es (Expr (Malgo Parse))
pAtom =
  choice
    [ pLabel,
      pGoto,
      pLiteral,
      pVariable,
      try pTuple,
      try pRecord,
      pBrace,
      pList,
      pSeq
    ]

-- | pLabel parses a label expression.
--
-- > label = "label" ident expr ;
pLabel :: (Features :> es) => Parser es (Expr (Malgo Parse))
pLabel = captureRange do
  reserved "label"
  name <- ident
  body <- pExpr
  pure $ \range -> Label range name body

-- | pGoto parses a goto expression.
--
-- > goto = "goto" "(" expr "," expr ")" ;
pGoto :: (Features :> es) => Parser es (Expr (Malgo Parse))
pGoto = captureRange do
  reserved "goto"
  (value, lbl) <- between (symbol "(") (symbol ")") do
    value <- pExpr
    symbol ","
    lbl <- pExpr
    pure (value, lbl)
  pure $ \range -> Goto range value lbl

-- | pTuple parses C-style tuple syntax with braces
-- > tuple = "{" expr ("," expr)+ "}" | "{" "}"
pTuple :: (Features :> es) => Parser es (Expr (Malgo Parse))
pTuple = captureRange do
  exprs <- between (symbol "{") (symbol "}") (sepBy pExpr (symbol ","))
  case exprs of
    [_] -> fail "c-style tuple must have at least two expressions or be empty"
    _ -> pure $ \range -> Tuple range exprs

-- | pRecord parses record syntax (separate from tuples in C-style)
pRecord :: (Features :> es) => Parser es (Expr (Malgo Parse))
pRecord = captureRange do
  fields <- between (symbol "{") (symbol "}") $ sepEndBy1 pField (symbol ",")
  pure $ \range -> Record range fields
  where
    pField = label "record field" do
      field <- ident
      reservedOperator "="
      value <- pExpr
      pure (field, value)

-- | pBrace parses function syntax with C-style clauses or copatterns
pBrace :: (Features :> es) => Parser es (Expr (Malgo Parse))
pBrace = captureRange do
  content <-
    between (symbol "{") (symbol "}")
      $ choice
        [ try pCodata,
          pFn
        ]
  pure $ \range -> content range
  where
    pFn = do
      clauses <- sepEndBy1 pClause (symbol ",")
      pure $ \range -> Fn range $ NonEmpty.fromList clauses

-- | pCodata parses codata expressions using C-style syntax
pCodata :: (Features :> es) => Parser es (Range -> Expr (Malgo Parse))
pCodata = do
  clauses <- sepEndBy1 pCodataClause (symbol ",")
  pure $ \range -> Codata range clauses
  where
    pCodataClause = do
      cp <- pCopattern
      reservedOperator "->"
      e <- pExpr
      pure (cp, e)

-- | pCopattern parses copatterns starting with # using C-style syntax
pCopattern :: (Features :> es) => Parser es (CoPat (Malgo Parse))
pCopattern = captureRange do
  symbol "#"
  pCopatternSuffix HoleP

-- | pCopatternSuffix parses copattern suffixes (projections and applications) using C-style syntax
pCopatternSuffix :: (Features :> es) => (Range -> CoPat (Malgo Parse)) -> Parser es (Range -> CoPat (Malgo Parse))
pCopatternSuffix cp =
  choice
    [ try do
        reservedOperator "."
        field <- ident
        pCopatternSuffix (\range -> ProjectP range (cp range) field),
      try do
        parenStart <- getSourcePos
        symbol "("
        -- Parse C-style pattern arguments (comma-separated in parentheses)
        -- Empty argument list () is treated as ({})
        pats <- sepBy pPat (symbol ",")
        symbol ")"
        parenEnd <- getSourcePos
        -- For C-style, we need to handle multiple patterns differently
        -- Apply each pattern as a separate ApplyP
        -- Empty pattern list () is treated as ({})
        let parenRange = Range parenStart parenEnd
            actualPats = if null pats then [TupleP parenRange []] else pats
            applyPats pat_list copat = case pat_list of
              [] -> copat
              (p : ps) -> applyPats ps (\range -> ApplyP range (copat range) p)
        pCopatternSuffix (applyPats actualPats cp),
      pure cp
    ]

-- | pList parses list literals using C-style syntax
pList :: (Features :> es) => Parser es (Expr (Malgo Parse))
pList = between (symbol "[") (symbol "]") do
  captureRange do
    elements <- sepEndBy pExpr (symbol ",")
    pure $ \range -> List range elements

-- | pSeq parses parenthesized sequences using C-style syntax
pSeq :: (Features :> es) => Parser es (Expr (Malgo Parse))
pSeq = between (symbol "(") (symbol ")") pStmts

-- * Statement Parsers

-- | pStmts parses statements using C-style syntax
pStmts :: (Features :> es) => Parser es (Expr (Malgo Parse))
pStmts = captureRange do
  stmts <- sepEndBy1 pStmt (symbol ";")
  pure $ \range -> Seq range $ NonEmpty.fromList stmts

-- | pStmt parses statements using C-style syntax
pStmt :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pStmt = pLet <|> pWith <|> pNoBind

-- | pLet parses let statements using C-style syntax
pLet :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pLet = captureRange do
  reserved "let"
  name <- ident
  reservedOperator "="
  body <- pExpr
  pure $ \range -> Let range name body

-- | pWith parses with statements using C-style syntax
pWith :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pWith = captureRange do
  reserved "with"
  choice
    [ try do
        name <- ident
        reservedOperator "="
        body <- pExpr
        pure $ \range -> With range (Just name) body,
      do
        body <- pExpr
        pure $ \range -> With range Nothing body
    ]

-- | pNoBind parses no-bind statements using C-style syntax
pNoBind :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pNoBind = captureRange do
  body <- pExpr
  pure $ \range -> NoBind range body

-- * Pattern Parsers

-- | pPat parses patterns with C-style syntax
pPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pPat = try pConP <|> pAtomPat

-- | pAtomPat parses atomic patterns with C-style syntax
pAtomPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pAtomPat =
  choice
    [ pVarP,
      pLiteralP,
      try pTupleP,
      pRecordP,
      pListP
    ]

-- | pConP parses constructor patterns with C-style syntax
pConP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pConP = captureRange do
  constructor <- ident
  patterns <- between (symbol "(") (symbol ")") (sepEndBy1 pPat (symbol ","))
  pure $ \range -> ConP range constructor patterns

-- | pTupleP parses C-style tuple patterns with braces
pTupleP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pTupleP = captureRange do
  patterns <- between (symbol "{") (symbol "}") (sepBy pPat (symbol ","))
  case patterns of
    [_] -> fail "c-style tuple must have at least two patterns or be empty"
    _ -> pure $ \range -> TupleP range patterns

-- | pRecordP parses record patterns using C-style syntax
pRecordP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pRecordP = captureRange do
  fields <- between (symbol "{") (symbol "}") $ sepEndBy1 pField (symbol ",")
  pure $ \range -> RecordP range fields
  where
    pField = do
      field <- ident
      reservedOperator "="
      value <- pPat
      pure (field, value)

-- | pListP parses list patterns using C-style syntax
pListP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pListP = captureRange do
  pats <- between (symbol "[") (symbol "]") $ sepEndBy pPat (symbol ",")
  pure $ \range -> ListP range pats

-- | pVarP parses a variable pattern.
--
-- > varPat = ident ;
pVarP :: Parser es (Pat (Malgo Parse))
pVarP = captureRange do
  name <- ident
  pure $ \range -> VarP range name

-- * Literal Parsers

-- | pLiteral parses a literal.
--
-- > literal = boxed ["#"] ;
-- > boxed = double | float | int | long | char | string ;
pLiteral :: Parser es (Expr (Malgo Parse))
pLiteral = captureRange do
  boxed <- pBoxed
  sharp <- optional (symbol "#")
  pure $ \range -> case sharp of
    Just _ -> Unboxed range $ coerce boxed
    Nothing -> Boxed range boxed

-- | pLiteralP parses a literal pattern.
pLiteralP :: Parser es (Pat (Malgo Parse))
pLiteralP = captureRange do
  boxed <- pBoxed
  sharp <- optional (symbol "#")
  pure $ \range -> case sharp of
    Just _ -> UnboxedP range $ coerce boxed
    Nothing -> BoxedP range boxed

pBoxed :: Parser es (Literal Boxed)
pBoxed = choice [try pReal, pInt, pChar, pString]

-- | pReal parses a real number.
--
-- > real = double | float ;
-- > double = FLOAT ;
-- > float = FLOAT "f" | FLOAT "F" ;
pReal :: Parser es (Literal Boxed)
pReal = lexeme do
  f <- L.float
  tail <- optional (string "f32" <|> string "f64")
  case tail of
    Just "f32" -> pure $ Float (realToFrac f)
    _ -> pure $ Double f

-- | pInt parses an integer.
--
-- > int = int32| int64 ;
-- > int32 = DECIMAL | DECIMAL "i32" ;
-- > int64 = DECIMAL "i64" ;
pInt :: Parser es (Literal Boxed)
pInt = lexeme do
  i <- L.decimal
  tail <- optional (string "i32" <|> string "i64")
  case tail of
    Just "i64" -> pure $ Int64 i
    _ -> pure $ Int32 (fromIntegral i)

-- | pChar parses a character.
--
-- > char = "'" CHAR "'" ;
pChar :: Parser es (Literal Boxed)
pChar = lexeme do
  void $ char '\''
  c <- L.charLiteral
  void $ char '\''
  pure $ Char c

-- | pString parses a string.
--
-- > string = "\"" CHAR* "\"" ;
pString :: Parser es (Literal Boxed)
pString = String <$> pStringLiteral

pStringLiteral :: (ConvertibleStrings String a) => Parser es a
pStringLiteral = lexeme do
  void $ char '"'
  str <- manyTill L.charLiteral (char '"')
  pure $ convertString str

-- * Utility Functions

-- | pVariable parses a variable.
--
-- > variable = ident ;
pVariable :: Parser es (Expr (Malgo Parse))
pVariable = captureRange do
  name <- ident
  pure $ \range -> Var range name

-- | pClause parses C-style clauses with optional parentheses
-- > clause = "(" pattern ("," pattern)* ")" "->" stmts
pClause :: (Features :> es) => Parser es (Clause (Malgo Parse))
pClause = captureRange do
  patterns <- between (symbol "(") (symbol ")") (sepEndBy pPat (symbol ",")) <* reservedOperator "->"
  body <- pStmts
  pure $ \range -> case patterns of
    [] -> Clause range (NE.singleton (VarP range "_")) body
    _ -> Clause range (NE.fromList patterns) body
