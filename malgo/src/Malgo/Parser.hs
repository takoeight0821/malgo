module Malgo.Parser (parseMalgo) where

import Control.Monad.Combinators.Expr
import Data.Foldable (foldl)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Void
import Koriel.Id (ModuleName (ModuleName))
import Malgo.Prelude hiding
  ( many,
    some,
    try,
  )
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | パーサー
--
-- ファイル1つにつきモジュール1つ
parseMalgo :: String -> Text -> Either (ParseErrorBundle Text Void) (Module (Malgo 'Parse))
parseMalgo = parse do
  sc
  mod <- pModule
  eof
  pure mod

-- entry point
pModule :: Parser (Module (Malgo 'Parse))
pModule = do
  void $ pKeyword "module"
  x <- pModuleName
  void $ pOperator "="
  ds <- between (symbol "{") (symbol "}") $ pDecl `endBy` pOperator ";"
  pure $ Module {_moduleName = ModuleName x, _moduleDefinition = ds}

-- module name
pModuleName :: Parser String
pModuleName = lexeme singleModuleName

singleModuleName :: Parser String
singleModuleName = some identLetter

-- toplevel declaration
pDecl :: Parser (Decl (Malgo 'Parse))
pDecl = pDataDef <|> pTypeSynonym <|> pInfix <|> pForeign <|> pImport <|> try pScSig <|> pScDef

pDataDef :: Parser (Decl (Malgo 'Parse))
pDataDef = label "toplevel type definition" $ do
  s <- getSourcePos
  void $ pKeyword "data"
  d <- upperIdent
  xs <- many lowerIdent
  void $ pOperator "="
  ts <- pConDef `sepBy` pOperator "|"
  pure $ DataDef s d xs ts
  where
    pConDef = (,) <$> upperIdent <*> many pSingleType

pTypeSynonym :: Parser (Decl (Malgo 'Parse))
pTypeSynonym = label "toplevel type synonym" do
  s <- getSourcePos
  void $ pKeyword "type"
  d <- upperIdent
  xs <- many lowerIdent
  void $ pOperator "="
  TypeSynonym s d xs <$> pType

pInfix :: Parser (Decl (Malgo 'Parse))
pInfix = label "infix declaration" $ do
  s <- getSourcePos
  a <-
    try (pKeyword "infixl" $> LeftA)
      <|> try (pKeyword "infixr" $> RightA)
      <|> (pKeyword "infix" $> NeutralA)
  i <- lexeme L.decimal
  x <- between (symbol "(") (symbol ")") operator
  pure $ Infix s a i x

pForeign :: Parser (Decl (Malgo 'Parse))
pForeign = label "foreign import" $ do
  s <- getSourcePos
  void $ pKeyword "foreign"
  void $ pKeyword "import"
  x <- lowerIdent
  void $ pOperator "::"
  Foreign s x <$> pType

pImport :: Parser (Decl (Malgo 'Parse))
pImport = label "import" $ do
  s <- getSourcePos
  void $ pKeyword "module"
  importList <-
    try (between (symbol "{") (symbol "}") (symbol ".." >> pure All))
      <|> between (symbol "{") (symbol "}") (Selected <$> (lowerIdent <|> upperIdent <|> between (symbol "(") (symbol ")") operator) `sepBy` symbol ",")
      <|> (As . ModuleName <$> pModuleName)
  void $ pOperator "="
  void $ pKeyword "import"
  modName <- ModuleName <$> pModuleName
  pure $ Import s modName importList

pScSig :: Parser (Decl (Malgo 'Parse))
pScSig =
  label "toplevel function signature" $
    ScSig
      <$> getSourcePos
      <*> (lowerIdent <|> between (symbol "(") (symbol ")") operator)
      <* pOperator "::"
      <*> pType

pScDef :: Parser (Decl (Malgo 'Parse))
pScDef =
  label "toplevel function definition" $
    ScDef
      <$> getSourcePos
      <*> (lowerIdent <|> between (symbol "(") (symbol ")") operator)
      <* pOperator "="
      <*> pExp

-- Expressions

pExp :: Parser (Exp (Malgo 'Parse))
pExp = pOpApp

pBoxed :: Parser (Literal Boxed)
pBoxed =
  label "boxed literal" $
    try (Float <$> lexeme (L.float <* string' "F"))
      <|> try (Double <$> lexeme L.float)
      <|> try (Int64 <$> lexeme (L.decimal <* string' "L"))
      <|> try (Int32 <$> lexeme L.decimal)
      <|> try (lexeme (Char <$> between (char '\'') (char '\'') L.charLiteral))
      <|> try (lexeme (String <$> (char '"' *> manyTill L.charLiteral (char '"'))))

pUnboxed :: Parser (Literal Unboxed)
pUnboxed =
  label "unboxed literal" $
    try (Double <$> lexeme (L.float <* char '#'))
      <|> try (Float <$> lexeme (L.float <* string' "F#"))
      <|> try (Int32 <$> lexeme (L.decimal <* char '#'))
      <|> try (Int64 <$> lexeme (L.decimal <* string' "L#"))
      <|> try (lexeme (Char <$> (between (char '\'') (char '\'') L.charLiteral <* char '#')))
      <|> try (lexeme (String <$> (char '"' *> manyTill L.charLiteral (char '"') <* char '#')))

pVariable :: Parser (Exp (Malgo 'Parse))
pVariable =
  label "variable" $ do
    s <- getSourcePos
    try (Var s <$> fmap (Just . ModuleName) singleModuleName <* char '.' <*> (lowerIdent <|> upperIdent))
      <|> Var s Nothing <$> (lowerIdent <|> upperIdent)

pFun :: Parser (Exp (Malgo 'Parse))
pFun =
  label "function literal" $
    between (symbol "{") (symbol "}") $
      Fn
        <$> getSourcePos
        <*> ( Clause
                <$> getSourcePos
                <*> (try (some pSinglePat <* pOperator "->") <|> pure [])
                <*> pStmts
            )
        `sepBy1` pOperator "|"

pStmts :: Parser [Stmt (Malgo 'Parse)]
pStmts = pStmt `sepBy1` pOperator ";"

pStmt :: Parser (Stmt (Malgo 'Parse))
pStmt = try pLet <|> pNoBind

pLet :: Parser (Stmt (Malgo 'Parse))
pLet = do
  void $ pKeyword "let"
  pos <- getSourcePos
  v <- lowerIdent
  void $ pOperator "="
  Let pos v <$> pExp

pNoBind :: Parser (Stmt (Malgo 'Parse))
pNoBind = NoBind <$> getSourcePos <*> pExp

pRecordP :: Parser (Pat (Malgo 'Parse))
pRecordP = between (symbol "{") (symbol "}") do
  s <- getSourcePos
  kvs <- pRecordPEntry `sepBy1` pOperator ","
  pure $ RecordP s kvs
  where
    pRecordPEntry = do
      label <- lowerIdent
      void $ pOperator ":"
      value <- pPat
      pure (label, value)

pSinglePat :: Parser (Pat (Malgo 'Parse))
pSinglePat =
  VarP <$> getSourcePos <*> lowerIdent
    <|> ConP <$> getSourcePos <*> upperIdent <*> pure []
    <|> UnboxedP <$> getSourcePos <*> pUnboxed
    <|> try
      ( between
          (symbol "(")
          (symbol ")")
          ( TupleP <$> getSourcePos <*> do
              x <- pPat
              void $ pOperator ","
              xs <- pPat `sepBy` pOperator ","
              pure $ x : xs
          )
      )
    <|> pRecordP
    <|> between
      (symbol "[")
      (symbol "]")
      (ListP <$> getSourcePos <*> pPat `sepBy1` pOperator ",")
    <|> between (symbol "(") (symbol ")") pPat

pPat :: Parser (Pat (Malgo 'Parse))
pPat =
  label "pattern" $ try (ConP <$> getSourcePos <*> upperIdent <*> some pSinglePat) <|> pSinglePat

pTuple :: Parser (Exp (Malgo 'Parse))
pTuple = label "tuple" $
  between (symbol "(") (symbol ")") $ do
    s <- getSourcePos
    x <- pExp
    void $ pOperator ","
    xs <- pExp `sepBy` pOperator ","
    pure $ Tuple s (x : xs)

pUnit :: Parser (Exp (Malgo 'Parse))
pUnit = between (symbol "(") (symbol ")") $ do
  s <- getSourcePos
  pure $ Tuple s []

pRecord :: Parser (Exp (Malgo 'Parse))
pRecord = between (symbol "{") (symbol "}") do
  s <- getSourcePos
  kvs <- pRecordEntry `sepBy1` pOperator ","
  pure $ Record s kvs
  where
    pRecordEntry = do
      label <- lowerIdent
      void $ pOperator ":"
      value <- pExp
      pure (label, value)

pList :: Parser (Exp (Malgo 'Parse))
pList = label "list" $
  between (symbol "[") (symbol "]") $ do
    s <- getSourcePos
    xs <- pExp `sepBy1` pOperator ","
    pure $ List s xs

pRecordAccess :: Parser (Exp (Malgo 'Parse))
pRecordAccess = do
  s <- getSourcePos
  l <- char '#' >> lowerIdent
  pure $ RecordAccess s l

pSingleExp' :: Parser (Exp (Malgo 'Parse))
pSingleExp' =
  try (Unboxed <$> getSourcePos <*> pUnboxed)
    <|> try (Boxed <$> getSourcePos <*> pBoxed)
    <|> pVariable
    <|> try pUnit
    <|> try pTuple
    <|> try pRecord
    <|> try pList
    <|> pList
    <|> pFun
    <|> pRecordAccess
    <|> between (symbol "(") (symbol ")") (Parens <$> getSourcePos <*> pExp)

pSingleExp :: Parser (Exp (Malgo 'Parse))
pSingleExp = try (Force <$> getSourcePos <* pOperator "!" <*> pSingleExp') <|> pSingleExp'

pApply :: Parser (Exp (Malgo 'Parse))
pApply = do
  s <- getSourcePos
  f <- pSingleExp
  xs <- some pSingleExp
  pure $ foldl (Apply s) f xs

pTerm :: Parser (Exp (Malgo 'Parse))
pTerm = try pApply <|> pSingleExp

pOpApp :: Parser (Exp (Malgo 'Parse))
pOpApp = makeExprParser pTerm opTable
  where
    opTable =
      [ [ InfixL $ do
            s <- getSourcePos
            op <- operator
            pure $ \l r -> OpApp s op l r
        ]
      ]

-- Types

pType :: Parser (Type (Malgo 'Parse))
pType = try pTyArr <|> pTyTerm

pTyVar :: Parser (Type (Malgo 'Parse))
pTyVar = label "type variable" $ TyVar <$> getSourcePos <*> lowerIdent

pTyCon :: Parser (Type (Malgo 'Parse))
pTyCon = label "type constructor" $ TyCon <$> getSourcePos <*> upperIdent

pTyTuple :: Parser (Type (Malgo 'Parse))
pTyTuple = between (symbol "(") (symbol ")") $ do
  s <- getSourcePos
  x <- pType
  void $ pOperator ","
  xs <- pType `sepBy` pOperator ","
  pure $ TyTuple s (x : xs)

pTyUnit :: Parser (Type (Malgo 'Parse))
pTyUnit = between (symbol "(") (symbol ")") $ do
  s <- getSourcePos
  pure $ TyTuple s []

pTyRecord :: Parser (Type (Malgo 'Parse))
pTyRecord = between (symbol "{") (symbol "}") do
  s <- getSourcePos
  kvs <- pTyRecordEntry `sepBy1` pOperator ","
  pure $ TyRecord s kvs
  where
    pTyRecordEntry = do
      label <- lowerIdent
      void $ pOperator ":"
      value <- pType
      pure (label, value)

pTyLazy :: Parser (Type (Malgo 'Parse))
pTyLazy = between (symbol "{") (symbol "}") $ TyLazy <$> getSourcePos <*> pType

pSingleType :: Parser (Type (Malgo 'Parse))
pSingleType =
  pTyVar
    <|> pTyCon
    <|> try pTyUnit
    <|> try pTyTuple
    <|> try pTyRecord
    <|> pTyLazy
    <|> between (symbol "(") (symbol ")") pType

pTyApp :: Parser (Type (Malgo 'Parse))
pTyApp = TyApp <$> getSourcePos <*> pSingleType <*> some pSingleType

pTyTerm :: Parser (Type (Malgo 'Parse))
pTyTerm = try pTyApp <|> pSingleType

pTyArr :: Parser (Type (Malgo 'Parse))
pTyArr = makeExprParser pTyTerm opTable
  where
    opTable =
      [ [ InfixR $ do
            s <- getSourcePos
            void $ pOperator "->"
            pure $ \l r -> TyArr s l r
        ]
      ]

-- combinators

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

identLetter :: Parser Char
identLetter = alphaNumChar <|> oneOf ("_#'" :: String)

opLetter :: Parser Char
opLetter = oneOf ("+-*/\\%=><:;|&!#." :: String)

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy identLetter)

pOperator :: Text -> Parser Text
pOperator op = lexeme (string op <* notFollowedBy opLetter)

reserved :: Parser Text
reserved =
  choice $
    map
      (try . pKeyword)
      [ "data",
        "exists",
        "forall",
        "foreign",
        "import",
        "infix",
        "infixl",
        "infixr",
        "let",
        "type",
        "module"
      ]

reservedOp :: Parser Text
reservedOp = choice $ map (try . pOperator) ["=", "::", "|", "->", ";", ",", "!"]

lowerIdent :: Parser String
lowerIdent =
  label "lower identifier" $
    lexeme do
      notFollowedBy reserved <|> notReserved
      (:) <$> (lowerChar <|> char '_') <*> many identLetter

upperIdent :: Parser String
upperIdent =
  label "upper identifier" $
    lexeme do
      notFollowedBy reserved <|> notReserved
      (:) <$> upperChar <*> many identLetter

operator :: Parser String
operator =
  label "operator" $
    lexeme do
      notFollowedBy reservedOp
      some opLetter

notReserved :: Parser ()
notReserved = do
  word <- lookAhead reserved
  registerFancyFailure (Set.singleton $ ErrorFail $ "unexpected '" <> Text.unpack word <> "'\nThis is a reserved keyword")