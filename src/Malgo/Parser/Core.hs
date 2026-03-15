module Malgo.Parser.Core
  ( -- * Parser Type and Effects
    Parser,

    -- * Common Parsing Functions
    skipPragma,

    -- * Lexical Analysis
    space,
    lexeme,
    decimal,
    symbol,
    ident,
    identStart,
    identContinue,
    reserved,
    anyReserved,
    reservedKeywords,
    operator,
    operatorChar,
    reservedOperator,
    anyReservedOperator,
    reservedOperators,

    -- * Combinators
    optional,
    manyUnaryOp,
    captureRange,
  )
where

import Data.Text.Lazy qualified as TL
import Effectful
import Malgo.Prelude hiding (All)
import Text.Megaparsec hiding (optional, parse)
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L

-- | Parser type used across all parser modules
type Parser es = ParsecT Void TL.Text (Eff es)

-- | skipPragma skips a pragma.
--
-- > pragma = "#" any* "\n" ;
skipPragma :: Parser es ()
skipPragma = lexeme do
  void $ char '#'
  void $ takeWhileP (Just "pragma") (/= '\n')

-- * Lexical Analysis

-- | space skips zero or more white space characters and comments.
space :: Parser es ()
space = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | lexeme consumes the given parser and skips trailing white space.
lexeme :: Parser es a -> Parser es a
lexeme = L.lexeme space

-- | decimal consumes an integer.
decimal :: Parser es Int
decimal = lexeme L.decimal

-- | symbol consumes the given string.
symbol :: TL.Text -> Parser es ()
symbol = void . L.symbol space

-- | ident consumes an identifier.
ident :: Parser es Text
ident = lexeme do
  notFollowedBy anyReserved
  TL.toStrict . TL.pack <$> ((:) <$> identStart <*> many identContinue)

-- TODO: use XID_Start
identStart :: Parser es Char
identStart = letterChar <|> char '_'

-- TODO: use XID_Continue
identContinue :: Parser es Char
identContinue = alphaNumChar <|> char '_' <|> char '#'

reserved :: TL.Text -> Parser es ()
reserved w
  | w `elem` reservedKeywords = void $ lexeme (string w <* notFollowedBy identContinue)
  | otherwise = fail $ "reserved keyword: " <> show w

anyReserved :: Parser es ()
anyReserved = choice $ map (try . reserved) reservedKeywords

reservedKeywords :: [TL.Text]
reservedKeywords =
  [ "class",
    "def",
    "data",
    "exists",
    "forall",
    "foreign",
    "goto",
    "impl",
    "import",
    "infix",
    "infixl",
    "infixr",
    "label",
    "let",
    "type",
    "module",
    "with"
  ]

operator :: Parser es Text
operator = lexeme do
  notFollowedBy anyReservedOperator
  convertString <$> some operatorChar

operatorChar :: Parser es Char
operatorChar = oneOf ("+-*/\\%=><:;|&!#." :: String)

reservedOperator :: TL.Text -> Parser es ()
reservedOperator w
  | w `elem` reservedOperators = void $ lexeme (string w <* notFollowedBy operatorChar)
  | otherwise = fail $ "reserved symbol: " <> show w

anyReservedOperator :: Parser es ()
anyReservedOperator = choice $ map (try . reservedOperator) reservedOperators

reservedOperators :: [TL.Text]
reservedOperators = ["=>", "=", ":", "|", "->", ";", ".", ",", "!", "#|", "|#", "~"]

-- * Combinators

-- | optional tries to parse the given parser and returns `Nothing` if it fails.
optional :: Parser es a -> Parser es (Maybe a)
optional p = try (fmap Just p) <|> pure Nothing

-- | manyUnaryOp parses zero or more unary operators and returns a function that applies them in order.
manyUnaryOp :: (MonadPlus f) => f (c -> c) -> f (c -> c)
manyUnaryOp singleUnaryOp = foldr1 (>>>) <$> some singleUnaryOp

-- | captureRange captures the source range of the parsed text.
captureRange :: (MonadParsec e s m, TraversableStream s) => m (Range -> b) -> m b
captureRange action = do
  start <- getSourcePos
  result <- action
  result . Range start <$> getSourcePos
