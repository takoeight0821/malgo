{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Token
  ( Token (..),
    Tag (..),
    _sourcePos,
    _tag,
  )
where

import Koriel.Prelude
import Language.Malgo.Pretty
import Text.Parsec.Pos (SourcePos)
import qualified Text.PrettyPrint.HughesPJClass as P

data Tag
  = LET
  | IN
  | VAL
  | FUN
  | TYPE
  | EXTERN
  | AND
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | COMMA
  | COLON
  | SEMICOLON
  | EQUAL
  | COLON_EQUAL
  | FN
  | IF
  | THEN
  | ELSE
  | DOT
  | PLUS
  | PLUS_DOT
  | MINUS
  | MINUS_DOT
  | ASTERISK
  | ASTERISK_DOT
  | SLASH
  | SLASH_DOT
  | PERCENT
  | ARROW
  | EQUAL_EQUAL
  | NEQ
  | LT
  | GT
  | LE
  | GE
  | AND_AND
  | OR_OR
  | ARRAY
  | LARROW
  | BAR
  | DARROW
  | MATCH
  | WITH
  | ID
      { _id :: String
      }
  | LID
      { _lid :: String
      }
  | INT
      { _int :: Integer
      }
  | FLOAT
      { _float :: Double
      }
  | BOOL
      { _bool :: Bool
      }
  | CHAR
      { _char :: Char
      }
  | STRING
      { _str :: String
      }
  | TY_INT
  | TY_FLOAT
  | TY_BOOL
  | TY_CHAR
  | TY_STRING
  deriving stock (Eq, Show)

newtype Token
  = Token (SourcePos, Tag)
  deriving stock (Eq, Show)

_sourcePos :: Token -> SourcePos
_sourcePos (Token a) = fst a

_tag :: Token -> Tag
_tag (Token a) = snd a

instance Pretty Token where
  pPrint (Token (p, t)) = pPrint p P.$+$ pPrint t

instance Pretty Tag where
  pPrint LET = "let"
  pPrint IN = "in"
  pPrint VAL = "val"
  pPrint FUN = "fun"
  pPrint TYPE = "type"
  pPrint EXTERN = "extern"
  pPrint AND = "and"
  pPrint LPAREN = "("
  pPrint RPAREN = ")"
  pPrint LBRACK = "["
  pPrint RBRACK = "]"
  pPrint LBRACE = "{"
  pPrint RBRACE = "}"
  pPrint COMMA = ","
  pPrint COLON = ":"
  pPrint SEMICOLON = ";"
  pPrint EQUAL = "="
  pPrint COLON_EQUAL = ":="
  pPrint FN = "fn"
  pPrint IF = "if"
  pPrint THEN = "then"
  pPrint ELSE = "else"
  pPrint DOT = "."
  pPrint PLUS = "+"
  pPrint PLUS_DOT = "+."
  pPrint MINUS = "-"
  pPrint MINUS_DOT = "-."
  pPrint ASTERISK = "*"
  pPrint ASTERISK_DOT = "*."
  pPrint SLASH = "/"
  pPrint SLASH_DOT = "/."
  pPrint PERCENT = "%"
  pPrint ARROW = "->"
  pPrint EQUAL_EQUAL = "=="
  pPrint NEQ = "/="
  pPrint Language.Malgo.Token.LT = "<"
  pPrint Language.Malgo.Token.GT = ">"
  pPrint LE = "<="
  pPrint GE = ">="
  pPrint AND_AND = "&&"
  pPrint OR_OR = "||"
  pPrint ARRAY = "array"
  pPrint LARROW = "<-"
  pPrint BAR = "|"
  pPrint DARROW = "=>"
  pPrint MATCH = "match"
  pPrint WITH = "with"
  pPrint (ID x) = P.text x
  pPrint (LID x) = P.text x
  pPrint (INT x) = P.integer x
  pPrint (FLOAT x) = P.double x
  pPrint (BOOL x) = pPrint x
  pPrint (CHAR x) = pPrint x
  pPrint (STRING x) = pPrint x
  pPrint TY_INT = "Int"
  pPrint TY_FLOAT = "Float"
  pPrint TY_BOOL = "Bool"
  pPrint TY_CHAR = "Char"
  pPrint TY_STRING = "String"