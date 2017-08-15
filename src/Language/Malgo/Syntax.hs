{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Syntax where

import           Data.List       (intercalate)
import           Text.Parsec.Pos (SourcePos)

type Name = String
data AST = Symbol Name
         | Int Integer
         | Float Double
         | Bool Bool
         | Char Char
         | String String
         | Typed AST AST
         | List [AST]
         | Tree [AST]
  deriving (Eq, Show)

textAST (Symbol name) = name
textAST (Int i)       = show i
textAST (Float f)     = show f
textAST (Bool True)   = "#t"
textAST (Bool False)  = "#f"
textAST (Char c)      = show c
textAST (String s)    = show s
textAST (Typed a t)   = textAST a ++ ":" ++ textAST t
textAST (List xs)     = "[" ++ unwords (map textAST xs) ++ "]"
textAST (Tree [Symbol "quote", Symbol s]) = "'" ++ s
textAST (Tree xs)     = "(" ++ unwords (map textAST xs) ++ ")"

sample1 = Tree [Symbol "def", Typed (Symbol "ans") (Symbol "Int"), Int 42]
sample2 = Typed (Tree [ Symbol "if"
                      , Tree [Symbol "==", Symbol "ans", Int 42]
                      , Tree [Symbol  "quote", Symbol  "yes"]
                      , Tree [Symbol  "quote", Symbol  "no"]]) (Symbol  "Symbol")

sample3 = Typed (Tree [Symbol  "def"
                      , Tree [Typed (Symbol  "f") (Symbol  "Int"), Typed (Symbol  "x") (Symbol  "Int")]
                      , Tree [Symbol  "*", Symbol  "x", Symbol  "x"]]) (Symbol  "Symbol")

sample4 = Typed (List [Char 'c', Tree [Symbol  "quote", Symbol  "b"]]) (Tree [Symbol  "List", Symbol  "Symbol"])