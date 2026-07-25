{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Sequent.Core.Full
  ( Program (..),
    Producer (..),
    Consumer (..),
    Statement (..),
    Branch (..),
  )
where

import Data.Binary (Binary)
import Data.Map qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Malgo.Module
import Malgo.Prelude
import Malgo.SExpr hiding (Char, Double, Float, String)
import Malgo.Sequent.Fun (Literal, Name, Pattern, Tag)

data Program = Program
  { definitions :: [(Range, Name, Name, Statement)],
    dependencies :: [ModuleName]
  }
  deriving stock (Show, Generic)
  deriving anyclass (Binary)
  deriving (Resource) via (ViaBinary Program)

instance ToSExpr Program where
  toSExpr (Program defs dependencies) = S.L $ map (\(_, name, return, body) -> toSExpr (name, return, body)) defs <> [S.L $ map toSExpr dependencies]

data Producer where
  Var :: Range -> Name -> Producer
  Literal :: Range -> Literal -> Producer
  Construct :: Range -> Tag -> [Producer] -> [Consumer] -> Producer
  Lambda :: Range -> [Name] -> Statement -> Producer
  Object :: Range -> Map Text (Name, Statement) -> Producer
  Do :: Range -> Name -> Statement -> Producer
  Mu :: Range -> Name -> Statement -> Producer

deriving stock instance Show Producer

deriving stock instance Generic Producer

deriving anyclass instance Binary Producer

deriving via (ViaBinary Producer) instance Resource Producer

instance HasRange Producer where
  range (Var range _) = range
  range (Literal range _) = range
  range (Construct range _ _ _) = range
  range (Lambda range _ _) = range
  range (Object range _) = range
  range (Do range _ _) = range
  range (Mu range _ _) = range

instance ToSExpr Producer where
  toSExpr (Var _ name) = toSExpr name
  toSExpr (Literal _ literal) = toSExpr literal
  toSExpr (Construct _ tag producers consumers) =
    S.L [S.A "construct", toSExpr tag, S.L $ map toSExpr producers, S.L $ map toSExpr consumers]
  toSExpr (Lambda _ names statement) = S.L [S.A "lambda", S.L $ map toSExpr names, toSExpr statement]
  toSExpr (Object _ kvs) = S.L $ map (\(k, v) -> S.L [toSExpr k, toSExpr v]) $ Map.toList kvs
  toSExpr (Do _ name statement) = S.L [S.A "do", toSExpr name, toSExpr statement]
  toSExpr (Mu _ name statement) = S.L [S.A "mu", toSExpr name, toSExpr statement]

data Consumer where
  Label :: Range -> Name -> Consumer
  Apply :: Range -> [Producer] -> [Consumer] -> Consumer
  Project :: Range -> Text -> Consumer -> Consumer
  Then :: Range -> Name -> Statement -> Consumer
  Finish :: Range -> Consumer
  Select :: Range -> [Branch] -> Consumer

deriving stock instance Show Consumer

deriving stock instance Generic Consumer

deriving anyclass instance Binary Consumer

deriving via (ViaBinary Consumer) instance Resource Consumer

instance ToSExpr Consumer where
  toSExpr (Label _ name) = toSExpr name
  toSExpr (Apply _ producers consumers) = S.L [S.A "apply", S.L $ map toSExpr producers, S.L $ map toSExpr consumers]
  toSExpr (Project _ field return) = S.L [S.A "project", toSExpr field, toSExpr return]
  toSExpr (Then _ name statement) = S.L [S.A "then", toSExpr name, toSExpr statement]
  toSExpr (Finish _) = S.A "finish"
  toSExpr (Select _ branches) = S.L $ S.A "select" : map toSExpr branches

data Statement where
  Cut :: Producer -> Consumer -> Statement
  Primitive :: Range -> Text -> [Producer] -> Consumer -> Statement
  Invoke :: Range -> Name -> Consumer -> Statement
  ExternalCall :: Range -> Text -> [Producer] -> Consumer -> Statement
  BinOp :: Range -> Text -> Producer -> Producer -> Consumer -> Statement
  Ifz :: Range -> Producer -> Statement -> Statement -> Statement

deriving stock instance (Show Consumer) => Show Statement

deriving stock instance Generic Statement

deriving anyclass instance (Binary Consumer) => Binary Statement

deriving via (ViaBinary Statement) instance (Resource Statement)

instance HasRange Statement where
  range (Cut producer _) = range producer
  range (Primitive range _ _ _) = range
  range (Invoke range _ _) = range
  range (ExternalCall range _ _ _) = range
  range (BinOp range _ _ _ _) = range
  range (Ifz range _ _ _) = range

instance ToSExpr Statement where
  toSExpr (Cut producer consumer) = S.L ["cut", toSExpr producer, toSExpr consumer]
  toSExpr (Primitive _ name producers consumer) =
    S.L [S.A "prim", toSExpr name, S.L $ map toSExpr producers, toSExpr consumer]
  toSExpr (Invoke _ name consumer) = S.L ["invoke", toSExpr name, toSExpr consumer]
  toSExpr (ExternalCall _ name producers consumer) =
    S.L [S.A "external-call", toSExpr name, S.L $ map toSExpr producers, toSExpr consumer]
  toSExpr (BinOp _ op lhs rhs consumer) =
    S.L [S.A "binop", toSExpr op, toSExpr lhs, toSExpr rhs, toSExpr consumer]
  toSExpr (Ifz _ cond then_ else_) =
    S.L [S.A "ifz", toSExpr cond, toSExpr then_, toSExpr else_]

data Branch = Branch
  { range :: Range,
    pattern :: Pattern,
    statement :: Statement
  }

deriving stock instance Show Branch

deriving stock instance Generic Branch

deriving anyclass instance Binary Branch

deriving via (ViaBinary Branch) instance (Resource Branch)

instance ToSExpr Branch where
  toSExpr (Branch _ pattern statement) = S.L [toSExpr pattern, toSExpr statement]
