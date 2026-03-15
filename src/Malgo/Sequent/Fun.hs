{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Malgo.Sequent.Fun
  ( Name,
    Tag (..),
    Literal (..),
    Program (..),
    Expr (..),
    Branch (..),
    Pattern (..),
    HasRange (..),
  )
where

import Data.Map qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Data.Store (Store)
import Malgo.Id
import Malgo.Module (ModuleName, Resource, ViaStore (..))
import Malgo.Prelude
import Malgo.SExpr (ToSExpr (..))
import Malgo.SExpr qualified as S

type Name = Id

-- | Tag is used to distinguish different structures.
data Tag = Tuple | Tag Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Store)
  deriving (Resource) via (ViaStore Tag)

instance ToSExpr Tag where
  toSExpr Tuple = S.A "tuple"
  toSExpr (Tag t) = toSExpr t

data Literal
  = Int32 Int32
  | Int64 Int64
  | Float Float
  | Double Double
  | Char Char
  | String Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Store)
  deriving (Resource) via (ViaStore Literal)

instance ToSExpr Literal where
  toSExpr (Int32 n) = S.A $ S.Int (fromIntegral n) (Just "i32")
  toSExpr (Int64 n) = S.A $ S.Int (fromIntegral n) (Just "i64")
  toSExpr (Float n) = S.A $ S.Float n
  toSExpr (Double n) = S.A $ S.Double n
  toSExpr (Char c) = S.A $ S.Char c
  toSExpr (String t) = S.A $ S.String t

data Program = Program
  { definitions :: [(Range, Name, Expr)],
    dependencies :: [ModuleName]
  }

instance ToSExpr Program where
  toSExpr (Program definitions dependencies) =
    S.L
      [ S.A "program",
        S.L $ map (\(_, name, body) -> toSExpr (name, body)) definitions,
        S.L $ map toSExpr dependencies
      ]

data Expr
  = Var Range Name
  | Literal Range Literal
  | Construct Range Tag [Expr]
  | Let Range Name Expr Expr
  | Lambda Range [Name] Expr
  | Object Range (Map Text Expr)
  | Apply Range Expr [Expr]
  | Project Range Expr Text
  | Primitive Range Text [Expr]
  | Select Range Expr [Branch]
  | Invoke Range Name
  | Fix Range Name Expr
  deriving stock (Show)

instance HasRange Expr where
  range (Var r _) = r
  range (Literal r _) = r
  range (Construct r _ _) = r
  range (Let r _ _ _) = r
  range (Lambda r _ _) = r
  range (Object r _) = r
  range (Apply r _ _) = r
  range (Project r _ _) = r
  range (Primitive r _ _) = r
  range (Select r _ _) = r
  range (Invoke r _) = r
  range (Fix r _ _) = r

instance ToSExpr Expr where
  toSExpr (Var _ name) = toSExpr name
  toSExpr (Literal _ literal) = toSExpr literal
  toSExpr (Construct _ tag arguments) = S.L [toSExpr tag, S.L $ map toSExpr arguments]
  toSExpr (Let _ name value body) = S.L [S.A "let", toSExpr name, toSExpr value, toSExpr body]
  toSExpr (Lambda _ parameters body) = S.L [S.A "lambda", S.L $ map toSExpr parameters, toSExpr body]
  toSExpr (Object _ fields) = S.L [S.A "object", S.L $ map toSExpr $ Map.toList fields]
  toSExpr (Apply _ callee arguments) = S.L [S.A "apply", toSExpr callee, S.L $ map toSExpr arguments]
  toSExpr (Project _ callee field) = S.L [S.A "project", toSExpr callee, toSExpr field]
  toSExpr (Primitive _ operator arguments) = S.L [S.A "primitive", toSExpr operator, S.L $ map toSExpr arguments]
  toSExpr (Select _ scrutinees branches) = S.L [S.A "select", toSExpr scrutinees, S.L $ map toSExpr branches]
  toSExpr (Invoke _ name) = S.L [S.A "invoke", toSExpr name]
  toSExpr (Fix _ name body) = S.L [S.A "fix", toSExpr name, toSExpr body]

data Branch = Branch Range Pattern Expr
  deriving stock (Show)

instance HasRange Branch where
  range (Branch r _ _) = r

instance ToSExpr Branch where
  toSExpr (Branch _ pattern body) = S.L [toSExpr pattern, toSExpr body]

data Pattern
  = PVar Range Name
  | PLiteral Range Literal
  | Destruct Range Tag [Pattern]
  | Expand Range (Map Text Pattern)
  deriving stock (Show, Generic)
  deriving anyclass (Store)
  deriving (Resource) via (ViaStore Pattern)

instance HasRange Pattern where
  range (PVar r _) = r
  range (PLiteral r _) = r
  range (Destruct r _ _) = r
  range (Expand r _) = r

instance ToSExpr Pattern where
  toSExpr (PVar _ name) = toSExpr name
  toSExpr (PLiteral _ literal) = toSExpr literal
  toSExpr (Destruct _ tag patterns) = S.L [toSExpr tag, S.L $ map toSExpr patterns]
  toSExpr (Expand _ fields) = S.L [S.A "expand", S.L $ map toSExpr $ Map.toList fields]
