{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Sequent.Core.Flat
  ( flatProgram,
    FlatPass (..),
    Program (..),
    Producer (..),
    Consumer (..),
    Statement (..),
    Branch (..),
  )
where

import Control.Lens (traverseOf, _2)
import Data.Map qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Data.Store (Store)
import Data.Traversable (for)
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Malgo.Id
import Malgo.Module (ModuleName, Resource, ViaStore (..))
import Malgo.Pass
import Malgo.Prelude
import Malgo.SExpr (ToSExpr (..))
import Malgo.Sequent.Core.Full qualified as Full
import Malgo.Sequent.Fun (Literal, Name, Pattern, Tag)

data FlatPass = FlatPass

instance Pass FlatPass where
  type Input FlatPass = Full.Program
  type Output FlatPass = Program
  type ErrorType FlatPass = Void
  type Effects FlatPass es = (State Uniq :> es, Reader ModuleName :> es)

  runPassImpl _ = flatProgram

-- | Flattens a program into a program with no nested do expressions.
flatProgram :: (State Uniq :> es, Reader ModuleName :> es) => Full.Program -> Eff es Program
flatProgram Full.Program {definitions, dependencies} = do
  definitions <- traverse flatDefinition definitions
  pure Program {..}

flatDefinition :: (State Uniq :> es, Reader ModuleName :> es) => (t1, t2, t3, Full.Statement) -> Eff es (t1, t2, t3, Statement)
flatDefinition (range, name, return, statement) = (range,name,return,) <$> flatStatement statement

-- | Wip is a temporary data structure to represent the intermediate state of the flattening process.
data Wip
  = Do' Range Name Statement
  | Zero Producer

cut :: Wip -> Consumer -> Statement
cut (Do' range name statement) consumer = Join range name consumer statement
cut (Zero producer) consumer = Cut producer consumer

flatStatement :: (State Uniq :> es, Reader ModuleName :> es) => Full.Statement -> Eff es Statement
flatStatement (Full.Cut producer consumer) = do
  producer <- flatProducer producer
  consumer <- flatConsumer consumer
  case producer of
    Do' range name statement -> pure $ Join range name consumer statement
    Zero producer -> pure $ Cut producer consumer
flatStatement (Full.Primitive range name producers consumer) = do
  (zeros, mproducer, rest) <- split producers
  case mproducer of
    Just producer -> do
      var <- newTemporalId "var"
      producer' <- flatProducer producer
      primitive <- flatStatement (Full.Primitive range name (zeros <> [Full.Var range var] <> rest) consumer)
      pure
        $ cut producer'
        $ Then range var primitive
    Nothing -> do
      producers' <- for zeros \zero -> do
        zero' <- flatProducer zero
        case zero' of
          Zero producer' -> pure producer'
          Do' {} -> error "impossible"
      consumer' <- flatConsumer consumer
      pure $ Primitive range name producers' consumer'
flatStatement (Full.Invoke range name consumer) = do
  consumer' <- flatConsumer consumer
  pure $ Invoke range name consumer'
flatStatement (Full.ExternalCall range name producers consumer) = do
  (zeros, mproducer, rest) <- split producers
  case mproducer of
    Just producer -> do
      var <- newTemporalId "var"
      producer' <- flatProducer producer
      stmt <- flatStatement (Full.ExternalCall range name (zeros <> [Full.Var range var] <> rest) consumer)
      pure
        $ cut producer'
        $ Then range var stmt
    Nothing -> do
      producers' <- for zeros \zero -> do
        zero' <- flatProducer zero
        case zero' of
          Zero producer' -> pure producer'
          Do' {} -> error "impossible"
      consumer' <- flatConsumer consumer
      pure $ ExternalCall range name producers' consumer'
flatStatement (Full.BinOp range op lhs rhs consumer) = do
  lhs' <- flatProducer lhs
  rhs' <- flatProducer rhs
  consumer' <- flatConsumer consumer
  case lhs' of
    Do' r1 n1 s1 -> do
      var1 <- newTemporalId "var"
      stmt <- flatStatement (Full.BinOp range op (Full.Var range var1) rhs consumer)
      pure $ cut lhs' $ Then r1 var1 stmt
    Zero lhsP -> case rhs' of
      Do' r2 n2 s2 -> do
        var2 <- newTemporalId "var"
        stmt <- flatStatement (Full.BinOp range op lhs (Full.Var range var2) consumer)
        pure $ cut rhs' $ Then r2 var2 stmt
      Zero rhsP -> pure $ BinOp range op lhsP rhsP consumer'
flatStatement (Full.Ifz range cond then_ else_) = do
  cond' <- flatProducer cond
  then_' <- flatStatement then_
  else_' <- flatStatement else_
  case cond' of
    Do' r n s -> do
      var <- newTemporalId "var"
      pure $ cut cond' $ Then r var $ Ifz range (Var range var) then_' else_'
    Zero condP -> pure $ Ifz range condP then_' else_'

flatProducer :: (State Uniq :> es, Reader ModuleName :> es) => Full.Producer -> Eff es Wip
flatProducer (Full.Var range name) = pure $ Zero (Var range name)
flatProducer (Full.Literal range literal) = pure $ Zero (Literal range literal)
flatProducer (Full.Construct range tag producers consumers) = do
  (zeros, mproducer, rest) <- split producers
  case mproducer of
    Just producer -> do
      label <- newTemporalId "label"
      var <- newTemporalId "var"
      constructor <- flatProducer (Full.Construct range tag (zeros <> [Full.Var range var] <> rest) consumers)
      producer' <- flatProducer producer
      pure
        $ Do' range label
        $ cut producer'
        $ Then range var
        $ cut constructor
        $ Label range label
    Nothing -> do
      producers' <- for zeros \zero -> do
        zero' <- flatProducer zero
        case zero' of
          Zero producer' -> pure producer'
          Do' {} -> error "impossible"
      consumers' <- traverse flatConsumer consumers
      pure $ Zero (Construct range tag producers' consumers')
flatProducer (Full.Lambda range names statement) = do
  statement <- flatStatement statement
  pure $ Zero (Lambda range names statement)
flatProducer (Full.Object range fields) = do
  fields <- traverseOf (traverse . _2) flatStatement fields
  pure $ Zero (Object range fields)
flatProducer (Full.Do range name statement) = do
  statement' <- flatStatement statement
  pure $ Do' range name statement'
flatProducer (Full.Mu range name statement) = do
  statement' <- flatStatement statement
  pure $ Zero (Mu range name statement')
flatProducer (Full.Cocase range branches) = do
  branches' <- for branches \(d, vars, s) -> do
    s' <- flatStatement s
    pure (d, vars, s')
  pure $ Zero (Cocase range branches')

flatConsumer :: (State Uniq :> es, Reader ModuleName :> es) => Full.Consumer -> Eff es Consumer
flatConsumer (Full.Label range name) = pure $ Label range name
flatConsumer (Full.Apply range producers consumers) = do
  (zeros, mproducer, rest) <- split producers
  case mproducer of
    Just producer -> do
      outer <- newTemporalId "outer"
      inner <- newTemporalId "inner"
      apply <- flatConsumer (Full.Apply range (zeros <> [Full.Var range inner] <> rest) consumers)
      producer' <- flatProducer producer
      pure
        $ Then range outer
        $ cut producer'
        $ Then range inner
        $ cut (Zero (Var range outer)) apply
    Nothing -> do
      producers' <- for zeros \zero -> do
        zero' <- flatProducer zero
        case zero' of
          Zero producer' -> pure producer'
          Do' {} -> error "impossible" -- zeros only contains Rank Zero producers
      consumers' <- traverse flatConsumer consumers
      pure $ Apply range producers' consumers'
flatConsumer (Full.Project range field return) = do
  return' <- flatConsumer return
  pure $ Project range field return'
flatConsumer (Full.Then range name statement) = do
  statement' <- flatStatement statement
  pure $ Then range name statement'
flatConsumer (Full.Finish range) = pure $ Finish range
flatConsumer (Full.Select range branches) = do
  branches <- traverse flatBranch branches
  pure $ Select range branches
flatConsumer (Full.Destructor range name producers consumer) = do
  (zeros, mproducer, rest) <- split producers
  case mproducer of
    Just producer -> do
      outer <- newTemporalId "outer"
      inner <- newTemporalId "inner"
      destr <- flatConsumer (Full.Destructor range name (zeros <> [Full.Var range inner] <> rest) consumer)
      producer' <- flatProducer producer
      pure
        $ Then range outer
        $ cut producer'
        $ Then range inner
        $ cut (Zero (Var range outer)) destr
    Nothing -> do
      producers' <- for zeros \zero -> do
        zero' <- flatProducer zero
        case zero' of
          Zero producer' -> pure producer'
          Do' {} -> error "impossible"
      consumer' <- flatConsumer consumer
      pure $ Destructor range name producers' consumer'

flatBranch :: (State Uniq :> es, Reader ModuleName :> es) => Full.Branch -> Eff es Branch
flatBranch (Full.Branch range pattern statement) = do
  statement <- flatStatement statement
  pure $ Branch range pattern statement

split :: (State Uniq :> es, Reader ModuleName :> es) => [Full.Producer] -> Eff es ([Full.Producer], Maybe Full.Producer, [Full.Producer])
split = aux []
  where
    aux :: (State Uniq :> es, Reader ModuleName :> es) => [Full.Producer] -> [Full.Producer] -> Eff es ([Full.Producer], Maybe Full.Producer, [Full.Producer])
    aux acc [] = pure (reverse acc, Nothing, [])
    aux acc (p : ps) = do
      if isValue p
        then aux (p : acc) ps
        else pure (reverse acc, Just p, ps)

isValue :: Full.Producer -> Bool
isValue Full.Var {} = True
isValue Full.Literal {} = True
isValue (Full.Construct _ _ ps _) = all isValue ps
isValue Full.Lambda {} = True
isValue Full.Object {} = True
isValue Full.Do {} = False
isValue Full.Mu {} = False
isValue Full.Cocase {} = True

data Program = Program
  { definitions :: [(Range, Name, Name, Statement)],
    dependencies :: [ModuleName]
  }
  deriving stock (Show, Generic)
  deriving anyclass (Store)
  deriving (Resource) via (ViaStore Program)

instance ToSExpr Program where
  toSExpr (Program defs dependencies) = S.L $ map (\(_, name, return, body) -> toSExpr (name, return, body)) defs <> [S.L $ map toSExpr dependencies]

data Producer where
  Var :: Range -> Name -> Producer
  Literal :: Range -> Literal -> Producer
  Construct :: Range -> Tag -> [Producer] -> [Consumer] -> Producer
  Lambda :: Range -> [Name] -> Statement -> Producer
  Object :: Range -> Map Text (Name, Statement) -> Producer
  Mu :: Range -> Name -> Statement -> Producer
  Cocase :: Range -> [(Text, [Name], Statement)] -> Producer

deriving stock instance Show Producer

deriving stock instance Generic Producer

deriving anyclass instance Store Producer

deriving via (ViaStore Producer) instance Resource Producer

instance HasRange Producer where
  range (Var range _) = range
  range (Literal range _) = range
  range (Construct range _ _ _) = range
  range (Lambda range _ _) = range
  range (Object range _) = range
  range (Mu range _ _) = range
  range (Cocase range _) = range

instance ToSExpr Producer where
  toSExpr (Var _ name) = toSExpr name
  toSExpr (Literal _ literal) = toSExpr literal
  toSExpr (Construct _ tag producers consumers) =
    S.L [S.A "construct", toSExpr tag, S.L $ map toSExpr producers, S.L $ map toSExpr consumers]
  toSExpr (Lambda _ names statement) = S.L [S.A "lambda", S.L $ map toSExpr names, toSExpr statement]
  toSExpr (Object _ kvs) = S.L $ map (\(k, v) -> S.L [toSExpr k, toSExpr v]) $ Map.toList kvs
  toSExpr (Mu _ name statement) = S.L [S.A "mu", toSExpr name, toSExpr statement]
  toSExpr (Cocase _ branches) =
    S.L $ S.A "cocase" : map (\(d, vars, s) -> S.L [toSExpr d, S.L $ map toSExpr vars, toSExpr s]) branches

data Consumer where
  Label :: Range -> Name -> Consumer
  Apply :: Range -> [Producer] -> [Consumer] -> Consumer
  Project :: Range -> Text -> Consumer -> Consumer
  Then :: Range -> Name -> Statement -> Consumer
  Finish :: Range -> Consumer
  Select :: Range -> [Branch] -> Consumer
  Destructor :: Range -> Text -> [Producer] -> Consumer -> Consumer

deriving stock instance Show Consumer

deriving stock instance Generic Consumer

deriving anyclass instance Store Consumer

deriving via (ViaStore Consumer) instance Resource Consumer

instance ToSExpr Consumer where
  toSExpr (Label _ name) = toSExpr name
  toSExpr (Apply _ producers consumers) = S.L [S.A "apply", S.L $ map toSExpr producers, S.L $ map toSExpr consumers]
  toSExpr (Project _ field return) = S.L [S.A "project", toSExpr field, toSExpr return]
  toSExpr (Then _ name statement) = S.L [S.A "then", toSExpr name, toSExpr statement]
  toSExpr (Finish _) = S.A "finish"
  toSExpr (Select _ branches) = S.L $ S.A "select" : map toSExpr branches
  toSExpr (Destructor _ name producers consumer) =
    S.L [S.A "destructor", toSExpr name, S.L $ map toSExpr producers, toSExpr consumer]

data Statement where
  Cut :: Producer -> Consumer -> Statement
  Join :: Range -> Name -> Consumer -> Statement -> Statement
  Primitive :: Range -> Text -> [Producer] -> Consumer -> Statement
  Invoke :: Range -> Name -> Consumer -> Statement
  ExternalCall :: Range -> Text -> [Producer] -> Consumer -> Statement
  BinOp :: Range -> Text -> Producer -> Producer -> Consumer -> Statement
  Ifz :: Range -> Producer -> Statement -> Statement -> Statement

deriving stock instance (Show Consumer) => Show Statement

deriving stock instance Generic Statement

deriving anyclass instance (Store Consumer) => Store Statement

deriving via (ViaStore Statement) instance (Resource Statement)

instance HasRange Statement where
  range (Cut producer _) = range producer
  range (Join x _ _ _) = range x
  range (Primitive range _ _ _) = range
  range (Invoke range _ _) = range
  range (ExternalCall range _ _ _) = range
  range (BinOp range _ _ _ _) = range
  range (Ifz range _ _ _) = range

instance ToSExpr Statement where
  toSExpr (Cut producer consumer) = S.L ["cut", toSExpr producer, toSExpr consumer]
  toSExpr (Join _ name consumer statement) = S.L ["join", toSExpr name, toSExpr consumer, toSExpr statement]
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

deriving anyclass instance Store Branch

deriving via (ViaStore Branch) instance (Resource Branch)

instance ToSExpr Branch where
  toSExpr (Branch _ pattern statement) = S.L [toSExpr pattern, toSExpr statement]
