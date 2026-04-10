{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Malgo.Sequent.Core.Join
  ( joinProgram,
    JoinPass (..),
    Program (..),
    Producer (..),
    Consumer (..),
    Statement (..),
    Branch (..),
  )
where

import Data.Map qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Data.Store (Store)
import Data.Traversable (for)
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local
import Lens.Micro (traverseOf, _2)
import Malgo.Id
import Malgo.Module
import Malgo.Pass
import Malgo.Prelude
import Malgo.SExpr
import Malgo.Sequent.Core.Flat qualified as Flat
import Malgo.Sequent.Fun (Literal, Name, Pattern, Tag)

data JoinPass = JoinPass

instance Pass JoinPass where
  type Input JoinPass = Flat.Program
  type Output JoinPass = Program
  type ErrorType JoinPass = Void
  type Effects JoinPass es = (State Uniq :> es, Reader ModuleName :> es)

  runPassImpl _ = joinProgram

joinProgram :: (State Uniq :> es, Reader ModuleName :> es) => Flat.Program -> Eff es Program
joinProgram Flat.Program {..} = do
  definitions <- traverse joinDefinition definitions
  pure Program {definitions, dependencies}

joinDefinition :: (State Uniq :> es, Reader ModuleName :> es) => (Range, Name, Name, Flat.Statement) -> Eff es (Range, Name, Name, Statement)
joinDefinition (range, name, return, statement) = do
  statement <- runJoin $ joinStatement statement
  pure (range, name, return, statement)

runJoin :: Eff (Writer (Endo b) : es) b -> Eff es b
runJoin m = uncurry (flip appEndo) <$> runWriter m

joinStatement :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo Statement) :> es) => Flat.Statement -> Eff es Statement
joinStatement (Flat.Cut producer consumer) = do
  producer <- joinProducer producer
  consumer <- joinConsumer consumer
  pure $ Cut producer consumer
joinStatement (Flat.Join range name consumer statement) = do
  statement <- runJoin $ joinStatement statement
  consumer <- joinConsumer' consumer
  pure $ Join range name consumer statement
joinStatement (Flat.Primitive range name producers return) = do
  producers <- traverse joinProducer producers
  return <- joinConsumer return
  pure $ Primitive range name producers return
joinStatement (Flat.Invoke range name return) = do
  return <- joinConsumer return
  pure $ Invoke range name return
joinStatement (Flat.ExternalCall range name producers return) = do
  producers <- traverse joinProducer producers
  return <- joinConsumer return
  pure $ ExternalCall range name producers return
joinStatement (Flat.BinOp range op lhs rhs return) = do
  lhs <- joinProducer lhs
  rhs <- joinProducer rhs
  return <- joinConsumer return
  pure $ BinOp range op lhs rhs return
joinStatement (Flat.Ifz range cond then_ else_) = do
  cond <- joinProducer cond
  then_ <- runJoin $ joinStatement then_
  else_ <- runJoin $ joinStatement else_
  pure $ Ifz range cond then_ else_

joinProducer :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo Statement) :> es) => Flat.Producer -> Eff es Producer
joinProducer (Flat.Var range name) = pure $ Var range name
joinProducer (Flat.Literal range literal) = pure $ Literal range literal
joinProducer (Flat.Construct range tag producers returns) = do
  producers <- traverse joinProducer producers
  returns <- traverse joinConsumer returns
  pure $ Construct range tag producers returns
joinProducer (Flat.Lambda range names statement) = do
  statement <- runJoin $ joinStatement statement
  pure $ Lambda range names statement
joinProducer (Flat.Object range fields) = do
  fields <- traverseOf (traverse . _2) (runJoin . joinStatement) fields
  pure $ Object range fields
joinProducer (Flat.Mu range name statement) = do
  statement <- runJoin $ joinStatement statement
  pure $ Mu range name statement
joinProducer (Flat.Cocase range branches) = do
  branches <- for branches \(d, vars, s) -> do
    s' <- runJoin $ joinStatement s
    pure (d, vars, s')
  pure $ Cocase range branches

joinConsumer :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo Statement) :> es) => Flat.Consumer -> Eff es Name
joinConsumer (Flat.Label _ name) = pure name
joinConsumer (Flat.Apply range producers returns) = do
  producers <- traverse joinProducer producers
  returns <- traverse joinConsumer returns
  tellJoin range "apply" $ Apply range producers returns
joinConsumer (Flat.Project range field return) = do
  return <- joinConsumer return
  tellJoin range "project" $ Project range field return
joinConsumer (Flat.Then range name statement) = do
  statement <- runJoin $ joinStatement statement
  tellJoin range "then" $ Then range name statement
joinConsumer (Flat.Finish range) = tellJoin range "finish" $ Finish range
joinConsumer (Flat.Select range branches) = do
  branches <- traverse joinBranch branches
  tellJoin range "select" $ Select range branches
joinConsumer (Flat.Destructor range name producers return) = do
  producers <- traverse joinProducer producers
  return <- joinConsumer return
  tellJoin range "destructor" $ Destructor range name producers return

joinConsumer' :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo Statement) :> es) => Flat.Consumer -> Eff es Consumer
joinConsumer' (Flat.Label range name) = pure (Label range name)
joinConsumer' (Flat.Apply range producers returns) = do
  producers <- traverse joinProducer producers
  returns <- traverse joinConsumer returns
  pure $ Apply range producers returns
joinConsumer' (Flat.Project range field return) = do
  return <- joinConsumer return
  pure $ Project range field return
joinConsumer' (Flat.Then range name statement) = do
  statement <- runJoin $ joinStatement statement
  pure $ Then range name statement
joinConsumer' (Flat.Finish range) = pure $ Finish range
joinConsumer' (Flat.Select range branches) = do
  branches <- traverse joinBranch branches
  pure $ Select range branches
joinConsumer' (Flat.Destructor range name producers return) = do
  producers <- traverse joinProducer producers
  return <- joinConsumer return
  pure $ Destructor range name producers return

joinBranch :: (State Uniq :> es, Reader ModuleName :> es) => Flat.Branch -> Eff es Branch
joinBranch (Flat.Branch range pattern statement) = do
  statement <- runJoin $ joinStatement statement
  pure $ Branch range pattern statement

tellJoin :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo Statement) :> es) => Range -> Text -> Consumer -> Eff es Id
tellJoin range name consumer = do
  name <- newTemporalId name
  tell $ Endo $ Join range name consumer
  pure name

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
  Construct :: Range -> Tag -> [Producer] -> [Name] -> Producer
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
  Apply :: Range -> [Producer] -> [Name] -> Consumer
  Project :: Range -> Text -> Name -> Consumer
  Then :: Range -> Name -> Statement -> Consumer
  Finish :: Range -> Consumer
  Select :: Range -> [Branch] -> Consumer
  Destructor :: Range -> Text -> [Producer] -> Name -> Consumer

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
  Cut :: Producer -> Name -> Statement
  Join :: Range -> Name -> Consumer -> Statement -> Statement
  Primitive :: Range -> Text -> [Producer] -> Name -> Statement
  Invoke :: Range -> Name -> Name -> Statement
  ExternalCall :: Range -> Text -> [Producer] -> Name -> Statement
  BinOp :: Range -> Text -> Producer -> Producer -> Name -> Statement
  Ifz :: Range -> Producer -> Statement -> Statement -> Statement

deriving stock instance Show Statement

deriving stock instance Generic Statement

deriving anyclass instance Store Statement

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
