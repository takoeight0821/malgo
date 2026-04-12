module Malgo.Sequent.Core.JoinCheck
  ( assertJoin,
  )
where

import Control.Exception (evaluate)
import Malgo.Sequent.Core.Join
import Test.Hspec (expectationFailure)
import Prelude

-- | Assert that a Join IR program is structurally well-formed by
-- traversing all nodes and forcing evaluation. This catches lazy
-- evaluation errors (bottom values from broken transformations).
assertJoin :: Program -> IO ()
assertJoin (Program defs _)
  | null defs = expectationFailure "Join program has no definitions"
  | otherwise = mapM_ checkDefinition defs

checkDefinition :: (a, b, c, Statement) -> IO ()
checkDefinition (_, _, _, stmt) = checkStatement stmt

checkStatement :: Statement -> IO ()
checkStatement (Cut producer name) = do
  checkProducer producer
  evaluate name >> pure ()
checkStatement (Join _ name consumer stmt) = do
  evaluate name >> pure ()
  checkConsumer consumer
  checkStatement stmt
checkStatement (Primitive _ _ producers name) = do
  mapM_ checkProducer producers
  evaluate name >> pure ()
checkStatement (Invoke _ _ name) =
  evaluate name >> pure ()
checkStatement (ExternalCall _ _ producers name) = do
  mapM_ checkProducer producers
  evaluate name >> pure ()
checkStatement (BinOp _ _ lhs rhs name) = do
  checkProducer lhs
  checkProducer rhs
  evaluate name >> pure ()
checkStatement (Ifz _ cond thenStmt elseStmt) = do
  checkProducer cond
  checkStatement thenStmt
  checkStatement elseStmt

checkProducer :: Producer -> IO ()
checkProducer (Var _ _) = pure ()
checkProducer (Literal _ _) = pure ()
checkProducer (Construct _ _ producers names) = do
  mapM_ checkProducer producers
  mapM_ (\n -> evaluate n >> pure ()) names
checkProducer (Lambda _ _ stmt) = checkStatement stmt
checkProducer (Object _ fields) =
  mapM_ (\(_, stmt) -> checkStatement stmt) fields
checkProducer (Mu _ _ stmt) = checkStatement stmt
checkProducer (Cocase _ branches) =
  mapM_ (\(_, _, s) -> checkStatement s) branches

checkConsumer :: Consumer -> IO ()
checkConsumer (Label _ name) = evaluate name >> pure ()
checkConsumer (Apply _ producers names) = do
  mapM_ checkProducer producers
  mapM_ (\n -> evaluate n >> pure ()) names
checkConsumer (Project _ _ name) = evaluate name >> pure ()
checkConsumer (Then _ name stmt) = do
  evaluate name >> pure ()
  checkStatement stmt
checkConsumer (Finish _) = pure ()
checkConsumer (Select _ branches) =
  mapM_ (\(Branch _ _ s) -> checkStatement s) branches
checkConsumer (Destructor _ _ producers name) = do
  mapM_ checkProducer producers
  evaluate name >> pure ()
