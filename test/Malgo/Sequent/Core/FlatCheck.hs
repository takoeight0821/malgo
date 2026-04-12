module Malgo.Sequent.Core.FlatCheck
  ( assertFlat,
    isValueProducer,
  )
where

import Malgo.Sequent.Core.Flat
import Test.Hspec (expectationFailure)
import Prelude

-- | Assert that a Flat IR program satisfies the flatness invariant:
-- all producer arguments in Primitive, ExternalCall, BinOp, and Construct
-- positions are "values" (Var, Literal, or recursively-value Construct).
assertFlat :: Program -> IO ()
assertFlat (Program defs _) = mapM_ checkDefinition defs
  where
    checkDefinition (_, _, _, stmt) = checkStatement stmt

checkStatement :: Statement -> IO ()
checkStatement (Cut producer consumer) = do
  checkProducer producer
  checkConsumer consumer
checkStatement (Join _ _ consumer stmt) = do
  checkConsumer consumer
  checkStatement stmt
checkStatement (Primitive _ _ producers consumer) = do
  mapM_ assertValue producers
  checkConsumer consumer
checkStatement (Invoke _ _ consumer) =
  checkConsumer consumer
checkStatement (ExternalCall _ _ producers consumer) = do
  mapM_ assertValue producers
  checkConsumer consumer
checkStatement (BinOp _ _ lhs rhs consumer) = do
  assertValue lhs
  assertValue rhs
  checkConsumer consumer
checkStatement (Ifz _ cond thenStmt elseStmt) = do
  checkProducer cond
  checkStatement thenStmt
  checkStatement elseStmt

checkProducer :: Producer -> IO ()
checkProducer (Var _ _) = pure ()
checkProducer (Literal _ _) = pure ()
checkProducer (Construct _ _ producers consumers) = do
  mapM_ assertValue producers
  mapM_ checkConsumer consumers
checkProducer (Lambda _ _ stmt) = checkStatement stmt
checkProducer (Object _ fields) = mapM_ (checkStatement . snd) fields
checkProducer (Mu _ _ stmt) = checkStatement stmt
checkProducer (Cocase _ branches) = mapM_ (\(_, _, s) -> checkStatement s) branches

checkConsumer :: Consumer -> IO ()
checkConsumer (Label _ _) = pure ()
checkConsumer (Apply _ producers consumers) = do
  mapM_ checkProducer producers
  mapM_ checkConsumer consumers
checkConsumer (Project _ _ consumer) = checkConsumer consumer
checkConsumer (Then _ _ stmt) = checkStatement stmt
checkConsumer (Finish _) = pure ()
checkConsumer (Select _ branches) = mapM_ (\(Branch _ _ s) -> checkStatement s) branches
checkConsumer (Destructor _ _ producers consumer) = do
  mapM_ checkProducer producers
  checkConsumer consumer

assertValue :: Producer -> IO ()
assertValue p
  | isValueProducer p = pure ()
  | otherwise = expectationFailure $ "Expected value producer, got: " <> take 200 (show p)

isValueProducer :: Producer -> Bool
isValueProducer (Var _ _) = True
isValueProducer (Literal _ _) = True
isValueProducer (Construct _ _ ps _) = all isValueProducer ps
isValueProducer (Lambda _ _ _) = True
isValueProducer (Object _ _) = True
isValueProducer (Mu _ _ _) = False
isValueProducer (Cocase _ _) = True
