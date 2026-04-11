module Malgo.Sequent.Core.JoinCheck
  ( assertJoin,
  )
where

import Data.IORef
import Malgo.Sequent.Core.Join
import Test.Hspec (expectationFailure)
import Prelude

-- | Assert that a Join IR program is structurally well-formed by
-- traversing all nodes. This catches:
-- 1. Lazy evaluation errors (bottom values from broken transformations)
-- 2. Structural consistency (all nodes are reachable and well-formed)
--
-- Note: Full scope checking is not performed because top-level definitions
-- are implicitly in scope and tracking them requires global analysis.
assertJoin :: Program -> IO ()
assertJoin (Program defs _) = do
  counter <- newIORef (0 :: Int)
  mapM_ (checkDefinition counter) defs
  count <- readIORef counter
  if count > 0
    then pure ()
    else expectationFailure "Join program has no nodes (empty program)"

checkStatement :: IORef Int -> Statement -> IO ()
checkStatement c (Cut producer name) = do
  tick c
  checkProducer c producer
  seq name (pure ())
checkStatement c (Join _ name consumer stmt) = do
  tick c
  seq name (pure ())
  checkConsumer c consumer
  checkStatement c stmt
checkStatement c (Primitive _ _ producers name) = do
  tick c
  mapM_ (checkProducer c) producers
  seq name (pure ())
checkStatement c (Invoke _ _ name) = do
  tick c
  seq name (pure ())
checkStatement c (ExternalCall _ _ producers name) = do
  tick c
  mapM_ (checkProducer c) producers
  seq name (pure ())
checkStatement c (BinOp _ _ lhs rhs name) = do
  tick c
  checkProducer c lhs
  checkProducer c rhs
  seq name (pure ())
checkStatement c (Ifz _ cond thenStmt elseStmt) = do
  tick c
  checkProducer c cond
  checkStatement c thenStmt
  checkStatement c elseStmt

checkDefinition :: IORef Int -> (a, b, c, Statement) -> IO ()
checkDefinition c (_, _, _, stmt) = checkStatement c stmt

checkProducer :: IORef Int -> Producer -> IO ()
checkProducer c (Var _ _) = tick c
checkProducer c (Literal _ _) = tick c
checkProducer c (Construct _ _ producers names) = do
  tick c
  mapM_ (checkProducer c) producers
  seq names (pure ())
checkProducer c (Lambda _ _ stmt) = do
  tick c
  checkStatement c stmt
checkProducer c (Object _ fields) = do
  tick c
  mapM_ (\(_, stmt) -> checkStatement c stmt) fields
checkProducer c (Mu _ _ stmt) = do
  tick c
  checkStatement c stmt
checkProducer c (Cocase _ branches) = do
  tick c
  mapM_ (\(_, _, s) -> checkStatement c s) branches

checkConsumer :: IORef Int -> Consumer -> IO ()
checkConsumer c (Label _ _) = tick c
checkConsumer c (Apply _ producers names) = do
  tick c
  mapM_ (checkProducer c) producers
  seq names (pure ())
checkConsumer c (Project _ _ name) = do
  tick c
  seq name (pure ())
checkConsumer c (Then _ name stmt) = do
  tick c
  seq name (pure ())
  checkStatement c stmt
checkConsumer c (Finish _) = tick c
checkConsumer c (Select _ branches) = do
  tick c
  mapM_ (\(Branch _ _ s) -> checkStatement c s) branches
checkConsumer c (Destructor _ _ producers name) = do
  tick c
  mapM_ (checkProducer c) producers
  seq name (pure ())

tick :: IORef Int -> IO ()
tick c = modifyIORef' c (+ 1)
