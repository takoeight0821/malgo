{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Big-step evaluator for Join IR.
-- Unlike the CPS evaluator in 'Malgo.Sequent.Eval', this evaluator
-- directly evaluates statements to values using recursive descent.
-- An IORef bridges the gap between Consumer callbacks (returning ())
-- and the big-step return-value style.
module Malgo.Sequent.BigStepEval (BigStepEvalPass (..), bigStepEvalProgram) where

import Data.Map qualified as Map
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Static.Local (State)
import Malgo.Id
import Malgo.Module (ModuleName)
import Malgo.Pass
import Malgo.Prelude
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Eval
  ( Env,
    EvalError (..),
    Handlers,
    Toplevels,
    Value (..),
    emptyEnv,
    extendEnv,
    extendEnv',
    fetchPrimitive,
    isZeroValue,
    lookupEnv,
    match,
  )
import Malgo.Sequent.Fun (Literal (..), Name, Tag (..))

data BigStepEvalPass = BigStepEvalPass

instance Pass BigStepEvalPass where
  type Input BigStepEvalPass = (ModuleName, Handlers, Program)
  type Output BigStepEvalPass = ()
  type ErrorType BigStepEvalPass = EvalError
  type Effects BigStepEvalPass es = (State Uniq :> es, IOE :> es)

  runPassImpl _ (moduleName, handlers, program) =
    runReader moduleName $ runReader handlers $ bigStepEvalProgram program

-- | An IORef used to bridge between Consumer callbacks (returning ())
-- and big-step evaluation (returning Value).
type ResultRef = IORef Value

bigStepEvalProgram :: (Error EvalError :> es, State Uniq :> es, Reader ModuleName :> es, Reader Handlers :> es, IOE :> es) => Program -> Eff es ()
bigStepEvalProgram (Program {definitions}) = do
  let toplevels = Map.fromList [(name, (ret, statement)) | (_, name, ret, statement) <- definitions]
  case Map.keys toplevels & find (\name -> name.name == "main") of
    Just name -> do
      let (ret, statement) = fromMaybe (error "main not found in toplevels") $ Map.lookup name toplevels
      ref <- newIORef (Struct Tuple [])
      runReader toplevels
        $ runReader emptyEnv do
          finish <- newTemporalId "finish"
          _ <-
            evalStatement ref
              $ Join (range statement) finish (Finish (range statement))
              $ Join
                (range statement)
                ret
                (Apply (range statement) [Construct (range statement) Tuple [] []] [finish])
                statement
          pure ()
    Nothing -> pure ()

-- | Big-step evaluation of a statement. Returns the value that reaches
-- the innermost Finish consumer.
evalStatement ::
  ( Error EvalError :> es,
    Reader Env :> es,
    Reader Toplevels :> es,
    Reader Handlers :> es,
    IOE :> es
  ) =>
  ResultRef ->
  Statement ->
  Eff es Value
evalStatement ref (Cut (Mu _ name stmt) consumer) = do
  consumerValue <- lookupEnv (range stmt) consumer
  local (extendEnv name consumerValue)
    $ evalStatement ref stmt
evalStatement ref (Cut producer consumer) = do
  value <- evalProducer producer
  applyConsumer ref (range producer) consumer value
evalStatement ref (Join _ label consumer statement) = do
  env <- ask @Env
  let covalue = mkConsumerValue ref env consumer
  local (extendEnv label covalue) do
    evalStatement ref statement
evalStatement ref (Primitive r name producers consumer) = do
  values <- traverse evalProducer producers
  result <- fetchPrimitive name r values
  applyConsumer ref r consumer result
evalStatement ref (Invoke r name consumer) = do
  toplevels <- ask @Toplevels
  case Map.lookup name toplevels of
    Just (ret, statement) -> do
      covalue <- lookupEnv r consumer
      local (extendEnv ret covalue) do
        evalStatement ref statement
    Nothing -> throwError (UndefinedVariable r name)
evalStatement ref (ExternalCall range name producers consumer) = do
  values <- traverse evalProducer producers
  result <- fetchPrimitive name range values
  applyConsumer ref range consumer result
evalStatement ref (BinOp range op lhs rhs consumer) = do
  lhsVal <- evalProducer lhs
  rhsVal <- evalProducer rhs
  result <- fetchPrimitive op range [lhsVal, rhsVal]
  applyConsumer ref range consumer result
evalStatement ref (Ifz _ cond thenBranch elseBranch) = do
  condVal <- evalProducer cond
  if isZeroValue condVal
    then evalStatement ref thenBranch
    else evalStatement ref elseBranch

-- | Evaluate a producer to a value.
evalProducer ::
  ( Error EvalError :> es,
    Reader Env :> es,
    Reader Toplevels :> es,
    Reader Handlers :> es,
    IOE :> es
  ) =>
  Producer ->
  Eff es Value
evalProducer (Var r name) = lookupEnv r name
evalProducer (Literal _ literal) = pure $ Immediate literal
evalProducer (Construct r tag producers consumers) = do
  values <- traverse evalProducer producers
  covalues <- traverse (lookupEnv r) consumers
  pure $ Struct tag (values <> covalues)
evalProducer (Lambda _ parameters statement) = do
  env <- ask @Env
  pure $ Function env parameters statement
evalProducer (Object _ fields) = do
  env <- ask @Env
  pure $ Record env fields
evalProducer (Mu _ name stmt) = do
  ref <- newIORef (Struct Tuple [])
  local (extendEnv name (Consumer $ \v -> writeIORef ref v))
    $ evalStatement ref stmt

-- | Create a Consumer value that writes its result to the shared ResultRef.
mkConsumerValue ::
  ResultRef ->
  Env ->
  Consumer ->
  Value
mkConsumerValue ref env consumer = Consumer $ \value -> do
  result <- local (const env) $ applyConsumerDirect ref consumer value
  liftIO $ writeIORef ref result

-- | Apply a named consumer to a value.
-- Invokes the Consumer callback, then reads the result from the shared IORef.
applyConsumer ::
  ( Error EvalError :> es,
    Reader Env :> es,
    Reader Toplevels :> es,
    Reader Handlers :> es,
    IOE :> es
  ) =>
  ResultRef ->
  Range ->
  Name ->
  Value ->
  Eff es Value
applyConsumer ref r name value = do
  covalue <- lookupEnv r name
  case covalue of
    Consumer repr -> do
      repr value
      liftIO $ readIORef ref
    _ -> throwError $ ExpectConsumer r value

-- | Directly apply a Consumer AST node to a value.
-- Returns the resulting value without going through the IORef bridge.
applyConsumerDirect ::
  ( Error EvalError :> es,
    Reader Env :> es,
    Reader Toplevels :> es,
    Reader Handlers :> es,
    IOE :> es
  ) =>
  ResultRef ->
  Consumer ->
  Value ->
  Eff es Value
applyConsumerDirect ref (Label r label) given = do
  covalue <- lookupEnv r label
  case covalue of
    Consumer repr -> do
      repr given
      liftIO $ readIORef ref
    _ -> throwError $ ExpectConsumer r covalue
applyConsumerDirect ref (Apply r producers consumers) given = do
  values <- traverse evalProducer producers
  covalues <- traverse (lookupEnv r) consumers
  case given of
    Function env parameters statement ->
      local (const $ extendEnv' (zip parameters $ values <> covalues) env) do
        evalStatement ref statement
    _ -> throwError $ ExpectFunction r given
applyConsumerDirect ref (Project r field consumer) given = do
  covalue <- lookupEnv r consumer
  case given of
    Record env fields -> do
      (name, statement) <- case Map.lookup field fields of
        Just value -> pure value
        Nothing -> throwError $ NoSuchField r field given
      local (const $ extendEnv name covalue env) do
        evalStatement ref statement
    _ -> throwError $ ExpectRecord r given
applyConsumerDirect ref (Then _ name statement) given = do
  local (extendEnv name given) do
    evalStatement ref statement
applyConsumerDirect _ (Finish _) given = pure given
applyConsumerDirect ref (Select r branches) given = go branches
  where
    go [] = throwError $ NoMatch r given
    go (Branch {pattern, statement} : rest) = do
      bindings <- match pattern given
      case bindings of
        Just bindings -> do
          local (extendEnv' bindings) $ evalStatement ref statement
        Nothing -> go rest
