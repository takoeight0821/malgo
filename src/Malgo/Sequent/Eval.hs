{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Sequent.Eval (Value (..), EvalError (..), Env (..), emptyEnv, Handlers (..), evalProgram, EvalPass (..)) where

import Control.Exception (Exception)
import Data.IntMap.Strict qualified as IntMap
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Text qualified as T
import Data.Traversable (for)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Static.Local (State)
import Malgo.Id
import Malgo.Module (ModuleName)
import Malgo.Pass
import Malgo.Prelude hiding (getContents)
import Malgo.SExpr (sShow)
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Fun (Literal (..), Name, Pattern (..), Tag (..))

data EvalPass = EvalPass

instance Pass EvalPass where
  type Input EvalPass = (ModuleName, Handlers, Program)
  type Output EvalPass = ()
  type ErrorType EvalPass = EvalError
  type Effects EvalPass es = (State Uniq :> es, IOE :> es)

  runPassImpl _ (moduleName, handlers, program) = runReader moduleName $ runReader handlers $ evalProgram program

fromConsumer :: Env -> Consumer -> Value
fromConsumer env consumer = Consumer $ \value -> do
  local (const env) $ evalConsumer consumer value

data Value where
  Immediate :: Literal -> Value
  Struct :: Tag -> [Value] -> Value
  Function :: Env -> [Name] -> Statement -> Value
  Record :: Env -> Map Text (Name, Statement) -> Value
  Consumer ::
    ( forall es.
      ( Error EvalError :> es,
        Reader Env :> es,
        Reader Toplevels :> es,
        Reader Handlers :> es,
        IOE :> es
      ) =>
      Value -> Eff es ()
    ) ->
    Value

deriving via (PrettyShow Value) instance Pretty Value

instance Show Value where
  showsPrec d (Immediate literal) = showParen (d > 10) $ showString "Immediate " . showsPrec 11 literal
  showsPrec d (Struct tag values) = showParen (d > 10) $ showString "Struct " . showsPrec 11 tag . showString " " . showsPrec 11 values
  showsPrec d (Function _env params stmt) = showParen (d > 10) $ showString "Function <env> " . showsPrec 11 params . showString " " . shows (sShow @_ @String stmt)
  showsPrec d (Record _env fields) =
    let fields' = fmap (\(name, statement) -> (name, sShow @_ @String statement)) fields
     in showParen (d > 10) $ showString "Record <env> " . showsPrec 11 fields'
  showsPrec d (Consumer _) = showParen (d > 10) $ showString "Consumer <consumer>"

instance Eq Value where
  Immediate a == Immediate b = a == b
  Struct a b == Struct c d = a == c && b == d
  _ == _ = False

data EvalError
  = UndefinedVariable Range Name
  | ExpectConsumer Range Value
  | ExpectFunction Range Value
  | ExpectRecord Range Value
  | ExpectNumber Range Value
  | NoSuchField Range Text Value
  | NoMatch Range Value
  | PrimitiveNotImplemented Range Text [Value]
  | InvalidArguments Range Text [Value]

instance Show EvalError where
  show (UndefinedVariable range name) = show $ pretty range <> ": Undefined variable: " <> pretty name
  show (ExpectConsumer range value) = show $ pretty range <> ": Expecting a consumer, but got: " <> pretty value
  show (ExpectFunction range value) = show $ pretty range <> ": Expecting a function, but got: " <> pretty value
  show (ExpectRecord range value) = show $ pretty range <> ": Expecting a record, but got: " <> pretty value
  show (ExpectNumber range value) = show $ pretty range <> ": Expecting a number, but got: " <> pretty value
  show (NoSuchField range field value) = show $ pretty range <> ": No such field: " <> pretty field <> " in " <> pretty value
  show (NoMatch range value) = show $ pretty range <> ": No match for " <> pretty value
  show (PrimitiveNotImplemented range name values) = show $ pretty range <> ": Primitive " <> pretty name <> " is not implemented for " <> pretty values
  show (InvalidArguments range name values) = show $ pretty range <> ": Invalid arguments for " <> pretty name <> ": " <> pretty values

instance Exception EvalError

type Toplevels = Map Name (Name, Statement)

data Handlers = Handlers
  { stdin :: IO (Maybe Char),
    stdout :: Char -> IO (),
    stderr :: Char -> IO ()
  }

data Env = Env
  { localBindings :: !(IntMap.IntMap Value), -- Internal/Temporal IDs (high frequency)
    externalBindings :: !(Map Name Value) -- External IDs (low frequency)
  }
  deriving stock (Show)

-- | Extract integer key from Name for IntMap lookup
-- Returns Nothing for External IDs
nameToIntKey :: Name -> Maybe Int
nameToIntKey Id {sort = Internal n} = Just n
nameToIntKey Id {sort = Temporal n} = Just n
nameToIntKey Id {sort = External} = Nothing
{-# INLINE nameToIntKey #-}

emptyEnv :: Env
emptyEnv = Env IntMap.empty Map.empty

extendEnv :: Name -> Value -> Env -> Env
extendEnv name value env = case nameToIntKey name of
  Just key -> env {localBindings = IntMap.insert key value env.localBindings}
  Nothing -> env {externalBindings = Map.insert name value env.externalBindings}
{-# INLINE extendEnv #-}

extendEnv' :: [(Name, Value)] -> Env -> Env
extendEnv' pairs env =
  let (locals, externals) = partitionBindings pairs
   in env
        { localBindings = foldr (uncurry IntMap.insert) env.localBindings locals,
          externalBindings = foldr (uncurry Map.insert) env.externalBindings externals
        }
  where
    partitionBindings :: [(Name, Value)] -> ([(Int, Value)], [(Name, Value)])
    partitionBindings = foldr classify ([], [])
      where
        classify (n, v) (ls, es) = case nameToIntKey n of
          Just key -> ((key, v) : ls, es)
          Nothing -> (ls, (n, v) : es)
{-# INLINE extendEnv' #-}

lookupEnv :: (Error EvalError :> es, Reader Env :> es) => Range -> Name -> Eff es Value
lookupEnv range name = do
  env <- ask @Env
  case nameToIntKey name of
    Just key -> case IntMap.lookup key env.localBindings of
      Just value -> pure value
      Nothing -> throwError (UndefinedVariable range name)
    Nothing -> case Map.lookup name env.externalBindings of
      Just value -> pure value
      Nothing -> throwError (UndefinedVariable range name)
{-# INLINE lookupEnv #-}

jump :: (Error EvalError :> es, Reader Toplevels :> es, Reader Env :> es, Reader Handlers :> es, IOE :> es) => Range -> Name -> Value -> Eff es ()
jump range name value = do
  covalue <- lookupEnv range name
  case covalue of
    Consumer repr -> repr value
    _ -> throwError $ ExpectConsumer range value

lookupToplevel :: (Reader Toplevels :> es, Error EvalError :> es) => Range -> Name -> Eff es (Name, Statement)
lookupToplevel range name = do
  toplevels <- ask @Toplevels
  case Map.lookup name toplevels of
    Just value -> pure value
    Nothing -> throwError (UndefinedVariable range name)

evalProgram :: (Error EvalError :> es, State Uniq :> es, Reader ModuleName :> es, Reader Handlers :> es, IOE :> es) => Program -> Eff es ()
evalProgram (Program {definitions}) = do
  let toplevels = Map.fromList [(name, (return, statement)) | (_, name, return, statement) <- definitions]
  case Map.keys toplevels & find (\name -> name.name == "main") of
    Just name -> do
      let (return, statement) = fromJust $ Map.lookup name toplevels
      runReader toplevels
        $ runReader emptyEnv do
          finish <- newTemporalId "finish"
          evalStatement
            $ Join (range statement) finish (Finish (range statement))
            $ Join
              (range statement)
              return
              (Apply (range statement) [Construct (range statement) Tuple [] []] [finish])
              statement
    Nothing -> pure () -- No main function

evalStatement :: (Error EvalError :> es, Reader Env :> es, Reader Toplevels :> es, Reader Handlers :> es, IOE :> es) => Statement -> Eff es ()
evalStatement (Cut producer consumer) = do
  value <- evalProducer producer
  jump (range producer) consumer value
evalStatement (Join _ label consumer statement) = do
  env <- ask @Env
  let value = fromConsumer env consumer
  local (extendEnv label value) do
    evalStatement statement
evalStatement (Primitive range name producers consumer) = do
  producers <- traverse evalProducer producers
  result <- fetchPrimitive name range producers
  jump range consumer result
evalStatement (Invoke range name consumer) = do
  (return, statement) <- lookupToplevel range name
  covalue <- lookupEnv range consumer
  local (extendEnv return covalue) do
    evalStatement statement
{-# INLINE evalStatement #-}

evalProducer :: (Error EvalError :> es, Reader Env :> es) => Producer -> Eff es Value
evalProducer (Var range name) = lookupEnv range name
evalProducer (Literal _ literal) = pure $ Immediate literal
evalProducer (Construct range tag producers consumers) = do
  producers <- traverse evalProducer producers
  consumers <- traverse (lookupEnv range) consumers
  pure $ Struct tag (producers <> consumers)
evalProducer (Lambda _ parameters statement) = do
  env <- ask @Env
  pure $ Function env parameters statement
evalProducer (Object _ fields) = do
  env <- ask @Env
  pure $ Record env fields
{-# INLINE evalProducer #-}

evalConsumer :: (Error EvalError :> es, Reader Env :> es, Reader Toplevels :> es, Reader Handlers :> es, IOE :> es) => Consumer -> Value -> Eff es ()
evalConsumer (Label range label) given = do
  covalue <- lookupEnv range label
  case covalue of
    Consumer repr -> repr given
    _ -> throwError $ ExpectConsumer range covalue
evalConsumer (Apply range producers consumers) given = do
  producers <- traverse evalProducer producers
  consumers <- traverse (lookupEnv range) consumers
  case given of
    Function env parameters statement ->
      local (const $ extendEnv' (zip parameters $ producers <> consumers) env) do
        evalStatement statement
    _ -> throwError $ ExpectFunction range given
evalConsumer (Project range field consumer) given = do
  covalue <- lookupEnv range consumer
  case given of
    Record env fields -> do
      (name, statement) <- case Map.lookup field fields of
        Just value -> pure value
        Nothing -> throwError $ NoSuchField range field given
      local (const $ extendEnv name covalue env) do
        evalStatement statement
    _ -> throwError $ ExpectRecord range given
evalConsumer (Then _ name statement) given = do
  local (extendEnv name given) do
    evalStatement statement
evalConsumer (Finish _) _ = pure ()
evalConsumer (Select range branches) given = go branches
  where
    go [] = throwError $ NoMatch range given
    go (Branch {pattern, statement} : rest) = do
      bindings <- match pattern given
      case bindings of
        Just bindings -> do
          local (extendEnv' bindings) $ evalStatement statement
        Nothing -> go rest
{-# INLINE evalConsumer #-}

-- | Difference list for O(1) append
type DList a = [a] -> [a]

emptyDList :: DList a
emptyDList = \x -> x
{-# INLINE emptyDList #-}

singletonDList :: a -> DList a
singletonDList x = (x :)
{-# INLINE singletonDList #-}

appendDList :: DList a -> DList a -> DList a
appendDList = (.)
{-# INLINE appendDList #-}

dlistToList :: DList a -> [a]
dlistToList dl = dl []
{-# INLINE dlistToList #-}

match :: (Error EvalError :> es, Reader Env :> es, Reader Toplevels :> es, Reader Handlers :> es, IOE :> es) => Pattern -> Value -> Eff es (Maybe [(Name, Value)])
match pat val = fmap dlistToList <$> matchDL pat val
{-# INLINE match #-}

-- | Single-pass pattern matching using difference lists
matchDL :: (Error EvalError :> es, Reader Env :> es, Reader Toplevels :> es, Reader Handlers :> es, IOE :> es) => Pattern -> Value -> Eff es (Maybe (DList (Name, Value)))
matchDL (PVar _ name) value = pure $ Just $ singletonDList (name, value)
matchDL (PLiteral _ literal) (Immediate literal')
  | literal == literal' = pure $ Just emptyDList
  | otherwise = pure Nothing
matchDL (Destruct _ tag patterns) (Struct tag' values)
  | tag == tag' = matchMany patterns values
  | otherwise = pure Nothing
matchDL (Expand _ patterns) (Record env fields) = local (const env) do
  let pairs = Map.toList $ Map.intersectionWith (,) patterns fields
  matchExpand pairs
matchDL _ _ = pure Nothing

-- | Match multiple patterns against values (single pass with early return)
matchMany :: (Error EvalError :> es, Reader Env :> es, Reader Toplevels :> es, Reader Handlers :> es, IOE :> es) => [Pattern] -> [Value] -> Eff es (Maybe (DList (Name, Value)))
matchMany [] [] = pure $ Just emptyDList
matchMany (p : ps) (v : vs) = do
  result <- matchDL p v
  case result of
    Nothing -> pure Nothing -- Early return on failure
    Just bindings -> do
      rest <- matchMany ps vs
      pure $ appendDList bindings <$> rest
matchMany _ _ = pure Nothing -- Length mismatch
{-# INLINE matchMany #-}

-- | Match expand patterns (for records)
matchExpand :: (Error EvalError :> es, Reader Env :> es, Reader Toplevels :> es, Reader Handlers :> es, IOE :> es) => [(Text, (Pattern, (Name, Statement)))] -> Eff es (Maybe (DList (Name, Value)))
matchExpand [] = pure $ Just emptyDList
matchExpand ((_, (pattern, (return, statement))) : rest) = do
  -- Evaluate the statement and capture result via IORef
  ref <- newIORef Nothing
  local (extendEnv return (Consumer $ writeIORef ref . Just)) do
    _ <- evalStatement statement
    value <- readIORef ref
    case value of
      Nothing -> pure Nothing
      Just v -> do
        result <- matchDL pattern v
        case result of
          Nothing -> pure Nothing -- Early return
          Just bindings -> do
            restResult <- matchExpand rest
            pure $ appendDList bindings <$> restResult
{-# INLINE matchExpand #-}

-- | Convert any Value to a Text representation for printing
valueToText :: Value -> Text
valueToText (Immediate (Int32 n)) = convertString (show n)
valueToText (Immediate (Int64 n)) = convertString (show n)
valueToText (Immediate (Float f)) = convertString (show f)
valueToText (Immediate (Double d)) = convertString (show d)
valueToText (Immediate (Char c)) = T.singleton c
valueToText (Immediate (String s)) = s
valueToText (Struct Tuple []) = "{}"
valueToText (Struct Tuple values) = "{" <> T.intercalate ", " (map valueToText values) <> "}"
valueToText (Struct (Tag name) []) = name
valueToText (Struct (Tag name) values) = name <> "(" <> T.intercalate ", " (map valueToText values) <> ")"
valueToText (Function {}) = "<function>"
valueToText (Record {}) = "<record>"
valueToText (Consumer _) = "<consumer>"

fetchPrimitive :: (Error EvalError :> es, Reader Handlers :> es, IOE :> es) => Text -> Range -> [Value] -> Eff es Value
fetchPrimitive "malgo_unsafe_cast" = \cases
  _ [value] -> pure value
  range values -> throwError $ InvalidArguments range "malgo_unsafe_cast" values
fetchPrimitive name | "malgo_add_" `T.isPrefixOf` name = binary name addValue
fetchPrimitive name | "malgo_sub_" `T.isPrefixOf` name = binary name subValue
fetchPrimitive name | "malgo_mul_" `T.isPrefixOf` name = binary name mulValue
-- TODO: add "malgo_div_" and "malgo_mod_"
fetchPrimitive name | "malgo_eq_" `T.isPrefixOf` name = binary name eqValue
fetchPrimitive name | "malgo_ne_" `T.isPrefixOf` name = binary name neValue
fetchPrimitive name | "malgo_lt_" `T.isPrefixOf` name = binary name ltValue
fetchPrimitive name | "malgo_le_" `T.isPrefixOf` name = binary name leValue
fetchPrimitive name | "malgo_gt_" `T.isPrefixOf` name = binary name gtValue
fetchPrimitive name | "malgo_ge_" `T.isPrefixOf` name = binary name geValue
fetchPrimitive name | "malgo_" `T.isPrefixOf` name && "to_string" `T.isSuffixOf` name = toString name
fetchPrimitive "malgo_print_string" = \cases
  _ [Immediate (String text)] -> do
    Handlers {stdout} <- ask @Handlers
    putTextTo stdout text
    pure $ Struct Tuple []
  range values -> throwError $ InvalidArguments range "malgo_print_string" values
fetchPrimitive "malgo_newline" = \_ _ -> do
  Handlers {stdout} <- ask @Handlers
  liftIO $ stdout '\n'
  pure $ Struct Tuple []
fetchPrimitive "malgo_get_contents" = \_ _ -> do
  Immediate . String <$> getContents
fetchPrimitive "malgo_string_append" = \cases
  _ [Immediate (String a), Immediate (String b)] -> pure $ Immediate $ String $ a <> b
  range values -> throwError $ InvalidArguments range "malgo_string_append" values
fetchPrimitive "malgo_print" = \cases
  _ [value] -> do
    Handlers {stdout} <- ask @Handlers
    let text = valueToText value
    putTextTo stdout text
    pure $ Struct Tuple []
  range values -> throwError $ InvalidArguments range "malgo_print" values
fetchPrimitive name = \range values -> throwError $ PrimitiveNotImplemented range name values

getContents :: (IOE :> es, Reader Handlers :> es) => Eff es Text
getContents = do
  Handlers {stdin} <- ask @Handlers
  char <- liftIO stdin
  case char of
    Just char -> do
      T.cons char <$> getContents
    Nothing -> pure ""

putTextTo :: (IOE :> es) => (Char -> IO ()) -> Text -> Eff es ()
putTextTo stream text = do
  let string = convertString @_ @String text
  liftIO $ traverse_ stream string

toString :: (Error EvalError :> es) => Text -> Range -> [Value] -> Eff es Value
toString _ _ [Immediate (Int32 n)] = pure $ Immediate $ String $ T.pack $ show n
toString _ _ [Immediate (Int64 n)] = pure $ Immediate $ String $ T.pack $ show n
toString _ _ [Immediate (Float n)] = pure $ Immediate $ String $ T.pack $ show n
toString _ _ [Immediate (Double n)] = pure $ Immediate $ String $ T.pack $ show n
toString _ _ [Immediate (String s)] = pure $ Immediate $ String s
toString name range values = throwError $ InvalidArguments range name values

binary :: (Error EvalError :> es) => Text -> (Range -> Value -> Value -> Eff es Value) -> Range -> [Value] -> Eff es Value
binary _ f range [a, b] = f range a b
binary name _ range values = throwError $ InvalidArguments range name values

addValue :: (Error EvalError :> es) => Range -> Value -> Value -> Eff es Value
addValue _ (Immediate (Int32 a)) (Immediate (Int32 b)) = pure $ Immediate $ Int32 $ a + b
addValue _ (Immediate (Int64 a)) (Immediate (Int64 b)) = pure $ Immediate $ Int64 $ a + b
addValue _ (Immediate (Float a)) (Immediate (Float b)) = pure $ Immediate $ Float $ a + b
addValue _ (Immediate (Double a)) (Immediate (Double b)) = pure $ Immediate $ Double $ a + b
addValue range a b = throwError $ InvalidArguments range "malgo_add" [a, b]

subValue :: (Error EvalError :> es) => Range -> Value -> Value -> Eff es Value
subValue _ (Immediate (Int32 a)) (Immediate (Int32 b)) = pure $ Immediate $ Int32 $ a - b
subValue _ (Immediate (Int64 a)) (Immediate (Int64 b)) = pure $ Immediate $ Int64 $ a - b
subValue _ (Immediate (Float a)) (Immediate (Float b)) = pure $ Immediate $ Float $ a - b
subValue _ (Immediate (Double a)) (Immediate (Double b)) = pure $ Immediate $ Double $ a - b
subValue range a b = throwError $ InvalidArguments range "malgo_sub" [a, b]

mulValue :: (Error EvalError :> es) => Range -> Value -> Value -> Eff es Value
mulValue _ (Immediate (Int32 a)) (Immediate (Int32 b)) = pure $ Immediate $ Int32 $ a * b
mulValue _ (Immediate (Int64 a)) (Immediate (Int64 b)) = pure $ Immediate $ Int64 $ a * b
mulValue _ (Immediate (Float a)) (Immediate (Float b)) = pure $ Immediate $ Float $ a * b
mulValue _ (Immediate (Double a)) (Immediate (Double b)) = pure $ Immediate $ Double $ a * b
mulValue range a b = throwError $ InvalidArguments range "malgo_mul" [a, b]

eqValue :: Range -> Value -> Value -> Eff es Value
eqValue _ v1 v2 = if v1 == v2 then pure $ Immediate $ Int32 1 else pure $ Immediate $ Int32 0

neValue :: Range -> Value -> Value -> Eff es Value
neValue _ v1 v2 = if v1 /= v2 then pure $ Immediate $ Int32 1 else pure $ Immediate $ Int32 0

ltValue :: (Error EvalError :> es) => Range -> Value -> Value -> Eff es Value
ltValue _ (Immediate (Int32 a)) (Immediate (Int32 b)) = pure $ Immediate $ Int32 $ if a < b then 1 else 0
ltValue _ (Immediate (Int64 a)) (Immediate (Int64 b)) = pure $ Immediate $ Int32 $ if a < b then 1 else 0
ltValue _ (Immediate (Float a)) (Immediate (Float b)) = pure $ Immediate $ Int32 $ if a < b then 1 else 0
ltValue _ (Immediate (Double a)) (Immediate (Double b)) = pure $ Immediate $ Int32 $ if a < b then 1 else 0
ltValue range a b = throwError $ InvalidArguments range "malgo_lt" [a, b]

leValue :: (Error EvalError :> es) => Range -> Value -> Value -> Eff es Value
leValue _ (Immediate (Int32 a)) (Immediate (Int32 b)) = pure $ Immediate $ Int32 $ if a <= b then 1 else 0
leValue _ (Immediate (Int64 a)) (Immediate (Int64 b)) = pure $ Immediate $ Int32 $ if a <= b then 1 else 0
leValue _ (Immediate (Float a)) (Immediate (Float b)) = pure $ Immediate $ Int32 $ if a <= b then 1 else 0
leValue _ (Immediate (Double a)) (Immediate (Double b)) = pure $ Immediate $ Int32 $ if a <= b then 1 else 0
leValue range a b = throwError $ InvalidArguments range "malgo_le" [a, b]

gtValue :: (Error EvalError :> es) => Range -> Value -> Value -> Eff es Value
gtValue _ (Immediate (Int32 a)) (Immediate (Int32 b)) = pure $ Immediate $ Int32 $ if a > b then 1 else 0
gtValue _ (Immediate (Int64 a)) (Immediate (Int64 b)) = pure $ Immediate $ Int32 $ if a > b then 1 else 0
gtValue _ (Immediate (Float a)) (Immediate (Float b)) = pure $ Immediate $ Int32 $ if a > b then 1 else 0
gtValue _ (Immediate (Double a)) (Immediate (Double b)) = pure $ Immediate $ Int32 $ if a > b then 1 else 0
gtValue range a b = throwError $ InvalidArguments range "malgo_gt" [a, b]

geValue :: (Error EvalError :> es) => Range -> Value -> Value -> Eff es Value
geValue _ (Immediate (Int32 a)) (Immediate (Int32 b)) = pure $ Immediate $ Int32 $ if a >= b then 1 else 0
geValue _ (Immediate (Int64 a)) (Immediate (Int64 b)) = pure $ Immediate $ Int32 $ if a >= b then 1 else 0
geValue _ (Immediate (Float a)) (Immediate (Float b)) = pure $ Immediate $ Int32 $ if a >= b then 1 else 0
geValue _ (Immediate (Double a)) (Immediate (Double b)) = pure $ Immediate $ Int32 $ if a >= b then 1 else 0
geValue range a b = throwError $ InvalidArguments range "malgo_ge" [a, b]
