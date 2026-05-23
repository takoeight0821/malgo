{-# LANGUAGE UndecidableInstances #-}

-- | Internal type representation and constraint types for type inference.
-- Separate from surface syntax types to keep inference internals clean.
module Malgo.Infer.Constraint
  ( -- * Internal type representation
    Ty (..),
    Scheme (..),
    Subst,
    Level,

    -- * Constraint types
    TyConstraint (..),

    -- * Type operations
    applySubst,
    freeVars,
    occursIn,
    generalize,
    instantiate,

    -- * Generation state
    GenState (..),
    GenM,
    freshTyVar,
    addConstraint,
    enterLevel,
    exitLevel,
    currentSubst,

    -- * Standard types
    tyInt32,
    tyInt64,
    tyFloat,
    tyDouble,
    tyChar,
    tyString,
    tyUnit,

    -- * Errors
    InferError (..),

    -- * Helpers
    dummyRange,
  )
where

import Control.Exception (Exception (..))
import Data.List (intersperse)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State, gets, modify)
import Malgo.Id (Id, newTemporalId)
import Malgo.Module (ModuleName)
import Malgo.Prelude hiding (Constraint)
import Text.Megaparsec.Pos qualified as Megaparsec

-- | Level for let-polymorphism.
-- Higher level means deeper let-nesting.
type Level = Int

-- | Internal type representation for inference.
-- Separate from surface syntax Type to allow inference-specific constructs.
--
-- Binders ('TForall', 'TMu') are nameless; bound occurrences are represented
-- by 'TBound' with a de Bruijn *index* (0 = innermost enclosing binder).
-- This makes alpha-equivalent types structurally equal under '(==)', so the
-- unifier's cycle-detection set requires no canonicalization.
data Ty
  = -- | Type variable with its creation level. Identity is carried by 'Id'
    -- so that variables originating from different scopes never collide on
    -- their surface name (e.g. two distinct 'a's coming from a signature
    -- and a synonym parameter).
    TVar Id Level
  | -- | de Bruijn index for a variable bound by an enclosing 'TForall'/'TMu'.
    -- Index 0 refers to the immediately enclosing binder.
    TBound Int
  | -- | Type constructor (e.g., Int32, Bool)
    TCon T.Text
  | -- | Function arrow: arg -> result
    TArr Ty Ty
  | -- | Type application: f a
    TApp Ty Ty
  | -- | Tuple type
    TTuple [Ty]
  | -- | Record type with optional row tail for row polymorphism
    TRecord [(T.Text, Ty)] (Maybe Ty)
  | -- | Variant type with optional row tail for row polymorphism
    TVariant [(T.Text, [Ty])] (Maybe Ty)
  | -- | Bottom type (for non-terminating expressions)
    TBottom
  | -- | Forall quantifier. The bound variable is anonymous; use 'TBound 0'
    -- inside the body to refer to it.
    TForall Ty
  | -- | Recursive type: mu. ty. Use 'TBound 0' inside the body for
    -- the self-reference.
    TMu Ty
  deriving stock (Eq, Show, Ord)

instance Pretty Ty where
  pretty = prettyTyWith []

-- | Pretty-print a 'Ty' given an environment of bound-variable names.
-- @env@ is ordered innermost-first: @env !! 0@ is the name for 'TBound 0'.
prettyTyWith :: [T.Text] -> Ty -> Doc ann
prettyTyWith _ (TVar name _) = pretty name
prettyTyWith env (TBound i)
  | i < length env = pretty (env !! i)
  | otherwise = "?" <> pretty i
prettyTyWith _ (TCon name) = pretty name
prettyTyWith env (TArr arg ret) = "(" <> prettyTyWith env arg <> " -> " <> prettyTyWith env ret <> ")"
prettyTyWith env (TApp f a) = "(" <> prettyTyWith env f <> " " <> prettyTyWith env a <> ")"
prettyTyWith env (TTuple ts) = "(" <> mconcat (intersperse ", " $ map (prettyTyWith env) ts) <> ")"
prettyTyWith env (TRecord fields rowTail) =
  "{"
    <> mconcat (intersperse ", " $ map (\(k, v) -> pretty k <> " : " <> prettyTyWith env v) fields)
    <> maybe "" (\r -> " | " <> prettyTyWith env r) rowTail
    <> "}"
prettyTyWith env (TVariant cases rowTail) =
  "["
    <> mconcat (intersperse " | " $ map (\(c, ts) -> pretty c <> " " <> mconcat (intersperse " " $ map (prettyTyWith env) ts)) cases)
    <> maybe "" (\r -> " | " <> prettyTyWith env r) rowTail
    <> "]"
prettyTyWith _ TBottom = "_|_"
prettyTyWith env (TForall body) =
  let n = boundVarName (length env)
   in "(forall " <> pretty n <> ". " <> prettyTyWith (n : env) body <> ")"
prettyTyWith env (TMu body) =
  let n = boundVarName (length env)
   in "(mu " <> pretty n <> ". " <> prettyTyWith (n : env) body <> ")"

boundVarName :: Int -> T.Text
boundVarName n
  | n < 26 = T.singleton (toEnum (fromEnum 'a' + n))
  | otherwise = "t" <> T.pack (show (n - 26))

-- | Type scheme for let-polymorphism
data Scheme = Scheme
  { vars :: [Id],
    ty :: Ty
  }
  deriving stock (Eq, Show)

-- | Type substitution keyed by 'Id' so that distinct variables sharing a
-- surface name (e.g. an outer-scope @a@ vs a synonym parameter @a@) cannot
-- collide.
type Subst = Map Id Ty

-- | Apply a substitution to a type.
-- Only 'TVar' nodes (inference metavariables) are substituted; 'TBound'
-- (de Bruijn-indexed binder occurrences) are never touched.
applySubst :: Subst -> Ty -> Ty
applySubst subst ty@(TVar name _) = case Map.lookup name subst of
  Just ty' -> applySubst subst ty'
  Nothing -> ty
applySubst _ ty@(TBound _) = ty
applySubst _ ty@(TCon _) = ty
applySubst subst (TArr a b) = TArr (applySubst subst a) (applySubst subst b)
applySubst subst (TApp f a) = TApp (applySubst subst f) (applySubst subst a)
applySubst subst (TTuple ts) = TTuple (map (applySubst subst) ts)
applySubst subst (TRecord fields rowTail) =
  TRecord
    (map (second (applySubst subst)) fields)
    (fmap (applySubst subst) rowTail)
applySubst subst (TVariant cases rowTail) =
  TVariant
    (map (second (map (applySubst subst))) cases)
    (fmap (applySubst subst) rowTail)
applySubst _ TBottom = TBottom
applySubst subst (TForall ty) = TForall (applySubst subst ty)
applySubst subst (TMu ty) = TMu (applySubst subst ty)

-- | Free type variables (inference metavariables) in a type.
-- 'TBound' indices are not free variables; they are bound by an enclosing
-- 'TForall'/'TMu' and do not appear in the result set.
freeVars :: Ty -> Set Id
freeVars (TVar name _) = Set.singleton name
freeVars (TBound _) = Set.empty
freeVars (TCon _) = Set.empty
freeVars (TArr a b) = freeVars a <> freeVars b
freeVars (TApp f a) = freeVars f <> freeVars a
freeVars (TTuple ts) = foldMap freeVars ts
freeVars (TRecord fields rowTail) =
  foldMap (freeVars . snd) fields <> foldMap freeVars rowTail
freeVars (TVariant cases rowTail) =
  foldMap (\(_, ts) -> foldMap freeVars ts) cases <> foldMap freeVars rowTail
freeVars TBottom = Set.empty
freeVars (TForall ty) = freeVars ty
freeVars (TMu ty) = freeVars ty

-- | Occurs check: does inference metavariable @name@ occur free in a type?
-- 'TBound' indices are never the target of an occurs check.
occursIn :: Id -> Ty -> Bool
occursIn name (TVar v _) = name == v
occursIn _ (TBound _) = False
occursIn _ (TCon _) = False
occursIn name (TArr a b) = occursIn name a || occursIn name b
occursIn name (TApp f a) = occursIn name f || occursIn name a
occursIn name (TTuple ts) = any (occursIn name) ts
occursIn name (TRecord fields rowTail) =
  any (occursIn name . snd) fields || maybe False (occursIn name) rowTail
occursIn name (TVariant cases rowTail) =
  any (\(_, ts) -> any (occursIn name) ts) cases || maybe False (occursIn name) rowTail
occursIn _ TBottom = False
occursIn name (TForall ty) = occursIn name ty
occursIn name (TMu ty) = occursIn name ty

-- | Generalize a type by quantifying over free variables not in the environment
-- and with level strictly greater than the given level.
generalize :: Level -> Ty -> Scheme
generalize lvl ty =
  let vars = [v | TVar v l <- collectVars ty, l > lvl]
   in Scheme {vars = nub vars, ty = ty}
  where
    collectVars :: Ty -> [Ty]
    collectVars tv@(TVar _ _) = [tv]
    collectVars (TBound _) = []
    collectVars (TCon _) = []
    collectVars (TArr a b) = collectVars a <> collectVars b
    collectVars (TApp f a) = collectVars f <> collectVars a
    collectVars (TTuple ts) = concatMap collectVars ts
    collectVars (TRecord fields rowTail) =
      concatMap (collectVars . snd) fields <> concatMap collectVars (maybeToList rowTail)
    collectVars (TVariant cases rowTail) =
      concatMap (\(_, ts) -> concatMap collectVars ts) cases <> concatMap collectVars (maybeToList rowTail)
    collectVars TBottom = []
    -- TBound indices are not free metavariables, so TForall/TMu bodies are
    -- traversed without any filtering.
    collectVars (TForall t) = collectVars t
    collectVars (TMu t) = collectVars t

-- | Instantiate a scheme by replacing quantified variables with fresh type variables
instantiate ::
  ( State GenState :> es,
    State Uniq :> es,
    Reader ModuleName :> es
  ) =>
  Scheme ->
  Eff es Ty
instantiate Scheme {vars, ty} = do
  freshVars <- traverse (\v -> (v,) <$> freshTyVar) vars
  let subst = Map.fromList freshVars
  pure $ applySubst subst ty

-- | Type constraints generated during inference
data TyConstraint
  = -- | Unification: t1 = t2
    CUnify Range Ty Ty
  | -- | Bottom propagation: if any source is bottom, target is bottom
    CBottomProp [Ty] Ty
  deriving stock (Eq, Show)

-- | State for constraint generation
data GenState = GenState
  { constraints :: [TyConstraint],
    currentLevel :: Level,
    solvedSubst :: Subst
  }
  deriving stock (Show)

-- | Constraint generation monad
type GenM es = (State GenState :> es, Error InferError :> es)

-- | Fresh type variable at the current level. Uses 'newTemporalId' so the
-- variable carries a globally unique 'Id' rather than a possibly-colliding
-- 'Text' name, which lets 'applySubst' / 'occursIn' / 'unify' compare
-- variables structurally without false positives.
freshTyVar ::
  ( State GenState :> es,
    State Uniq :> es,
    Reader ModuleName :> es
  ) =>
  Eff es Ty
freshTyVar = do
  st <- gets @GenState (.currentLevel)
  freshId <- newTemporalId "_t"
  pure $ TVar freshId st

-- | Add a constraint
addConstraint :: (State GenState :> es) => TyConstraint -> Eff es ()
addConstraint c = modify (\s -> s {constraints = c : s.constraints})

-- | Enter a deeper let-nesting level
enterLevel :: (State GenState :> es) => Eff es ()
enterLevel = modify (\s -> s {currentLevel = s.currentLevel + 1})

-- | Exit a let-nesting level
exitLevel :: (State GenState :> es) => Eff es ()
exitLevel = modify (\s -> s {currentLevel = s.currentLevel - 1})

-- | Get the accumulated substitution
currentSubst :: (State GenState :> es) => Eff es Subst
currentSubst = gets (.solvedSubst)

-- | Standard types
tyInt32, tyInt64, tyFloat, tyDouble, tyChar, tyString, tyUnit :: Ty
tyInt32 = TCon "Int32#"
tyInt64 = TCon "Int64#"
tyFloat = TCon "Float#"
tyDouble = TCon "Double#"
tyChar = TCon "Char#"
tyString = TCon "String#"
tyUnit = TTuple []

-- | A dummy range for compiler-generated constraints
dummyRange :: Range
dummyRange =
  let pos = Megaparsec.SourcePos "<infer>" (Megaparsec.mkPos 1) (Megaparsec.mkPos 1)
   in Range {_start = pos, _end = pos}

-- | Inference error type
data InferError
  = UnificationError Range Ty Ty T.Text
  | UnboundVariable Range Id
  | OccursCheckError Range Id Ty
  | NotImplemented Range T.Text
  | -- | Type synonym refers (transitively) to itself in a use position.
    CyclicSynonym Range Id
  | -- | Synonym applied with the wrong number of arguments. Fields: expected, got.
    SynonymArityMismatch Range Id Int Int
  deriving stock (Show)

instance Exception InferError where
  displayException (UnificationError _pos expected actual msg) =
    "Type error: "
      <> T.unpack msg
      <> "\n  Expected: "
      <> show (pretty expected)
      <> "\n  Actual: "
      <> show (pretty actual)
  displayException (UnboundVariable _pos name) =
    "Unbound variable: " <> show (pretty name)
  displayException (OccursCheckError _pos varName ty) =
    "Occurs check failed: type variable '"
      <> show (pretty varName)
      <> "' occurs in "
      <> show (pretty ty)
  displayException (NotImplemented _pos feature) =
    "Type inference not yet implemented for: " <> T.unpack feature
  displayException (CyclicSynonym _pos name) =
    "Cyclic type synonym: " <> show (pretty name)
  displayException (SynonymArityMismatch _pos name expected got) =
    "Type synonym '"
      <> show (pretty name)
      <> "' expects "
      <> show expected
      <> " argument(s) but got "
      <> show got
