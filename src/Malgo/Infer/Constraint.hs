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
import Data.List (intersperse, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Effectful.State.Static.Local (State, get, gets, modify)
import Malgo.Id (Id)
import Malgo.Prelude hiding (Constraint, State, get, gets, modify, put)
import Text.Megaparsec.Pos qualified as Megaparsec

-- | Level for let-polymorphism.
-- Higher level means deeper let-nesting.
type Level = Int

-- | Internal type representation for inference.
-- Separate from surface syntax Type to allow inference-specific constructs.
data Ty
  = -- | Type variable with its creation level
    TVar T.Text Level
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
  | -- | Forall quantifier (used in Schemes after generalization)
    TForall T.Text Ty
  deriving stock (Eq, Show, Ord)

instance Pretty Ty where
  pretty (TVar name _) = pretty name
  pretty (TCon name) = pretty name
  pretty (TArr arg ret) = "(" <> pretty arg <> " -> " <> pretty ret <> ")"
  pretty (TApp f a) = "(" <> pretty f <> " " <> pretty a <> ")"
  pretty (TTuple ts) = "(" <> mconcat (intersperse ", " $ map pretty ts) <> ")"
  pretty (TRecord fields rowTail) =
    "{"
      <> mconcat (intersperse ", " $ map (\(k, v) -> pretty k <> " : " <> pretty v) fields)
      <> maybe "" (\r -> " | " <> pretty r) rowTail
      <> "}"
  pretty (TVariant cases rowTail) =
    "["
      <> mconcat (intersperse " | " $ map (\(c, ts) -> pretty c <> " " <> mconcat (intersperse " " $ map pretty ts)) cases)
      <> maybe "" (\r -> " | " <> pretty r) rowTail
      <> "]"
  pretty TBottom = "_|_"
  pretty (TForall v ty) = "(forall " <> pretty v <> ". " <> pretty ty <> ")"

-- | Type scheme for let-polymorphism
data Scheme = Scheme
  { vars :: [T.Text],
    ty :: Ty
  }
  deriving stock (Eq, Show)

-- | Type substitution
type Subst = Map T.Text Ty

-- | Apply a substitution to a type
applySubst :: Subst -> Ty -> Ty
applySubst subst ty@(TVar name _) = case Map.lookup name subst of
  Just ty' -> applySubst subst ty'
  Nothing -> ty
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
applySubst subst (TForall v ty) =
  TForall v (applySubst (Map.delete v subst) ty)

-- | Free type variables in a type
freeVars :: Ty -> Set T.Text
freeVars (TVar name _) = Set.singleton name
freeVars (TCon _) = Set.empty
freeVars (TArr a b) = freeVars a <> freeVars b
freeVars (TApp f a) = freeVars f <> freeVars a
freeVars (TTuple ts) = foldMap freeVars ts
freeVars (TRecord fields rowTail) =
  foldMap (freeVars . snd) fields <> foldMap freeVars rowTail
freeVars (TVariant cases rowTail) =
  foldMap (\(_, ts) -> foldMap freeVars ts) cases <> foldMap freeVars rowTail
freeVars TBottom = Set.empty
freeVars (TForall v ty) = Set.delete v (freeVars ty)

-- | Occurs check: does a type variable occur in a type?
occursIn :: T.Text -> Ty -> Bool
occursIn name (TVar v _) = name == v
occursIn _ (TCon _) = False
occursIn name (TArr a b) = occursIn name a || occursIn name b
occursIn name (TApp f a) = occursIn name f || occursIn name a
occursIn name (TTuple ts) = any (occursIn name) ts
occursIn name (TRecord fields rowTail) =
  any (occursIn name . snd) fields || maybe False (occursIn name) rowTail
occursIn name (TVariant cases rowTail) =
  any (\(_, ts) -> any (occursIn name) ts) cases || maybe False (occursIn name) rowTail
occursIn _ TBottom = False
occursIn name (TForall v ty) = name /= v && occursIn name ty

-- | Generalize a type by quantifying over free variables not in the environment
-- and with level strictly greater than the given level.
generalize :: Level -> Ty -> Scheme
generalize lvl ty =
  let vars = [v | TVar v l <- collectVars ty, l > lvl]
   in Scheme {vars = nub vars, ty = ty}
  where
    collectVars :: Ty -> [Ty]
    collectVars tv@(TVar _ _) = [tv]
    collectVars (TCon _) = []
    collectVars (TArr a b) = collectVars a <> collectVars b
    collectVars (TApp f a) = collectVars f <> collectVars a
    collectVars (TTuple ts) = concatMap collectVars ts
    collectVars (TRecord fields rowTail) =
      concatMap (collectVars . snd) fields <> concatMap collectVars (maybeToList rowTail)
    collectVars (TVariant cases rowTail) =
      concatMap (\(_, ts) -> concatMap collectVars ts) cases <> concatMap collectVars (maybeToList rowTail)
    collectVars TBottom = []
    collectVars (TForall _ t) = collectVars t

-- | Instantiate a scheme by replacing quantified variables with fresh type variables
instantiate :: (State GenState :> es) => Scheme -> Eff es Ty
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
  { nextVar :: Int,
    constraints :: [TyConstraint],
    currentLevel :: Level,
    solvedSubst :: Subst
  }
  deriving stock (Show)

-- | Constraint generation monad
type GenM es = (State GenState :> es, Error InferError :> es)

-- | Fresh type variable at the current level
freshTyVar :: (State GenState :> es) => Eff es Ty
freshTyVar = do
  st <- get
  let name = "_t" <> T.pack (show st.nextVar)
  modify (\s -> s {nextVar = s.nextVar + 1})
  pure $ TVar name st.currentLevel

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
  | OccursCheckError Range T.Text Ty
  | NotImplemented Range T.Text
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
      <> T.unpack varName
      <> "' occurs in "
      <> show (pretty ty)
  displayException (NotImplemented _pos feature) =
    "Type inference not yet implemented for: " <> T.unpack feature
