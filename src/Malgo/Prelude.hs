{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Prelude
  ( -- * Reexports
    module Control.Arrow,
    module Lens.Micro,
    module Control.Monad,
    module Control.Monad.Extra,
    module Control.Monad.IO.Class,
    module Data.Bifunctor,
    module Data.Bitraversable,
    module Data.ByteString.Short,
    module Data.Char,
    module Data.Coerce,
    module Data.Data,
    module Data.Either,
    module Data.Foldable,
    module Data.Foldable.Extra,
    module Data.Function,
    module Data.Functor,
    module Data.Hashable,
    module Data.Int,
    module Data.Kind,
    module Data.List,
    module Data.List.NonEmpty,
    module Data.Map.Strict,
    module Data.Maybe,
    module Data.Semigroup,
    module Data.Set,
    module Data.String,
    module Data.String.Conversions,
    module Data.Text,
    module Data.Void,
    module GHC.Exts,
    module GHC.Generics,
    module GHC.Stack,
    module Prelude,
    module System.IO,
    IORef,
    Pretty (..),
    Doc,
    errorDoc,
    render,
    maybeParens,

    -- * Utilities
    identity,
    pass,
    foldMapM,
    replaceOf,
    chomp,
    asumMap,
    PrettyShow (..),

    -- * Lens utilities
    view,

    -- * Lift IO functions

    -- ** Show
    hPrint,

    -- ** String
    hPutStr,
    hPutStrLn,

    -- ** Text
    hPutText,
    hPutTextLn,
    putText,

    -- ** IORef
    newIORef,
    readIORef,
    modifyIORef,
    writeIORef,

    -- * Flag
    Flag (..),
    Target (..),
    EvalMode (..),

    -- * Range
    Range (..),
    HasRange (..),
    errorOn,
    warningOn,

    -- * Uniq
    getUniq,
    Uniq (..),
  )
where

import Control.Applicative
import Control.Arrow ((<<<), (>>>))
import Control.Monad
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteString.Short (ShortByteString)
import Data.Char
import Data.Coerce
import Data.Data (Typeable)
import Data.Either
import Data.Foldable
import Data.Foldable.Extra
import Data.Function (applyWhen, fix, on, (&))
import Data.Functor
import Data.Functor.Const (Const (..), getConst)
import Data.Hashable (Hashable)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Int (Int32, Int64)
import Data.Kind (Constraint)
import Data.List (dropWhileEnd, foldl', sort, transpose)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Alt (..))
import Data.Semigroup
import Data.Set (Set)
import Data.Store ()
import Data.Store.TH (makeStore)
import Data.String
import Data.String.Conversions
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Void
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, state)
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro (ASetter, Getting, over)
import Lens.Micro.TH
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Text (hPutDoc, renderStrict)
import System.Exit (exitFailure)
import System.IO (Handle, stderr, stdin, stdout)
import System.IO qualified
import Text.Megaparsec.Pos (SourcePos (..), unPos)
import Text.Megaparsec.Pos qualified as Megaparsec
import Prelude hiding (id, unzip)

errorDoc :: (HasCallStack) => Doc x -> a
errorDoc x = Prelude.error $ renderString $ layoutSmart defaultLayoutOptions x

-- Pretty SourcePos
instance Pretty Megaparsec.SourcePos where
  pretty = pretty . convertString @_ @Text . Megaparsec.sourcePosPretty

render :: Doc ann -> Text
render = renderStrict . layoutSmart defaultLayoutOptions

maybeParens :: Bool -> Doc ann -> Doc ann
maybeParens True = parens
maybeParens False = identity

identity :: a -> a
identity x = x
{-# INLINE identity #-}

pass :: (Applicative f) => f ()
pass = pure ()
{-# INLINE pass #-}

-- | @foldMapM@ from rio
foldMapM :: (Foldable t, Monad m, Monoid w) => (a -> m w) -> t a -> m w
foldMapM f =
  foldlM
    ( \acc a -> do
        w <- f a
        pure $! mappend acc w
    )
    mempty

view :: Getting a s a -> s -> a
view l s = getConst (l Const s)
{-# INLINE view #-}

replaceOf :: (Eq b) => ASetter s t b b -> b -> b -> s -> t
replaceOf l x x' = over l (\v -> if v == x then x' else v)
{-# INLINE replaceOf #-}

chomp :: String -> String
chomp = dropWhileEnd (`elem` ['\r', '\n'])
{-# INLINE chomp #-}

asumMap :: forall b m f a. (Foldable f, Alternative m) => (a -> m b) -> f a -> m b
asumMap = coerce (foldMap :: (a -> Alt m b) -> f a -> Alt m b)
{-# INLINE asumMap #-}

newtype PrettyShow a = PrettyShow a

instance (Show a) => Pretty (PrettyShow a) where
  pretty (PrettyShow a) = pretty $ convertString @_ @Text $ show a

-- Lift IO funcitons

-- | Lifted version of 'System.IO.hPrint'.
hPrint :: (MonadIO m, Show a) => Handle -> a -> m ()
hPrint handle x = liftIO $ System.IO.hPrint handle x

-- | Lifted version of 'System.IO.hPutStr'.
hPutStr :: (MonadIO m) => Handle -> String -> m ()
hPutStr handle x = liftIO $ System.IO.hPutStr handle x

-- | Lifted version of 'System.IO.hPutStrLn'.
hPutStrLn :: (MonadIO m) => Handle -> String -> m ()
hPutStrLn handle x = liftIO $ System.IO.hPutStrLn handle x

-- | Lifted version of 'T.hPutStr'.
hPutText :: (MonadIO m) => Handle -> Text -> m ()
hPutText handle x = liftIO $ T.hPutStr handle x

-- | Lifted version of 'T.hPutStrLn'.
hPutTextLn :: (MonadIO m) => Handle -> Text -> m ()
hPutTextLn handle x = liftIO $ T.hPutStrLn handle x

putText :: (MonadIO m) => Text -> m ()
putText = hPutText stdout

newIORef :: (MonadIO m) => a -> m (IORef a)
newIORef = liftIO . IORef.newIORef

readIORef :: (MonadIO m) => IORef a -> m a
readIORef ref = liftIO $ IORef.readIORef ref

modifyIORef :: (MonadIO m) => IORef a -> (a -> a) -> m ()
modifyIORef ref f = liftIO $ IORef.modifyIORef ref f

writeIORef :: (MonadIO m) => IORef a -> a -> m ()
writeIORef ref a = liftIO $ IORef.writeIORef ref a

-- | Compilation target backend.
data Target = TargetEval | TargetScheme
  deriving stock (Eq, Show)

-- | Evaluation mode (small-step CPS or big-step).
data EvalMode = EvalSmallStep | EvalBigStep
  deriving stock (Eq, Show)

data Flag = Flag
  { noOptimize :: Bool,
    lambdaLift :: Bool,
    debugMode :: Bool,
    testMode :: Bool,
    target :: Target,
    evalMode :: EvalMode,
    useInfer :: Bool
  }

instance Hashable Megaparsec.Pos

instance Hashable SourcePos

makeStore ''Megaparsec.Pos

makeStore ''SourcePos

-- | Range of a token.
data Range = Range
  { _start :: SourcePos,
    _end :: SourcePos
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

makeStore ''Range

class HasRange a where
  range :: a -> Range

instance HasRange Range where
  range = identity

instance HasRange Void where
  range = absurd

instance Semigroup Range where
  Range s1 e1 <> Range s2 e2 = Range (min s1 s2) (max e1 e2)

instance Pretty Range where
  pretty (Range start end) =
    pretty (convertString @_ @Text $ sourceName start)
      <> ": line "
      <> pretty (unPos (sourceLine start))
      <> ", column "
      <> pretty (unPos (sourceColumn start))
      <> " - line "
      <> pretty (unPos (sourceLine end))
      <> ", column "
      <> pretty (unPos (sourceColumn end))

errorOn :: (MonadIO m, Pretty a) => a -> Doc ann -> m b
errorOn range x = do
  liftIO $ hPutDoc stderr $ pretty range <> ": " <> x
  liftIO exitFailure

warningOn :: (MonadIO m, Pretty a) => a -> Doc ann -> m ()
warningOn range x = do
  liftIO $ hPutDoc stderr $ pretty range <> ": " <> x

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m = pretty (Map.toList m)

newtype Uniq = Uniq Int

getUniq :: (State Uniq :> es) => Eff es Int
getUniq = state $ \(Uniq u) -> (u, Uniq $ u + 1)
