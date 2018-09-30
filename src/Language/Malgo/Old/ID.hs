{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Malgo.Old.ID
  (ID(..), RawID, TypedID, idName, idUniq, idMeta, newID) where

import           Control.Lens          (makeLenses)
import           Data.Outputable
import           Language.Malgo.Old.Monad
import           Language.Malgo.Old.Pretty
import           Language.Malgo.Old.Type
import           Universum             hiding (Type)

data ID a = ID { _idName :: Text, _idUniq :: Int, _idMeta :: a }
  deriving (Show, Ord, Read, Generic, Outputable)

type RawID = ID ()

type TypedID = ID Type

instance Eq (ID a) where
  x == y = _idUniq x == _idUniq y

makeLenses ''ID

ignore :: a -> b -> b
ignore = flip const

instance Pretty a => Pretty (ID a) where
  pPrint (ID n u m) =
#ifdef SHOW_META
    pPrint n <> "." <> pPrint u <> braces (pPrint m)
#else
    ignore m $ pPrint n <> "." <> pPrint u
#endif

instance HasType a => HasType (ID a) where
  typeOf i = typeOf $ view idMeta i

newID :: MonadMalgo f => a -> Text -> f (ID a)
newID m n =
  ID n <$> newUniq <*> pure m