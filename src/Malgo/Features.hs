{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Features
  ( Feature (..),
    FeatureFlags (..),
    Features (..),
    getFeatureFlags,
    hasFeature,
    addFeatures,
    runFeatures,
    parseFeatures,
    isMalgo2025Enabled,
  )
where

import Data.Set qualified as Set
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local (evalState, get, put)
import Malgo.Prelude

-- | Feature definition
data Feature
  = CStyleApply
  | Malgo2025
  | Experimental String
  deriving stock (Eq, Ord, Show)

-- | FeatureFlags is a wrapper for Set Feature
newtype FeatureFlags = FeatureFlags (Set Feature)
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

-- | Features effect definition
data Features :: Effect where
  GetFeatureFlags :: Features m FeatureFlags
  HasFeature :: Feature -> Features m Bool
  AddFeatures :: FeatureFlags -> Features m ()

type instance DispatchOf Features = Dynamic

-- | Get the current FeatureFlags
getFeatureFlags :: (Features :> es) => Eff es FeatureFlags
getFeatureFlags = send GetFeatureFlags

-- | Check if a feature is enabled
hasFeature :: (Features :> es) => Feature -> Eff es Bool
hasFeature feature = send (HasFeature feature)

-- | Add features to the current FeatureFlags
addFeatures :: (Features :> es) => FeatureFlags -> Eff es ()
addFeatures feature = send (AddFeatures feature)

runFeatures :: FeatureFlags -> Eff (Features ': es) a -> Eff es a
runFeatures flags = reinterpret_ (evalState flags) \case
  GetFeatureFlags -> get
  HasFeature feature -> do
    FeatureFlags fs <- get
    pure $ Set.member feature fs
  AddFeatures (FeatureFlags newFeatures) -> do
    FeatureFlags fs <- get
    put $ FeatureFlags (Set.union fs newFeatures)

parseFeatures :: [Text] -> FeatureFlags
parseFeatures = FeatureFlags . Set.fromList . map (parseFeature . convertString)
  where
    parseFeature :: String -> Feature
    parseFeature "c-style-apply" = CStyleApply
    parseFeature "malgo-2025" = Malgo2025
    parseFeature ('e' : 'x' : 'p' : 'e' : 'r' : 'i' : 'm' : 'e' : 'n' : 't' : 'a' : 'l' : '-' : name) = Experimental name
    parseFeature name = error $ "Unknown feature: " <> name

-- | Check if the Malgo2025 feature is enabled
isMalgo2025Enabled :: (Features :> es) => Eff es Bool
isMalgo2025Enabled = hasFeature Malgo2025
