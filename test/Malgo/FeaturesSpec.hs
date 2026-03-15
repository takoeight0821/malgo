{-# LANGUAGE OverloadedStrings #-}

module Malgo.FeaturesSpec (spec) where

import Control.Exception
import Data.Set (fromList)
import Effectful
import Malgo.Features
import Malgo.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Features effect" $ do
  let cStyleApply = CStyleApply
      malgo2025 = Malgo2025
      experimentalX = Experimental "X"
      flags = FeatureFlags (fromList [cStyleApply])
      flagsWithExp = FeatureFlags (fromList [cStyleApply, experimentalX])
      flagsWithMalgo2025 = FeatureFlags (fromList [malgo2025])

  it "hasFeature returns True for enabled features" $ do
    result <- runEff $ runFeatures flags $ hasFeature cStyleApply
    result `shouldBe` True

  it "hasFeature returns False for disabled features" $ do
    result <- runEff $ runFeatures flags $ hasFeature experimentalX
    result `shouldBe` False

  it "getFeatureFlags returns the correct FeatureFlags" $ do
    result <- runEff $ runFeatures flagsWithExp getFeatureFlags
    result `shouldBe` flagsWithExp

  it "parseFeatures parses known features" $ do
    let features = ["c-style-apply", "experimental-X"]
        expected = FeatureFlags (fromList [CStyleApply, Experimental "X"])
    parseFeatures features `shouldBe` expected

  it "parseFeatures parses malgo-2025 feature" $ do
    let features = ["malgo-2025"]
        expected = FeatureFlags (fromList [Malgo2025])
    parseFeatures features `shouldBe` expected

  it "parseFeatures throws error on unknown feature" $ do
    let features = ["unknown-feature"]
    evaluate (parseFeatures features) `shouldThrow` anyErrorCall

  it "isMalgo2025Enabled returns True when Malgo2025 is enabled" $ do
    result <- runEff $ runFeatures flagsWithMalgo2025 isMalgo2025Enabled
    result `shouldBe` True

  it "isMalgo2025Enabled returns False when Malgo2025 is not enabled" $ do
    result <- runEff $ runFeatures flags isMalgo2025Enabled
    result `shouldBe` False
