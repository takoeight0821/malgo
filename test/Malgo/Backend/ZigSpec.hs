module Malgo.Backend.ZigSpec (spec) where

import Malgo.Backend.Zig.Emit (mangleId)
import Malgo.Id
import Malgo.Module (ModuleName (..))
import Malgo.Prelude
import Test.Hspec

mkExternalId :: Text -> Id
mkExternalId name = Id {name, moduleName = ModuleName "Test", sort = External}

mkInternalId :: Text -> Int -> Id
mkInternalId name uniq = Id {name, moduleName = ModuleName "Test", sort = Internal uniq}

spec :: Spec
spec = do
  describe "Zig raw-identifier mangling" do
    it "wraps every identifier in @\"...\"@, sidestepping keyword collisions" do
      mangleId (mkExternalId "error") `shouldBe` "@\"Test.error\""
      mangleId (mkExternalId "fn") `shouldBe` "@\"Test.fn\""
      mangleId (mkExternalId "test") `shouldBe` "@\"Test.test\""
    it "distinguishes same-named identifiers by uniq" do
      mangleId (mkInternalId "x" 1) `shouldNotBe` mangleId (mkInternalId "x" 2)
    it "escapes embedded quotes and backslashes" do
      mangleId (mkExternalId "a\"b\\c") `shouldBe` "@\"Test.a\\\"b\\\\c\""
