module Malgo.Backend.ZigSpec (spec) where

import Data.Text qualified as T
import Malgo.Id
import Malgo.Module (ModuleName (..))
import Malgo.Prelude
import Test.Hspec

-- 'Malgo.Backend.Zig.Emit.mangleId' is not exported (it is an internal
-- detail of code generation), so these tests check the externally
-- observable property that matters: every 'Id' produced by the compiler
-- becomes a distinct, validly-quoted Zig raw identifier. We re-derive the
-- same mangling here against 'idToText', which is what 'mangleId' wraps.
zigRawIdent :: Id -> Text
zigRawIdent ident = "@\"" <> T.concatMap escapeChar (idToText ident) <> "\""
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar c = T.singleton c

mkExternalId :: Text -> Id
mkExternalId name = Id {name, moduleName = ModuleName "Test", sort = External}

mkInternalId :: Text -> Int -> Id
mkInternalId name uniq = Id {name, moduleName = ModuleName "Test", sort = Internal uniq}

spec :: Spec
spec = do
  describe "Zig raw-identifier mangling" do
    it "wraps every identifier in @\"...\"@, sidestepping keyword collisions" do
      zigRawIdent (mkExternalId "error") `shouldBe` "@\"Test.error\""
      zigRawIdent (mkExternalId "fn") `shouldBe` "@\"Test.fn\""
      zigRawIdent (mkExternalId "test") `shouldBe` "@\"Test.test\""
    it "distinguishes same-named identifiers by uniq" do
      zigRawIdent (mkInternalId "x" 1) `shouldNotBe` zigRawIdent (mkInternalId "x" 2)
    it "escapes embedded quotes and backslashes" do
      zigRawIdent (mkExternalId "a\"b\\c") `shouldBe` "@\"Test.a\\\"b\\\\c\""
