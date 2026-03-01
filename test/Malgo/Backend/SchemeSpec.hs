module Malgo.Backend.SchemeSpec (spec) where

import Data.Map qualified as Map
import Data.Text qualified as T
import Malgo.Backend.Scheme
import Malgo.Id
import Malgo.Module (ModuleName (..))
import Malgo.Prelude
import Malgo.Sequent.Core.Join qualified as Join
import Malgo.Sequent.Fun (Literal (..), Pattern (..), Tag (..))
import Test.Hspec
import Text.Megaparsec.Pos (initialPos)

dummyRange :: Range
dummyRange = Range (initialPos "") (initialPos "")

mkExternalId :: Text -> Id
mkExternalId name = Id {name, moduleName = ModuleName "Test", sort = External}

mkInternalId :: Text -> Int -> Id
mkInternalId name uniq = Id {name, moduleName = ModuleName "Test", sort = Internal uniq}

mkTemporalId :: Text -> Int -> Id
mkTemporalId name uniq = Id {name, moduleName = ModuleName "Test", sort = Temporal uniq}

spec :: Spec
spec = do
  describe "mangleId" do
    it "mangles external identifiers" do
      mangleId (mkExternalId "foo") `shouldBe` "Test_dot_foo"

    it "mangles internal identifiers with unique suffix" do
      mangleId (mkInternalId "bar" 42) `shouldBe` "bar_42"

    it "mangles temporal identifiers" do
      mangleId (mkTemporalId "tmp" 7) `shouldBe` "_t_tmp_7"

    it "handles hash character in names" do
      mangleId (mkInternalId "x#y" 0) `shouldBe` "x_hash_y_0"

    it "handles Greek letters" do
      mangleId (mkInternalId "\x03B1" 0) `shouldBe` "alpha_0"
      mangleId (mkInternalId "\x03BB" 0) `shouldBe` "lambda__0"

  describe "escapeString" do
    it "escapes backslashes" do
      escapeString "a\\b" `shouldBe` "a\\\\b"

    it "escapes double quotes" do
      escapeString "a\"b" `shouldBe` "a\\\"b"

    it "escapes newlines" do
      escapeString "a\nb" `shouldBe` "a\\nb"

    it "escapes tabs" do
      escapeString "a\tb" `shouldBe` "a\\tb"

    it "leaves plain text unchanged" do
      escapeString "hello" `shouldBe` "hello"

  describe "compileToScheme" do
    it "compiles a simple program with one definition" do
      let mainId = mkExternalId "main"
          retId = mkTemporalId "ret" 0
          body = Join.Cut (Join.Literal dummyRange (Int32 42)) retId
          program =
            Join.Program
              { definitions = [(dummyRange, mainId, retId, body)],
                dependencies = []
              }
          result = T.unpack $ compileToScheme program
      result `shouldContain` "define"
      result `shouldContain` "42"
      result `shouldContain` "malgo-main"

    it "compiles lambda expressions" do
      let xId = mkInternalId "x" 0
          retId = mkTemporalId "ret" 0
          body = Join.Cut (Join.Var dummyRange xId) retId
          lam = Join.Lambda dummyRange [xId] body
          mainId = mkExternalId "main"
          mainRet = mkTemporalId "ret" 1
          mainBody = Join.Cut lam mainRet
          program =
            Join.Program
              { definitions = [(dummyRange, mainId, mainRet, mainBody)],
                dependencies = []
              }
          result = T.unpack $ compileToScheme program
      result `shouldContain` "lambda"

    it "compiles construct expressions" do
      let mainId = mkExternalId "main"
          retId = mkTemporalId "ret" 0
          body = Join.Cut (Join.Construct dummyRange (Tag "Just") [Join.Literal dummyRange (Int32 1)] []) retId
          program =
            Join.Program
              { definitions = [(dummyRange, mainId, retId, body)],
                dependencies = []
              }
          result = T.unpack $ compileToScheme program
      result `shouldContain` "'Just"

    it "compiles object expressions" do
      let mainId = mkExternalId "main"
          retId = mkTemporalId "ret" 0
          fieldRet = mkTemporalId "k" 0
          fieldBody = Join.Cut (Join.Literal dummyRange (Int32 1)) fieldRet
          obj = Join.Object dummyRange (Map.singleton "x" (fieldRet, fieldBody))
          body = Join.Cut obj retId
          program =
            Join.Program
              { definitions = [(dummyRange, mainId, retId, body)],
                dependencies = []
              }
          result = T.unpack $ compileToScheme program
      result `shouldContain` "cons"
      result `shouldContain` "'x"

    it "compiles primitive operations" do
      let mainId = mkExternalId "main"
          retId = mkTemporalId "ret" 0
          a = Join.Literal dummyRange (Int32 1)
          b = Join.Literal dummyRange (Int32 2)
          body = Join.Primitive dummyRange "add_i32" [a, b] retId
          program =
            Join.Program
              { definitions = [(dummyRange, mainId, retId, body)],
                dependencies = []
              }
          result = T.unpack $ compileToScheme program
      result `shouldContain` "+"

    it "compiles invoke statements" do
      let mainId = mkExternalId "main"
          retId = mkTemporalId "ret" 0
          fId = mkExternalId "f"
          body = Join.Invoke dummyRange fId retId
          program =
            Join.Program
              { definitions = [(dummyRange, mainId, retId, body)],
                dependencies = []
              }
          result = T.unpack $ compileToScheme program
      result `shouldContain` "Test_dot_f"

    it "compiles select with pattern matching" do
      let mainId = mkExternalId "main"
          retId = mkTemporalId "ret" 0
          xId = mkInternalId "x" 0
          branch1 =
            Join.Branch
              { range = dummyRange,
                pattern = PLiteral dummyRange (Int32 0),
                statement = Join.Cut (Join.Literal dummyRange (String "zero")) retId
              }
          branch2 =
            Join.Branch
              { range = dummyRange,
                pattern = PVar dummyRange xId,
                statement = Join.Cut (Join.Literal dummyRange (String "other")) retId
              }
          selectCons = Join.Select dummyRange [branch1, branch2]
          joinName = mkTemporalId "select" 0
          body = Join.Join dummyRange joinName selectCons (Join.Cut (Join.Literal dummyRange (Int32 42)) joinName)
          program =
            Join.Program
              { definitions = [(dummyRange, mainId, retId, body)],
                dependencies = []
              }
          result = T.unpack $ compileToScheme program
      result `shouldContain` "cond"
      result `shouldContain` "equal?"
