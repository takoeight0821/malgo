module Malgo.Backend.SchemeSpec (spec) where

import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Text qualified as T
import Effectful.Reader.Static (runReader)
import Malgo.Backend.Scheme
import Malgo.Id
import Malgo.Module (ModuleName (..))
import Malgo.Monad (runMalgoM)
import Malgo.Parser (ParserPass (..))
import Malgo.Pass (runCompileError, runPass)
import Malgo.Prelude
import Malgo.Rename (RenamePass (..), genBuiltinRnEnv)
import Malgo.Sequent.Core.Flat (flatProgram)
import Malgo.Sequent.Core.Join qualified as Join
import Malgo.Sequent.Fun (Literal (..), Pattern (..), Tag (..))
import Malgo.Sequent.ToCore (toCore)
import Malgo.Sequent.ToFun (ToFunPass (..))
import Malgo.Syntax (Module (..))
import Malgo.TestUtils (flag, testcaseDir, withFreshQueryDB)
import System.FilePath ((</>))
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
          result = T.unpack $ compileToScheme (ModuleName "Test") program
      result `shouldContain` "define"
      result `shouldContain` "42"
      result `shouldContain` "Test_dot_main malgo-finish"

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
          result = T.unpack $ compileToScheme (ModuleName "Test") program
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
          result = T.unpack $ compileToScheme (ModuleName "Test") program
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
          result = T.unpack $ compileToScheme (ModuleName "Test") program
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
          result = T.unpack $ compileToScheme (ModuleName "Test") program
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
          result = T.unpack $ compileToScheme (ModuleName "Test") program
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
          result = T.unpack $ compileToScheme (ModuleName "Test") program
      result `shouldContain` "cond"
      result `shouldContain` "equal?"

    it "compiles Mu expressions" do
      let mainId = mkExternalId "main"
          retId = mkTemporalId "ret" 0
          kId = mkTemporalId "k" 0
          muBody = Join.Cut (Join.Literal dummyRange (Int32 7)) kId
          mu = Join.Mu dummyRange kId muBody
          body = Join.Cut mu retId
          program =
            Join.Program
              { definitions = [(dummyRange, mainId, retId, body)],
                dependencies = []
              }
          result = T.unpack $ compileToScheme (ModuleName "Test") program
      result `shouldContain` "lambda"
      result `shouldContain` "7"

    it "compiles Cocase expressions" do
      let mainId = mkExternalId "main"
          retId = mkTemporalId "ret" 0
          kId = mkTemporalId "k" 0
          branch1Body = Join.Cut (Join.Literal dummyRange (Int32 1)) kId
          branch2Body = Join.Cut (Join.Literal dummyRange (Int32 2)) kId
          cocase = Join.Cocase dummyRange [("head", [kId], branch1Body), ("tail", [kId], branch2Body)]
          body = Join.Cut cocase retId
          program =
            Join.Program
              { definitions = [(dummyRange, mainId, retId, body)],
                dependencies = []
              }
          result = T.unpack $ compileToScheme (ModuleName "Test") program
      result `shouldContain` "cond"
      result `shouldContain` "'head"
      result `shouldContain` "'tail"

    it "compiles Destructor consumers" do
      let mainId = mkExternalId "main"
          retId = mkTemporalId "ret" 0
          kId = mkTemporalId "k" 0
          dtor = Join.Destructor dummyRange "head" [] kId
          joinName = mkTemporalId "dtor" 0
          cocaseBody = Join.Cut (Join.Literal dummyRange (Int32 42)) kId
          cocase = Join.Cocase dummyRange [("head", [kId], cocaseBody)]
          body = Join.Join dummyRange joinName dtor (Join.Cut cocase joinName)
          program =
            Join.Program
              { definitions = [(dummyRange, mainId, retId, body)],
                dependencies = []
              }
          result = T.unpack $ compileToScheme (ModuleName "Test") program
      result `shouldContain` "'head"
      result `shouldContain` "cocase"

    it "compiles ExternalCall statements" do
      let mainId = mkExternalId "main"
          retId = mkTemporalId "ret" 0
          body = Join.ExternalCall dummyRange "putstr" [Join.Literal dummyRange (String "hi")] retId
          program =
            Join.Program
              { definitions = [(dummyRange, mainId, retId, body)],
                dependencies = []
              }
          result = T.unpack $ compileToScheme (ModuleName "Test") program
      result `shouldContain` "display"
      result `shouldContain` "hi"

    it "compiles BinOp statements" do
      let mainId = mkExternalId "main"
          retId = mkTemporalId "ret" 0
          lhs = Join.Literal dummyRange (Int32 3)
          rhs = Join.Literal dummyRange (Int32 4)
          body = Join.BinOp dummyRange "add_i32" lhs rhs retId
          program =
            Join.Program
              { definitions = [(dummyRange, mainId, retId, body)],
                dependencies = []
              }
          result = T.unpack $ compileToScheme (ModuleName "Test") program
      result `shouldContain` "+"
      result `shouldContain` "3"
      result `shouldContain` "4"

    it "compiles Ifz statements" do
      let mainId = mkExternalId "main"
          retId = mkTemporalId "ret" 0
          cond = Join.Literal dummyRange (Int32 0)
          thenBranch = Join.Cut (Join.Literal dummyRange (String "zero")) retId
          elseBranch = Join.Cut (Join.Literal dummyRange (String "nonzero")) retId
          body = Join.Ifz dummyRange cond thenBranch elseBranch
          program =
            Join.Program
              { definitions = [(dummyRange, mainId, retId, body)],
                dependencies = []
              }
          result = T.unpack $ compileToScheme (ModuleName "Test") program
      result `shouldContain` "eqv?"
      result `shouldContain` "0"
      result `shouldContain` "zero"
      result `shouldContain` "nonzero"

  describe "compileToScheme e2e (.mlg -> Join -> Scheme)" do
    it "compiles Test2.mlg through the parser/lowering pipeline" do
      result <- compileTestcaseToScheme "Test2.mlg"
      result `shouldContain` "Test2_dot_mlg_dot_rtob"
      result `shouldContain` "(eq? (car %v) 'R)"
      result `shouldContain` "\"OK\""

    it "compiles HelloBoxed.mlg with constructor wrapping and calls" do
      result <- compileTestcaseToScheme "HelloBoxed.mlg"
      result `shouldContain` "HelloBoxed_dot_mlg_dot_main"
      result `shouldContain` "HelloBoxed_dot_mlg_dot_putStrLn"
      result `shouldContain` "\"Hello, world\""

compileTestcaseToScheme :: FilePath -> IO String
compileTestcaseToScheme testcase = do
  let srcPath = testcaseDir </> testcase
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError $ withFreshQueryDB do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fun <- runReader renamed.moduleName $ runPass ToFunPass renamed.moduleDefinition
    program <- runReader renamed.moduleName $ toCore fun >>= flatProgram >>= Join.joinProgram
    pure $ T.unpack $ compileToScheme renamed.moduleName program
