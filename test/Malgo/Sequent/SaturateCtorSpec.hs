-- | Unit tests for saturated-constructor inlining at Fun IR. Corpus-wide
-- safety is covered transitively by every other backend/IR spec that
-- drives 'Malgo.Sequent.ToCore.toCore' (it now runs first thing inside
-- 'toCore', so 'Malgo.Sequent.ToCoreSpec', the Zig backend's corpus
-- oracle, and the golden suites all exercise it); end-to-end behavior by
-- the zig-golden byte-parity harness and the interpreter's own goldens.
module Malgo.Sequent.SaturateCtorSpec (spec) where

import Data.Maybe (listToMaybe)
import Malgo.Id
import Malgo.Module (ModuleName (..))
import Malgo.Prelude
import Malgo.SExpr (sShow)
import Malgo.Sequent.Fun
import Malgo.Sequent.SaturateCtor (saturateProgram)
import Test.Hspec
import Text.Megaparsec.Pos (initialPos)

mkName :: Text -> Int -> Id
mkName name uniq = Id {name, moduleName = ModuleName "SaturateCtorTest", sort = Temporal uniq}

dummyRange :: Range
dummyRange = Range (initialPos "") (initialPos "")

-- | @Cons = \\p1 -> \\p2 -> Cons p1 p2@, the shape
-- 'Malgo.Sequent.ToFun.fromConstructor' produces for an arity-2
-- constructor.
consDef :: Name -> (Range, Name, Expr)
consDef ctorName =
  ( dummyRange,
    ctorName,
    Lambda dummyRange [p1] (Lambda dummyRange [p2] (Construct dummyRange (Tag "Cons") [Var dummyRange p1, Var dummyRange p2]))
  )
  where
    p1 = mkName "p1" 20
    p2 = mkName "p2" 21

nilDef :: Name -> (Range, Name, Expr)
nilDef ctorName = (dummyRange, ctorName, Construct dummyRange (Tag "Nil") [])

cons, nil, f, x, xs, mapListName :: Name
cons = mkName "Cons" 0
nil = mkName "Nil" 0
f = mkName "f" 1
x = mkName "x" 2
xs = mkName "xs" 3
mapListName = mkName "mapList" 4

program :: [(Range, Name, Expr)] -> Program
program definitions = Program {definitions, dependencies = []}

lookupDef :: Name -> Program -> Maybe Expr
lookupDef name prog = listToMaybe [body | (_, defName, body) <- prog.definitions, defName == name]

sh :: Expr -> Text
sh = sShow

spec :: Spec
spec = describe "saturated-constructor inlining (Fun IR)" do
  it "inlines Cons x xs (immediate arguments) into a direct Construct" do
    let caller = mkName "caller" 5
        callDef =
          ( dummyRange,
            caller,
            Apply dummyRange (Apply dummyRange (Invoke dummyRange cons) [Var dummyRange x]) [Var dummyRange xs]
          )
        prog = program [consDef cons, callDef]
    fmap sh (lookupDef caller (saturateProgram prog))
      `shouldBe` Just (sh (Construct dummyRange (Tag "Cons") [Var dummyRange x, Var dummyRange xs]))

  it "inlines Cons (f x) (mapList f xs) — non-immediate arguments — into a direct Construct" do
    let caller = mkName "caller" 5
        fx = Apply dummyRange (Var dummyRange f) [Var dummyRange x]
        mapListCall = Apply dummyRange (Apply dummyRange (Invoke dummyRange mapListName) [Var dummyRange f]) [Var dummyRange xs]
        callDef =
          ( dummyRange,
            caller,
            Apply dummyRange (Apply dummyRange (Invoke dummyRange cons) [fx]) [mapListCall]
          )
        prog = program [consDef cons, callDef]
    -- mapList isn't a recognized constructor, so its own call is left
    -- exactly as it was -- only the outer Cons spine is rewritten.
    fmap sh (lookupDef caller (saturateProgram prog))
      `shouldBe` Just (sh (Construct dummyRange (Tag "Cons") [fx, mapListCall]))

  it "leaves a partial application (Cons applied to only one argument) untouched" do
    let caller = mkName "caller" 5
        callDef = (dummyRange, caller, Apply dummyRange (Invoke dummyRange cons) [Var dummyRange x])
        prog = program [consDef cons, callDef]
    fmap sh (lookupDef caller (saturateProgram prog)) `shouldBe` Just (sh (Apply dummyRange (Invoke dummyRange cons) [Var dummyRange x]))

  it "inlines an arity-0 constructor referenced directly" do
    let caller = mkName "caller" 5
        callDef = (dummyRange, caller, Invoke dummyRange nil)
        prog = program [nilDef nil, callDef]
    fmap sh (lookupDef caller (saturateProgram prog)) `shouldBe` Just (sh (Construct dummyRange (Tag "Nil") []))

  it "rewraps an over-applied constructor's extra arguments as an Apply on the built value" do
    let caller = mkName "caller" 5
        extra = mkName "extra" 6
        callDef =
          ( dummyRange,
            caller,
            Apply dummyRange (Apply dummyRange (Apply dummyRange (Invoke dummyRange cons) [Var dummyRange x]) [Var dummyRange xs]) [Var dummyRange extra]
          )
        prog = program [consDef cons, callDef]
    fmap sh (lookupDef caller (saturateProgram prog))
      `shouldBe` Just (sh (Apply dummyRange (Construct dummyRange (Tag "Cons") [Var dummyRange x, Var dummyRange xs]) [Var dummyRange extra]))

  it "rewrites nested inside a Let/Lambda/Select without disturbing the surrounding structure" do
    let caller = mkName "caller" 5
        v = mkName "v" 7
        consCall = Apply dummyRange (Apply dummyRange (Invoke dummyRange cons) [Var dummyRange x]) [Var dummyRange xs]
        callDef = (dummyRange, caller, Let dummyRange v consCall (Var dummyRange v))
        prog = program [consDef cons, callDef]
    fmap sh (lookupDef caller (saturateProgram prog))
      `shouldBe` Just (sh (Let dummyRange v (Construct dummyRange (Tag "Cons") [Var dummyRange x, Var dummyRange xs]) (Var dummyRange v)))
