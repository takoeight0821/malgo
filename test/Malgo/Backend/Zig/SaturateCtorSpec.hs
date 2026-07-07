-- | Unit tests for saturated-constructor inlining. Corpus-wide safety is
-- covered by 'Malgo.Backend.Zig.PerceusSpec's oracle, which runs the real
-- pipeline (saturation included) through the RcCheck linearity checker;
-- end-to-end behavior by the zig-golden byte-parity harness.
module Malgo.Backend.Zig.SaturateCtorSpec (spec) where

import Data.Maybe (listToMaybe)
import Malgo.Backend.Zig.SaturateCtor (saturateProgram)
import Malgo.Id
import Malgo.Module (ModuleName (..))
import Malgo.Prelude
import Malgo.SExpr (sShow)
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Fun (Name, Tag (..))
import Test.Hspec
import Text.Megaparsec.Pos (initialPos)

mkName :: Text -> Int -> Id
mkName name uniq = Id {name, moduleName = ModuleName "SaturateCtorTest", sort = Temporal uniq}

dummyRange :: Range
dummyRange = Range (initialPos "") (initialPos "")

-- | The curried-constructor definition @Cons = \\x -> \\y -> Cons x y@'s
-- compiled shape, as produced by 'Malgo.Sequent.ToFun.fromConstructor' and
-- the Core\/Flat\/Join passes.
consDef :: Name -> (Range, Name, Name, Statement)
consDef ctorName =
  ( dummyRange,
    ctorName,
    ret,
    Cut
      ( Lambda
          dummyRange
          [p1, k1]
          (Cut (Lambda dummyRange [p2, k2] (Cut (Construct dummyRange (Tag "Cons") [Var dummyRange p1, Var dummyRange p2] []) k2)) k1)
      )
      ret
  )
  where
    p1 = mkName "p1" 20
    p2 = mkName "p2" 21
    k1 = mkName "k1" 22
    k2 = mkName "k2" 23
    ret = mkName "ret" 24

nilDef :: Name -> (Range, Name, Name, Statement)
nilDef ctorName =
  (dummyRange, ctorName, ret, Cut (Construct dummyRange (Tag "Nil") [] []) ret)
  where
    ret = mkName "ret" 10

-- | @Cons x xs@ from a caller whose own return continuation is @kFinal@:
-- @Join k2 (Apply [xs] [kFinal]) (Join k1 (Apply [x] [k2]) (Invoke Cons k1))@
-- (outermost-first nesting, matching the Join pass's actual output order
-- for a curried two-argument application).
callSite :: Name -> Name -> Name -> Name -> Statement
callSite ctorName x xs kFinal =
  Join dummyRange k2 (Apply dummyRange [Var dummyRange xs] [kFinal])
    $ Join dummyRange k1 (Apply dummyRange [Var dummyRange x] [k2])
    $ Invoke dummyRange ctorName k1
  where
    k1 = mkName "callk1" 10
    k2 = mkName "callk2" 11

cons, x, xs, kFinal, callerRet, callerName :: Name
cons = mkName "Cons" 0
x = mkName "x" 1
xs = mkName "xs" 2
kFinal = mkName "kFinal" 3
callerRet = mkName "callerRet" 4
callerName = mkName "caller" 5

spec :: Spec
spec = describe "saturated-constructor inlining" do
  it "inlines a fully-saturated call into a direct Construct in the caller" do
    let callDef = (dummyRange, callerName, callerRet, callSite cons x xs kFinal)
        program = Program {definitions = [consDef cons, callDef], dependencies = []}
    lookupDefText callerName (saturateProgram program)
      `shouldBe` Just (sShow (Cut (Construct dummyRange (Tag "Cons") [Var dummyRange x, Var dummyRange xs] []) kFinal))

  it "leaves a partial application untouched" do
    let k1 = mkName "callk1" 10
        partialCall = Join dummyRange k1 (Apply dummyRange [Var dummyRange x] [kFinal]) (Invoke dummyRange cons k1)
        callDef = (dummyRange, callerName, callerRet, partialCall)
        program = Program {definitions = [consDef cons, callDef], dependencies = []}
    lookupDefText callerName (saturateProgram program) `shouldBe` Just (sShow partialCall)

  it "inlines an arity-0 constructor directly" do
    let nil = mkName "Nil" 0
        callDef = (dummyRange, callerName, callerRet, Invoke dummyRange nil callerRet)
        program = Program {definitions = [nilDef nil, callDef], dependencies = []}
    lookupDefText callerName (saturateProgram program)
      `shouldBe` Just (sShow (Cut (Construct dummyRange (Tag "Nil") [] []) callerRet))

  it "aborts when an intermediate join is used more than once" do
    let k1 = mkName "callk1" 10
        k2 = mkName "callk2" 11
        -- k1 is referenced twice here (once by the aliasing Construct,
        -- once as the Apply target) -- not a shape a real compile
        -- produces, but exactly what the use-count guard exists to catch.
        aliasedCall =
          Join dummyRange k2 (Apply dummyRange [Var dummyRange xs] [kFinal])
            $ Join dummyRange k1 (Apply dummyRange [Var dummyRange x] [k2])
            $ Cut (Construct dummyRange Tuple [Var dummyRange k1] []) callerRet
        callDef = (dummyRange, callerName, callerRet, aliasedCall)
        program = Program {definitions = [consDef cons, callDef], dependencies = []}
    lookupDefText callerName (saturateProgram program) `shouldBe` Just (sShow aliasedCall)

  it "drops the now-dead join bindings after inlining" do
    let callDef = (dummyRange, callerName, callerRet, callSite cons x xs kFinal)
        program = Program {definitions = [consDef cons, callDef], dependencies = []}
        rewritten = saturateProgram program
    case lookupDef callerName rewritten of
      Just (Join {}) -> expectationFailure "dead join binding was not eliminated"
      Just _ -> pure ()
      Nothing -> expectationFailure "caller definition disappeared"

lookupDef :: Name -> Program -> Maybe Statement
lookupDef name program = listToMaybe [stmt | (_, defName, _, stmt) <- program.definitions, defName == name]

lookupDefText :: Name -> Program -> Maybe Text
lookupDefText name = fmap sShow . lookupDef name
