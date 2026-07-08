-- | Unit tests for reuse-hint insertion at Fun IR. Corpus-wide safety is
-- covered transitively by every other backend/IR spec that drives
-- 'Malgo.Sequent.ToCore.toCore' (it now runs right after
-- 'Malgo.Sequent.SaturateCtor.saturateProgram'); end-to-end allocation
-- behavior by 'Malgo.Backend.Zig.Reuse' golden/corpus checks and
-- @MALGO_RC_STATS@ measurements.
module Malgo.Sequent.ReuseSpecializeSpec (spec) where

import Data.Maybe (listToMaybe)
import Effectful (runPureEff)
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local (evalState)
import Malgo.Id
import Malgo.Module (ModuleName (..))
import Malgo.Prelude
import Malgo.SExpr (sShow)
import Malgo.Sequent.Fun
import Malgo.Sequent.ReuseSpecialize (specializeProgram)
import Test.Hspec
import Text.Megaparsec.Pos (initialPos)

mkName :: Text -> Int -> Id
mkName name uniq = Id {name, moduleName = testModule, sort = Temporal uniq}

testModule :: ModuleName
testModule = ModuleName "ReuseSpecializeTest"

dummyRange :: Range
dummyRange = Range (initialPos "") (initialPos "")

program :: [(Range, Name, Expr)] -> Program
program definitions = Program {definitions, dependencies = []}

lookupDef :: Name -> Program -> Maybe Expr
lookupDef name prog = listToMaybe [body | (_, defName, body) <- prog.definitions, defName == name]

sh :: Expr -> Text
sh = sShow

-- | Runs 'specializeProgram' from a fresh 'Uniq' counter, matching the
-- effect stack 'Malgo.Sequent.ToCore.toCore' runs it under.
run :: Program -> Program
run prog = runPureEff $ runReader testModule $ evalState (Uniq 0) $ specializeProgram prog

v :: Range -> Name -> Expr
v = Var

-- | Mirrors 'Malgo.Sequent.ReuseSpecialize.instrumentReconstructions'\'s own
-- binding order for a qualifying reconstruction: every argument gets a
-- fresh @reuseArg@ binding (left to right), then one @reuseHint@ binding,
-- then the 'Construct' over the now-bound names. @nextUniq@ is the 'Uniq'
-- value the first fresh name in this particular reconstruction is minted
-- with (i.e. how many fresh names earlier branches/arguments already
-- consumed).
expectedInstrumented :: Int -> Tag -> [Expr] -> Name -> Expr
expectedInstrumented nextUniq tag args scrutinee =
  foldr (\(n, val) acc -> Let dummyRange n val acc) withHint namedArgs
  where
    namedArgs = zipWith (\i val -> (mkName "reuseArg" (nextUniq + i), val)) [0 ..] args
    hint = mkName "reuseHint" (nextUniq + length args)
    reconstruct = Construct dummyRange tag (map (Var dummyRange . fst) namedArgs)
    withHint = Let dummyRange hint (Primitive dummyRange "reuseHint" [Var dummyRange scrutinee]) reconstruct

spec :: Spec
spec = describe "reuse-hint insertion (Fun IR)" do
  it "instruments a mapList-shaped self-recursive rebuild, leaving the base case untouched" do
    let mapListName = mkName "mapList" 0
        f = mkName "f" 1
        xs = mkName "xs" 2
        wildcard = mkName "_" 3
        f2 = mkName "f2" 4
        x2 = mkName "x2" 5
        xs2 = mkName "xs2" 6
        recCall = Apply dummyRange (Apply dummyRange (Invoke dummyRange mapListName) [v dummyRange f2]) [v dummyRange xs2]
        fx = Apply dummyRange (v dummyRange f2) [v dummyRange x2]
        nilBranch = Branch dummyRange (Destruct dummyRange Tuple [PVar dummyRange wildcard, Destruct dummyRange (Tag "Nil") []]) (Construct dummyRange (Tag "Nil") [])
        consBranch = Branch dummyRange (Destruct dummyRange Tuple [PVar dummyRange f2, Destruct dummyRange (Tag "Cons") [PVar dummyRange x2, PVar dummyRange xs2]]) (Construct dummyRange (Tag "Cons") [fx, recCall])
        body =
          Lambda dummyRange [f]
            $ Lambda dummyRange [xs]
            $ Select dummyRange (Construct dummyRange Tuple [v dummyRange f, v dummyRange xs]) [nilBranch, consBranch]
        prog = program [(dummyRange, mapListName, body)]

        expectedConsBody = expectedInstrumented 0 (Tag "Cons") [fx, recCall] xs
        expectedBody =
          Lambda dummyRange [f]
            $ Lambda dummyRange [xs]
            $ Select
              dummyRange
              (Construct dummyRange Tuple [v dummyRange f, v dummyRange xs])
              [ nilBranch,
                Branch dummyRange (Destruct dummyRange Tuple [PVar dummyRange f2, Destruct dummyRange (Tag "Cons") [PVar dummyRange x2, PVar dummyRange xs2]]) expectedConsBody
              ]
    fmap sh (lookupDef mapListName (run prog)) `shouldBe` Just (sh expectedBody)

  it "instruments both arms of an if-guarded rebuild (insert-shaped) independently, forcing arguments before the hint" do
    let insertName = mkName "insert" 0
        ltName = mkName "lt" 1
        ifName = mkName "if" 2
        x = mkName "x" 3
        tree = mkName "tree" 4
        x2 = mkName "x2" 5
        val = mkName "val" 6
        left = mkName "left" 7
        right = mkName "right" 8
        wc = mkName "_" 9
        cond = Apply dummyRange (Apply dummyRange (Invoke dummyRange ltName) [v dummyRange x2]) [v dummyRange val]
        recL = Apply dummyRange (Apply dummyRange (Invoke dummyRange insertName) [v dummyRange x2]) [v dummyRange left]
        recR = Apply dummyRange (Apply dummyRange (Invoke dummyRange insertName) [v dummyRange x2]) [v dummyRange right]
        thenBody = Construct dummyRange (Tag "Node") [v dummyRange val, recL, v dummyRange right]
        elseBody = Construct dummyRange (Tag "Node") [v dummyRange val, v dummyRange left, recR]
        ifCall thenB elseB =
          Apply
            dummyRange
            ( Apply
                dummyRange
                (Apply dummyRange (Invoke dummyRange ifName) [cond])
                [Lambda dummyRange [wc] thenB]
            )
            [Lambda dummyRange [wc] elseB]
        leafBranch = Branch dummyRange (Destruct dummyRange Tuple [PVar dummyRange x2, Destruct dummyRange (Tag "Leaf") []]) (Construct dummyRange (Tag "Node") [v dummyRange x2, Invoke dummyRange (mkName "Leaf" 0), Invoke dummyRange (mkName "Leaf" 0)])
        nodeBranch = Branch dummyRange (Destruct dummyRange Tuple [PVar dummyRange x2, Destruct dummyRange (Tag "Node") [PVar dummyRange val, PVar dummyRange left, PVar dummyRange right]]) (ifCall thenBody elseBody)
        body =
          Lambda dummyRange [x]
            $ Lambda dummyRange [tree]
            $ Select dummyRange (Construct dummyRange Tuple [v dummyRange x, v dummyRange tree]) [leafBranch, nodeBranch]
        prog = program [(dummyRange, insertName, body)]

        -- thenBody is visited first (3 args -> reuseArg 0,1,2 + reuseHint 3),
        -- so elseBody's own fresh names start at 4 (3 args -> 4,5,6 + reuseHint 7).
        expectedThenBody = expectedInstrumented 0 (Tag "Node") [v dummyRange val, recL, v dummyRange right] tree
        expectedElseBody = expectedInstrumented 4 (Tag "Node") [v dummyRange val, v dummyRange left, recR] tree
        expectedNodeBranch = Branch dummyRange (Destruct dummyRange Tuple [PVar dummyRange x2, Destruct dummyRange (Tag "Node") [PVar dummyRange val, PVar dummyRange left, PVar dummyRange right]]) (ifCall expectedThenBody expectedElseBody)
        expectedBody =
          Lambda dummyRange [x]
            $ Lambda dummyRange [tree]
            $ Select dummyRange (Construct dummyRange Tuple [v dummyRange x, v dummyRange tree]) [leafBranch, expectedNodeBranch]
    fmap sh (lookupDef insertName (run prog)) `shouldBe` Just (sh expectedBody)

  it "leaves a branch with two recursive calls into one Construct untouched (ambiguous which to reuse)" do
    let mirrorName = mkName "mirror" 0
        tree = mkName "tree" 1
        val = mkName "val" 2
        left = mkName "left" 3
        right = mkName "right" 4
        recL = Apply dummyRange (Invoke dummyRange mirrorName) [v dummyRange left]
        recR = Apply dummyRange (Invoke dummyRange mirrorName) [v dummyRange right]
        nodeBranch = Branch dummyRange (Destruct dummyRange (Tag "Node") [PVar dummyRange val, PVar dummyRange left, PVar dummyRange right]) (Construct dummyRange (Tag "Node") [v dummyRange val, recR, recL])
        body = Lambda dummyRange [tree] (Select dummyRange (v dummyRange tree) [nodeBranch])
        prog = program [(dummyRange, mirrorName, body)]
    fmap sh (lookupDef mirrorName (run prog)) `shouldBe` Just (sh body)

  it "leaves a rebuild through a different tag than the matched pattern untouched" do
    let fName = mkName "f" 0
        xs = mkName "xs" 1
        x2 = mkName "x2" 2
        xs2 = mkName "xs2" 3
        recCall = Apply dummyRange (Invoke dummyRange fName) [v dummyRange xs2]
        consBranch = Branch dummyRange (Destruct dummyRange (Tag "Cons") [PVar dummyRange x2, PVar dummyRange xs2]) (Construct dummyRange (Tag "Snoc") [v dummyRange x2, recCall])
        body = Lambda dummyRange [xs] (Select dummyRange (v dummyRange xs) [consBranch])
        prog = program [(dummyRange, fName, body)]
    fmap sh (lookupDef fName (run prog)) `shouldBe` Just (sh body)

  it "leaves a smart-constructor rebuild (recursive call not fed directly into a Construct) untouched" do
    -- Models runtime/malgo/Map.mlg's AVL insert, which rebuilds via a
    -- rebalancing helper (mkNode/rotation) rather than a direct Construct.
    let insertName = mkName "insert" 0
        mkNodeName = mkName "mkNode" 1
        x = mkName "x" 2
        tree = mkName "tree" 3
        val = mkName "val" 4
        left = mkName "left" 5
        right = mkName "right" 6
        recCall = Apply dummyRange (Apply dummyRange (Invoke dummyRange insertName) [v dummyRange x]) [v dummyRange left]
        rebuild = Apply dummyRange (Apply dummyRange (Apply dummyRange (Invoke dummyRange mkNodeName) [recCall]) [v dummyRange val]) [v dummyRange right]
        nodeBranch = Branch dummyRange (Destruct dummyRange (Tag "Node") [PVar dummyRange val, PVar dummyRange left, PVar dummyRange right]) rebuild
        body = Lambda dummyRange [x] (Lambda dummyRange [tree] (Select dummyRange (v dummyRange tree) [nodeBranch]))
        prog = program [(dummyRange, insertName, body)]
    fmap sh (lookupDef insertName (run prog)) `shouldBe` Just (sh body)

  it "leaves a non-recursive base case untouched" do
    let fName = mkName "f" 0
        xs = mkName "xs" 1
        nilBranch = Branch dummyRange (Destruct dummyRange (Tag "Nil") []) (Construct dummyRange (Tag "Nil") [])
        body = Lambda dummyRange [xs] (Select dummyRange (v dummyRange xs) [nilBranch])
        prog = program [(dummyRange, fName, body)]
    fmap sh (lookupDef fName (run prog)) `shouldBe` Just (sh body)

  it "leaves a definition that isn't Lambda+Select shaped completely untouched" do
    let fName = mkName "f" 0
        body = Construct dummyRange (Tag "Unit") []
        prog = program [(dummyRange, fName, body)]
    fmap sh (lookupDef fName (run prog)) `shouldBe` Just (sh body)
