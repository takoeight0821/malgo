-- | Two layers of protection for the Perceus pass:
--
--   1. Exact-placement unit tests: hand-built 'Ir.Func's in,
--      'perceusFunc' out, compared structurally. These pin the insertion
--      rules themselves (entry drops, dup-per-extra-occurrence,
--      per-branch drops, borrowed-let promotion\/elision, the
--      dup-captures-then-drop-self protocol).
--   2. A corpus oracle: every golden test case is compiled to Ir, run
--      through Perceus, and checked by the 'RcCheck' linearity checker —
--      covering every control-flow path of the whole corpus (including
--      paths no golden input executes) with no Zig toolchain in the loop.
module Malgo.Backend.Zig.PerceusSpec (specWith) where

import Effectful.Reader.Static (runReader)
import Malgo.Backend.Zig (ZigStages (..), runZigStages)
import Malgo.Backend.Zig.Ir
import Malgo.Backend.Zig.Perceus (perceusFunc)
import Malgo.Backend.Zig.RcCheck (RcViolation (..), checkFunc, checkProgram)
import Malgo.Id
import Malgo.Module (ArtifactPath, ModuleName (..))
import Malgo.Monad (runMalgoM)
import Malgo.Prelude
import Malgo.Sequent.Fun (Literal (..), Tag (..))
import Malgo.TestUtils
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, takeBaseName, (</>))
import Test.Hspec
import Text.Megaparsec.Pos (initialPos)

specWith :: ArtifactPath -> ArtifactPath -> Spec
specWith builtin prelude = do
  placementSpec
  rcCheckSpec
  corpusSpec builtin prelude

-- Distinct test-local names. The uniq is what distinguishes them.
mkName :: Text -> Int -> Id
mkName name uniq = Id {name, moduleName = ModuleName "PerceusTest", sort = Temporal uniq}

dummyRange :: Range
dummyRange = Range (initialPos "") (initialPos "")

mkFunc :: FuncKind -> [Id] -> Block -> Func
mkFunc kind params body =
  Func
    { range = dummyRange,
      name = mkName "fn" 0,
      kind,
      selfVar = mkName "self" 1,
      params,
      body
    }

-- Shared vars for the hand-built cases.
vA, vB, vK, vS, vH, vL, vF1, vF2, vC :: Id
vA = mkName "a" 10
vB = mkName "b" 11
vK = mkName "k" 12
vS = mkName "s" 13
vH = mkName "h" 14
vL = mkName "l" 15
vF1 = mkName "f1" 16
vF2 = mkName "f2" 17
vC = mkName "c" 18

placementSpec :: Spec
placementSpec = describe "Perceus dup/drop placement" do
  it "drops an unused parameter at function entry" do
    let fn = mkFunc TopLevelFn [vA, vK] (Block [Let vL (Lit (Int32 1))] (TApplyCo vK vL))
    (perceusFunc fn).body
      `shouldBe` Block [Drop vA, Let vL (Lit (Int32 1))] (TApplyCo vK vL)

  it "dups once for a variable consumed twice by one construction" do
    let fn = mkFunc TopLevelFn [vA, vK] (Block [Let vS (MkStruct Tuple [vA, vA])] (TApplyCo vK vS))
    (perceusFunc fn).body
      `shouldBe` Block [Dup vA, Let vS (MkStruct Tuple [vA, vA])] (TApplyCo vK vS)

  it "dups per occurrence when the consumed variable stays live" do
    let fn = mkFunc TopLevelFn [vA, vK] (Block [Let vS (MkStruct Tuple [vA])] (TCallClosure vK [vA, vS]))
    (perceusFunc fn).body
      `shouldBe` Block [Dup vA, Let vS (MkStruct Tuple [vA])] (TCallClosure vK [vA, vS])

  it "drops, at each branch top, what only the other branch uses" do
    let fn =
          mkFunc
            TopLevelFn
            [vA, vB, vK]
            (Block [] (TIf (GIsZero vA) (Block [] (TApplyCo vK vA)) (Block [] (TApplyCo vK vB))))
    (perceusFunc fn).body
      `shouldBe` Block
        []
        ( TIf
            (GIsZero vA)
            (Block [Drop vB] (TApplyCo vK vA))
            (Block [Drop vA] (TApplyCo vK vB))
        )

  it "promotes a live borrowed read with a dup, then drops the dead root" do
    let fn = mkFunc TopLevelFn [vS, vK] (Block [Let vH (ReadPath (PField (PRoot vS) 0))] (TApplyCo vK vH))
    (perceusFunc fn).body
      `shouldBe` Block
        [Let vH (ReadPath (PField (PRoot vS) 0)), Dup vH, Drop vS]
        (TApplyCo vK vH)

  it "elides a dead borrowed read entirely" do
    let fn = mkFunc TopLevelFn [vS, vK] (Block [Let vH (ReadPath (PField (PRoot vS) 0))] (TApplyCo vK vS))
    (perceusFunc fn).body `shouldBe` Block [] (TApplyCo vK vS)

  it "dups used captures before dropping self (the closure protocol)" do
    let fn0 = mkFunc ClosureFn [vK] (Block [Let vC (ReadCapture (mkName "self" 1) 0)] (TApplyCo vK vC))
    (perceusFunc fn0).body
      `shouldBe` Block
        [Let vC (ReadCapture (mkName "self" 1) 0), Dup vC, Drop (mkName "self" 1)]
        (TApplyCo vK vC)

  it "dups a record forced more than once (call-by-name re-runs fields)" do
    let fn =
          mkFunc
            TopLevelFn
            [vS, vK]
            (Block [Let vF1 (Force vS "a"), Let vF2 (Force vS "b")] (TCallClosure vK [vF1, vF2]))
    (perceusFunc fn).body
      `shouldBe` Block
        [Dup vS, Let vF1 (Force vS "a"), Let vF2 (Force vS "b")]
        (TCallClosure vK [vF1, vF2])

  it "drops everything else before a Finish return" do
    let fn = mkFunc TopLevelFn [vA, vB] (Block [] (TReturn vB))
    (perceusFunc fn).body `shouldBe` Block [Drop vA] (TReturn vB)

  it "inserts nothing on a bare panic path" do
    -- Both operands are consumed by the then-branch and the else-branch is
    -- a bare panic (RC-exempt), so the function comes back unchanged.
    let body = Block [] (TIf (GIsZero vA) (Block [] (TApplyCo vK vA)) (Block [] (TPanic "no matching branch")))
        fn = mkFunc TopLevelFn [vA, vK] body
    (perceusFunc fn).body `shouldBe` body

rcCheckSpec :: Spec
rcCheckSpec = describe "RcCheck linearity checker" do
  it "accepts every placement-spec output" do
    let fns =
          [ mkFunc TopLevelFn [vA, vK] (Block [Let vL (Lit (Int32 1))] (TApplyCo vK vL)),
            mkFunc TopLevelFn [vA, vK] (Block [Let vS (MkStruct Tuple [vA, vA])] (TApplyCo vK vS)),
            mkFunc TopLevelFn [vS, vK] (Block [Let vH (ReadPath (PField (PRoot vS) 0))] (TApplyCo vK vH)),
            mkFunc ClosureFn [vK] (Block [Let vC (ReadCapture (mkName "self" 1) 0)] (TApplyCo vK vC)),
            mkFunc TopLevelFn [vS, vK] (Block [Let vF1 (Force vS "a"), Let vF2 (Force vS "b")] (TCallClosure vK [vF1, vF2]))
          ]
    for_ fns \fn -> checkFunc (perceusFunc fn) `shouldBe` []

  it "flags consuming the same reference twice" do
    let fn = mkFunc TopLevelFn [vA, vK] (Block [] (TCallClosure vK [vA, vA]))
    checkFunc fn `shouldSatisfy` any \case
      UseAfterConsume _ v -> v == vA
      _ -> False

  it "flags an owned reference never consumed" do
    let fn = mkFunc TopLevelFn [vA, vK] (Block [Let vL (Lit (Int32 1))] (TApplyCo vK vL))
    checkFunc fn `shouldSatisfy` any \case
      UnconsumedAtExit _ vs -> vA `elem` vs
      _ -> False

  it "flags reading a borrowed alias after its root was consumed" do
    let fn =
          mkFunc
            TopLevelFn
            [vS, vK]
            ( Block
                [ Let vH (ReadPath (PRoot vS)),
                  Drop vS,
                  Dup vH
                ]
                (TApplyCo vK vH)
            )
    checkFunc fn `shouldSatisfy` any \case
      DupOfDead _ v -> v == vH
      _ -> False

corpusSpec :: ArtifactPath -> ArtifactPath -> Spec
corpusSpec builtin prelude = describe "corpus linearity (all golden testcases)" do
  testcases <- runIO do
    files <- listDirectory testcaseDir
    pure $ filter (isExtensionOf "mlg") files
  reuseFired <- runIO $ newIORef False
  parallel $ for_ testcases \testcase ->
    it (takeBaseName testcase) do
      -- saturateProgram already ran inside compileTestCase's toCore call.
      (moduleName, program) <- compileTestCase builtin prelude (testcaseDir </> testcase)
      stages <- runMalgoM flag $ runReader moduleName $ runZigStages program
      -- The conversion itself never inserts RC ops...
      for_ stages.closureConv.funcs \fn ->
        hasRcOps fn.body `shouldBe` False
      -- ...and the full pipeline (scrutinee-tuple peephole, Perceus, then
      -- Reuse — mirroring Zig.hs's order) stays linear on every path,
      -- including its new reuse-token obligations.
      checkProgram stages.reuse `shouldBe` Right ()
      when (any (hasReuseOp . (.body)) stages.reuse.funcs) $ writeIORef reuseFired True
  -- A silent no-op pairing pass (matching nothing corpus-wide) would still
  -- pass every linearity check above; this is the guard against that.
  it "pairs at least one Drop/MkStruct somewhere in the corpus" do
    readIORef reuseFired `shouldReturn` True

hasRcOps :: Block -> Bool
hasRcOps (Block stmts term) =
  any isRcOp stmts || case term of
    TIf _ t e -> hasRcOps t || hasRcOps e
    _ -> False
  where
    isRcOp (Dup _) = True
    isRcOp (Drop _) = True
    isRcOp (DropReuse {}) = False
    isRcOp (Let _ _) = False

hasReuseOp :: Block -> Bool
hasReuseOp (Block stmts term) =
  any isReuseOp stmts || case term of
    TIf _ t e -> hasReuseOp t || hasReuseOp e
    _ -> False
  where
    isReuseOp (DropReuse {}) = True
    isReuseOp _ = False
