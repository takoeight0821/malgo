-- | Two layers of protection for the Reuse pass, mirroring
-- 'Malgo.Backend.Zig.PerceusSpec':
--
--   1. Exact-placement unit tests: hand-built post-Perceus 'Ir.Func's in,
--      'reuseFunc' out, compared structurally. These pin the pairing
--      rules themselves (nearest-LIFO pairing, unpaired drops flushed
--      back out unchanged, the 'Ir.PanicExpr' barrier, per-branch
--      independence).
--   2. RcCheck token-linearity tests, run directly against hand-built
--      'Ir.Program's containing 'Ir.DropReuse'\/'Ir.MkStructReuse'.
--
-- The corpus oracle (does the full saturate+peephole+perceus+reuse
-- pipeline stay linear on every golden testcase, and does reuse pairing
-- fire at least once somewhere in the corpus) lives in
-- 'Malgo.Backend.Zig.PerceusSpec.corpusSpec', which this module extends.
--
-- 'runMalgoM' resets its 'Malgo.Prelude.Uniq' counter to 0 for every call,
-- so within one 'runReuse' invocation the pass's fresh reuse-token names
-- are deterministic: @reuse_0@, @reuse_1@, ... in the order 'reuseFunc'
-- encounters each pairing.
module Malgo.Backend.Zig.ReuseSpec (spec) where

import Effectful.Reader.Static (runReader)
import Malgo.Backend.Zig.Ir
import Malgo.Backend.Zig.RcCheck (RcViolation (..), checkProgram)
import Malgo.Backend.Zig.Reuse (reuseFunc)
import Malgo.Id
import Malgo.Module (ModuleName (..))
import Malgo.Monad (runMalgoM)
import Malgo.Prelude
import Malgo.Sequent.Fun (Tag (..))
import Malgo.TestUtils (flag)
import Test.Hspec
import Text.Megaparsec.Pos (initialPos)

mkName :: Text -> Int -> Id
mkName name uniq = Id {name, moduleName = ModuleName "ReuseTest", sort = Temporal uniq}

dummyRange :: Range
dummyRange = Range (initialPos "") (initialPos "")

mkFunc :: [Id] -> Block -> Func
mkFunc params body =
  Func
    { range = dummyRange,
      name = mkName "fn" 0,
      kind = TopLevelFn,
      selfVar = mkName "self" 1,
      params,
      body
    }

vA, vB, vK, vS1, vS2 :: Id
vA = mkName "a" 10
vB = mkName "b" 11
vK = mkName "k" 12
vS1 = mkName "s1" 13
vS2 = mkName "s2" 14

-- | The Nth fresh reuse token 'reuseFunc' mints (0-indexed), given
-- 'runMalgoM' always starts 'Malgo.Prelude.Uniq' at 0.
tok :: Int -> Id
tok = mkName "reuse"

runReuse :: Func -> IO Func
runReuse fn = runMalgoM flag $ runReader (ModuleName "ReuseTest") $ reuseFunc fn

spec :: Spec
spec = do
  placementSpec
  rcCheckSpec

placementSpec :: Spec
placementSpec = describe "Reuse token pairing" do
  it "pairs a Drop with a later same-shape MkStruct" do
    let fn = mkFunc [vA, vB, vK] (Block [Drop vA, Let vS1 (MkStruct Tuple [vB])] (TApplyCo vK vS1))
    fn' <- runReuse fn
    fn'.body `shouldBe` Block [DropReuse (tok 0) vA 1, Let vS1 (MkStructReuse (tok 0) Tuple [vB])] (TApplyCo vK vS1)

  it "pairs LIFO: the most recently dropped variable wins the nearest MkStruct" do
    let fn =
          mkFunc
            [vA, vB, vK]
            ( Block
                [ Drop vA,
                  Drop vB,
                  Let vS1 (MkStruct Tuple [vK]),
                  Let vS2 (MkStruct Tuple [vK])
                ]
                (TCallClosure vK [vS1, vS2])
            )
    fn' <- runReuse fn
    -- vB was dropped last, so it pairs with the first MkStruct (token
    -- reuse_0); vA, dropped first, pairs with the second (reuse_1).
    fn'.body
      `shouldBe` Block
        [ DropReuse (tok 0) vB 1,
          Let vS1 (MkStructReuse (tok 0) Tuple [vK]),
          DropReuse (tok 1) vA 1,
          Let vS2 (MkStructReuse (tok 1) Tuple [vK])
        ]
        (TCallClosure vK [vS1, vS2])

  it "flushes an unpaired Drop (no later MkStruct at all) back out unchanged" do
    let fn = mkFunc [vA, vK] (Block [Drop vA] (TApplyCo vK vA))
    fn' <- runReuse fn
    fn'.body `shouldBe` Block [Drop vA] (TApplyCo vK vA)

  it "flushes the extra pending drop when there are more drops than MkStructs" do
    let fn =
          mkFunc
            [vA, vB, vK]
            (Block [Drop vA, Drop vB, Let vS1 (MkStruct Tuple [vK])] (TCallClosure vK [vS1]))
    fn' <- runReuse fn
    -- vB (nearest) pairs with the only MkStruct; vA, left pending when the
    -- list ends, must come back out as a plain Drop -- losing it here
    -- would leak.
    fn'.body
      `shouldBe` Block
        [DropReuse (tok 0) vB 1, Let vS1 (MkStructReuse (tok 0) Tuple [vK]), Drop vA]
        (TCallClosure vK [vS1])

  it "does not pair a Drop that comes after the MkStruct" do
    let fn = mkFunc [vA, vB, vK] (Block [Let vS1 (MkStruct Tuple [vB]), Drop vA] (TCallClosure vK [vS1]))
    fn' <- runReuse fn
    fn'.body `shouldBe` Block [Let vS1 (MkStruct Tuple [vB]), Drop vA] (TCallClosure vK [vS1])

  it "treats a bare PanicExpr as a barrier: no pairing survives past it" do
    let fn = mkFunc [vA, vK] (Block [Drop vA, Let vB (PanicExpr "no match")] (TApplyCo vK vB))
    fn' <- runReuse fn
    fn'.body `shouldBe` Block [Drop vA, Let vB (PanicExpr "no match")] (TApplyCo vK vB)

  it "pairs within each TIf branch independently" do
    let fn =
          mkFunc
            [vA, vB, vK]
            ( Block
                []
                ( TIf
                    (GIsZero vA)
                    (Block [Drop vA, Let vS1 (MkStruct Tuple [vB])] (TApplyCo vK vS1))
                    (Block [Drop vB] (TApplyCo vK vA))
                )
            )
    fn' <- runReuse fn
    fn'.body
      `shouldBe` Block
        []
        ( TIf
            (GIsZero vA)
            (Block [DropReuse (tok 0) vA 1, Let vS1 (MkStructReuse (tok 0) Tuple [vB])] (TApplyCo vK vS1))
            (Block [Drop vB] (TApplyCo vK vA))
        )

rcCheckSpec :: Spec
rcCheckSpec = describe "RcCheck reuse-token linearity" do
  it "accepts a well-formed DropReuse/MkStructReuse pair" do
    let fn = mkFunc [vA, vB, vK] (Block [DropReuse (tok 0) vA 1, Let vS1 (MkStructReuse (tok 0) Tuple [vB])] (TApplyCo vK vS1))
        program = Program {funcs = [fn], entry = Nothing}
    checkProgram program `shouldBe` Right ()

  it "flags MkStructReuse referencing an unavailable token" do
    let fn = mkFunc [vA, vK] (Block [Let vS1 (MkStructReuse (tok 0) Tuple [vA])] (TApplyCo vK vS1))
        program = Program {funcs = [fn], entry = Nothing}
    checkProgram program `shouldSatisfy` \case
      Left violations -> any (\case TokenUnavailable _ t -> t == tok 0; _ -> False) violations
      Right () -> False

  it "flags a token left unconsumed at a terminator" do
    let fn = mkFunc [vA, vK] (Block [DropReuse (tok 0) vA 1] (TApplyCo vK vK))
        program = Program {funcs = [fn], entry = Nothing}
    checkProgram program `shouldSatisfy` \case
      Left violations -> any (\case TokenUnconsumed _ ts -> tok 0 `elem` ts; _ -> False) violations
      Right () -> False
