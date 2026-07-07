-- | Exact-shape unit tests for the scrutinee-tuple elimination pass.
-- Corpus-wide safety is covered by 'Malgo.Backend.Zig.PerceusSpec's
-- oracle, which runs the real pipeline (peephole included) through the
-- RcCheck linearity checker; end-to-end behavior by the zig-golden
-- parity harness.
module Malgo.Backend.Zig.PeepholeSpec (spec) where

import Malgo.Backend.Zig.Ir
import Malgo.Backend.Zig.Peephole (peepholeFunc)
import Malgo.Id
import Malgo.Module (ModuleName (..))
import Malgo.Prelude
import Malgo.Sequent.Fun (Tag (..))
import Test.Hspec
import Text.Megaparsec.Pos (initialPos)

mkName :: Text -> Int -> Id
mkName name uniq = Id {name, moduleName = ModuleName "PeepholeTest", sort = Temporal uniq}

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

vA, vB, vK, vH, vT, vU :: Id
vA = mkName "a" 10
vB = mkName "b" 11
vK = mkName "k" 12
vH = mkName "h" 13
vT = mkName "t" 14
vU = mkName "u" 15

spec :: Spec
spec = describe "scrutinee-tuple elimination" do
  it "eliminates a match tuple: reads re-rooted, true root test deleted, alias substituted" do
    -- The shape fromClauses produces: fresh tuple of the parameters,
    -- guarded Select arm reading fields out of it.
    let fn =
          mkFunc
            [vA, vK]
            ( Block
                [ Let vT (MkStruct Tuple [vA, vK]),
                  Let vH (ReadPath (PField (PRoot vT) 0))
                ]
                ( TIf
                    (GAnd [TTagEq (PRoot vT) Tuple, TTagEq (PField (PRoot vT) 0) (Tag "Cons")])
                    (Block [] (TApplyCo vK vH))
                    (Block [] (TPanic "no match"))
                )
            )
    (peepholeFunc fn).body
      `shouldBe` Block
        []
        ( TIf
            (GAnd [TTagEq (PRoot vA) (Tag "Cons")])
            (Block [] (TApplyCo vK vA))
            (Block [] (TPanic "no match"))
        )

  it "re-roots reads deeper than one field" do
    let fn =
          mkFunc
            [vA, vK]
            ( Block
                [ Let vT (MkStruct Tuple [vA, vK]),
                  Let vH (ReadPath (PField (PField (PRoot vT) 0) 1))
                ]
                (TApplyCo vK vH)
            )
    (peepholeFunc fn).body
      `shouldBe` Block [Let vH (ReadPath (PField (PRoot vA) 1))] (TApplyCo vK vH)

  it "keeps a tuple consumed by a terminator" do
    let body = Block [Let vT (MkStruct Tuple [vA])] (TApplyCo vK vT)
        fn = mkFunc [vA, vK] body
    (peepholeFunc fn).body `shouldBe` body

  it "keeps a tuple observed by a primitive" do
    let body =
          Block
            [ Let vT (MkStruct Tuple [vA]),
              Let vH (Prim "malgo_print" [vT])
            ]
            (TApplyCo vK vH)
        fn = mkFunc [vA, vK] body
    (peepholeFunc fn).body `shouldBe` body

  it "keeps a tuple whose root test does not match its construction" do
    let body =
          Block
            [Let vT (MkStruct Tuple [vA])]
            ( TIf
                (GAnd [TTagEq (PRoot vT) (Tag "Cons")])
                (Block [] (TApplyCo vK vT))
                (Block [] (TPanic "no match"))
            )
        fn = mkFunc [vA, vK] body
    (peepholeFunc fn).body `shouldBe` body

  it "eliminates a tuple used in both branches of a TIf" do
    let fn =
          mkFunc
            [vA, vB, vK]
            ( Block
                [Let vT (MkStruct Tuple [vA, vB])]
                ( TIf
                    (GIsZero vA)
                    (Block [Let vH (ReadPath (PField (PRoot vT) 0))] (TApplyCo vK vH))
                    (Block [Let vU (ReadPath (PField (PRoot vT) 1))] (TApplyCo vK vU))
                )
            )
    (peepholeFunc fn).body
      `shouldBe` Block
        []
        ( TIf
            (GIsZero vA)
            (Block [] (TApplyCo vK vA))
            (Block [] (TApplyCo vK vB))
        )

  it "eliminates nested tuples via the fixpoint" do
    let inner = mkName "inner" 20
    let fn =
          mkFunc
            [vA, vB, vK]
            ( Block
                [ Let inner (MkStruct Tuple [vA, vB]),
                  Let vT (MkStruct Tuple [inner]),
                  Let vH (ReadPath (PField (PField (PRoot vT) 0) 1))
                ]
                (TApplyCo vK vH)
            )
    (peepholeFunc fn).body `shouldBe` Block [] (TApplyCo vK vB)

  it "substitutes pure aliases even without a tuple" do
    let fn =
          mkFunc
            [vA, vK]
            (Block [Let vH (ReadPath (PRoot vA))] (TApplyCo vK vH))
    (peepholeFunc fn).body `shouldBe` Block [] (TApplyCo vK vA)
