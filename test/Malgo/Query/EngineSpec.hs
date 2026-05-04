module Malgo.Query.EngineSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Malgo.Module (ModuleName (..))
import Malgo.Prelude
import Malgo.Query.Engine (reverseDepClosure)
import Test.Hspec

-- | Build a forward dep map from a list of @(modName, [deps])@ pairs.
mkDeps :: [(ModuleName, [ModuleName])] -> Map ModuleName (Set ModuleName)
mkDeps = Map.fromList . map (\(m, ds) -> (m, Set.fromList ds))

a, b, c, d, e :: ModuleName
a = ModuleName "A"
b = ModuleName "B"
c = ModuleName "C"
d = ModuleName "D"
e = ModuleName "E"

spec :: Spec
spec = describe "reverseDepClosure" do
  it "is empty when nothing depends on the target" do
    let depsOf = mkDeps [(a, []), (b, [])]
    reverseDepClosure depsOf a `shouldBe` Set.empty

  it "picks up direct importers" do
    -- B depends on A, so invalidating A must invalidate B.
    let depsOf = mkDeps [(a, []), (b, [a])]
    reverseDepClosure depsOf a `shouldBe` Set.singleton b

  it "picks up transitive importers" do
    -- C -> B -> A. Invalidating A must reach C even though C does not
    -- name A directly.
    let depsOf = mkDeps [(a, []), (b, [a]), (c, [b])]
    reverseDepClosure depsOf a `shouldBe` Set.fromList [b, c]

  it "excludes the target itself" do
    -- The wrapper 'invalidateWithRdeps' adds the target separately.
    let depsOf = mkDeps [(a, [b]), (b, [])]
    reverseDepClosure depsOf b `shouldBe` Set.singleton a

  it "ignores siblings that do not depend on the target" do
    -- D has its own subtree; nothing in it must be invalidated by A.
    let depsOf =
          mkDeps
            [ (a, []),
              (b, [a]),
              (d, []),
              (e, [d])
            ]
    reverseDepClosure depsOf a `shouldBe` Set.singleton b

  it "terminates on cyclic graphs" do
    -- Defensive: even if a future module system permits cycles, the
    -- closure must not loop forever.
    let depsOf = mkDeps [(a, [b]), (b, [a])]
    reverseDepClosure depsOf a `shouldBe` Set.singleton b
