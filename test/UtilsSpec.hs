module UtilsSpec (
    spec
)
where

import Test.Hspec

import Utils

spec :: Spec
spec = do
  clampSpec

clampSpec :: SpecWith ()
clampSpec = do
    describe "clamp (unit tests)" $ do
        it "clamp on a value below the range, gives the range lower bound" $ do
            (clamp (-5) 0 3) `shouldBe` (0::Int)

        it "clamp on a value above the range, gives the range upper bound" $ do
            (clamp 10 0 3) `shouldBe` (3::Int)

        it "clamp on a value inside the range, gives the value" $ do
            (clamp 2 0 3) `shouldBe` (2::Int)