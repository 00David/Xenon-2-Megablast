{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RockSpec (
    TestRock(..),
    spec
)
where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Sequence as Seq

import GameSetup
import GameState.Rock
import Graphics.Assets
import Objects.Objects
import Objects.Hitbox
import Typeclasses.Invariant
import AssetsSpec(TestGameAssets(..))
import ObjectsSpec(TestObject(..))

spec :: Spec
spec = do
    initRockSpec
    startInitRockSpec
    moveRockSpec
    moveRockQuickCheckSpec
    insideYScreenRockSpec
    getTranslatedRockAssetQuickCheckSpec
    invariantLawsSpec
    renderableLawSpec
    collidableLawsSpec

-- Initializes Rocks veryfing their invariant
newtype TestRock = TestRock { getRock :: Rock }deriving (Eq, Show)
instance Arbitrary TestRock where
    arbitrary :: Gen TestRock
    arbitrary = do
        asset <- choose (0, nbRockAssets - 1)
        forward <- arbitrary

        leftSide <- arbitrary
        x <- arbitrary
        y <- arbitrary


        let obj = initStaticObject ((Seq.index rockHitbox asset) x y leftSide)
            rock = if leftSide
                then LeftRock obj asset forward
                else RightRock obj asset forward

        return (TestRock rock)

prop_initRock_preservesInvariant :: TestObject -> Bool -> Bool -> Property
prop_initRock_preservesInvariant (TestObject obj) leftSide forward =
    let isStatic = case obj of
                    (StaticO _) -> True
                    _ -> False
    in isStatic ==> -- filter by keeping only static objects
        forAll (choose (0, nbRockAssets - 1)) $ \asset ->
            prop_inv_rock (initRock obj asset leftSide forward)

prop_startInitRock_preservesInvariant :: XCoord -> YCoord -> Bool -> Bool -> Property
prop_startInitRock_preservesInvariant x y leftSide forward = 
    forAll (choose (0, (nbRockAssets-1))) $ \asset ->
        prop_inv_rock (startInitRock x y asset leftSide forward)

initRockSpec :: SpecWith ()
initRockSpec = do
    describe "initRock (QuickCheck)" $ do
        it "preserves the Rock invariant for valid Rocks" $
            property prop_initRock_preservesInvariant

startInitRockSpec :: SpecWith ()
startInitRockSpec = do
    describe "startInitRock (QuickCheck)" $ do
        it "preserves the Rock invariant for start Rock" $
            property prop_startInitRock_preservesInvariant

moveRockSpec :: Spec
moveRockSpec = do
    describe "moveRock (unit tests)" $ do
        it "moves towards the bottom the rock according to a screen scrolling speed of 5" $ do
            let obj = initStaticObject (initHitboxCircle 0 0 10)
                r = LeftRock obj 0 True
                r' = moveRock r 5
                obj' = rockObject r'

            centerHitbox (objectHitbox obj') `shouldBe` (0, (-5))

        it "moves the rock according to a screen scrolling speed of 0 (don't move it)" $ do
            let obj = initStaticObject (initHitboxCircle 0 0 10)
                r = LeftRock obj 0 True
                r' = moveRock r 0
                obj' = rockObject r'

            centerHitbox (objectHitbox obj') `shouldBe` centerHitbox (objectHitbox obj)

moveRockQuickCheckSpec :: Spec
moveRockQuickCheckSpec = do
    describe "moveRock (QuickCheck)" $ do
        it "satisfies moveRock post-condition for all valid parameters" $
            property ( \(TestRock r) ss ->
                (prop_inv_rock r && (prop_pre_moveRock r ss)) 
                ==> let r' = moveRock r ss
                    in prop_inv_rock r' && (prop_post_moveRock r ss)
            )

insideYScreenRockSpec :: Spec
insideYScreenRockSpec = do
    describe "insideScreenRock (unit tests)" $ do
        it "rock above screen is inside" $ do
            let obj = initStaticObject (initHitboxCircle 0 (topYScreenBound+100) 10)
                r = LeftRock obj 0 True
            insideYScreenRock r `shouldBe` True
        it "rock inside screen is inside" $ do
            let obj = initStaticObject (initHitboxCircle 0 0 10)
                r = LeftRock obj 0 True
            insideYScreenRock r `shouldBe` True
        it "rock below screen is outside" $ do
            let obj = initStaticObject (initHitboxCircle 0 (bottomYScreenBound-100) 10)
                r = LeftRock obj 0 True
            insideYScreenRock r `shouldBe` False

getTranslatedRockAssetQuickCheckSpec :: Spec
getTranslatedRockAssetQuickCheckSpec = do
    describe "getTranslatedRockAsset (QuickCheck)" $ do
        it "satisfies getTranslatedRockAsset post-condition for all valid parameters" $
            property (\(TestGameAssets ga) (TestRock rock) -> 
                prop_inv_rock rock ==> prop_post_getTranslatedRockAsset ga rock
                )

-- ============================================================
-- ======================== LAWS ==============================
-- ============================================================

invariantLawsSpec :: Spec
invariantLawsSpec = do
    describe "Invariant laws (QuickCheck)" $ do
        it "law_invariant_stable for Rock" $
            property (
                \(TestRock rock) -> law_invariant_stable rock
            )
        it "law_invariant_idempotent for Rock" $
            property (
                \(TestRock rock) -> law_invariant_idempotent rock
            )

renderableLawSpec :: Spec
renderableLawSpec = do
    describe "Renderable laws (QuickCheck)" $ do
        it "law_renderable_finite for Rock" $
            property (\(TestGameAssets ga) (TestRock rock) ->
                law_renderable_finite ga rock
            )

collidableLawsSpec :: Spec
collidableLawsSpec = do
    describe "Collidable laws (QuickCheck)" $ do
        it "law_collidable_reflexive for Rock" $
            property ( \(TestRock o) ->
                prop_inv_rock o 
                ==> law_collidable_reflexive o
            )
        it "law_collidable_symmetric for Rock with another Rock" $
            property ( \(TestRock r1) (TestRock r2) ->
                prop_inv_rock r1 && prop_inv_rock r2 
                ==> law_collidable_symmetric r1 r2
            )
        it "law_collidable_symmetric for Rock with another Object" $
            property ( \(TestRock r1) (TestObject o2) ->
                prop_inv_rock r1 && prop_inv_object o2 
                ==> law_collidable_symmetric r1 o2
            )

        it "law_collidable_will_collide for Rock with another Rock" $
            property ( \(TestRock r1) (TestRock r2) ->
                prop_inv_rock r1 && prop_inv_rock r2 
                ==> law_collidable_will_collide r1 r2
            )
        it "law_collidable_will_collide for Rock with another Object" $
            property ( \(TestRock r1) (TestObject o2) ->
                prop_inv_rock r1 && prop_inv_object o2 
                ==> law_collidable_will_collide r1 o2
            )