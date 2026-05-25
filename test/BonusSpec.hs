{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BonusSpec (
    TestBonus(..),
    TestPlayerShootBonus(..),
    spec
)
where

import Test.Hspec
import Test.QuickCheck

import System.Random

import GameSetup
import GameState.Bonus
import GameState.Enemy
import Objects.Objects
import Objects.Hitbox
import Graphics.Assets
import Typeclasses.Invariant

import AssetsSpec(TestGameAssets(..))
import HitboxSpec(TestHitbox(..))
import ObjectsSpec(TestObject(..))
import EnemySpec(TestEnemy(..))

spec :: Spec
spec = do
    initPlayerShootBonusSpec
    startInitPlayerShootBonusSpec
    generateBonusForEnemySpec
    generateNewBonusesSpec
    moveBonusSpec
    moveBonusQuickCheckSpec
    insideScreenOrAboveBonusSpec
    getTranslatedBonusAssetQuickCheckSpec
    invariantLawsSpec
    renderableLawSpec
    collidableLawsSpec

newtype TestPlayerShootBonus = TestPlayerShootBonus { getPlayerShootBonus :: PlayerShootBonus} deriving (Eq, Show)
instance Arbitrary TestPlayerShootBonus where
    arbitrary :: Gen TestPlayerShootBonus
    arbitrary = do
        bonusType <- elements
            [ ShootFaster
            , DelayDecreased
            , MoreDamages
            , BiggerShots
            ]
        return (TestPlayerShootBonus bonusType)

-- Initializes Bonuses veryfing their invariant
newtype TestBonus = TestBonus { getTBonus :: Bonus } deriving (Eq, Show)
instance Arbitrary TestBonus where
    arbitrary :: Gen TestBonus
    arbitrary = do
            (TestHitbox h) <- arbitrary
            (TestPlayerShootBonus bonus) <- arbitrary
            return (TestBonus (initPlayerShootBonus (initStaticObject h) bonus))

prop_initPlayerShootBonus_preservesInvariant :: TestObject -> TestPlayerShootBonus -> Property
prop_initPlayerShootBonus_preservesInvariant (TestObject obj) (TestPlayerShootBonus psb) =
    not (isMovable obj) ==> prop_inv_bonus (initPlayerShootBonus obj psb)

prop_startInitPlayerShootBonus_preservesInvariant :: XCoord -> YCoord -> TestPlayerShootBonus -> Bool
prop_startInitPlayerShootBonus_preservesInvariant x y (TestPlayerShootBonus psb) =
    prop_inv_bonus (startInitPlayerShootBonus x y psb)

initPlayerShootBonusSpec :: Spec
initPlayerShootBonusSpec = do
    describe "initPlayerShootBonus (QuickCheck)" $ do
        it "preserves Bonus invariant" $
            property prop_initPlayerShootBonus_preservesInvariant

startInitPlayerShootBonusSpec :: Spec
startInitPlayerShootBonusSpec = do
    describe "startInitPlayerShootBonus (QuickCheck)" $ do
        it "preserves Bonus invariant at start" $
            property prop_startInitPlayerShootBonus_preservesInvariant

generateBonusForEnemySpec :: Spec
generateBonusForEnemySpec = do
    describe "generateBonusForEnemy (QuickCheck)" $ do
        it "satisfies generateBonusForEnemy post-condition for valid enemies" $
            property (
                \(TestEnemy e) seed ->
                prop_inv_enemy e 
                ==> prop_post_generateBonusForEnemy e (mkStdGen seed, [])
            )

generateNewBonusesSpec :: Spec
generateNewBonusesSpec = do
    describe "generateNewBonuses (QuickCheck)" $ do
        it "satisfies generateNewBonuses post-condition for valid enemies" $
            property (\seed (beforeE :: [TestEnemy]) (afterE :: [TestEnemy]) ->
                    let beforeEnemies = map getEnemy beforeE
                        afterEnemies  = map getEnemy afterE
                        gen = mkStdGen seed
                    in (prop_pre_generateNewBonuses gen beforeEnemies afterEnemies)
                    ==> prop_post_generateNewBonuses gen beforeEnemies afterEnemies
            )

moveBonusSpec :: Spec
moveBonusSpec = do
    describe "moveBonus (unit tests)" $ do
        it "moves bonus towards bottom according to a screen scrolling speed of 10" $ do
            let b = startInitPlayerShootBonus 0 0 ShootFaster
                b' = moveBonus b 10
                obj = bonusObject b'
            centerHitbox (objectHitbox obj) `shouldBe` (0,(-10))
        it "moves bonus towards bottom according to a screen scrolling speed of 1" $ do
            let b = startInitPlayerShootBonus 0 0 ShootFaster
                b' = moveBonus b 1
                obj = bonusObject b'
            centerHitbox (objectHitbox obj) `shouldBe` (0,(-1))

moveBonusQuickCheckSpec :: Spec
moveBonusQuickCheckSpec = do
    describe "moveBonus (QuickCheck)" $ do
        it "satisfies moveBonus post-condition for valid parameters" $
            property (\(TestBonus b) ss ->
                    prop_inv_bonus b && (prop_pre_moveBonus b ss) 
                    ==> let b' = moveBonus b ss
                        in prop_inv_bonus b' && prop_post_moveBonus b ss
            )

insideScreenOrAboveBonusSpec :: Spec
insideScreenOrAboveBonusSpec = do
    describe "insideScreenOrAboveBonus (unit tests)" $ do
        it "bonus above screen is inside" $ do
            let b = startInitPlayerShootBonus 0 (topYScreenBound+100) ShootFaster
            insideScreenOrAboveBonus b `shouldBe` True
        it "bonus below screen is outside" $ do
            let b = startInitPlayerShootBonus 0 (bottomYScreenBound-100) ShootFaster
            insideScreenOrAboveBonus b `shouldBe` False

getTranslatedBonusAssetQuickCheckSpec :: Spec
getTranslatedBonusAssetQuickCheckSpec = do
    describe "getTranslatedBonusAsset (QuickCheck)" $ do
        it "satisfies getTranslatedBonusAsset post-condition for valid parameters" $
            property (\(TestGameAssets ga) (TestBonus b) ->
                    prop_inv_bonus b ==> prop_post_getTranslatedBonusAsset ga b
            )

-- ============================================================
-- ======================== LAWS ==============================
-- ============================================================

invariantLawsSpec :: Spec
invariantLawsSpec = do
    describe "Invariant laws (QuickCheck)" $ do
        it "law_invariant_stable for Bonus" $
            property (\(TestBonus b) ->
                    law_invariant_stable b
            )
        it "law_invariant_idempotent for Bonus" $
            property (\(TestBonus b) ->
                    law_invariant_idempotent b
            )

renderableLawSpec :: Spec
renderableLawSpec = do
    describe "Renderable laws (QuickCheck)" $ do
        it "law_renderable_finite for Bonus" $
            property (\(TestGameAssets ga) (TestBonus b) ->
                    law_renderable_finite ga b
            )

collidableLawsSpec :: Spec
collidableLawsSpec = do
    describe "Collidable laws (QuickCheck)" $ do
        it "law_collidable_reflexive for Bonus" $
            property (\(TestBonus b) ->
                prop_inv_bonus b ==> law_collidable_reflexive b
            )
        it "law_collidable_symmetric Bonus with another Bonus" $
            property (\(TestBonus b1) (TestBonus b2) ->
                prop_inv_bonus b1 && prop_inv_bonus b2
                ==> law_collidable_symmetric b1 b2
            )
        it "law_collidable_symmetric Bonus with another Object" $
            property (\(TestBonus b) (TestObject o) ->
                prop_inv_bonus b && prop_inv_object o
                ==> law_collidable_symmetric b o
            )
        it "law_collidable_will_collide Bonus with another Bonus" $
            property (\(TestBonus b1) (TestBonus b2) ->
                prop_inv_bonus b1 && prop_inv_bonus b2
                ==> law_collidable_will_collide b1 b2
            )
        it "law_collidable_will_collide Bonus with another Object" $
            property (\(TestBonus b) (TestObject o) ->
                prop_inv_bonus b && prop_inv_object o
                ==> law_collidable_will_collide b o
            )