{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ProjectileSpec (
    TestProjectile(..),
    spec
)
where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Sequence as Seq

import GameSetup
import GameState.Projectile
import Graphics.Assets
import Objects.Objects
import Objects.Hitbox
import Typeclasses.Invariant
import AssetsSpec(TestGameAssets(..))
import HitboxSpec(TestHitbox(..))
import ObjectsSpec(TestObject(..), TestDirection(..), TestObjectSpeed(..))

spec :: Spec
spec = do
    initPlayerShotSpec
    startInitPlayerShotSpec
    initEnemyShotSpec
    startInitEnemyShotSpec
    projectileObjectSpec
    projectileAssetSpec
    projectileDamageSpec
    projectilePlayerIdSpec
    isPlayerShotSpec
    moveProjectileSpec
    moveProjectileQuickCheckSpec
    insideScreenProjectileSpec
    getTranslatedProjectileAssetQuickCheckSpec
    invariantLawsSpec
    renderableLawSpec
    collidableLawsSpec

-- Initializes Projectiles veryfing their invariant
newtype TestProjectile = TestProjectile { getProjectile :: Projectile }deriving (Eq, Show)
instance Arbitrary TestProjectile where
    arbitrary :: Gen TestProjectile
    arbitrary = do
        (TestHitbox h) <- arbitrary
        (TestDirection d) <- arbitrary
        (TestObjectSpeed os) <- arbitrary

        assetPlayerShot <- choose (0, nbPlayerShotAssets - 1)
        playerId <- choose (1, 2)

        assetEnemyShot  <- choose (0, nbEnemyShotAssets - 1)
        
        dmg <- getPositive <$> arbitrary

        shot <- oneof
            [ return (PlayerShot (initMovableObject h d os) assetPlayerShot dmg playerId)
            , return (EnemyShot (initMovableObject h d os) assetEnemyShot dmg)
            ]
        return (TestProjectile shot)

prop_initPlayerShot_preservesInvariant :: TestObject -> Damage -> Property
prop_initPlayerShot_preservesInvariant (TestObject obj) d =
    forAll (choose (0, (nbPlayerShotAssets-1))) $ \asset ->
    forAll (choose (1, 2)) $ \pId ->
        isMovable obj && d >= 1 ==> prop_inv_projectile (initPlayerShot obj asset d pId)

prop_startInitPlayerShot_preservesInvariant :: XCoord -> YCoord -> TestObjectSpeed -> Damage -> Property
prop_startInitPlayerShot_preservesInvariant x y (TestObjectSpeed os) d =
    forAll (choose (0, (nbPlayerShotAssets-1))) $ \asset ->
    forAll (choose (1, 2)) $ \pId ->
        d >= 1 ==> prop_inv_projectile (startInitPlayerShot x y os asset d pId)

prop_initEnemyShot_preservesInvariant :: TestObject -> Damage -> Property
prop_initEnemyShot_preservesInvariant (TestObject obj) d =
    forAll (choose (0, (nbEnemyShotAssets-1))) $ \asset ->
        isMovable obj && d >= 1 ==> prop_inv_projectile (initEnemyShot obj asset d)

prop_startInitEnemyShot_preservesInvariant :: XCoord -> YCoord -> TestObjectSpeed -> Damage -> Property
prop_startInitEnemyShot_preservesInvariant x y (TestObjectSpeed os) d =
    forAll (choose (0, (nbEnemyShotAssets-1))) $ \asset ->
        d >= 1 ==> prop_inv_projectile (startInitEnemyShot x y os asset d)

initPlayerShotSpec :: SpecWith ()
initPlayerShotSpec = do
    describe "initPlayerShot (QuickCheck)" $ do
        it "preserves the Projectile invariant for valid player shots" $
            property prop_initPlayerShot_preservesInvariant

startInitPlayerShotSpec :: SpecWith ()
startInitPlayerShotSpec = do
    describe "startInitPlayerShot (QuickCheck)" $ do
        it "preserves the Projectile invariant for start player shots" $
            property prop_startInitPlayerShot_preservesInvariant

initEnemyShotSpec :: SpecWith ()
initEnemyShotSpec = do
    describe "initEnemyShot (QuickCheck)" $ do
        it "preserves the Projectile invariant for valid enemy shots" $
            property prop_initEnemyShot_preservesInvariant

startInitEnemyShotSpec :: SpecWith ()
startInitEnemyShotSpec = do
    describe "startInitEnemyShot (QuickCheck)" $ do
        it "preserves the Projectile invariant for start enemy shots" $
            property prop_startInitEnemyShot_preservesInvariant

projectileObjectSpec :: Spec
projectileObjectSpec = do
    describe "projectileObject (unit tests)" $ do
        it "returns correct Object for player shot" $ do
            let asset = 0
                pId = 1
                d = 1
                h = (initHitboxCircle 0 0 (((Seq.index widthEnemyShotAssets asset) + (Seq.index heightEnemyShotAssets asset)) / 4.0))
                projO = (initMovableObject h (initDirection 0 1) (initObjectSpeed 1))
                ps  = (initPlayerShot projO asset d pId)
            projectileObject ps `shouldBe` projO
        it "returns correct Object for enemy shot" $ do
            let asset = 0
                d = 10
                h = (initHitboxCircle 0 0 (((Seq.index widthEnemyShotAssets asset) + (Seq.index heightEnemyShotAssets asset)) / 4.0))
                projO = (initMovableObject h (initDirection 0 (-1)) (initObjectSpeed 1))
                es  = (initEnemyShot projO asset d)
            projectileObject es `shouldBe` projO

projectileAssetSpec :: Spec
projectileAssetSpec = do
    describe "projectileAsset (unit tests)" $ do
        it "returns correct asset for player shot" $ do
            let asset = 1
                pId = 1
                d = 1
                h = (initHitboxCircle 0 0 (((Seq.index widthEnemyShotAssets asset) + (Seq.index heightEnemyShotAssets asset)) / 4.0))
                projO = (initMovableObject h (initDirection 0 1) (initObjectSpeed 1))
                ps  = (initPlayerShot projO asset d pId)
            projectileAsset ps `shouldBe` asset
        it "returns correct asset for enemy shot" $ do
            let asset = 0
                d = 10
                h = (initHitboxCircle 0 0 (((Seq.index widthEnemyShotAssets asset) + (Seq.index heightEnemyShotAssets asset)) / 4.0))
                projO = (initMovableObject h (initDirection 0 (-1)) (initObjectSpeed 1))
                es  = (initEnemyShot projO asset d)
            projectileAsset es `shouldBe` asset

projectileDamageSpec :: Spec
projectileDamageSpec = do
    describe "projectileDamage (unit tests)" $ do
        it "returns correct damage for player shot" $ do
            let asset = 1
                pId = 1
                d = 1
                h = (initHitboxCircle 0 0 (((Seq.index widthEnemyShotAssets asset) + (Seq.index heightEnemyShotAssets asset)) / 4.0))
                projO = (initMovableObject h (initDirection 0 1) (initObjectSpeed 1))
                ps  = (initPlayerShot projO asset d pId)
            projectileDamage ps `shouldBe` d
        it "returns correct damage for enemy shot" $ do
            let asset = 0
                d = 10
                h = (initHitboxCircle 0 0 (((Seq.index widthEnemyShotAssets asset) + (Seq.index heightEnemyShotAssets asset)) / 4.0))
                projO = (initMovableObject h (initDirection 0 (-1)) (initObjectSpeed 1))
                es  = (initEnemyShot projO asset d)
            projectileDamage es `shouldBe` d

projectilePlayerIdSpec :: Spec
projectilePlayerIdSpec = do
    describe "projectilePlayerId (unit tests)" $ do
        it "returns correct player id for player shot" $ do
            let asset = 1
                pId = 1
                d = 1
                h = (initHitboxCircle 0 0 (((Seq.index widthEnemyShotAssets asset) + (Seq.index heightEnemyShotAssets asset)) / 4.0))
                projO = (initMovableObject h (initDirection 0 1) (initObjectSpeed 1))
                ps  = (initPlayerShot projO asset d pId)
            projectilePlayerId ps `shouldBe` pId
        it "returns a default player id of 0 for enemy shot" $ do
            let asset = 0
                d = 10
                h = (initHitboxCircle 0 0 (((Seq.index widthEnemyShotAssets asset) + (Seq.index heightEnemyShotAssets asset)) / 4.0))
                projO = (initMovableObject h (initDirection 0 (-1)) (initObjectSpeed 1))
                es  = (initEnemyShot projO asset d)
            projectilePlayerId es `shouldBe` 0

isPlayerShotSpec :: Spec
isPlayerShotSpec = do
    describe "isPlayerShot (unit tests)" $ do
        it "returns True for a player1 shot" $ do
            let asset = 0
                pId = 1
                d = 1
                ps  = (startInitPlayerShot 0 0 (initObjectSpeed 1) asset d pId)
            isPlayerShot ps `shouldBe` True
        it "returns True for a player2 shot" $ do
            let asset = 0
                pId = 2
                d = 1
                ps  = (startInitPlayerShot 0 0 (initObjectSpeed 1) asset d pId)
            isPlayerShot ps `shouldBe` True
        it "returns False for an enemy shot" $ do
            let asset = 0
                d = 10
                es  = (startInitEnemyShot 0 0 (initObjectSpeed 1) asset d)
            isPlayerShot es `shouldBe` False

moveProjectileSpec :: Spec
moveProjectileSpec = do
    describe "moveProjectile (unit tests)" $ do
        it "moves a player projectile towards the top, according to its speed of 8" $ do
            let asset = 0
                pId = 1
                d = 10
                ps  = (startInitPlayerShot 0 0 (initObjectSpeed 8) asset d pId)

                ps' = moveProjectile ps screenDefaultSpeed
                obj' = projectileObject ps'
            centerHitbox (objectHitbox obj') `shouldBe` (0, 8)
        it "moves an enemy projectile towards the bottom, according to its speed of 2" $ do
            let asset = 0
                d = 10
                es  = (startInitEnemyShot 0 0 (initObjectSpeed 2) asset d)

                es' = moveProjectile es screenDefaultSpeed
                obj' = projectileObject es'
            centerHitbox (objectHitbox obj') `shouldBe` (0, (-2))

moveProjectileQuickCheckSpec :: Spec
moveProjectileQuickCheckSpec = do
    describe "moveProjectile (QuickCheck)" $ do
        it "satisfies moveProjectile post-condition for all valid parameters" $
            property ( \(TestProjectile proj) ss ->
                (prop_inv_projectile proj && (prop_pre_moveProjectile proj ss)) 
                ==> let proj' = moveProjectile proj ss
                    in prop_inv_projectile proj' && (prop_post_moveProjectile proj ss)
            )

insideScreenProjectileSpec :: Spec
insideScreenProjectileSpec = do
    describe "insideScreenProjectile (inside for enemy shots above screen) (unit tests)" $ do
        it "player projectile above the screen is outside" $ do
            let asset = 0
                pId = 1
                d = 1
                ps = (startInitPlayerShot 0 (topYScreenBound + 500) (initObjectSpeed 1) asset d pId)
            insideScreenProjectile ps `shouldBe` False
        it "player projectile at the center of the screen is inside" $ do
            let asset = 0
                pId = 1
                d = 1
                ps = startInitPlayerShot 0 0 (initObjectSpeed 1) asset d pId
            insideScreenProjectile ps `shouldBe` True
        it "enemy projectile above the screen is inside" $ do
            let asset = 0
                d = 10
                es = startInitEnemyShot 0 (bottomYScreenBound + 500) (initObjectSpeed 1) asset d
            insideScreenProjectile es `shouldBe` True
        it "enemy projectile below the screen is outside" $ do
            let asset = 0
                d = 10
                es = startInitEnemyShot 0 (bottomYScreenBound - 500) (initObjectSpeed 1) asset d
            insideScreenProjectile es `shouldBe` False
        it "enemy projectile at the center of the screen is inside" $ do
            let asset = 0
                d = 10
                es = startInitEnemyShot 0 0 (initObjectSpeed 1) asset d
            insideScreenProjectile es `shouldBe` True

getTranslatedProjectileAssetQuickCheckSpec :: Spec
getTranslatedProjectileAssetQuickCheckSpec = do
    describe "getTranslatedProjectileAsset (QuickCheck)" $ do
        it "satisfies getTranslatedProjectileAsset post-condition for all valid parameters" $
            property (\(TestGameAssets ga) (TestProjectile proj) -> 
                prop_inv_projectile proj ==> prop_post_getTranslatedProjectileAsset ga proj
                )

-- ============================================================
-- ======================== LAWS ==============================
-- ============================================================

invariantLawsSpec :: Spec
invariantLawsSpec = do
    describe "Invariant laws (QuickCheck)" $ do
        it "law_invariant_stable for Projectile" $
            property (
                \(TestProjectile proj) -> law_invariant_stable proj
            )
        it "law_invariant_idempotent for Projectile" $
            property (
                \(TestProjectile proj) -> law_invariant_idempotent proj
            )

renderableLawSpec :: Spec
renderableLawSpec = do
    describe "Renderable laws (QuickCheck)" $ do
        it "law_renderable_finite for Projectile" $
            property (\(TestGameAssets ga) (TestProjectile proj) ->
                law_renderable_finite ga proj
            )

collidableLawsSpec :: Spec
collidableLawsSpec = do
    describe "Collidable laws (QuickCheck)" $ do
        it "law_collidable_reflexive for Projectile" $
            property (\(TestProjectile proj) ->
                prop_inv_projectile proj ==> law_collidable_reflexive proj
            )
        it "law_collidable_symmetric for Projectile with another Projectile" $
            property (\(TestProjectile proj1) (TestProjectile proj2) ->
                prop_inv_projectile proj1 && prop_inv_projectile proj2 
                ==> law_collidable_symmetric proj1 proj2
            )
        it "law_collidable_symmetric for Projectile with another Object" $
            property (\(TestProjectile proj1) (TestObject o2) ->
                prop_inv_projectile proj1 && prop_inv_object o2 
                ==> law_collidable_symmetric proj1 o2
            )
        it "law_collidable_will_collide for Projectile with another Projectile" $
            property (\(TestProjectile proj1) (TestProjectile proj2) ->
                prop_inv_projectile proj1 && prop_inv_projectile proj2 
                ==> law_collidable_will_collide proj1 proj2
            )
        it "law_collidable_will_collide for Projectile with another Object" $
            property (\(TestProjectile proj1) (TestObject o2) ->
                prop_inv_projectile proj1 && prop_inv_object o2 
                ==> law_collidable_will_collide proj1 o2
            )