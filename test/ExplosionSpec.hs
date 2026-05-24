{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ExplosionSpec (
    TestExplosion(..),
    spec
)
where

import Test.Hspec
import Test.QuickCheck

import GameSetup
import Graphics.Assets
import Graphics.Explosion
import Objects.Objects
import Objects.Hitbox
import Typeclasses.Invariant
import AssetsSpec(TestGameAssets(..))
import ObjectsSpec(TestObject(..))

spec :: Spec
spec = do
    initExplosionSpec
    startInitExplosionSpec
    runExplosionSpec
    runExplosionQuickCheckSpec
    getExplosionsSpec
    getExplosionsQuickCheckSpec
    getTranslatedExplosionAssetQuickCheckSpec
    invariantLawsSpec
    renderableLawSpec

newtype TestExplosion = TestExplosion { getExplosion :: Explosion } deriving (Eq, Show)
instance Arbitrary TestExplosion where
    arbitrary :: Gen TestExplosion
    arbitrary = do    
        x <- arbitrary
        y <- arbitrary
        frameCounter <- choose (1, nbFramesPerExplosionPhase)
        phase <- choose (0, (nbHitAssets-1))
        return $ TestExplosion (Explosion x y frameCounter phase)

prop_initExplosion_preservesInvariant :: XCoord -> YCoord -> Property
prop_initExplosion_preservesInvariant x y =
    forAll (choose (1, nbFramesPerExplosionPhase)) $ \cpt ->
    forAll (choose (0, nbHitAssets - 1)) $ \phase ->
        prop_inv_explosion (initExplosion x y cpt phase)

prop_startInitExplosion_preservesInvariant :: XCoord -> YCoord -> Property
prop_startInitExplosion_preservesInvariant x y = property $ prop_inv_explosion (startInitExplosion x y) 

initExplosionSpec :: SpecWith ()
initExplosionSpec = do
    describe "initExplosion (QuickCheck)" $ do
        it "preserves the Explosion invariant for valid Explosions" $
            property prop_initExplosion_preservesInvariant

startInitExplosionSpec :: SpecWith ()
startInitExplosionSpec = do
    describe "startInitExplosion (QuickCheck)" $ do
        it "preserves the Explosion invariant for start Explosions" $
            property prop_startInitExplosion_preservesInvariant

runExplosionSpec :: Spec
runExplosionSpec = do
    describe "runExplosion (unit tests)" $ do
        it "increments frame counter when not at max" $ do
            let e = initExplosion 10 20 1 0
            runExplosion e `shouldBe` Just (initExplosion 10 20 2 0)
        it "advances phase when frame counter reaches max" $ do
            let e = initExplosion 10 20 nbFramesPerExplosionPhase 0
            runExplosion e `shouldBe` Just (initExplosion 10 20 1 1)
        it "returns Nothing when last phase finishes" $ do
            let e = initExplosion 10 20 nbFramesPerExplosionPhase (nbHitAssets - 1)
            runExplosion e `shouldBe` Nothing

runExplosionQuickCheckSpec :: Spec
runExplosionQuickCheckSpec = do
    describe "runExplosion (QuickCheck)" $ do
        it "satisfies runExplosion post-condition for all valid Explosions" $
            property ( \(TestExplosion e) ->
                prop_inv_explosion e ==>
                    case runExplosion e of
                        Just e' -> prop_inv_explosion e'
                        Nothing -> True
            )

getExplosionsSpec :: Spec
getExplosionsSpec = do
    describe "getExplosions (unit tests)" $ do
        it "no disappearance -> no explosion" $ do
            let o = initStaticObject (initHitboxCircle 0 0 3)
            getExplosions [o] [o] `shouldBe` []
        it "one disappearance -> one explosion" $ do
            let o = initStaticObject (initHitboxCircle 0 0 3)
                res = getExplosions [o] []
            length res `shouldBe` 1
        it "explosion is centered on object" $ do
            let o = initStaticObject (initHitboxCircle 5 7 3)
                expls = getExplosions [o] []
                (Explosion x y _ _) = case expls of
                    [] -> error "list cannot be empty"
                    (e:_) -> e
            x `shouldBe` 5
            y `shouldBe` 7

getExplosionsQuickCheckSpec :: Spec
getExplosionsQuickCheckSpec = do
    describe "getExplosions (QuickCheck)" $ do
        it "satisfies getExplosions post-condition for valid Object lists" $
            property (
                \(beforeCols :: [TestObject]) -> -- first construct a list of random valid objects
                forAll (sublistOf beforeCols) $ \afterCols -> -- then construct a sub-list of that first list

                    let beforeCollisions = map getObject beforeCols -- extract the contained valid objects from the Arbitrary wrappers
                        afterCollisions  = map getObject afterCols

                        expls = getExplosions beforeCollisions afterCollisions
                    in
                        all prop_inv_object beforeCollisions &&
                        all prop_inv_object afterCollisions ==>
                            all prop_inv_explosion expls
                )

getTranslatedExplosionAssetQuickCheckSpec :: Spec
getTranslatedExplosionAssetQuickCheckSpec = do
    describe "getTranslatedExplosionAsset (QuickCheck)" $ do
        it "satisfies getTranslatedExplosionAsset post-condition for all valid parameters" $
            property (\(TestGameAssets ga) (TestExplosion expl) -> 
                prop_inv_explosion expl ==> prop_post_getTranslatedExplosionAsset ga expl
                )

-- ============================================================
-- ======================== LAWS ==============================
-- ============================================================

invariantLawsSpec :: Spec
invariantLawsSpec = do
    describe "Invariant laws (QuickCheck)" $ do

        it "law_invariant_stable for Explosion" $
            property (
                \(TestExplosion expl) ->
                    law_invariant_stable expl
            )

        it "law_invariant_idempotent for Explosion" $
            property (
                \(TestExplosion expl) ->
                    law_invariant_idempotent expl
            )

renderableLawSpec :: Spec
renderableLawSpec = do
    describe "Renderable laws (QuickCheck)" $ do

        it "law_renderable_finite for Explosion" $
            property (\(TestGameAssets ga) (TestExplosion expl) ->
                law_renderable_finite ga expl
            )