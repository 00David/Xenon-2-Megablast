{-# LANGUAGE InstanceSigs #-}
module BackgroundSpec (
    TestBackground(..),
    spec
)
where

import Graphics.Gloss (Picture (Blank))

import Test.Hspec
import Test.QuickCheck

import GameSetup
import Graphics.Assets
import Graphics.Background
import Typeclasses.Invariant
import AssetsSpec(TestGameAssets(..))

spec :: Spec
spec = do
    initBackgroundSpec
    updateBackgroundSpec
    updateBackgroundQuickCheckSpec
    getTranslatedBackgroundsQuickCheckSpec
    invariantLawsSpec
    renderableLawSpec

-- Initializes Backgrounds veryfing their invariant
newtype TestBackground = TestBackground { getBackground :: Background } deriving (Eq, Show)
instance Arbitrary TestBackground where
    arbitrary :: Gen TestBackground
    arbitrary = do    
        scrollingSpeed <- arbitrary
        y <- choose (0, heightBackgroundPicture-1)
        return $ TestBackground (Background Blank  scrollingSpeed y)

prop_initBackground_preservesInvariant :: Float -> YCoord -> Property
prop_initBackground_preservesInvariant scrollingSpeed y =
    y >= 0 && y < heightBackgroundPicture ==> prop_inv_background (initBackground Blank scrollingSpeed y) 

initBackgroundSpec :: SpecWith ()
initBackgroundSpec = do
    describe "initBackground (QuickCheck)" $ do
        it "preserves the Background invariant for valid Backgrounds" $
            property prop_initBackground_preservesInvariant

updateBackgroundSpec :: SpecWith ()
updateBackgroundSpec = do
    describe "updateBackground (unit tests)" $ do

        it "does not change background when dt = 0" $ do
            let bg = Background Blank 5 10
            updateBackground 0 bg `shouldBe` bg

        it "moves background upward when speed > 0" $ do
            let bg = Background Blank 10 50
                (Background _ _ y') = updateBackground 1 bg
            y' `shouldBe` (50 - 10)

        it "moves background less when dt is smaller" $ do
            let bg = Background Blank 10 50
                (Background _ _ y1) = updateBackground 1 bg
                (Background _ _ y2) = updateBackground 0.5 bg
            (50 - y1) `shouldBe` 10
            (50 - y2) `shouldBe` 5

updateBackgroundQuickCheckSpec :: Spec
updateBackgroundQuickCheckSpec = do
    describe "updateBackground (QuickCheck)" $ do
        it "satisfies updateBackground post-condition for all valid parameters" $
            property (\dt (TestBackground bgnd) -> 
                prop_inv_background bgnd && prop_pre_updateBackground dt bgnd
                ==> let bgndPost = updateBackground dt bgnd
                in prop_inv_background bgndPost && prop_post_updateBackground dt bgnd
                )

getTranslatedBackgroundsQuickCheckSpec :: Spec
getTranslatedBackgroundsQuickCheckSpec = do
    describe "getTranslatedBackgrounds (QuickCheck)" $ do
        it "always returns 3 pictures for valid backgrounds" $
            property (\(TestBackground bg) -> prop_post_getTranslatedBackgrounds bg)

-- ============================================================
-- ======================== LAWS ==============================
-- ============================================================

invariantLawsSpec :: Spec
invariantLawsSpec = do
    describe "Invariant laws (QuickCheck)" $ do
        it "law_invariant_stable for Background" $
            property (
                \(TestBackground bgnd) -> law_invariant_stable bgnd
            )

        it "law_invariant_idempotent for Background" $
            property (
                \(TestBackground bgnd) -> law_invariant_idempotent bgnd
            )

renderableLawSpec :: Spec
renderableLawSpec = do
    describe "Renderable laws (QuickCheck)" $ do
        it "law_renderable_finite for Background" $
            property (\(TestGameAssets ga) (TestBackground bg) -> law_renderable_finite ga bg
            )