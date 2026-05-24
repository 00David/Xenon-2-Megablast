{-# LANGUAGE InstanceSigs #-}
module AssetsSpec (
    TestGameAssets(..),
    TestHealthBarAssets(..),
    TestDigitAssets(..),
    spec
)
where

import Graphics.Gloss (Picture (Blank), Picture(Circle), rectangleSolid)

import qualified Data.Sequence as Seq

import Test.Hspec
import Test.QuickCheck

import GameSetup
import Graphics.Assets

-- Only the most 'interesting' Assets functions are tested here (thus not IO import asset functions)

spec :: Spec
spec = do
    completeScoreStringSpec
    completeScoreStringQuicCheckSpec
    scoreToStringSpec
    scoreToStringQuickCheckSpec
    getTranslatedScoreAssetsQuickCheckSpec
    getTranslatedHealthAssetsQuicCheckSpec

-- ============================================================
-- ============= QUICKCHECK AUTOMATED TEST MOCKS ==============
-- ============================================================

genPicture :: Gen Picture
genPicture = elements[Blank, Circle 5 , rectangleSolid 10 10] -- Random Picture

newtype TestGameAssets = TestGameAssets { getGameAssets :: GameAssets } deriving (Show)
instance Arbitrary TestGameAssets where
    arbitrary :: Gen TestGameAssets
    arbitrary = do

        (TestDigitAssets digitAssets) <- arbitrary
        (TestHealthBarAssets hb1) <- arbitrary
        (TestHealthBarAssets hb2) <- arbitrary

        ga <- GameAssets
            <$> genPicture -- p1Pic
            <*> genPicture -- p2Pic
            <*> (Seq.fromList <$> vectorOf nbPlayerExplosionAssets genPicture) -- p1ExplosionPics
            <*> (Seq.fromList <$> vectorOf nbPlayerExplosionAssets genPicture) -- p2ExplosionPics
            <*> genPicture -- pDamagedPic
            <*> genPicture -- pInvinciblePic
            <*> (Seq.fromList <$> vectorOf 4 genPicture) -- pBoosterPics
            <*> (Seq.fromList <$> vectorOf nbEnemiesAssets genPicture) -- enemiesPics
            <*> genPicture -- bottomLeftPic
            <*> genPicture -- bottomBarPic
            <*> genPicture -- bottomRightPic
            <*> pure digitAssets -- digitPics
            <*> pure hb1 -- p1HealthBarPics
            <*> pure hb2 -- p2HealthBarPics
            <*> (Seq.fromList <$> vectorOf nbRockAssets genPicture) -- leftWallPics
            <*> (Seq.fromList <$> vectorOf nbRockAssets genPicture) -- rightWallPics
            <*> (Seq.fromList <$> vectorOf nbPlayerShotAssets genPicture) -- player1ShotPics
            <*> (Seq.fromList <$> vectorOf nbPlayerShotAssets genPicture) -- player2ShotPics
            <*> (Seq.fromList <$> vectorOf nbEnemyShotAssets genPicture) -- enemyShotPics
            <*> (Seq.fromList <$> vectorOf nbHitAssets genPicture) -- hitPics
            <*> (Seq.fromList <$> vectorOf nbPlayerBonusAssets genPicture) -- playerShootBonusPics
        return (TestGameAssets ga)

newtype TestHealthBarAssets = TestHealthBarAssets { getHealthBarAssets :: HealtBarAssets } deriving (Show)
instance Arbitrary TestHealthBarAssets where
    arbitrary :: Gen TestHealthBarAssets
    arbitrary = do
        pics <- vectorOf 11 genPicture
        return $ TestHealthBarAssets $ HealtBarAssets (Seq.fromList pics)

newtype TestDigitAssets = TestDigitAssets { getDigitAssets :: DigitAssets } deriving (Show)
instance Arbitrary TestDigitAssets where
    arbitrary :: Gen TestDigitAssets
    arbitrary = do
        digitPicsList <- vectorOf 10 genPicture

        pPic <- genPicture
        d1Black <- genPicture
        d2Black <- genPicture
        pBlack <- genPicture

        return $ TestDigitAssets $ DigitAssets { 
            digits = Seq.fromList digitPicsList
            , letterPPic = pPic
            , digit1BlackPic = d1Black
            , digit2BlackPic = d2Black
            , letterPBlackPic = pBlack
            }

-- ============================================================
-- ======================= BOTTOM BAR =========================
-- ============================================================

completeScoreStringSpec :: Spec
completeScoreStringSpec = do
    describe "completeScoreString (unit tests)" $ do
        it "\"0123\" completed by 3 '0's at the beginning" $ do
            completeScoreString "0123" `shouldBe` "0000123"

        it "\"3210\" completed by 3 '0's at the beginning" $ do
            completeScoreString "3210" `shouldBe` "0003210"

        it "\"1234567\" not completed" $ do
            completeScoreString "1234567" `shouldBe` "1234567"

        it "\"0\" completed by 6 '0's at the beginning" $ do
            completeScoreString "0" `shouldBe` "0000000"

-- Randomly generates a string of 'len' digits (at most 7 digits)
genValidScoreString :: Gen String
genValidScoreString = do
    len <- choose (0,7)
    vectorOf len (elements ['0'..'9'])

completeScoreStringQuicCheckSpec :: Spec
completeScoreStringQuicCheckSpec = do
    describe "completeScoreString (QuickCheck)" $ do
        it "satisfies completeScoreString post-condition for all valid Strings" $
            property (forAll genValidScoreString (\s ->
                    prop_post_completeScoreString s
                    ))

scoreToStringSpec :: Spec
scoreToStringSpec = do
    describe "scoreToString (unit tests)" $ do
        it "0123 completed by 3 '0's at the beginning, and converted to a String" $ do
            scoreToString 0123 `shouldBe` "0000123"

        it "3210 completed by 3 '0's at the beginning, and converted to a String" $ do
            scoreToString 3210 `shouldBe` "0003210"

        it "1234567 not completed, and converted to a String" $ do
            scoreToString 1234567 `shouldBe` "1234567"

        it "0 completed by 6 '0's at the beginning, and converted to a String" $ do
            scoreToString 0 `shouldBe` "0000000"

        it "9999999999 capped at \"9999999\" (7 digits)" $ do
            scoreToString 9999999999 `shouldBe` "9999999"

scoreToStringQuickCheckSpec :: Spec
scoreToStringQuickCheckSpec = do
    describe "scoreToString (QuickCheck)" $ do
        it "satisfies scoreToString post-condition for all valid scores (positive)" $
            property (\score -> prop_post_scoreToString (abs score))

getTranslatedScoreAssetsQuickCheckSpec :: Spec
getTranslatedScoreAssetsQuickCheckSpec = do
    describe "getTranslatedScoreAssets (QuickCheck)" $ do
        it "satisfies getTranslatedScoreAssets post-condition for all valid parameters" $
            property (\b score (TestGameAssets ga) -> prop_post_getTranslatedScoreAssets ga b (abs score))

-- ============================================================
-- ======================= HEALTH BAR =========================
-- ============================================================

-- Randomly generates a valid health, part of [0, 100]
genValidHealth :: Gen Int
genValidHealth = do
    health <- choose (0,100)
    return health

getTranslatedHealthAssetsQuicCheckSpec :: Spec
getTranslatedHealthAssetsQuicCheckSpec = do
    describe "getTranslatedHealthAssets (QuickCheck)" $ do
        it "satisfies getTranslatedHealthAssets post-condition for all valid player healths" $
            property (
                forAll genValidHealth (\player1Health ->
                forAll genValidHealth (\player2Health ->
                \(TestGameAssets ga) ->
                    prop_pre_getTranslatedHealthAssets ga player1Health player2Health
                    ==> prop_post_getTranslatedHealthAssets ga player1Health player2Health
                ))
            )