module AssetsSpec (
    TestGameAssets(..),
    spec
)
where

import Graphics.Gloss (Picture (Blank), Picture(Circle), rectangleSolid)

import qualified Data.Sequence as Seq

import Test.Hspec
import Test.QuickCheck

import Graphics.Assets
import PlayerSpec(TestPlayer(..))
import GameState.Player

-- Only the most 'interesting' Assets functions are tested here (thus not IO functions)

spec :: Spec
spec = do
    completeScoreStringSpec
    completeScoreStringQuicCheckSpec
    scoreToStringSpec
    scoreToStringQuickCheckSpec
    getTranslatedScoreAssetsQuickCheckSpec
    getTranslatedHealthAssetsQuicCheckSpec
    getTranslatedBoosterAssetsQuickCheckSpec

-- ============================================================
-- ============= QUICKCHECK AUTOMATED TEST MOCKS ==============
-- ============================================================

genPicture :: Gen Picture
genPicture = elements[Blank, Circle 5 , rectangleSolid 10 10]

newtype TestGameAssets = TestGameAssets { getGameAssets :: GameAssets } deriving (Show)
instance Arbitrary TestGameAssets where
    arbitrary = do
        p1 <- genPicture
        p2 <- genPicture
        virus <- genPicture

        boosters <- vectorOf 4 genPicture

        digitAssets <- getDigitAssets <$> arbitrary
        hb1 <- getHealthBarAssets <$> arbitrary
        hb2 <- getHealthBarAssets <$> arbitrary

        leftWalls <- vectorOf 4 genPicture
        rightWalls <- vectorOf 4 genPicture

        return $ TestGameAssets $ GameAssets { 
            p1Pic = p1
            , p2Pic = p2
            , pBoosterPics = boosters
            , virusPic = virus
            , bottomLeftPic = Blank
            , bottomBarPic = Blank
            , bottomRightPic = Blank
            , digitPics = digitAssets
            , p1HealthBarPics = hb1
            , p2HealthBarPics = hb2
            , leftWallPics = leftWalls
            , rightWallPics = rightWalls
        }

newtype TestHealthBarAssets = TestHealthBarAssets { getHealthBarAssets :: HealtBarAssets } deriving (Show)
instance Arbitrary TestHealthBarAssets where
    arbitrary = do
        pics <- vectorOf 11 genPicture
        return $ TestHealthBarAssets $ HealtBarAssets (Seq.fromList pics)

newtype TestDigitAssets = TestDigitAssets { getDigitAssets :: DigitAssets } deriving (Show)
instance Arbitrary TestDigitAssets where
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
        it "satisfies scoreToString post-condition for all Strings" $
            property (\s -> prop_post_scoreToString s)

getTranslatedScoreAssetsQuickCheckSpec :: Spec
getTranslatedScoreAssetsQuickCheckSpec = do
    describe "getTranslatedScoreAssets (QuickCheck)" $ do
        it "satisfies getTranslatedScoreAssets post-condition for all parameters" $
            property (\b score (TestGameAssets ga) -> prop_post_getTranslatedScoreAssets b score ga)

-- ============================================================
-- ==================== HEALTH BAR ASSETS =====================
-- ============================================================

getTranslatedHealthAssetsQuicCheckSpec :: Spec
getTranslatedHealthAssetsQuicCheckSpec = do
    describe "getTranslatedHealthAssets (QuickCheck)" $ do
        it "satisfies getTranslatedHealthAssets post-condition for all possible player healths" $
            property (\player1Health player2Health (TestGameAssets ga) -> 
                prop_pre_getTranslatedHealthAssets player1Health player2Health ga
                ==> prop_post_getTranslatedHealthAssets player1Health player2Health ga)

-- ============================================================
-- ====================== BOOSTER ASSETS ======================
-- ============================================================

getTranslatedBoosterAssetsQuickCheckSpec :: Spec
getTranslatedBoosterAssetsQuickCheckSpec = do
    describe "getTranslatedBoosterAssets (QuickCheck)" $ do
        it "satisfies getTranslatedBoosterAssets post-condition for all valid players" $
            property (\(TestPlayer player) (TestGameAssets ga) -> 
                prop_inv_player player
                ==> prop_post_getTranslatedBoosterAssets player ga
                )