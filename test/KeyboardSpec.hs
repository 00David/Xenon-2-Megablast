{-# LANGUAGE InstanceSigs #-}
module KeyboardSpec (
    TestKeyboard(..),
    TestKey(..),
    spec
)
where

import Test.Hspec
import Test.QuickCheck

import Graphics.Gloss.Interface.IO.Interact

import Data.Set

import GameSetup
import Keyboard
import Objects.Objects

spec :: Spec
spec = do
    player1NewDirectionSpeedSpec
    player1NewDirectionSpeedQuickCheckSpec
    player2NewDirectionSpeedSpec
    player2NewDirectionSpeedQuickCheckSpec

newtype TestKey = TestKey { getKey :: Key } deriving (Eq, Show)
instance Arbitrary TestKey where
    arbitrary :: Gen TestKey
    arbitrary = do
        key <- oneof 
            [ SpecialKey <$> elements [KeyLeft, KeyRight, KeyUp, KeyDown, KeyBackspace, KeyDelete, KeyEnter, KeyEsc, KeySpace]
            , Char <$> elements ['a'..'z']]
        return $ TestKey key

newtype TestKeyboard = TestKeyboard { getKeyboard :: Keyboard } deriving (Eq, Show)
instance Arbitrary TestKeyboard where
    arbitrary :: Gen TestKeyboard
    arbitrary = do
        keys <- listOf arbitrary
        return $ TestKeyboard (fromList (Prelude.map getKey keys))

player1NewDirectionSpeedSpec :: Spec
player1NewDirectionSpeedSpec = do
  describe "player1NewDirectionSpeed (unit tests)" $ do

    it "no key pressed -> no movement" $ do
        let kbd = initKeyboard
            (dir, os) = player1NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection 0 0
        os  `shouldBe` initObjectSpeed 0

    it "left key (q) -> move left" $ do
        let kbd = insert (Char 'q') initKeyboard
            (dir, os) = player1NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection (-1) 0
        os  `shouldBe` initObjectSpeed playerDefaultSpeed

    it "left key (a) -> move left (AZERTY alternative)" $ do
        let kbd = insert (Char 'a') initKeyboard
            (dir, os) = player1NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection (-1) 0
        os  `shouldBe` initObjectSpeed playerDefaultSpeed

    it "right key -> move right" $ do
        let kbd = insert (Char 'd') initKeyboard
            (dir, os) = player1NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection 1 0
        os  `shouldBe` initObjectSpeed playerDefaultSpeed

    it "up key (z) -> move up" $ do
        let kbd = insert (Char 'z') initKeyboard
            (dir, os) = player1NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection 0 1
        os  `shouldBe` initObjectSpeed playerDefaultSpeed

    it "up key (w) -> move up (QWERTY)" $ do
        let kbd = insert (Char 'w') initKeyboard
            (dir, os) = player1NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection 0 1
        os  `shouldBe` initObjectSpeed playerDefaultSpeed

    it "down key (s) -> move down" $ do
        let kbd = insert (Char 's') initKeyboard
            (dir, os) = player1NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection 0 (-1)
        os  `shouldBe` initObjectSpeed playerDefaultSpeed

    it "opposite keys cancel (q + d)" $ do
        let kbd = insert (Char 'q')
                $ insert (Char 'd')
                $ initKeyboard
            (dir, os) = player1NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection 0 0
        os  `shouldBe` initObjectSpeed 0

player1NewDirectionSpeedQuickCheckSpec :: Spec
player1NewDirectionSpeedQuickCheckSpec = do
    describe "player1NewDirectionSpeed (QuickCheck)" $ do
        it "satisfies player1NewDirectionSpeed post-condition for all Keyboards and positive delta times" $
            property (\(TestKeyboard kbd) dt ->
                prop_pre_playerNewDirectionSpeed kbd dt
                ==> let (dir, os) = player1NewDirectionSpeed kbd dt
                    in prop_inv_direction dir && prop_inv_objectSpeed os)

player2NewDirectionSpeedSpec :: Spec
player2NewDirectionSpeedSpec = do
  describe "player2NewDirectionSpeed (unit tests)" $ do

    it "no key pressed -> no movement" $ do
        let kbd = initKeyboard
            (dir, os) = player2NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection 0 0
        os  `shouldBe` initObjectSpeed 0

    it "left key -> move left" $ do
        let kbd = insert (SpecialKey KeyLeft) initKeyboard
            (dir, os) = player2NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection (-1) 0
        os  `shouldBe` initObjectSpeed playerDefaultSpeed

    it "right key -> move right" $ do
        let kbd = insert (SpecialKey KeyRight) initKeyboard
            (dir, os) = player2NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection 1 0
        os  `shouldBe` initObjectSpeed playerDefaultSpeed

    it "up key -> move up" $ do
        let kbd = insert (SpecialKey KeyUp) initKeyboard
            (dir, os) = player2NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection 0 1
        os  `shouldBe` initObjectSpeed playerDefaultSpeed

    it "down key -> move down" $ do
        let kbd = insert (SpecialKey KeyDown) initKeyboard
            (dir, os) = player2NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection 0 (-1)
        os  `shouldBe` initObjectSpeed playerDefaultSpeed

    it "opposite keys cancel (left + right)" $ do
        let kbd = insert (SpecialKey KeyLeft)
                $ insert (SpecialKey KeyRight)
                $ initKeyboard
            (dir, os) = player2NewDirectionSpeed kbd 1.0
        dir `shouldBe` initDirection 0 0
        os  `shouldBe` initObjectSpeed 0

player2NewDirectionSpeedQuickCheckSpec :: Spec
player2NewDirectionSpeedQuickCheckSpec = do
    describe "player2NewDirectionSpeed (QuickCheck)" $ do
        it "satisfies player2NewDirectionSpeed post-condition for all Keyboards and positive delta times" $
            property (\(TestKeyboard kbd) dt ->
                prop_pre_playerNewDirectionSpeed kbd dt
                ==> let (dir, os) = player2NewDirectionSpeed kbd dt
                    in prop_inv_direction dir && prop_inv_objectSpeed os)