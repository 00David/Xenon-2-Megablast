{-# LANGUAGE InstanceSigs #-}
module PlayerSpec (
    TestPlayer(..),
    spec
)
where

import Graphics.Gloss ( Picture(Blank) )

import Test.Hspec
import Test.QuickCheck

import GameState.Player
import Objects.Objects
import ObjectsSpec(TestObject(..))

spec :: Spec
spec = do
    initAlivePlayerSpec
    initDeadPlayerSpec
    playerObjectSpec
    playerIdSpec
    playerLifesSpec
    playerHealthSpec
    playerScoreSpec
    playerExplAnimationSpec
    isPlayerDeadSpec

newtype TestPlayer = TestPlayer { getPlayer :: Player } deriving (Eq, Show)
instance Arbitrary TestPlayer where
    arbitrary = oneof [
        do --Alive player
        obj <- getObject <$> arbitrary
        pId <- elements [1, 2]
        lifes <- elements [1, 2, 3]
        health <- choose (1, 100)
        score <- abs <$> arbitrary
        return $ TestPlayer (AliveP obj pId lifes health score)
        , do -- Dead player
        obj <- getObject <$> arbitrary
        pId <- elements [1, 2]
        score <- abs <$> arbitrary
        anim <- choose (1, 7)
        return $ TestPlayer (DeadP obj pId score anim)
        ]

prop_initAlivePlayer_preservesInvariant :: Property
prop_initAlivePlayer_preservesInvariant =
  forAll (arbitrary :: Gen TestObject) $ \obj ->
  forAll (elements [1,2]) $ \pId ->
  forAll (elements [1,2,3]) $ \l ->
  forAll (choose (1,100)) $ \h ->
  forAll (abs <$> arbitrary) $ \s ->
    prop_inv_object (getObject obj)
    ==> prop_inv_player (initAlivePlayer (getObject obj) pId l h s)

prop_initDeadPlayer_preservesInvariant :: Property
prop_initDeadPlayer_preservesInvariant =
  forAll (arbitrary :: Gen TestObject) $ \obj ->
  forAll (elements [1,2]) $ \pId ->
  forAll (abs <$> arbitrary) $ \s ->
  forAll (choose (1,7)) $ \anim ->
    prop_inv_object (getObject obj)
    ==> prop_inv_player (initDeadPlayer (getObject obj) pId s anim)

initAlivePlayerSpec :: SpecWith ()
initAlivePlayerSpec = do
    describe "initAlivePlayer (QuickCheck)" $ do
        it "preserves the Player invariant for valid alive Players" $
            property prop_initAlivePlayer_preservesInvariant

initDeadPlayerSpec :: SpecWith ()
initDeadPlayerSpec = do
    describe "initDeadPlayer (QuickCheck)" $ do
        it "preserves the Player invariant for valid dead Players" $
            property prop_initDeadPlayer_preservesInvariant

playerObjectSpec :: Spec
playerObjectSpec = do
    describe "playerObject (unit tests)" $ do
        it "returns correct Object for alive Player" $ do
            let po = initPlayerObject Blank 10 20 (initDirection 1 0) (ObjectSpeed 3)
                p  = initAlivePlayer po 3 100 10
            playerObject p `shouldBe` po

        it "returns correct Object for dead Player" $ do
            let po = initPlayerObject Blank 5 5 (initDirection 0 0) (ObjectSpeed 0)
                p  = initDeadPlayer po 10 3
            playerObject p `shouldBe` po

playerIdSpec :: Spec
playerIdSpec = do
    describe "playerId (unit tests)" $ do
        it "returns id 1 for alive Player" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (ObjectSpeed 0)
                p  = initAlivePlayer po 1 2 100 0
            playerId p `shouldBe` 1

        it "returns id 2 for dead Player" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (ObjectSpeed 0)
                p  = initDeadPlayer po 2 10 5
            playerId p `shouldBe` 2

playerLifesSpec :: Spec
playerLifesSpec = do
    describe "playerLifes (unit tests)" $ do
        it "returns lifes for alive Player" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (ObjectSpeed 0)
                p  = initAlivePlayer po 1 2 100 0
            playerLifes p `shouldBe` 2

        it "returns 0 for dead Player" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (ObjectSpeed 0)
                p  = initDeadPlayer po 1 10 5
            playerLifes p `shouldBe` 0

playerHealthSpec :: Spec
playerHealthSpec = do
    describe "playerHealth (unit tests)" $ do

        it "returns health for alive Player" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (ObjectSpeed 0)
                p  = initAlivePlayer po 1 3 75 0
            playerHealth p `shouldBe` 75

        it "returns 0 for dead Player" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (ObjectSpeed 0)
                p  = initDeadPlayer po 2 0 2
            playerHealth p `shouldBe` 0

playerScoreSpec :: Spec
playerScoreSpec = do
    describe "playerScore (unit tests)" $ do

        it "returns score for alive Player" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (ObjectSpeed 0)
                p  = initAlivePlayer po 1 3 100 42
            playerScore p `shouldBe` 42

        it "returns score for dead Player" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (ObjectSpeed 0)
                p  = initDeadPlayer po 2 99 5
            playerScore p `shouldBe` 99

playerExplAnimationSpec :: Spec
playerExplAnimationSpec = do
    describe "playerExplAnimation (unit tests)" $ do

        it "returns 0 for alive Player" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (ObjectSpeed 0)
                p  = initAlivePlayer po 1 3 100 10
            playerExplAnimation p `shouldBe` 0

        it "returns animation for dead Player" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (ObjectSpeed 0)
                p  = initDeadPlayer po 2 10 6
            playerExplAnimation p `shouldBe` 6

isPlayerDeadSpec :: Spec
isPlayerDeadSpec = do
    describe "isPlayerDead (unit tests)" $ do
        it "returns False for alive Player" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (ObjectSpeed 0)
                p  = initAlivePlayer po 1 3 100 10
            isPlayerDead p `shouldBe` False

        it "returns True for dead Player" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (ObjectSpeed 0)
                p  = initDeadPlayer po 2 10 3
            isPlayerDead p `shouldBe` True