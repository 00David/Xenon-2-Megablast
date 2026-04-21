{-# LANGUAGE InstanceSigs #-}
module PlayerSpec (
    TestPlayer(..),
    spec
)
where

import Test.Hspec
import Test.QuickCheck

import GameState.Player
import Objects.Objects
import ObjectsSpec(TestObject(..))

spec :: Spec
spec = do
    initPlayerSpec

newtype TestPlayer = TestPlayer { getPlayer :: Player } deriving (Eq, Show)
instance Arbitrary TestPlayer where
    arbitrary = do
        obj <- getObject <$> arbitrary
        lifes <- elements [0, 1, 2, 3]
        health <- choose (0, 100)
        score <- abs <$> arbitrary
        return $ TestPlayer (Player obj lifes health score)

prop_initPlayer_preservesInvariant :: Property
prop_initPlayer_preservesInvariant =
  forAll (arbitrary :: Gen TestObject) $ \obj ->
  forAll (elements [0,1,2,3]) $ \l ->
  forAll (choose (0,100)) $ \h ->
  forAll (abs <$> arbitrary) $ \s ->
    prop_inv_object (getObject obj)
    ==> prop_inv_player (initPlayer (getObject obj) l h s)

initPlayerSpec :: SpecWith ()
initPlayerSpec = do
    describe "initPlayer" $ do
        it "preserves the Player invariant for valid Players" $
            property prop_initPlayer_preservesInvariant