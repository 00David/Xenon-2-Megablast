{-# LANGUAGE InstanceSigs #-}
module EnemySpec (
    --TestEnemy(..),
    spec
)
where

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = return()
{--

import GameState.Enemy
import Objects.Objects
import ObjectsSpec(TestObject(..))

spec :: Spec
spec = do
    initEnemySpec

newtype TestEnemy = TestEnemy { getEnemy :: Enemy } deriving (Eq, Show)
instance Arbitrary TestEnemy where
    arbitrary = do
        obj <- getObject <$> arbitrary
        health <- getPositive <$> arbitrary
        return $ TestEnemy (Enemy obj health)

prop_initEnemy_preservesInvariant :: Property
prop_initEnemy_preservesInvariant =
  forAll (arbitrary :: Gen TestObject) $ \obj ->
  forAll (getPositive <$> arbitrary) $ \h ->
    prop_inv_object (getObject obj)
    ==> prop_inv_enemy (initEnemy (getObject obj) h)

initEnemySpec :: SpecWith ()
initEnemySpec = do
    describe "initEnemy (QuickCheck)" $ do
        it "preserves the Enemy invariant for valid Enemies" $
            property prop_initEnemy_preservesInvariant
--}