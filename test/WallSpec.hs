{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WallSpec (
    TestFiniteWall(..),
    TestInfiniteWall(..),
    TestGameWalls(..),
    spec
)
where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Sequence as Seq
import System.Random

import GameSetup
import GameState.Rock
import GameState.Wall
import Objects.Hitbox
import Objects.Objects
import Typeclasses.Invariant
import ObjectsSpec(TestObject(..))

spec :: Spec
spec = do
    -- Finite Walls
    initFiniteWallSpec
    startFiniteWallSpec
    filterWallSpec
    moveFiniteWallSpec
    moveFiniteWallQuickCheck
    insideScreenFiniteWallSpec
    invariantFiniteLawsSpec
    collidableFiniteLawsSpec
    -- Infinite Walls
    initInfiniteWallSpec
    partialMapWallQuickCheck
    partialFilterWallQuickCheck
    infiniteWallToFiniteListQuickCheck
    partialMoveInfiniteWallQuickCheck
    invariantInfiniteLawsSpec
    collidableInfiniteLawsSpec
    -- GameWalls
    initGameWallsSpec
    startGameWallsSpec
    moveGameWallsQuickCheck
    addFiniteWallQuickCheck
    invariantGameWallsLawsSpec
    collidableGameWallsLawsSpec

-- The project uses only walls of rocks, so my test walls will be concretely  
-- made of rocks (It was too complicated to have them completely generic).

-- ============================================================
-- =================== FINITE WALLS ===========================
-- ============================================================

-- Initializes Finite Wall of rocks veryfing their invariant
newtype TestFiniteWall a = TestFiniteWall { getFiniteWall :: FiniteWall a } deriving (Eq, Show)
instance Arbitrary (TestFiniteWall Rock) where
    arbitrary :: Gen (TestFiniteWall Rock)
    arbitrary = do
        n <- choose(1,200)  -- number of rocks part of the wall (non emty)

        -- first rock position is randomly generated
        x0 <- arbitrary
        y0 <- arbitrary

        objs <- mkRocks n (x0,y0) []
        return $ TestFiniteWall (initFiniteWall objs)

        where
            -- Generates a wall of overlapping rocks
            -- First parameter : the number of remaining rocks to create
            -- Second parameter : (x, y) of the center of the current rock to create
            -- Third parameter : the list containing all rocks of the wall so far created
            mkRocks :: Int -> (Float, Float) -> [Rock] -> Gen [Rock]
            mkRocks 0 _ acc = return (reverse acc)
            mkRocks k (x,y) acc = do
                leftSide <- arbitrary
                forward <- arbitrary
                asset <- choose(0, (nbRockAssets-1))

                let obj = initStaticObject ((Seq.index rockHitbox asset) x y leftSide)
                    rock = if leftSide
                            then LeftRock obj asset forward
                            else RightRock obj asset forward
                    
                    xNext = x
                    yNext = y + 15 -- vertical overlap for rocks case
                mkRocks (k-1) (xNext,yNext) (rock : acc)

-- we don't use initFiniteWall, because we don't past him a [Rock] parameter, but directly an arbitrary FiniteWall Rock
prop_finiteWall_preservesInvariant :: TestFiniteWall Rock -> Bool
prop_finiteWall_preservesInvariant (TestFiniteWall w) = prop_inv_finiteWall w

prop_startFiniteWall_preservesInvariant :: Property
prop_startFiniteWall_preservesInvariant =
    forAll arbitrary $ \(seed :: Int) ->
        let gen = mkStdGen seed
            (wall, _) = startFiniteWall gen
        in prop_inv_finiteWall wall

initFiniteWallSpec :: Spec
initFiniteWallSpec = do
    describe "initFiniteWall (QuickCheck)" $ do
        it "preserves the Wall invariant for valid finite rock Walls" $
            property prop_finiteWall_preservesInvariant

startFiniteWallSpec :: Spec
startFiniteWallSpec = do
    describe "startFiniteWallSpec (QuickCheck)" $ do
        it "preserves the Wall invariant for start finite rock Walls" $
            property prop_startFiniteWall_preservesInvariant

filterWallSpec :: Spec
filterWallSpec = do
    describe "filterWall (unit tests)" $ do
        it "does not change the wall, for 2 rocks inside of the screen filtered by insideYScreenRockSpec" $
            let r1 = startInitRock 0 0 0 True True
                r2 = startInitRock 0 10 0 True True
                wall = initFiniteWall ([r1, r2])
            in filterWall insideYScreenRock wall `shouldBe` wall
        it "does not change the wall, for 2 rocks inside of the screen filtered by insideYScreenRockSpec : postcondition check" $
            let r1 = startInitRock 0 0 0 True True
                r2 = startInitRock 0 10 0 True True
                wall = initFiniteWall ([r1, r2])
            in (prop_post_filterWall insideYScreenRock wall) `shouldBe` True
        it "delete a rock from a wall containing a rock outside of the screen, by filtering with insideYScreenRockSpec" $
            let r1 = startInitRock 0 (bottomYScreenBound-50) 0 True True -- outside
                r2 = startInitRock 0 (bottomYScreenBound-40) 0 True True
                wall = initFiniteWall ([r1, r2])
            in filterWall insideYScreenRock wall `shouldBe` initFiniteWall ([r2])
        it "delete a rock from a wall containing a rock outside of the screen, by filtering with insideYScreenRockSpec : postcondition check" $
            let r1 = startInitRock 0 (bottomYScreenBound-50) 0 True True -- outside
                r2 = startInitRock 0 (bottomYScreenBound-40) 0 True True
                wall = initFiniteWall ([r1, r2])
            in (prop_post_filterWall insideYScreenRock wall) `shouldBe` True

moveFiniteWallSpec :: Spec
moveFiniteWallSpec = do
    describe "moveFiniteWall (unit tests)" $ do
        it "moves towards the bottom wall rocks according to a screen scrolling speed of 5" $
            let r1 = startInitRock 0 0 0 True True
                r2 = startInitRock 0 10 0 True True
                wall = initFiniteWall ([r1, r2])
                ss = 5
                
                (FiniteWall rocks) = moveFiniteWall wall ss
                (r1', r2') = case rocks of
                    (x:y:_) -> (x,y)
                    _ -> error "impossible"

                (x1', y1') = centerHitbox (objectHitbox (rockObject r1'))
                (x2', y2') = centerHitbox (objectHitbox (rockObject r2'))
            in (x1', y1', x2', y2') `shouldBe` (0, (-5), 0, (5))
        it "does not move wall rocks according to a screen scrolling speed of 0" $
            let r1 = startInitRock 0 0 0 True True
                r2 = startInitRock 0 10 0 True True
                wall = initFiniteWall ([r1, r2])
                ss = 0
                
                (FiniteWall rocks) = moveFiniteWall wall ss
                (r1', r2') = case rocks of
                    (x:y:_) -> (x,y)
                    _ -> error "impossible"

                (x1', y1') = centerHitbox (objectHitbox (rockObject r1'))
                (x2', y2') = centerHitbox (objectHitbox (rockObject r2'))
            in (x1', y1', x2', y2') `shouldBe` (0, 0, 0, 10)

moveFiniteWallQuickCheck :: Spec
moveFiniteWallQuickCheck = do
    describe "moveFiniteWall (QuickCheck)" $ do
        it "satisfies moveFiniteWall post-condition for all valid parameters" $
            property ( \(TestFiniteWall wall::TestFiniteWall Rock) ss ->
                (prop_inv_finiteWall wall && (prop_pre_moveFiniteWall wall ss)) 
                ==> let wall' = moveFiniteWall wall ss
                    in prop_inv_finiteWall wall' && (prop_post_moveFiniteWall wall ss)
            )

insideScreenFiniteWallSpec :: Spec
insideScreenFiniteWallSpec = do
    describe "insideScreenFiniteWall (unit tests)" $ do
        it "an entire wall of rocks is inside the screen" $
            let r1 = startInitRock 0 0 0 True True
                r2 = startInitRock 0 10 0 True True
                wall = initFiniteWall ([r1, r2])
            in insideScreenFiniteWall wall `shouldBe` True
        it "a rock part of a wall is outside of the screen, implying the wall being outside of the screen" $
            let r1 = startInitRock 0 (bottomYScreenBound-50) 0 True True -- outside
                r2 = startInitRock 0 (bottomYScreenBound-40) 0 True True
                wall = initFiniteWall ([r1, r2])
            in insideScreenFiniteWall wall `shouldBe` False
        it "all rocks part of a wall are outside of the screen, implying the wall being outside of the screen" $
            let r1 = startInitRock 0 (bottomYScreenBound-100) 0 True True -- outside
                r2 = startInitRock 0 (bottomYScreenBound-100) 0 True True -- outside
                wall = initFiniteWall ([r1, r2])
            in insideScreenFiniteWall wall `shouldBe` False

-- ============================================================
-- ======================== LAWS ==============================
-- ============================================================

invariantFiniteLawsSpec :: Spec
invariantFiniteLawsSpec = do
    describe "Invariant laws (QuickCheck)" $ do
        it "law_invariant_stable for FiniteWall Rock" $
            property (
                \(TestFiniteWall wall::TestFiniteWall Rock) -> law_invariant_stable wall
            )
        it "law_invariant_idempotent for FiniteWall Rock" $
            property (
                \(TestFiniteWall wall::TestFiniteWall Rock) -> law_invariant_idempotent wall
            )

collidableFiniteLawsSpec :: Spec
collidableFiniteLawsSpec = do
    describe "Collidable laws (QuickCheck)" $ do
        it "law_collidable_reflexive for FiniteWall Rock" $
            property (\(TestFiniteWall wall::TestFiniteWall Rock) ->
                prop_inv_finiteWall wall 
                ==> law_collidable_reflexive wall
            )
        it "law_collidable_symmetric for FiniteWall Rock with another FiniteWall Rock" $
            property (\(TestFiniteWall wall::TestFiniteWall Rock) (TestFiniteWall wall2::TestFiniteWall Rock) ->
                prop_inv_finiteWall wall && prop_inv_finiteWall wall2 
                ==> law_collidable_symmetric wall wall2
            )
        it "law_collidable_symmetric for FiniteWall Rock with another Object" $
            property (\(TestFiniteWall wall::TestFiniteWall Rock) (TestObject o2) ->
                prop_inv_finiteWall wall && prop_inv_object o2 
                ==> law_collidable_symmetric wall o2
            )
        it "law_collidable_will_collide for FiniteWall Rock with another FiniteWall Rock" $
            property (\(TestFiniteWall wall1::TestFiniteWall Rock) (TestFiniteWall wall2::TestFiniteWall Rock) ->
                prop_inv_finiteWall wall1 && prop_inv_finiteWall wall2 
                ==> law_collidable_will_collide wall1 wall2
            )
        it "law_collidable_will_collide for FiniteWall Rock with another Object" $
            property (\(TestFiniteWall wall::TestFiniteWall Rock) (TestObject o2) ->
                prop_inv_finiteWall wall && prop_inv_object o2 
                ==> law_collidable_will_collide wall o2
            )

-- ============================================================
-- ================== INFINITE WALLS ==========================
-- ============================================================

-- Initializes Infinite Wall of rocks veryfing their invariant
newtype TestInfiniteWall a = TestInfiniteWall { getInfiniteWall :: InfiniteWall a } deriving (Eq, Show)
instance Arbitrary (TestInfiniteWall Rock) where
    arbitrary :: Gen (TestInfiniteWall Rock)
    arbitrary = do
        foreground <- arbitrary
        left <- arbitrary
        seed <- arbitrary  :: Gen Int

        let gen = mkStdGen seed
            wall = initInfiniteWall foreground left gen -- we directly use the existing initializer

        return (TestInfiniteWall wall)

prop_infiniteWall_preservesInvariant :: TestInfiniteWall Rock -> Bool
prop_infiniteWall_preservesInvariant (TestInfiniteWall w) = prop_inv_infiniteWall w

initInfiniteWallSpec :: Spec
initInfiniteWallSpec = do
    describe "initInfiniteWall (QuickCheck)" $ do
        it "preserves the Infinite Wall invariant for valid infinite rock Walls" $
            property prop_infiniteWall_preservesInvariant

partialMapWallQuickCheck :: Spec
partialMapWallQuickCheck = do
    describe "partialMapWall (QuickCheck)" $ do
        it "satisfies partialMapWall post-condition (infite wall invariant) for valid infinite rock Walls, using the moveRock function" $
            property ( \(TestInfiniteWall wall::TestInfiniteWall Rock) ss ->
                (prop_inv_infiniteWall wall) && ss >= 0
                ==> let wall' = partialMapWall (flip moveRock ss) wall
                    in prop_inv_infiniteWall wall'
            )

partialFilterWallQuickCheck :: Spec
partialFilterWallQuickCheck = do
    describe "partialFilterWall (QuickCheck)" $ do
        it "satisfies partialFilterWall post-condition (infite wall invariant) for valid infinite rock Walls, using the insideYScreenRock filtering function" $
            property ( \(TestInfiniteWall wall::TestInfiniteWall Rock) ->
                (prop_inv_infiniteWall wall)
                ==> let wall' = partialFilterWall insideYScreenRock wall
                    in prop_inv_infiniteWall wall'
            )

infiniteWallToFiniteListQuickCheck :: Spec
infiniteWallToFiniteListQuickCheck = do
    describe "infiniteWallToFiniteList (QuickCheck)" $ do
        it "satisfies infiniteWallToFiniteList post-condition for valid infinite rock Walls" $
            property ( \(TestInfiniteWall wall::TestInfiniteWall Rock) ->
                (prop_inv_infiniteWall wall)
                ==> (prop_post_infiniteWallToFiniteList wall)
            )

partialMoveInfiniteWallQuickCheck :: Spec
partialMoveInfiniteWallQuickCheck = do
    describe "partialMoveInfiniteWall (QuickCheck)" $ do
        it "satisfies partialMoveInfiniteWall post-condition for valid parameters" $
            property ( \(TestInfiniteWall wall::TestInfiniteWall Rock) ss ->
                (prop_inv_infiniteWall wall) && (prop_pre_partialMoveInfiniteWall wall ss)
                ==> let wall' = partialMoveInfiniteWall wall ss
                    in (prop_inv_infiniteWall wall') && (prop_post_partialMoveInfiniteWall wall ss)
            )

-- ============================================================
-- ======================== LAWS ==============================
-- ============================================================

invariantInfiniteLawsSpec :: Spec
invariantInfiniteLawsSpec = do
    describe "Invariant laws (QuickCheck)" $ do
        it "law_invariant_stable for InfiniteWall Rock" $
            property (
                \(TestInfiniteWall wall::TestInfiniteWall Rock) -> law_invariant_stable wall
            )
        it "law_invariant_idempotent for InfiniteWall Rock" $
            property (
                \(TestInfiniteWall wall::TestInfiniteWall Rock) -> law_invariant_idempotent wall
            )

collidableInfiniteLawsSpec :: Spec
collidableInfiniteLawsSpec = do
    describe "Collidable laws (QuickCheck)" $ do
        it "law_collidable_reflexive for InfiniteWall Rock" $
            property (\(TestInfiniteWall wall::TestInfiniteWall Rock) ->
                prop_inv_infiniteWall wall 
                ==> law_collidable_reflexive wall
            )
        it "law_collidable_symmetric for InfiniteWall Rock with another InfiniteWall Rock" $
            property (\(TestInfiniteWall wall::TestInfiniteWall Rock) (TestInfiniteWall wall2::TestInfiniteWall Rock) ->
                prop_inv_infiniteWall wall && prop_inv_infiniteWall wall2 
                ==> law_collidable_symmetric wall wall2
            )
        it "law_collidable_symmetric for InfiniteWall Rock with another Object" $
            property (\(TestInfiniteWall wall::TestInfiniteWall Rock) (TestObject o2) ->
                prop_inv_infiniteWall wall && prop_inv_object o2 
                ==> law_collidable_symmetric wall o2
            )
        it "law_collidable_will_collide for InfiniteWall Rock with another InfiniteWall Rock" $
            property (\(TestInfiniteWall wall1::TestInfiniteWall Rock) (TestInfiniteWall wall2::TestInfiniteWall Rock) ->
                prop_inv_infiniteWall wall1 && prop_inv_infiniteWall wall2 
                ==> law_collidable_will_collide wall1 wall2
            )
        it "law_collidable_will_collide for InfiniteWall Rock with another Object" $
            property (\(TestInfiniteWall wall::TestInfiniteWall Rock) (TestObject o2) ->
                prop_inv_infiniteWall wall && prop_inv_object o2 
                ==> law_collidable_will_collide wall o2
            )

-- ============================================================
-- ====================== GAME WALLS ==========================
-- ============================================================

-- Initializes Game Walls veryfing their invariant
newtype TestGameWalls = TestGameWalls { getGameWalls :: GameWalls } deriving (Eq, Show)
instance Arbitrary TestGameWalls where
    arbitrary :: Gen TestGameWalls
    arbitrary = do
        seed :: Int <- arbitrary

        nFiniteWalls <- choose (0, 10)
        finiteWalls <- vectorOf nFiniteWalls (getFiniteWall <$> arbitrary)

        let gen = mkStdGen seed
            gameWalls = startInitGameWalls gen
            -- add a certain amount of finite walls
            gameWalls' = foldl' addFiniteWall gameWalls finiteWalls
        return (TestGameWalls gameWalls')

prop_initGameWalls_preservesInvariant :: TestInfiniteWall Rock -> TestInfiniteWall Rock -> 
    TestInfiniteWall Rock -> TestInfiniteWall Rock -> [TestFiniteWall Rock] -> Property
prop_initGameWalls_preservesInvariant (TestInfiniteWall wall1) (TestInfiniteWall wall2)
    (TestInfiniteWall wall3) (TestInfiniteWall wall4) walls = 
        property $ prop_inv_gameWalls (initGameWalls wall1 wall2 wall3 wall4 (map getFiniteWall walls))
    
prop_startInitGameWalls_preservesInvariant :: Property
prop_startInitGameWalls_preservesInvariant =
    forAll arbitrary $ \(seed :: Int) ->
        let gen = mkStdGen seed
            gw = startInitGameWalls gen
        in prop_inv_gameWalls gw

initGameWallsSpec :: Spec
initGameWallsSpec = do
    describe "initGameWalls (QuickCheck)" $ do
        it "preserves GameWalls invariant for valid GameWalls" $
            property prop_initGameWalls_preservesInvariant

startGameWallsSpec :: Spec
startGameWallsSpec = do
    describe "startInitGameWalls (QuickCheck)" $ do
        it "preserves the GameWalls invariant for start game walls" $
            property prop_startInitGameWalls_preservesInvariant

moveGameWallsQuickCheck :: Spec
moveGameWallsQuickCheck = do
    describe "moveGameWalls (QuickCheck)" $ do
        it "satisfies moveGameWalls postcondition for valid parameters" $
            property (\(TestGameWalls gw) ss ->
                prop_inv_gameWalls gw && (prop_pre_moveGameWalls gw ss)
                ==> let gw' = moveGameWalls gw ss
                    in prop_inv_gameWalls gw'
            )

addFiniteWallQuickCheck :: Spec
addFiniteWallQuickCheck = do
    describe "addFiniteWall (QuickCheck)" $ do
        it "satisfies addFiniteWall postconditions for valid parameters" $
            property (\(TestGameWalls gw) (TestFiniteWall wall::TestFiniteWall Rock) ->
                prop_inv_gameWalls gw && prop_inv_finiteWall wall
                ==> let gw' = addFiniteWall gw wall
                    in prop_inv_gameWalls gw' && (prop_post_addFiniteWall gw wall)
            )

-- ============================================================
-- ======================== LAWS ==============================
-- ============================================================

invariantGameWallsLawsSpec :: Spec
invariantGameWallsLawsSpec = do
    describe "Invariant laws (QuickCheck)" $ do
        it "law_invariant_stable for GameWalls" $
            property (
                \(TestGameWalls gw) -> law_invariant_stable gw
            )
        it "law_invariant_idempotent for GameWalls" $
            property (
                \(TestGameWalls gw) -> law_invariant_idempotent gw
            )

collidableGameWallsLawsSpec :: Spec
collidableGameWallsLawsSpec = do
    describe "Collidable laws (QuickCheck)" $ do
        it "law_collidable_reflexive for GameWalls" $
            property (\(TestGameWalls gw) ->
                prop_inv_gameWalls gw 
                ==> law_collidable_reflexive gw
            )
        it "law_collidable_symmetric for GameWalls with another GameWalls" $
            property (\(TestGameWalls gw1) (TestGameWalls gw2) ->
                prop_inv_gameWalls gw1 && prop_inv_gameWalls gw2 
                ==> law_collidable_symmetric gw1 gw2
            )
        it "law_collidable_symmetric for GameWalls with another Object" $
            property (\(TestGameWalls gw1) (TestObject o2) ->
                prop_inv_gameWalls gw1 && prop_inv_object o2 
                ==> law_collidable_symmetric gw1 o2
            )
        it "law_collidable_will_collide for GameWalls with another GameWalls" $
            property (\(TestGameWalls gw1) (TestGameWalls gw2) ->
                prop_inv_gameWalls gw1 && prop_inv_gameWalls gw2 
                ==> law_collidable_will_collide gw1 gw2
            )
        it "law_collidable_will_collide for GameWalls with another Object" $
            property (\(TestGameWalls gw1) (TestObject o2) ->
                prop_inv_gameWalls gw1 && prop_inv_object o2 
                ==> law_collidable_will_collide gw1 o2
            )