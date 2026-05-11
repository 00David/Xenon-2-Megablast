module WallSpec (
    spec
)
where

import Graphics.Gloss (Picture (Blank))

import Test.Hspec
import Test.QuickCheck

import Objects.Hitbox
import Objects.Objects
import GameState.Wall

spec :: Spec
spec = do
  initFiniteWallSpec

-- ============================================================
-- ====================== WALLS ===============================
-- ============================================================

newtype TestFiniteWall = TestFiniteWall { getFiniteWall :: FiniteWall } deriving (Eq, Show)
instance Arbitrary TestFiniteWall where
    arbitrary = do
        n <- getPositive <$> arbitrary  -- number of objects (here only rectangles) part of the wall

        -- first rectangle position randomly generated
        x0 <- arbitrary
        y0 <- arbitrary

        objs <- mkRects n (x0,y0) []
        return $ TestFiniteWall (initFiniteWall objs)

        where
            -- Generates a wall of overlapping rectangles
            -- First parameter : the number of remaining rectangles to create
            -- Second parameter : (x, y) of the bottom left angle of the current Rectangle to create
            -- Third parameter : the list containing all objects of the wall so far created
            mkRects :: Int -> (Float, Float) -> [Object] -> Gen [Object]
            mkRects 0 _ acc = return (reverse acc)
            mkRects k (x,y) acc = do
                -- each rectangle of the wall has its width / height randomly generated (strictly positive) 
                w <- getPositive <$> arbitrary
                h <- getPositive <$> arbitrary
                let rect = Rectangle x y w h
                    xNext = x
                    yNext = y + h / 2 -- vertical overlap
                mkRects (k-1) (xNext,yNext) (StaticO Blank rect : acc)

prop_finiteWall_preservesInvariant :: TestFiniteWall -> Bool
prop_finiteWall_preservesInvariant (TestFiniteWall w) = prop_inv_finiteWall w

initFiniteWallSpec :: Spec
initFiniteWallSpec = do
    describe "initFiniteWall (QuickCheck)" $ do
        it "preserves the Wall invariant for valid finite Walls" $
            property prop_finiteWall_preservesInvariant