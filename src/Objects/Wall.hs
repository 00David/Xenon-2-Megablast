module Objects.Wall (module Objects.Wall) where

import Graphics.Gloss (Picture)

import System.Random

import Objects.Hitbox
import Objects.Objects
import GameSetup
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

-- ============================================================
-- ====================== WALLS ===============================
-- ============================================================

prop_wall_allStaticObject :: [Object] -> Bool
prop_wall_allStaticObject [] = True
prop_wall_allStaticObject (x:xs) = case x of
    (MovableO _ _ _ _) -> False
    (StaticO _ _) -> prop_wall_allStaticObject xs

prop_wall_allCollideWithNextObject :: [Object] -> Bool
prop_wall_allCollideWithNextObject [] = True
prop_wall_allCollideWithNextObject (_:[]) = True
prop_wall_allCollideWithNextObject (x:y:xs) = 
    (collisionHitbox (objectHitbox x) (objectHitbox y)) 
    && prop_wall_allCollideWithNextObject (y:xs)

-- a finite wall is a non empty list of static objects, with each collides with the next one in the list
newtype FiniteWall = FiniteWall [Object]
    deriving (Eq, Show)

prop_inv_finiteWall :: FiniteWall -> Bool
prop_inv_finiteWall (FiniteWall l) = length l > 0 && prop_wall_allStaticObject l && prop_wall_allCollideWithNextObject l

initFiniteWall :: [Object] -> FiniteWall
initFiniteWall l
    | length l == 0 = error "a wall must have at least one object"
    | not (prop_wall_allStaticObject l) = error "all wall objects must be static"
    | not (prop_wall_allCollideWithNextObject l) = error "each object must collide with the next in the list"
    | otherwise = FiniteWall l

-- an infinite wall is a non empty list of static objects
newtype InfiniteWall = InfiniteWall [Object]
    deriving (Eq, Show)

prop_inv_infiniteWall :: InfiniteWall -> Bool
prop_inv_infiniteWall (InfiniteWall l) =  -- only a sublist is checked
    let subList = take 100 l
    in length subList > 0 && prop_wall_allStaticObject subList

initInfiniteWall :: [Picture] -> Bool -> Bool -> StdGen -> InfiniteWall
initInfiniteWall assets foreground left gen =
    let 
        -- Maximum width among all wall assets.
        -- Used to compute how far walls may go outside the screen.
        maxW = maximum (toList widthWalls)
        -- Maximum allowed horizontal overflow outside the screen.
        overflow = maxW * 0.6

        -- Random X position bounds.
        -- Left walls slightly overflow on the left side, right walls slightly overflow on the right side.
        (lowerX, upperX) = 
            if left
                then (leftXScreenBound - overflow, leftXScreenBound)
                else (rightXScreenBound - maxW, rightXScreenBound - maxW + overflow)
        
         -- Vertical spacing between wall segments.
        cell = Seq.index heightWalls 0

        -- Starting Y coordinate.
        -- Background walls are vertically offset by half a cell.
        baseY = if foreground then bottomYScreenBound else bottomYScreenBound+(cell/2)

        -- Infinite sequence of Y coordinates for wall segments.
        ys = map (\i -> baseY + fromIntegral i * cell) ([0..] :: [Int])

        --  Random wall asset indexes
        randomWalls = randomRs (0, nbWallAssets - 1) gen
        -- Random X wall positions, might be outside of the screen
        randomX = randomRs (lowerX, upperX) gen

        -- Associates each wall asset index with a random X position.
        randomValues = zip randomWalls randomX

    in 
        let wall = InfiniteWall (zipWith makeWallObject ys randomValues)
        in 
            if not (prop_inv_infiniteWall wall)
                then error "invalid infinite wall"
                else wall

    where
        -- Creates an inidvidual wall object
        -- First argument : Y position
        -- Sencond argument : a pair (number of the wall asset index, X position)
        makeWallObject :: Float -> (Int, Float) -> Object
        makeWallObject y (numWall, x) = 
            let width = (Seq.index widthWalls numWall)
                height = (Seq.index heightWalls numWall)
                x2 = if left then x else 
                    case numWall of -- Offset on right screen side if walls of width < 90
                        0 -> x
                        1 -> x
                        2 -> x+3
                        3 -> x+6
                        i -> error (show i++" out of bounds")
            in initStaticObject (getPictureIndex assets numWall) (initHitboxRectangle x2 y width height)

        getPictureIndex :: [Picture] -> Int -> Picture
        getPictureIndex [] _ = error ("list out of bounds")
        getPictureIndex (y:ys) i = if i == 0 then y else  (getPictureIndex ys (i-1))

nbTakeInfiniteWalls :: Int
nbTakeInfiniteWalls = 20

-- Converts an infinite wall to a finite wall, by only keeping a sub-part of it.
infiniteToFiniteWall :: InfiniteWall -> FiniteWall
infiniteToFiniteWall (InfiniteWall wallObjects) = (FiniteWall (take nbTakeInfiniteWalls wallObjects))

data GameWalls = GameWalls {
    gameLeftWall :: InfiniteWall,
    gameBackgoundLeftWall :: InfiniteWall,
    gameRightWall :: InfiniteWall,
    gameBackgoundRightWall :: InfiniteWall,
    gameFiniteWalls :: [FiniteWall] -- other walls than the default ones
} deriving (Eq, Show)

prop_inv_gameWalls :: GameWalls -> Bool
prop_inv_gameWalls (GameWalls leftWall leftWall2 rightWall rightWall2 walls) =
    prop_inv_infiniteWall leftWall && prop_inv_infiniteWall leftWall2 &&
    prop_inv_infiniteWall rightWall && prop_inv_infiniteWall rightWall2 &&
    foldr (\w acc -> prop_inv_finiteWall w && acc) True walls

initGameWalls :: [Picture] -> [Picture] -> StdGen -> GameWalls
initGameWalls leftWallAssets rightWallAssets gen =
    -- Split the given generator in independant ones for avoiding weird patterns in next generated random values
    let (gen1, gen2) = split gen
        (gen3, gen4) = split gen2

        leftWall = initInfiniteWall leftWallAssets True True gen1
        leftWall2 = initInfiniteWall leftWallAssets False True gen2
        rightWall = initInfiniteWall rightWallAssets True False gen3
        rightWall2 = initInfiniteWall rightWallAssets False False gen4
    in (GameWalls leftWall leftWall2 rightWall rightWall2 [])

