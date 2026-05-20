{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module GameState.Wall (module GameState.Wall) where

import Graphics.Gloss (Picture)

import System.Random

import Data.Foldable (toList)

import GameSetup
import GameState.Rock
import Graphics.Assets
import Objects.Objects
import Typeclasses.Invariant
import Typeclasses.Movable

-- ============================================================
-- ====================== FINITE WALLS ========================
-- ============================================================

prop_wall_allCollideWithNext :: Collidable a => [a] -> Bool
prop_wall_allCollideWithNext [] = True
prop_wall_allCollideWithNext (_:[]) = True
prop_wall_allCollideWithNext (x:y:xs) = 
    collision x y && prop_wall_allCollideWithNext (y:xs)

-- a finite wall is a non empty list of collidables, with each colliding with the next one in the list
newtype FiniteWall a = FiniteWall [a]
    deriving (Eq, Show)

instance Functor FiniteWall where
    fmap :: (a -> b) -> FiniteWall a -> FiniteWall b
    fmap f (FiniteWall list) = FiniteWall (fmap f list)

instance Foldable FiniteWall where
    foldr :: (a -> b -> b) -> b -> FiniteWall a -> b
    foldr f z (FiniteWall xs) = foldr f z xs

prop_inv_finiteWall :: (Invariant a, Collidable a) => FiniteWall a -> Bool
prop_inv_finiteWall (FiniteWall l) = length l > 0 
    && all prop_inv l
    && prop_wall_allCollideWithNext l

initFiniteWall :: (Invariant a, Collidable a) => [a] -> FiniteWall a
initFiniteWall l
    | length l == 0 = error "a wall must have at least one collidable"
    | not (all prop_inv l) = error "each contained element must satisfy its invariant"
    | not (prop_wall_allCollideWithNext l) = error "collidable must collide with the next in the list"
    | otherwise = FiniteWall l

-- Filters a finite wall
filterWall :: (a -> Bool) -> FiniteWall a -> FiniteWall a
filterWall f (FiniteWall elems) = FiniteWall (filter f elems)

-- Moves a finite wall
moveFiniteWall :: (Movable a) => FiniteWall a -> ScreenScrollingSpeed -> FiniteWall a
moveFiniteWall w ss = (fmap (flip move ss) w)

-- Indicates if an entire finite wall is inside the screen
insideScreenFiniteWall :: (Movable a) => FiniteWall a -> Bool
insideScreenFiniteWall w = (all insideScreen w)

-- Creates a random finite wall of rocks
startFiniteWall :: StdGen -> (FiniteWall Rock, StdGen)
startFiniteWall gen =
    let 

        -- Maximum 30 rocks inside the generated wall
        (nbRocks, gen1) = randomR (1::Int, 30) gen

        -- X center of this wall
        (xCenter, gen2) = randomR ((leftXScreenBound+300)::Float, (rightXScreenBound-300)) gen1
        leftSide = xCenter <= 0  

        -- Maximum width among all rock assets.
        maxW = maximum (toList widthRocks)

        -- Maximum allowed horizontal overflow around the X center.
        overflow = (maxW/2) * 0.6
        (lowerX, upperX) = (xCenter - overflow, xCenter + overflow)

        -- Starting Y coordinate.
        baseY = topYScreenBound + 100

        -- Finite sequence of Y coordinates for wall segments (rocks).
        ys = map (\i -> baseY + fromIntegral i * 15) ([0 .. nbRocks - 1] :: [Int])

        --  Random rock asset indexes
        randomWalls = randomRs (0, nbRockAssets - 1) gen2
        -- Random X rock positions, might be outside of the screen
        randomX = randomRs (lowerX, upperX) gen2

        -- Associates each rock asset index with a random X position.
        randomValues = zip randomWalls randomX
    in 
        (initFiniteWall (zipWith (makeWallObject leftSide) ys randomValues), gen2)
    where
        -- Creates an inidvidual rock object, part of the infinite wall
        -- First argument : left or right sided rock 
        -- Second argument : Y position
        -- Third argument : a pair (number of the rock asset index, X position)
        makeWallObject :: Bool -> YCoord -> (Int, XCoord) -> Rock
        makeWallObject leftSide y (numRock, x) = startInitRock x y numRock leftSide True

-- ============================================================
-- ===================== INFINITE WALLS =======================
-- ============================================================

-- an infinite wall is a non empty list of static objects
newtype InfiniteWall a = InfiniteWall [a]

instance Eq a => Eq (InfiniteWall a) where
    (==) :: InfiniteWall a -> InfiniteWall a -> Bool
    (InfiniteWall xs) == (InfiniteWall ys) = (take nbTakeInfiniteWalls xs) == (take nbTakeInfiniteWalls ys)

instance Show a => Show (InfiniteWall a) where
    show :: InfiniteWall a -> String
    show (InfiniteWall xs) ="InfiniteWall " ++ show (take nbTakeInfiniteWalls xs) ++ "..."

instance Functor InfiniteWall where
    fmap :: (a -> b) -> InfiniteWall a -> InfiniteWall b
    fmap f (InfiniteWall list) = InfiniteWall (fmap f list)

nbTakeInfiniteWalls :: Int
nbTakeInfiniteWalls = 20

prop_inv_infiniteWall :: Invariant a => InfiniteWall a -> Bool
prop_inv_infiniteWall (InfiniteWall l) =  -- only a sublist is checked
    let subList = take nbTakeInfiniteWalls l
    in length subList > 0 && all prop_inv subList

-- Initializes an infinite wall of ROCKS
initInfiniteWall :: Bool -> Bool -> StdGen -> InfiniteWall Rock
initInfiniteWall foreground left gen =
    let 
        -- Maximum width among all rock assets.
        -- Used to compute how far walls may go outside the screen.
        maxW = maximum (toList widthRocks)
        -- Maximum allowed horizontal overflow outside the screen.
        overflow = (maxW/2) * 0.6

        -- Random X position bounds.
        -- Left walls can slightly overflow on the left side, right walls can slightly overflow on the right side.
        (lowerX, upperX) = 
            if left
                then (leftXScreenBound - overflow, leftXScreenBound + overflow)
                else (rightXScreenBound - overflow, rightXScreenBound + overflow)

        -- Starting Y coordinate.
        -- Background walls are vertically offset by half a cell.
        baseY = if foreground then bottomYScreenBound else bottomYScreenBound+(rockCell/2)

        -- Infinite sequence of Y coordinates for wall segments (rocks).
        -- The first nbTakeInfiniteWalls values are spaced normally,
        -- then all following values stay constant.
        ys =
            let firstYs =
                    map (\i -> baseY + fromIntegral i * rockCell)
                        ([0 .. nbTakeInfiniteWalls - 1] :: [Int])

                lastY = last firstYs
            in firstYs ++ repeat lastY

        --  Random rock asset indexes
        randomWalls = randomRs (0, nbRockAssets - 1) gen
        -- Random X rock positions, might be outside of the screen
        randomX = randomRs (lowerX, upperX) gen

        -- Associates each rock asset index with a random X position.
        randomValues = zip randomWalls randomX

    in 
        let wall = InfiniteWall (zipWith makeWallObject ys randomValues)
        in 
            if not (prop_inv_infiniteWall wall)
                then error "invalid infinite wall"
                else wall

    where
        -- Creates an inidvidual rock object, part of the infinite wall
        -- First argument : Y position
        -- Second argument : a pair (number of the rock asset index, X position)
        makeWallObject :: YCoord -> (Int, XCoord) -> Rock
        makeWallObject y (numRock, x) = 
            let x2 = if left then x else 
                    case numRock of -- Offset on right screen side if rocks of width < 90
                        0 -> x
                        1 -> x
                        2 -> x+3
                        3 -> x+6
                        i -> error (show i++" out of bounds")
            in startInitRock x2 y numRock left foreground

-- Partially maps an infinite wall : it will apply a function only on a prefix of it
partialMapWall :: forall a. (a -> a) -> InfiniteWall a -> InfiniteWall a
partialMapWall f (InfiniteWall elems) = InfiniteWall (aux 0 elems)
    where
        aux :: Int -> [a] -> [a]
        aux _ [] = error "wall must be infinite"
        aux i l@(x:xs)
            | i == nbTakeInfiniteWalls = l
            | otherwise = (f x):(aux (i+1) xs)

-- Partially filters an infinite wall : it will apply a filter only on a prefix of it
partialFilterWall :: forall a. (a -> Bool) -> InfiniteWall a -> InfiniteWall a
partialFilterWall f (InfiniteWall elems) = InfiniteWall (aux 0 elems)
    where
        aux :: Int -> [a] -> [a]
        aux _ [] = error "wall must be infinite"
        aux i l@(x:xs)
            | i == nbTakeInfiniteWalls = l
            | (f x) = x:(aux (i+1) xs)
            | otherwise = aux (i+1) xs

-- Converts an infinite wall to a finite wall, by only keeping a sub-part of it.
infiniteToFiniteWall :: InfiniteWall a -> FiniteWall a
infiniteToFiniteWall (InfiniteWall wallObjects) = (FiniteWall (take nbTakeInfiniteWalls wallObjects))

-- Moves partially an infinite wall
partialMoveInfiniteWall :: (Movable a) => InfiniteWall a -> ScreenScrollingSpeed -> InfiniteWall a
partialMoveInfiniteWall w ss = (partialMapWall (flip move ss) w)

-- Indicates if a prefix of an infinite wall is inside the screen
partialInsideScreenInfiniteWall :: (Movable a) => InfiniteWall a -> Bool
partialInsideScreenInfiniteWall (InfiniteWall wallObjects) = (all insideScreen (take nbTakeInfiniteWalls wallObjects))

-- ============================================================
-- ======================= GAME WALLS =========================
-- ============================================================

data GameWalls = GameWalls {
    gameLeftWall :: InfiniteWall Rock,
    gameBackgoundLeftWall :: InfiniteWall Rock,
    gameRightWall :: InfiniteWall Rock,
    gameBackgoundRightWall :: InfiniteWall Rock,
    gameFiniteWalls :: [FiniteWall Rock] -- other walls than the default ones
} deriving (Eq, Show)

prop_inv_gameWalls :: GameWalls -> Bool
prop_inv_gameWalls (GameWalls leftWall leftWall2 rightWall rightWall2 walls) =
    prop_inv_infiniteWall leftWall && prop_inv_infiniteWall leftWall2 &&
    prop_inv_infiniteWall rightWall && prop_inv_infiniteWall rightWall2 &&
    foldr (\w acc -> prop_inv_finiteWall w && acc) True walls

initGameWalls :: InfiniteWall Rock -> InfiniteWall Rock -> InfiniteWall Rock -> InfiniteWall Rock -> [FiniteWall Rock] -> GameWalls
initGameWalls left1 left2 right1 right2 walls
    | not (prop_inv_infiniteWall left1) = error "left wall 1 does not respect infinite invariant"
    | not (prop_inv_infiniteWall left2) = error "left wall 2 does not respect infinite invariant"
    | not (prop_inv_infiniteWall right1) = error "right wall 1 does not respect infinite invariant"
    | not (prop_inv_infiniteWall right2) = error "right wall 2 does not respect infinite invariant"
    | any (not . prop_inv_finiteWall) walls = error "a wall does not respect finite invariant"
    | otherwise = GameWalls left1 left2 right1 right2 walls

startInitGameWalls :: StdGen -> GameWalls
startInitGameWalls gen =
    -- Split the given generator in independant ones for avoiding weird patterns in next generated random values
    let (gen1, gen2) = split gen
        (gen3, gen4) = split gen2

        left1 = initInfiniteWall True True gen1
        left2 = initInfiniteWall False True gen2
        right1 = initInfiniteWall True False gen3
        right2 = initInfiniteWall False False gen4
    in (GameWalls left1 left2 right1 right2 [])

-- Moves game walls
moveGameWalls :: GameWalls -> ScreenScrollingSpeed -> GameWalls
moveGameWalls (GameWalls left1 left2 right1 right2 walls) ss =
    let 
        newLeft1 = move left1 ss
        newLeft2 = move left2 ss
        newRight1 = move right1 ss
        newRight2 = move right2 ss
        newWalls = (fmap (\w -> move w ss) walls)
    in 
        (initGameWalls newLeft1 newLeft2 newRight1 newRight2 newWalls)

-- Indicates if game walls are inside of the screen
insideScreenGameWalls :: GameWalls -> Bool
insideScreenGameWalls (GameWalls left1 left2 right1 right2 walls) =
    insideScreen left1
    && insideScreen left2
    && insideScreen right1
    && insideScreen right2
    && all (all insideScreen) walls

addFiniteWall :: GameWalls -> FiniteWall Rock -> GameWalls
addFiniteWall (GameWalls leftWall leftWall2 rightWall rightWall2 walls) newWall =
    initGameWalls leftWall leftWall2 rightWall rightWall2 (walls++[newWall])

prop_pre_addFiniteWall :: GameWalls -> FiniteWall Rock -> Bool
prop_pre_addFiniteWall _ newWall = length newWall > 0

prop_post_addFiniteWall :: GameWalls -> FiniteWall Rock -> Bool
prop_post_addFiniteWall gw@(GameWalls leftWall leftWall2 rightWall rightWall2 walls) newWall =
    let (GameWalls leftWall' leftWall2' rightWall' rightWall2' walls') = addFiniteWall gw newWall
    in leftWall' == leftWall && leftWall2' == leftWall2 && rightWall' == rightWall && rightWall2' == rightWall2
        && any (\w -> w == newWall) walls' && length walls' == ((length walls)+1)

-- ============================================================
-- ==================== WALLS INVARIANT =======================
-- ============================================================

instance (Invariant a, Collidable a) => Invariant (FiniteWall a) where
    prop_inv :: FiniteWall a -> Bool
    prop_inv = prop_inv_finiteWall

instance (Invariant a) => Invariant (InfiniteWall a) where
    prop_inv :: InfiniteWall a -> Bool
    prop_inv = prop_inv_infiniteWall

instance Invariant GameWalls where
    prop_inv :: GameWalls -> Bool
    prop_inv = prop_inv_gameWalls

-- ============================================================
-- ==================== WALLS MOVABLE =======================
-- ============================================================

instance (Movable a) => Movable (FiniteWall a) where
    move :: (FiniteWall a) -> ScreenScrollingSpeed -> (FiniteWall a)
    move = moveFiniteWall

    insideScreen :: (FiniteWall a) -> Bool
    insideScreen = insideScreenFiniteWall

instance (Movable a) => Movable (InfiniteWall a) where
    move :: (InfiniteWall a) -> ScreenScrollingSpeed -> (InfiniteWall a)
    move = partialMoveInfiniteWall

    insideScreen :: (InfiniteWall a) -> Bool
    insideScreen = partialInsideScreenInfiniteWall

instance Movable GameWalls where
    move :: GameWalls -> ScreenScrollingSpeed -> GameWalls
    move = moveGameWalls

    insideScreen :: GameWalls -> Bool
    insideScreen = insideScreenGameWalls

-- ============================================================
-- =================== WALLS RENDERABLE =======================
-- ============================================================

instance (Renderable a) => Renderable (FiniteWall a) where
    getTranslatedAssets :: GameAssets -> FiniteWall a -> [Picture]
    getTranslatedAssets ga (FiniteWall rends) = concatMap (getTranslatedAssets ga) rends

instance (Renderable a) => Renderable (InfiniteWall a) where
    getTranslatedAssets :: GameAssets -> InfiniteWall a -> [Picture]
    getTranslatedAssets ga wall = getTranslatedAssets ga (infiniteToFiniteWall wall)

instance Renderable GameWalls where
    getTranslatedAssets :: GameAssets -> GameWalls -> [Picture]
    getTranslatedAssets ga gw = getTranslatedGameWallAssets ga gw

-- Returns a list of translated game wall assets. For infinite walls, it only translates a finite sub-part of them.
getTranslatedGameWallAssets :: GameAssets -> GameWalls -> [Picture]
getTranslatedGameWallAssets ga (GameWalls left1 left2 right1 right2 finiteWalls) = 
    (getTranslatedAssets ga left2) ++ 
    (getTranslatedAssets ga left1) ++ 
    (getTranslatedAssets ga right2) ++ 
    (getTranslatedAssets ga right1) ++ 
    concatMap (getTranslatedAssets ga) finiteWalls

-- ============================================================
-- ==================== WALLS COLLIDABLE ======================
-- ============================================================

instance (Collidable a) => Collidable (FiniteWall a) where
    getObjects :: FiniteWall a -> [Object]
    getObjects (FiniteWall colls) = concatMap getObjects colls

    collision :: Collidable b => FiniteWall a -> b -> Bool
    collision wall other =
        let objs1 = getObjects wall
            objs2 = getObjects other
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) objs1

    willCollide :: Collidable b => FiniteWall a -> b -> ScreenScrollingSpeed -> Bool  
    willCollide wall other screenSpeed =
        let objs1 = getObjects wall
            objs2 = getObjects other
            movedObjs1 = map (\o -> moveObject o screenSpeed) objs1
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) movedObjs1

instance (Collidable a) => Collidable (InfiniteWall a) where
    getObjects :: InfiniteWall a -> [Object]
    getObjects wall = getObjects (infiniteToFiniteWall wall)

    collision :: Collidable b => InfiniteWall a -> b -> Bool
    collision wall other =
        let objs1 = getObjects wall
            objs2 = getObjects other
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) objs1

    willCollide :: Collidable b => InfiniteWall a -> b -> ScreenScrollingSpeed -> Bool  
    willCollide wall other screenSpeed =
        let objs1 = getObjects wall
            objs2 = getObjects other
            movedObjs1 = map (\o -> moveObject o screenSpeed) objs1
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) movedObjs1

instance Collidable GameWalls where
    getObjects :: GameWalls -> [Object]
    getObjects (GameWalls left1 left2 right1 right2 finiteWalls) =
        getObjects left1
        ++ getObjects left2
        ++ getObjects right1
        ++ getObjects right2
        ++ concatMap getObjects finiteWalls

    collision :: Collidable b => GameWalls -> b -> Bool
    collision gw other =
        let objs1 = getObjects gw
            objs2 = getObjects other
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) objs1

    willCollide :: Collidable b => GameWalls -> b -> ScreenScrollingSpeed -> Bool
    willCollide gw other screenSpeed =
        let objs1 = getObjects gw
            objs2 = getObjects other
            movedObjs1 = map (\o -> moveObject o screenSpeed) objs1
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) movedObjs1