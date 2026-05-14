{-# LANGUAGE InstanceSigs #-}
module GameState.Rock (module GameState.Rock) where

import Graphics.Gloss (Picture(Translate))

import qualified Data.Sequence as Seq

import GameSetup
import Graphics.Assets
import Invariant
import Objects.Hitbox
import Objects.Objects

-- ============================================================
-- ================= ROCK (part of walls) ====================
-- ============================================================

type RockAsset = Int -- 0, 1, 2 or 3

data Rock = Rock {
    rockObject :: Object,
    rockAsset :: RockAsset,
    rockLeftSide :: Bool
} deriving (Show, Eq)

prop_inv_rock :: Rock -> Bool
prop_inv_rock (Rock obj iType _) = 
    case obj of
    (MovableO _ _ _) -> False
    (StaticO _) -> iType >= 0 && iType <= (nbRockAssets-1)

initRock :: Object -> RockAsset -> Bool -> Rock
initRock obj sprite side = 
    case obj of
    (MovableO _ _ _) -> error "a rock cannot be represented by a movable object"
    (StaticO _) -> 
        if sprite >= 0 && sprite <= (nbRockAssets-1) 
            then (Rock obj sprite side)
            else error "invalid rock type"

-- Indicates if a rock is ~ inside the screen ~ : it must not be below a certain y coordinate to be inside
-- The first argument applies a little offset for this y limit if FALSE
insideScreenRock :: Bool -> Rock -> Bool
insideScreenRock foreground (Rock obj _ _) = 
    let (_,y) = centerHitbox (objectHitbox obj)
        limit = if foreground then (bottomYScreenBound-cell) else (bottomYScreenBound-cell+(cell/2))
    in  y >= limit

-- ============================================================
-- ===================== ROCK INVARIANT =======================
-- ============================================================

instance Invariant Rock where
    prop_inv :: Rock -> Bool
    prop_inv = prop_inv_rock 

-- ============================================================
-- ==================== ROCK RENDERABLE =======================
-- ============================================================

instance Renderable Rock where
    getTranslatedAssets :: GameAssets -> Rock -> [Picture]
    getTranslatedAssets ga rock = getTranslatedRockAsset ga rock

-- Returns a list of translated enemy assets.
getTranslatedRockAsset :: GameAssets -> Rock -> [Picture]
getTranslatedRockAsset ga (Rock ro sprite leftSide) = 
    let rockPic = Seq.index ((if leftSide then leftWallPics else rightWallPics) ga) sprite
        (rx, ry) = centerHitbox (objectHitbox ro)
        h = objectHitbox ro
    in [Translate rx ry rockPic] ++ (translateHitbox h)

-- ============================================================
-- =================== ROCK COLLIDABLE ======================
-- ============================================================

instance Collidable Rock where
    getObjects :: Rock -> [Object]
    getObjects rock = [rockObject rock]

    collision :: Collidable b => Rock -> b -> Bool
    collision rock other =
        let objs1 = getObjects rock
            objs2 = getObjects other
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) objs1

    willCollide :: Collidable b => Rock -> b -> Float -> Bool  
    willCollide rock other screenSpeed =
        let objs1 = getObjects rock
            objs2 = getObjects other
            movedObjs1 = map (\o -> moveObject o screenSpeed) objs1
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) movedObjs1