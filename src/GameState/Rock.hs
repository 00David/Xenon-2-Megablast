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

data Rock = Rock {
    rockObject :: Object,
    rockType :: Int, -- 0, 1, 2 or 3
    rockLeftSide :: Bool
} deriving (Show, Eq)

prop_inv_rock :: Rock -> Bool
prop_inv_rock (Rock obj iType _) = 
    case obj of
    (MovableO _ _ _) -> False
    (StaticO _) -> iType >= 0 && iType <= (nbRockAssets-1)

initRock :: Object -> Int -> Bool -> Rock
initRock obj i side = 
    case obj of
    (MovableO _ _ _) -> error "a rock cannot be represented by a movable object"
    (StaticO _) -> 
        if i >= 0 && i <= (nbRockAssets-1) 
            then (Rock obj i side)
            else error "invalid rock type"

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
getTranslatedRockAsset ga (Rock ro i leftSide) = 
    let rockPic = Seq.index ((if leftSide then leftWallPics else rightWallPics) ga) i
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