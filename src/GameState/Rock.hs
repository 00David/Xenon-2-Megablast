{-# LANGUAGE InstanceSigs #-}
module GameState.Rock (module GameState.Rock) where

import Graphics.Gloss (Picture(Translate))

import qualified Data.Sequence as Seq

import GameSetup
import Graphics.Assets
import Objects.Hitbox
import Objects.Objects
import Typeclasses.Invariant
import Typeclasses.Movable

-- ============================================================
-- ================= ROCK (part of walls) ====================
-- ============================================================

type RockAsset = Int -- 0, 1, 2 or 3

data Rock = 
    LeftRock {
        rockObject :: Object,
        rockAsset :: RockAsset,
        rockForward :: Bool
    } 
    | RightRock {
        rockObject :: Object,
        rockAsset :: RockAsset,
        rockForward :: Bool
    } deriving (Show, Eq)

prop_inv_rock :: Rock -> Bool
prop_inv_rock rock =
    let obj =  rockObject rock
        asset = rockAsset rock
    in case obj of
        (MovableO _ _ _) -> False
        (StaticO _) -> asset >= 0 && asset <= (nbRockAssets-1)

initRock :: Object -> RockAsset -> Bool -> Bool -> Rock
initRock obj asset leftSide forward = 
    case obj of
    (MovableO _ _ _) -> error "a rock cannot be represented by a movable object"
    (StaticO _) -> 
        if asset >= 0 && asset <= (nbRockAssets-1) 
            then 
                if leftSide 
                    then (LeftRock obj asset forward)
                    else (RightRock obj asset forward)
            else error "invalid rock type"

-- Moves a rock
moveRock :: Rock -> ScreenScrollingSpeed -> Rock
moveRock (LeftRock ro asset frwd) ss = initRock (moveObject ro ss) asset True frwd
moveRock (RightRock ro asset frwd) ss = initRock (moveObject ro ss) asset False frwd

-- Indicates if a rock is ~ inside the screen ~ : it must not be below a certain y coordinate to be inside
-- If forward, a little offset for this y limit is added
insideScreenRock :: Rock -> Bool
insideScreenRock rock = 
    let obj = rockObject rock
        (_,y) = centerHitbox (objectHitbox obj)
        limit = if rockForward rock then (bottomYScreenBound-rockCell) else (bottomYScreenBound-rockCell+(rockCell/2))
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

-- Returns a list of translated rock assets.
getTranslatedRockAsset :: GameAssets -> Rock -> [Picture]
getTranslatedRockAsset ga (LeftRock ro sprite _) = 
    let rockPic = Seq.index (leftWallPics ga) sprite
        (rx, ry) = centerHitbox (objectHitbox ro)
        h = objectHitbox ro
    in [Translate rx ry rockPic] ++ (translateHitbox h)
getTranslatedRockAsset ga (RightRock ro sprite _) = 
    let rockPic = Seq.index (rightWallPics ga) sprite
        (rx, ry) = centerHitbox (objectHitbox ro)
        h = objectHitbox ro
    in [Translate rx ry rockPic] ++ (translateHitbox h)

-- ============================================================
-- ====================== ROCK MOVABLE ========================
-- ============================================================

instance Movable Rock where
    move :: Rock -> ScreenScrollingSpeed -> Rock
    move = moveRock

    insideScreen :: Rock -> Bool
    insideScreen = insideScreenRock

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

    willCollide :: Collidable b => Rock -> b -> ScreenScrollingSpeed -> Bool  
    willCollide rock other screenSpeed =
        let objs1 = getObjects rock
            objs2 = getObjects other
            movedObjs1 = map (\o -> moveObject o screenSpeed) objs1
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) movedObjs1