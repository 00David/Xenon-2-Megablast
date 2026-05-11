{-# LANGUAGE InstanceSigs #-}
module GameState.Player (module GameState.Player) where

import Graphics.Gloss (Picture(Translate, Rotate))

import qualified Data.Sequence as Seq

import Graphics.Assets
import Invariant
import Objects.Hitbox
import Objects.Objects

-- ============================================================
-- ========================= PLAYER ===========================
-- ============================================================

data Player = AliveP Object Int Int Int Int
    | DeadP Object Int Int Int
    deriving (Eq, Show)

{-- 
When Alive, it has :
- a spatial representation of the player
- player id, 1 or 2
- player remaining lifes, inside of [1, 3]
- health for the current player life, inside of ]0, 100]
- player current score, positive
Whe Dead, it has :
- a spatial representation of the player
- player id, 1 or 2
- player current score, positive
- player current explosion animation, inside of [1, 7]
--}

prop_inv_player :: Player -> Bool
prop_inv_player (AliveP po pId lifes health score) = prop_inv_object po && (pId == 1 || pId == 2) && lifes >= 1 && lifes <= 3
    && health >= 1 && health <= 100 && score >= 0
prop_inv_player (DeadP po pId score anim) = prop_inv_object po && (pId == 1 || pId == 2) && score >= 0
    && anim >= 1 && anim <= 7

initPlayerObject :: Float -> Float -> Direction -> ObjectSpeed -> Object
initPlayerObject x y dir speed = 
    (initMovableObject 
        (playerHitbox x y)
        dir
        speed
    )

initAlivePlayer :: Object -> Int -> Int -> Int -> Int -> Player
initAlivePlayer po pId lifes health score
    | lifes < 0 || lifes > 3 = error "number of lifes outside of [0, 3], must be inside it"
    | health < 0 || health > 100 = error "current life health outside of [0, 100], must be inside it"
    | score < 0 = error "score must be positive"
    | otherwise = AliveP po pId lifes health score

initDeadPlayer :: Object -> Int -> Int -> Int -> Player
initDeadPlayer po pId score anim
    | score < 0 = error "score must be positive"
    | anim < 1 || anim > 7 = error "animation number must be inside of [1, 7]" 
    | otherwise = DeadP po pId score anim

playerObject :: Player -> Object
playerObject (AliveP o _ _ _ _) = o
playerObject (DeadP o _ _ _) = o

playerId :: Player -> Int
playerId (AliveP _ pId _ _ _) = pId
playerId (DeadP _ pId _ _) = pId

playerLifes :: Player -> Int
playerLifes (AliveP _ _ l _ _) = l
playerLifes (DeadP _ _ _ _) = 0

playerHealth :: Player -> Int
playerHealth (AliveP _ _ _ h _) = h
playerHealth (DeadP _ _ _ _) = 0

playerScore :: Player -> Int
playerScore (AliveP _ _ _ _ s) = s
playerScore (DeadP _ _ s _) = s

playerExplAnimation :: Player -> Int
playerExplAnimation (AliveP _ _ _ _ _) = 0
playerExplAnimation (DeadP _ _ _ anim) = anim

-- Indicates if a player is dead
isPlayerDead :: Player -> Bool
isPlayerDead (AliveP _ _ _ _ _) = False
isPlayerDead (DeadP _ _ _ _) = True

-- Creates the player composite hitbox, with given center coordinates
playerHitbox :: Float -> Float -> Hitbox
playerHitbox cx cy = 
    let
        -- Central body
        bodyWidth = 24
        bodyHeight = 69
        bodyX = cx - bodyWidth / 2
        bodyY = cy - 32
        
        -- Left wing
        leftWingWidth = 30
        leftWingHeight = 25
        leftWingX = cx - 52
        leftWingY = cy - 22
        
        -- Right wing
        rightWingWidth = 30
        rightWingHeight = 25
        rightWingX = cx + 24
        rightWingY = cy - 22
        
        -- Left back thruster
        leftThrusterWidth = 15
        leftThrusterHeight = 47
        leftThrusterX = cx - 22
        leftThrusterY = cy - 34
        
        -- Right back thruster
        rightThrusterWidth = 15
        rightThrusterHeight = 47
        rightThrusterX = cx + 8
        rightThrusterY = cy - 34
    in
        initHitboxes cx cy
            [ initHitboxRectangle bodyX bodyY bodyWidth bodyHeight
            , initHitboxRectangle leftWingX leftWingY leftWingWidth leftWingHeight
            , initHitboxRectangle rightWingX rightWingY rightWingWidth rightWingHeight
            , initHitboxRectangle leftThrusterX leftThrusterY leftThrusterWidth leftThrusterHeight
            , initHitboxRectangle rightThrusterX rightThrusterY rightThrusterWidth rightThrusterHeight
            ]

-- ============================================================
-- =================== PLAYER INVARIANT =======================
-- ============================================================

instance Invariant Player where
    prop_inv :: Player -> Bool
    prop_inv = prop_inv_player 

-- ============================================================
-- =================== PLAYER RENDERABLE ======================
-- ============================================================

instance Renderable Player where
    getTranslatedAssets :: GameAssets -> Player -> [Picture]
    getTranslatedAssets ga player = 
        getTranslatedBoosterAssets ga player ++
        getTranslatedPlayerAssets ga player

-- Returns a list of translated player assets.
getTranslatedPlayerAssets :: GameAssets -> Player -> [Picture]
getTranslatedPlayerAssets ga player = 
    let po = playerObject player
        (px, py) = centerHitbox (objectHitbox po)
        h = objectHitbox po
    in [Translate px py (p1Pic ga)] ++ (translateHitbox h)

-- Returns a list of translated booster assets for boosters only enabled when moving with the right player direction.
getTranslatedBoosterAssets :: GameAssets -> Player -> [Picture]
getTranslatedBoosterAssets ga player = 
    let po = playerObject player
        (Direction dx dy) = objectDirection po
        (px, py) = centerHitbox (objectHitbox po)

        boosters = pBoosterPics ga
        leftB   = Seq.index boosters 0
        rightB  = Seq.index boosters 1
        topL    = Seq.index boosters 2
        topR    = Seq.index boosters 3
    in 
        (if dy > 0
            then [ Translate (px-16) (py-50) leftB
                , Translate (px+16) (py-50) rightB ]
            else [])
        ++
        (if dy < 0
            then [ Translate (px-25) (py+17) topL
                    , Translate (px+25) (py+17) topR ]
            else [])
        ++
        (if dx < 0
            then [ Translate (px+60) (py-10) $ Rotate 270 leftB]
            else [])
        ++
        (if dx > 0
            then [ Translate (px-60) (py-10) $ Rotate 90 rightB]
            else [])

prop_post_getTranslatedBoosterAssets :: GameAssets -> Player -> Bool
prop_post_getTranslatedBoosterAssets ga player = 
    let boosterPics = getTranslatedBoosterAssets ga player
    in (length boosterPics) == 0 || (length boosterPics) == 2 || (length boosterPics) == 3

-- ============================================================
-- =================== PLAYER COLLIDABLE ======================
-- ============================================================

instance Collidable Player where
    getObjects :: Player -> [Object]
    getObjects player = [playerObject player]

    collision :: Collidable b => Player -> b -> Bool
    collision player other =
        let objs1 = getObjects player
            objs2 = getObjects other
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) objs1

    willCollide :: Collidable b => Player -> b -> Float -> Bool  
    willCollide player other screenSpeed =
        let objs1 = getObjects player
            objs2 = getObjects other
            movedObjs1 = map (\o -> moveObject o screenSpeed) objs1
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) movedObjs1