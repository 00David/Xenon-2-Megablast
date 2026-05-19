{-# LANGUAGE InstanceSigs #-}
module GameState.Player (module GameState.Player) where

import Debug.Trace (trace)

import Graphics.Gloss (Picture(Translate, Rotate))

import qualified Data.Sequence as Seq

import GameSetup
import GameState.Projectile
import Graphics.Assets
import Objects.Hitbox
import Objects.Objects
import Typeclasses.Damageable
import Typeclasses.Invariant
import Typeclasses.Movable

-- ============================================================
-- ========================= PLAYER ===========================
-- ============================================================

--type PlayerId = Int -- player id, 1 or 2
type Lifes = Int -- player remaining lifes, inside of [1, 3]
--type Health = Int -- health for the current player life, inside of ]0, 100]
--type Score = Int -- player current score, positive
--type ShootDelay = Int -- player shooting delay (in number of frames/second), >= 1, reseted at 1
--type FrameCounter = Int -- frame counter of the current explosion phase, inside of [1, 10]
--type ExplosionAnim = Int -- player current explosion animation sprite, inside of [0, 7]

data Player = AliveP Object PlayerId Lifes Health Score ShootDelay
    | DeadP Object PlayerId Score FrameCounter ExplosionAnim
    deriving (Eq, Show)

prop_inv_player :: Player -> Bool
prop_inv_player p@(AliveP po pId lifes health score shootD) = prop_inv_object po && (pId == 1 || pId == 2) && lifes >= 1 && lifes <= 3
    && health >= 1 && health <= 100 && score >= 0 && insideScreenPlayer p && shootD >= 1
prop_inv_player p@(DeadP po pId score frameCpt phase) = prop_inv_object po && (pId == 1 || pId == 2) && score >= 0
    && frameCpt >= 1 && frameCpt <= nbFramesPerExplosionPhase && phase >= 0 && phase <= nbPlayerExplosionAssets && insideScreenPlayer p

initPlayerObject :: Float -> Float -> Direction -> ObjectSpeed -> Object
initPlayerObject x y dir speed = 
    (initMovableObject 
        (playerHitbox x y)
        dir
        speed
    )

initAlivePlayer :: Object -> PlayerId -> Lifes -> Health -> Score -> ShootDelay -> Player
initAlivePlayer po pId lifes health score shootD
    | lifes < 0 || lifes > 3 = error "number of lifes outside of [0, 3], must be inside it"
    | health < 0 || health > 100 = error "current life health outside of [0, 100], must be inside it"
    | score < 0 = error "score must be positive"
    | shootD < 1 = error "shoot delay must be >= 1"
    | otherwise = AliveP po pId lifes health score shootD

initDeadPlayer :: Object -> PlayerId -> Score ->  FrameCounter -> ExplosionAnim -> Player
initDeadPlayer po pId score frameCpt phase
    | score < 0 = error "score must be positive"
    | frameCpt < 1 || frameCpt > nbFramesPerExplosionPhase = error "frame counter must be inside of [1, 10]"
    | phase < 0 || phase > nbPlayerExplosionAssets = error "animation phase must be inside of [0, 6]" 
    | otherwise = DeadP po pId score frameCpt phase

playerObject :: Player -> Object
playerObject (AliveP o _ _ _ _ _) = o
playerObject (DeadP o _ _ _ _) = o

playerLifes :: Player -> Lifes
playerLifes (AliveP _ _ l _ _ _) = l
playerLifes (DeadP _ _ _ _ _) = 0

playerHealth :: Player -> Health
playerHealth (AliveP _ _ _ h _ _) = h
playerHealth (DeadP _ _ _ _ _) = 0

playerScore :: Player -> Score
playerScore (AliveP _ _ _ _ s _) = s
playerScore (DeadP _ _ s _ _) = s

updatePlayerObject :: Player -> Object -> Player
updatePlayerObject (AliveP _ pId lifes health score shootD) newPo = initAlivePlayer newPo pId lifes health score shootD
updatePlayerObject (DeadP _ pId score frameCpt phase) newPo = initDeadPlayer newPo pId score frameCpt phase

prop_post_updatePlayerObject :: Player -> Object -> Bool
prop_post_updatePlayerObject p newPo =
    let newP = updatePlayerObject p newPo
    in case (p, newP) of
        ((AliveP _ pId lifes health score shootD), (AliveP obj' pId' lifes' health' score' shootD')) ->
            obj' == newPo && pId' == pId && lifes' == lifes && health' == health && score' == score && shootD' == shootD
        ((DeadP _ pId score frameCpt phase), (DeadP obj' pId' score' frameCpt' phase')) ->
            obj' == newPo && pId' == pId && score' == score && frameCpt' == frameCpt && phase' == phase
        (_,_)-> False

addScore :: Score -> Player -> Player
addScore s (AliveP obj pId lifes health score shootD) = initAlivePlayer obj pId lifes health (score+s) shootD
addScore s (DeadP obj pId score frameCpt phase) = initDeadPlayer obj pId (score+s) frameCpt phase

prop_pre_addScore :: Score -> Player -> Bool
prop_pre_addScore s _ = s >= 0

prop_post_addScore :: Score -> Player -> Bool
prop_post_addScore s p =
    let p' = addScore s p
    in case (p, p') of
        (AliveP obj1 pId1 lifes1 health1 score1 shootD1,
         AliveP obj2 pId2 lifes2 health2 score2 shootD2) ->
                obj1 == obj2
            && pId1 == pId2
            && lifes1 == lifes2
            && health1 == health2
            && score2 == score1 + s
            && shootD1 == shootD2

        (DeadP obj1 pId1 score1 frameCpt1 phase1,
         DeadP obj2 pId2 score2 frameCpt2 phase2) ->
                obj1 == obj2
            && pId1 == pId2
            && score2 == score1 + s
            && frameCpt1 == frameCpt2
            && phase1 == phase2
        _ -> False

-- Indicates if a player is dead. Otherwise he is alive.
isPlayerDead :: Player -> Bool
isPlayerDead (AliveP _ _ _ _ _ _) = False
isPlayerDead (DeadP _ _ _ _ _) = True

-- Moves a player, with X and Y indepedant directions : if one of those directions leads outside of the screen, 
-- the movement in the concerned direction will be independantly canceled.
-- Don't take into account walls, just screen borders with the bottom bar too.
movePlayer :: Player -> ScreenScrollingSpeed -> Player
movePlayer p@(DeadP _ _ _ _ _) _ = p
movePlayer p@(AliveP po pId lifes health score shootDelay) ss =
    let 
        (Direction dirX dirY) = objectDirection po
        (ObjectSpeed s) = objectSpeed po
        (x, y) = centerHitbox (objectHitbox po)
        
        -- Calculate potential new positions after movement
        dx = (fromIntegral dirX) * s
        dy = (fromIntegral dirY) * s
        newX = x + dx
        newY = y + dy
        
        -- Screen bounds
        leftBound = leftXScreenBound + (widthPlayer / 2)
        rightBound = rightXScreenBound - (widthPlayer / 2)
        topBound = topYScreenBound - (heightPlayer / 2)
        bottomBound = bottomYScreenWithBarBound + (heightPlayer / 2)
        
        -- Check each direction independently
        xInsideAfter = newX >= leftBound && newX <= rightBound
        yInsideAfter = newY >= bottomBound && newY <= topBound
        
        -- Keep only valid directions
        finalDirX = if xInsideAfter then dirX else 0
        finalDirY = if yInsideAfter then dirY else 0
        
    in if (finalDirX /= 0 || finalDirY /= 0)
        then
            let 
                finalDirection = initDirection finalDirX finalDirY
                newPo = initMovableObject (objectHitbox po) finalDirection (objectSpeed po)
                movedPo = moveObject newPo ss  -- UN SEUL APPEL à moveObject, pas de récursion
            in initAlivePlayer movedPo pId lifes health score shootDelay
        else p

prop_pre_movePlayer :: Player -> ScreenScrollingSpeed -> Bool
prop_pre_movePlayer p _ = insideScreenPlayer p

prop_post_movePlayer :: Player -> ScreenScrollingSpeed -> Bool
prop_post_movePlayer p@(DeadP _ _ _ _ _) ss = 
    let newP = movePlayer p ss
    in p == newP
prop_post_movePlayer p@(AliveP po pId lifes health score shootD) ss = 
    let newP = movePlayer p ss
    in case newP of
        newPP@(AliveP po' pId' lifes' health' score' shootD') -> 
            insideScreenPlayer newPP && po' == po && pId' == pId && lifes' == lifes && health' == health && score' == score && shootD' == shootD
        _ -> False

-- Indicates if a player is inside the screen (by considering the bottom bar as the bottom limit of the screen)
insideScreenPlayer :: Player -> Bool
insideScreenPlayer p = 
    let leftBound = leftXScreenBound + (widthPlayer / 2)
        rightBound = rightXScreenBound - (widthPlayer / 2)
        topBound = topYScreenBound - (heightPlayer / 2)
        bottomBound = bottomYScreenWithBarBound + (heightPlayer / 2)
        (x,y) = centerHitbox (objectHitbox (playerObject p))
    in  x >= leftBound && x <= rightBound && y >= bottomBound && y <= topBound

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

playerShot :: Player-> Maybe Projectile
playerShot (AliveP po pId _ _ _ _) =
    let (x,y) = centerHitbox (objectHitbox po)
        dir = initDirection 0 1
        s = initObjectSpeed playerDefaultShotSpeed
        assetIndex = 0
        projO = (playerShotObject dir s x (y+50) assetIndex)
    in Just (initPlayerShot projO assetIndex playerDefaultShotDamage pId)
playerShot (DeadP _ _ _ _ _) = Nothing

-- Run the dead player explosion animation and returns the updated player : either update him, or does nothing if not dead or animation is finished
runPlayerExplosion :: Player -> Player
runPlayerExplosion p@(AliveP _ _ _ _ _ _) = p
runPlayerExplosion (DeadP po pId score frameCpt phase)
    | phase == nbPlayerExplosionAssets = (initDeadPlayer po pId score frameCpt phase) -- dont't change anything if the explosion animation has been done (phase == nbPlayerExplosionAssets)
    | frameCpt < nbFramesPerExplosionPhase = (initDeadPlayer po pId score (frameCpt+1) phase) -- increments the frames counter if limit not reached (nbFramesPerExplosionPhase)
    | frameCpt == nbFramesPerExplosionPhase = (initDeadPlayer po pId score 1 (phase+1)) -- once the frames limit has been reached, reset the frame counter and go to the next explosion phase
    | otherwise = error $ "impossible case "++(show frameCpt)++" "++(show phase)

prop_post_runPlayerExplosion :: Player -> Bool
prop_post_runPlayerExplosion p =
    let p' = runPlayerExplosion p
    in case (p, p') of
        -- Alive stays the same
        (AliveP obj1 id1 l1 h1 s1 d1,
         AliveP obj2 id2 l2 h2 s2 d2) ->
            obj1 == obj2 && id1 == id2 && l1 == l2 && h1 == h2 && s1 == s2 && d1 == d2
        -- Dead : verify the correct evolution of the animation
        (DeadP obj1 id1 s1 frameCpt1 phase1,
         DeadP obj2 id2 s2 frameCpt2 phase2) ->
            obj1 == obj2 && id1 == id2 && s2 == s1
            && (
                (phase1 == nbPlayerExplosionAssets && frameCpt2 == frameCpt1 && phase2 == phase1)
             || (frameCpt1 < nbFramesPerExplosionPhase && frameCpt2 == frameCpt1 + 1 && phase2 == phase1)
             || (frameCpt1 == nbFramesPerExplosionPhase && frameCpt2 == 1 && phase2 == phase1 + 1)
               )
        _ -> False

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
        if not (isPlayerDead player) then
            getTranslatedBoosterAssets ga player ++
            getTranslatedPlayerAssets ga player
        else getTranslatedPlayerAssets ga player -- translate only the potential animation if dead

-- Returns a list of translated player assets.
getTranslatedPlayerAssets :: GameAssets -> Player -> [Picture]
getTranslatedPlayerAssets ga (AliveP po pId _ _ _ _) = 
    let (px, py) = centerHitbox (objectHitbox po)
        h = objectHitbox po
        pic = if pId == 1 then (p1Pic ga) else (p2Pic ga)
    in [Translate px py pic] ++ (translateHitbox h)
getTranslatedPlayerAssets ga (DeadP po pId _ _ phase) =
    let (px, py) = centerHitbox (objectHitbox po)
    in 
        if phase >= 0 && phase < nbPlayerExplosionAssets 
            then 
                if pId == 1 then [Translate px py (Seq.index (p1ExplosionPics ga) phase)]
                else [Translate px py (Seq.index (p2ExplosionPics ga) phase)]
        else [] -- when the animation has been done, no more rendering (phase == nbPlayerExplosionAssets)


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
-- ===================== PLAYER MOVABLE =======================
-- ============================================================

instance Movable Player where
    move :: Player -> ScreenScrollingSpeed -> Player
    move = movePlayer

    insideScreen :: Player -> Bool
    insideScreen = insideScreenPlayer

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

    willCollide :: Collidable b => Player -> b -> ScreenScrollingSpeed -> Bool  
    willCollide player other screenSpeed =
        let objs1 = getObjects player
            objs2 = getObjects other
            movedObjs1 = map (\o -> moveObject o screenSpeed) objs1
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) movedObjs1

-- ============================================================
-- ==================== PLAYER DAMAGEABLE ======================
-- ============================================================

instance Damageable Player where
    currentHealth :: Player -> Maybe Health
    currentHealth (AliveP _ _ _ h _ _) = Just h
    currentHealth (DeadP _ _ _ _ _) = Nothing

    takeDamage :: Damage -> Player -> Maybe Player
    takeDamage d (AliveP obj pId lifes health score shootD) =
        let newHealth = health-d
        in
            if newHealth > 0 then Just (initAlivePlayer obj pId lifes newHealth score shootD)
            else 
                let newLifes = lifes-1
                in 
                    if newLifes > 0 then Just (initAlivePlayer obj pId newLifes 100 score shootD) -- health restored at 100
                    else Just (initDeadPlayer obj pId score 1 0) -- dead
    takeDamage _ dead@(DeadP _ _ _ _ _) = Just dead