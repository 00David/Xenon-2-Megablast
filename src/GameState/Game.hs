{-# LANGUAGE InstanceSigs #-}
module GameState.Game (module GameState.Game) where

import Graphics.Gloss

import System.Random
import qualified Control.Monad.State as St

import Keyboard
import GameSetup
import GameState.Enemy
import GameState.Player
import GameState.Projectile
import GameState.Rock
import GameState.Wall
import Graphics.Assets
import Graphics.Background
import Invariant
import Objects.Objects
import Objects.Hitbox

-- ============================================================
-- =================== GAME INITIALISATION ====================
-- ============================================================

type FrameCounter = Int

data Game = Game { 
    keyboard :: Keyboard,
    state :: GameState, -- StartMenuOption or InGameInfos
    assets :: GameAssets,
    background :: Background,
    frameCounter :: FrameCounter
} deriving Show

prop_inv_game :: Game -> Bool
prop_inv_game (Game _ st _ bgnd nbFrames) = prop_inv_gameState st && prop_inv_background bgnd
    && nbFrames >= 0 && nbFrames < maxFramesToConsider

instance Invariant Game where
    prop_inv :: Game -> Bool
    prop_inv = prop_inv_game 

initGame :: Keyboard -> GameState -> GameAssets -> Background -> FrameCounter -> Game
initGame kbd gs ga bgnd fCounter = Game kbd gs ga bgnd (fCounter `mod` maxFramesToConsider)

startInitGame :: IO Game
startInitGame = do 
    assts <- initGameAssets
    bgnd <- initStartBackground
    return $ initGame initKeyboard (initStartMenu Start) assts bgnd 0

-- ============================================================
-- ====================== GAMESTATE ===========================
-- ============================================================

data GameState = -- not a State monad (see InGameState at the end of the file)
    StartMenu StartMenuOption
    | InGame InGameInfos
    deriving (Show)

prop_inv_gameState :: GameState -> Bool
prop_inv_gameState (StartMenu _) = True
prop_inv_gameState (InGame igi) = prop_inv_ingameinfos igi

instance Invariant GameState where
    prop_inv :: GameState -> Bool
    prop_inv = prop_inv_gameState 

initStartMenu :: StartMenuOption -> GameState
initStartMenu option = StartMenu option

initInGame :: InGameInfos -> GameState
initInGame gameInfos = InGame gameInfos

-- Initializes the game at start, with the given player coordinates
startInitInGame :: StdGen -> Float -> Float -> Float -> Float -> Float -> Float -> GameState
startInitInGame gen xVirus yVirus xP1 yP1 xP2 yP2
    | xP1 - (widthPlayer / 2) < leftXScreenBound
        || xP1 + (widthPlayer / 2) > rightXScreenBound  = error "player1 x out of screen"
    | yP1 - (heightPlayer / 2) < bottomYScreenWithBarBound
        || yP1 + (heightPlayer / 2) > topYScreenBound = error "player1 y out of screen"
    | xP2 - (widthPlayer / 2) < leftXScreenBound
        || xP2 + (widthPlayer / 2) > rightXScreenBound  = error "player2 x out of screen"
    | yP2 - (heightPlayer / 2) < bottomYScreenWithBarBound
        || yP2 + (heightPlayer / 2) > topYScreenBound = error "player2 y out of screen"
    | xVirus - (widthVirus / 2) < leftXScreenBound
        || xVirus + (widthVirus / 2) > rightXScreenBound  = error "virus x out of screen"
    | yVirus - (heightVirus / 2) < bottomYScreenWithBarBound
        || yVirus + (heightVirus / 2) > topYScreenBound = error "virus y out of screen"
    | otherwise =
        let newP1o = initPlayerObject xP1 yP1 (initDirection 0 0) (initObjectSpeed 0)
            newP1 = initAlivePlayer newP1o 1 3 100 0 playerDefaultShootDelay
            newP2o = initPlayerObject xP2 yP2 (initDirection 0 0) (initObjectSpeed 0)
            newP2 = initDeadPlayer newP2o 2 0 1
            newVo = initStaticEnemyRectangleObject xVirus yVirus
            newV = initEnemy newVo 1
            listEnemies = [newV]
            walls = startInitGameWalls gen
        in initInGame (initInGameInfos newP1 newP2 listEnemies walls [])

prop_pre_startInitInGame :: Picture -> Picture -> Float -> Float -> Float -> Float -> Float -> Float -> Bool
prop_pre_startInitInGame _ _ xVirus yVirus xP1 yP1 xP2 yP2 =
    
    -- Player 1
    xP1 - (widthPlayer / 2) >= leftXScreenBound &&
    xP1 + (widthPlayer / 2) <= rightXScreenBound &&
    yP1 - (heightPlayer / 2) >= bottomYScreenWithBarBound &&
    yP1 + (heightPlayer / 2) <= topYScreenBound &&

    -- Player 2
    xP2 - (widthPlayer / 2) >= leftXScreenBound &&
    xP2 + (widthPlayer / 2) <= rightXScreenBound &&
    yP2 - (heightPlayer / 2) >= bottomYScreenWithBarBound &&
    yP2 + (heightPlayer / 2) <= topYScreenBound &&

    -- Virus
    xVirus - (widthVirus / 2) >= leftXScreenBound &&
    xVirus + (widthVirus / 2) <= rightXScreenBound &&
    yVirus - (heightVirus / 2) >= bottomYScreenWithBarBound &&
    yVirus + (heightVirus / 2) <= topYScreenBound

-- ============================================================
-- =================== START MENU OPTION ======================
-- ============================================================

data StartMenuOption = Start | Option2
    deriving (Show, Eq)

-- ============================================================
-- ====================== IN GAME INFOS =======================
-- ============================================================

data InGameInfos = InGameInfos {
        gamePlayer1 :: Player,
        gamePlayer2 :: Player,
        gameEnemies :: [Enemy],
        gameWalls :: GameWalls,
        gameProjectiles :: [Projectile]
    } deriving (Show)

prop_inv_ingameinfos :: InGameInfos -> Bool
prop_inv_ingameinfos (InGameInfos p1 p2 enemies walls projs) = prop_inv_player p1 && prop_inv_player p2
    && foldr (\e acc -> prop_inv_enemy e && acc) True enemies
    && prop_inv_gameWalls walls && all prop_inv_projectile projs

instance Invariant InGameInfos where
    prop_inv :: InGameInfos -> Bool
    prop_inv = prop_inv_ingameinfos 

instance Renderable InGameInfos where
    getTranslatedAssets :: GameAssets -> InGameInfos -> [Picture]
    getTranslatedAssets ga (InGameInfos player1 player2 enemies walls projs) =
        (getTranslatedAssets ga walls) ++ (getTranslatedAssets ga player1) ++
        concatMap (getTranslatedAssets ga) enemies ++ 
        concatMap (getTranslatedAssets ga) projs

initInGameInfos :: Player -> Player -> [Enemy] -> GameWalls ->[Projectile] -> InGameInfos
initInGameInfos player1 player2 enemies walls projs = 
    InGameInfos player1 player2 enemies walls projs

-- Indicates if everything else than a player is the same for 2 given InGameInfos 
sameInGameInfosExceptPlayer :: Bool -> InGameInfos -> InGameInfos -> Bool
sameInGameInfosExceptPlayer isP1 (InGameInfos p1 p2 enemies gw projs) (InGameInfos p1' p2' enemies' gw' projs') =
    if isP1 
        then p2 == p2' && enemies == enemies' && gw == gw' && projs == projs'
        else p1 == p1' && enemies == enemies' && gw == gw' && projs == projs'

-- Sets player new given direction and speed
updatePlayerDirectionSpeed :: Bool -> (Direction, ObjectSpeed) -> InGameInfos -> ((), InGameInfos)
updatePlayerDirectionSpeed isP1 (newDir, newOS) (InGameInfos p1 p2 enemies gw projs) =
    let p = if isP1 then p1 else p2
        po = playerObject p
        (px, py) = centerHitbox (objectHitbox po)
        
        newPo = initPlayerObject px py newDir newOS
        newP = initAlivePlayer newPo (playerId p) (playerLifes p) (playerHealth p) (playerScore p) (playerShootDelay p)
    in 
        if isP1 then ((), initInGameInfos newP p2 enemies gw projs)
        else ((), initInGameInfos p1 newP enemies gw projs)

prop_pre_updatePlayerDirectionSpeed :: Bool -> (Direction, ObjectSpeed) -> InGameInfos -> Bool
prop_pre_updatePlayerDirectionSpeed isP1 _ (InGameInfos p1 p2 _ _ _) =
    let p = if isP1 then p1 else p2
    in not (isPlayerDead p)

prop_post_updatePlayerDirectionSpeed :: Bool -> (Direction, ObjectSpeed) -> InGameInfos -> Bool
prop_post_updatePlayerDirectionSpeed isP1 (dir, os) igi@(InGameInfos _ _ _ _ _) =
    let (_, igi'@(InGameInfos p1' p2' _ _ _)) = (updatePlayerDirectionSpeed isP1 (dir, os) igi)
        po1' = (playerObject p1')
        po2' = (playerObject p2')
        dir1' = (objectDirection po1')
        dir2' = (objectDirection po2')
        os1' = (objectSpeed po1')
        os2' = (objectSpeed po2')
    in (sameInGameInfosExceptPlayer isP1 igi igi') 
        && if isP1
            then not (isPlayerDead p1') && dir == dir1' && os == os1'
            else not (isPlayerDead p2') && dir == dir2' && os == os2'


-- Moves the player, according to its current object direction and speed
movePlayer :: Bool -> InGameInfos -> ((), InGameInfos)
movePlayer isP1 igi@(InGameInfos p1 p2 enemies walls projs) =
    let p = if isP1 then p1 else p2
        po = playerObject p
        (Direction dirX dirY) = objectDirection po
        (ObjectSpeed s) = objectSpeed po
        dxp = (fromIntegral dirX)*s -- player movement, for x, got from direction and speed
        dyp = (fromIntegral dirY)*s -- player movement, for y, got from direction and speed
        (px, py) = centerHitbox (objectHitbox po)

        newPX = px + dxp
        newPY = py + dyp
        leftBound = leftXScreenBound + (widthPlayer / 2)
        rightBound = rightXScreenBound - (widthPlayer / 2)
        topBound = topYScreenBound - (heightPlayer / 2)
        bottomBound = bottomYScreenWithBarBound + (heightPlayer / 2)

        -- try indepedently to move its object in X and Y
        objMoveX = initMovableObject (objectHitbox po) (Direction dirX 0) (objectSpeed po)
        objMoveY = initMovableObject (objectHitbox po) (Direction 0 dirY) (objectSpeed po)

        collideWallsX = willCollide objMoveX walls screenDefaultSpeed
        collideWallsY = willCollide objMoveY walls screenDefaultSpeed

        insideScreenX = newPX >= leftBound && newPX <= rightBound
        insideScreenY = newPY >= bottomBound && newPY <= topBound

        -- Directions can become 0 if it brings out of screen bounds or leads to a wall collision
        newDirX = if insideScreenX && not collideWallsX then dirX else 0
        newDirY = if insideScreenY && not collideWallsY then dirY else 0

    in if (newDirX /= 0 || newDirY /= 0) 
        then
            let newD = (initDirection newDirX newDirY)
                newPo1 = (initMovableObject (objectHitbox po) newD (objectSpeed po)) -- player object with new direction
                newPo2 = moveObject newPo1 screenDefaultSpeed -- player object with its hitbox having a new position
                newP = initAlivePlayer newPo2 (playerId p) (playerLifes p) (playerHealth p) (playerScore p) (playerShootDelay p)
            in 
                if isP1 then ((), initInGameInfos newP p2 enemies walls projs)
                else ((), initInGameInfos p1 newP enemies walls projs)
        else ((), igi)

prop_pre_movePlayer :: Bool -> InGameInfos -> Bool
prop_pre_movePlayer isP1 (InGameInfos p1 p2 _ _ _) = 
    let p = if isP1 then p1 else p2
    in not (isPlayerDead p)

prop_post_movePlayer :: Bool -> InGameInfos -> Bool
prop_post_movePlayer isP1 igi@(InGameInfos _ _ _ _ _) =
    let (_, igi'@(InGameInfos p1' p2' _ _ _)) = movePlayer isP1 igi
    in (sameInGameInfosExceptPlayer isP1 igi igi') && 
        if isP1
            then not (isPlayerDead p1') -- insideScreenPlayer p1' checked by the player invariant
            else not (isPlayerDead p2') -- insideScreenPlayer p2' checked by the player invariant


-- Sorts ennemies by keeping those alive, if they have collided with the given player + returns the number of collisions
keepAliveEnemies :: Player -> [Enemy] -> Int -> ([Enemy], Int)
keepAliveEnemies _ [] nbColls = ([], nbColls)
keepAliveEnemies player (enemy:xs) nbColls
    | collision player enemy && (enemyHealth enemy) == 1 = (keepAliveEnemies player xs (nbColls+1)) -- if the collided ennemy has 1 health -> HE IS DEAD
    | collision player enemy = -- if the collided ennemy has more than 1 health, he is kept whith a decreased health
        let newEnemy = initEnemy (enemyObject enemy) ((enemyHealth enemy)-1)
            (enemiesRec, nbCollsRec) = (keepAliveEnemies player xs nbColls)
        in (newEnemy:enemiesRec, nbCollsRec+1)
    | otherwise = -- no collision
        let (enemiesRec, nbCollsRec) = (keepAliveEnemies player xs nbColls)
        in (enemy:enemiesRec, nbCollsRec)

prop_pre_keepAliveEnemies :: Player -> [Enemy] -> Int -> Bool
prop_pre_keepAliveEnemies _ _ nbColls = nbColls == 0

prop_post_keepAliveEnemies :: Player -> [Enemy] -> Int -> Bool
prop_post_keepAliveEnemies player enemies nbColls =
    let (_, resNbColls) = keepAliveEnemies player enemies nbColls
    in resNbColls >= nbColls


-- Decreases enemies health if they collide with the player, once an enemy has no health (=0), he is deleted from the game infos
handleCollisionPlayerWithEnemies :: Bool -> InGameInfos -> ((), InGameInfos)
handleCollisionPlayerWithEnemies  isP1 (InGameInfos p1 p2 enemies gw projs) = 
    let player = if isP1 then p1 else p2
        (newEnemies, collisions) = keepAliveEnemies player enemies 0
        po = (playerObject player)
        pId = playerId player

        newHealth = (playerHealth player) - collisions*10
        newScore = (playerScore player) + collisions*47
        
        -- if the new health reaches 0 or less, a life is decreased and the health is reseted at 100
        newLifes = if newHealth <= 0 then ((playerLifes player)-1) else (playerLifes player)
        newHealth2 = if newHealth <= 0 && newLifes > 0 then 100 else newHealth

        -- if the new life counter is strictly negative : the player becomes dead
        newP = if newLifes <= 0 then (initDeadPlayer po pId newScore 1) else (initAlivePlayer po pId newLifes newHealth2 newScore (playerShootDelay player))
    in 
        if isP1 then ((), initInGameInfos newP p2 newEnemies gw projs)
        else ((), initInGameInfos p1 newP newEnemies gw projs)

prop_pre_handleCollisionPlayerWithEnemies :: Bool -> InGameInfos -> Bool
prop_pre_handleCollisionPlayerWithEnemies isP1 (InGameInfos p1 p2 _ _ _) =
    let p = if isP1 then p1 else p2
    in not (isPlayerDead p)

prop_post_handleCollisionPlayerWithEnemies :: Bool -> InGameInfos -> Bool
prop_post_handleCollisionPlayerWithEnemies isP1 igi@(InGameInfos p1 p2 enemies gw projs) =
    let (_, (InGameInfos p1' p2' enemies' gw' projs')) = (handleCollisionPlayerWithEnemies isP1 igi)
    in if isP1 
        then (length enemies') <= (length enemies) && (playerHealth p1) >= (playerHealth p1') 
            && (playerScore p1) <= (playerScore p1') && p2 == p2' && gw == gw' && projs == projs'
        else (length enemies') <= (length enemies) && (playerHealth p2) >= (playerHealth p2') 
            && (playerScore p2) <= (playerScore p2') && p1 == p1' && gw == gw' && projs == projs'


-- Moves walls according to current screen scrolling speed
moveWalls :: InGameInfos -> ((), InGameInfos)
moveWalls (InGameInfos p1 p2 enemies (GameWalls left1 left2 right1 right2 walls) projs) =
    let 
        newLeft1 = partialFilterWall (insideScreenRock True) (partialMapWall moveRock left1)
        newLeft2 = partialFilterWall (insideScreenRock False) (partialMapWall moveRock left2)
        newRight1 = partialFilterWall (insideScreenRock True) (partialMapWall moveRock right1)
        newRight2 = partialFilterWall (insideScreenRock False) (partialMapWall moveRock right2)
        newWalls = filter (not . null) (fmap (\w -> filterWall (insideScreenRock  True) (fmap moveRock w)) walls)

        newGameWalls = (initGameWalls newLeft1 newLeft2 newRight1 newRight2 newWalls)
    in 
        ((), initInGameInfos p1 p2 enemies newGameWalls projs)
    where 
        moveRock :: Rock -> Rock
        moveRock r = initRock (moveObject (rockObject r) screenDefaultSpeed) (rockAsset r) (rockLeftSide r)

prop_post_moveWalls :: InGameInfos -> Bool
prop_post_moveWalls igi@(InGameInfos p1 p2 ennemies _ projs) =
    let (_, (InGameInfos p1' p2' ennemies' 
            (GameWalls (InfiniteWall left1Rocks') (InfiniteWall left2Rocks') 
            (InfiniteWall right1Rocks') (InfiniteWall right2Rocks') walls') projs')) = moveWalls igi
    in 
        all (insideScreenRock True) (take nbTakeInfiniteWalls left1Rocks')
        && all (insideScreenRock False) (take nbTakeInfiniteWalls left2Rocks')
        && all (insideScreenRock True) (take nbTakeInfiniteWalls right1Rocks')
        && all (insideScreenRock False) (take nbTakeInfiniteWalls right2Rocks')
        && all (all (insideScreenRock True)) walls'
        && p1 == p1' && p2 == p2' && ennemies == ennemies' && projs == projs'

-- Bumps the player from a wall
bumpPlayerFromWalls :: Bool -> InGameInfos -> ((), InGameInfos)
bumpPlayerFromWalls isP1 igi@(InGameInfos p1 p2 enemies gw projs) =
    let p = if isP1 then p1 else p2
        po = playerObject p
        h = objectHitbox po
        (x,y) = centerHitbox h
        bottomYPlayer = y-(heightPlayer/2)
    in 
        -- if there is no collision, no state change
        if not (collision p gw) then ((), igi)
            
        -- if there is a collision, 2 cases :
        else 
            -- if the player, after the vertical bump, would still be in the screen : vertical bump
            if (bottomYPlayer-screenDefaultSpeed) >= bottomYScreenWithBarBound then 
                let newPo = initPlayerObject x (y-screenDefaultSpeed) (objectDirection po) (objectSpeed po)
                    newP = initAlivePlayer newPo (playerId p) (playerLifes p) (playerHealth p) (playerScore p) (playerShootDelay p)
                in 
                    if isP1 then ((), initInGameInfos newP p2 enemies gw projs)
                    else ((), initInGameInfos p1 newP enemies gw projs)

            -- if the player, after the vertical bump, would be outside of the screen : horizontal bump
            else 
                let bumpToLeft = x > 0
                    newPo = keepBumpin bumpToLeft x y po
                    newP = initAlivePlayer newPo (playerId p) (playerLifes p) (playerHealth p) (playerScore p) (playerShootDelay p)
                in 
                    if isP1 then ((), initInGameInfos newP p2 enemies gw projs)
                    else ((), initInGameInfos p1 newP enemies gw projs)
                where 
                    -- Repeats horizontal bumps until not having player's object colliding with a wall
                    keepBumpin :: Bool -> Float -> Float -> Object -> Object
                    keepBumpin toLeft x y obj =
                        let xOffset = if toLeft then (x-screenDefaultSpeed) else (x+screenDefaultSpeed)
                            objAfterBump = (initPlayerObject xOffset y (objectDirection obj) (objectSpeed obj))
                        in
                            if not (collision objAfterBump gw) then objAfterBump
                            else keepBumpin toLeft xOffset y objAfterBump

prop_pre_bumpPlayerFromWalls :: Bool -> InGameInfos -> Bool
prop_pre_bumpPlayerFromWalls isP1 (InGameInfos p1 p2 _ _ _) =
    let p = if isP1 then p1 else p2
    in not (isPlayerDead p)

prop_post_bumpPlayerFromWalls :: Bool -> InGameInfos -> Bool
prop_post_bumpPlayerFromWalls isP1 igi@(InGameInfos p1 p2 enemies gw projs) =
    let (_, (InGameInfos p1' p2' enemies' gw' projs')) = (bumpPlayerFromWalls isP1 igi)
    in if isP1 
        then not (isPlayerDead p1') &&  not (collision p1' gw')
            && p2 == p2' && enemies == enemies' && gw == gw' && projs == projs'
        else not (isPlayerDead p2') &&  not (collision p2' gw')
            && p1 == p1' && enemies == enemies' && gw == gw' && projs == projs'

-- ============================================================
-- ====================== IN GAME STATE =======================
-- ============================================================

type InGameState a = St.State InGameInfos a -- IN GAME STATE MONAD

-- MAIN IN GAME UPDATE FUNCTION
updateInGame :: Keyboard -> Float -> InGameState ()
updateInGame kbd deltaTime = do
    igi <- St.get

    -- move walls
    moveWallsSt

    let p1Dead = isPlayerDead (gamePlayer1 igi)
        p2Dead = isPlayerDead (gamePlayer2 igi)

    if not p1Dead
      then do
        -- bump the player1 from walls if needed
        bumpPlayerFromWallsSt True
        -- update player1 direction and speed
        updatePlayerDirectionSpeedSt True (player1NewDirectionSpeed kbd deltaTime)
        -- move player1
        movePlayerSt True
        -- handle collisions between player1 and enemies
        handleCollisionPlayerWithEnemiesSt True
      else return ()

    if not p2Dead
      then do
        -- bump the player2 from walls if needed
        bumpPlayerFromWallsSt True
        -- update player2 direction and speed
        updatePlayerDirectionSpeedSt False (player2NewDirectionSpeed kbd deltaTime)
        -- move player2
        movePlayerSt False
        -- handle collisions between player2 and enemies
        handleCollisionPlayerWithEnemiesSt False
      else return ()

prop_pre_updateInGame :: Keyboard -> Float -> Bool
prop_pre_updateInGame _ deltaTime = deltaTime >= 0

moveWallsSt :: InGameState ()
moveWallsSt = St.state moveWalls

bumpPlayerFromWallsSt :: Bool -> InGameState ()
bumpPlayerFromWallsSt isP1 = St.state (bumpPlayerFromWalls isP1)

updatePlayerDirectionSpeedSt :: Bool -> (Direction, ObjectSpeed) -> InGameState ()
updatePlayerDirectionSpeedSt isP1 ds = St.state (updatePlayerDirectionSpeed isP1 ds)

movePlayerSt :: Bool -> InGameState ()
movePlayerSt isP1 = St.state (movePlayer isP1)

handleCollisionPlayerWithEnemiesSt :: Bool -> InGameState ()
handleCollisionPlayerWithEnemiesSt isP1 = St.state (handleCollisionPlayerWithEnemies isP1)