module GameState.Game (module GameState.Game) where

import Graphics.Gloss

import qualified Control.Monad.State as St

import Keyboard
import GameSetup
import GameState.Player
import GameState.Enemy
import Graphics.Assets
import Graphics.Background
import Objects.Objects
import Objects.Hitbox

import Debug.Trace

-- ============================================================
-- =================== GAME INITIALISATION ====================
-- ============================================================

data Game = Game { 
    keyboard :: Keyboard,
    state :: GameState, -- StartMenuOption or InGameInfos
    assets :: GameAssets,
    background :: Background
} deriving Show

prop_inv_game :: Game -> Bool
prop_inv_game (Game _ st _ bgnd) = prop_inv_gameState st && prop_inv_background bgnd

initGame :: Keyboard -> GameState -> GameAssets -> Background -> Game
initGame kbd gs ga bgnd = Game kbd gs ga bgnd

initStartGame :: IO Game
initStartGame = do 
    assts <- initGameAssets
    bgnd <- initStartBackground
    return $ initGame initKeyboard (initStartMenu Start) assts bgnd

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

initStartMenu :: StartMenuOption -> GameState
initStartMenu option = StartMenu option

initInGame :: InGameInfos -> GameState
initInGame gameInfos = InGame gameInfos

-- Initializes the game at start, with the given player coordinates
startInitInGame :: Picture -> Picture -> Float -> Float -> Float -> Float -> Float -> Float -> GameState
startInitInGame picPlayer picVirus xVirus yVirus xP1 yP1 xP2 yP2
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
        let newP1o = initPlayerObject picPlayer xP1 yP1 (initDirection 0 0) (initObjectSpeed 0)
            newP1 = initAlivePlayer newP1o 3 100 0
            newP2o = initPlayerObject picPlayer xP2 yP2 (initDirection 0 0) (initObjectSpeed 0)
            newP2 = initDeadPlayer newP2o 0 1
            newVo = initStaticEnemyRectangleObject picVirus xVirus yVirus
            newV = initEnemy newVo 1
            listEnemies = [newV]
        in initInGame (initInGameInfos newP1 newP2 listEnemies)

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
        gameEnemies :: [Enemy]
    } deriving (Show)

prop_inv_ingameinfos :: InGameInfos -> Bool
prop_inv_ingameinfos (InGameInfos p1 p2 enemies) = prop_inv_player p1 && prop_inv_player p2
    && foldr (\e acc -> prop_inv_enemy e && acc) True enemies

initInGameInfos :: Player -> Player -> [Enemy] -> InGameInfos
initInGameInfos player1 player2 enemies = InGameInfos player1 player2 enemies

-- Sets player new given direction and speed
updatePlayerDirectionSpeed :: Bool -> (Direction, ObjectSpeed) -> InGameInfos -> ((), InGameInfos)
updatePlayerDirectionSpeed isP1 (newDir, newOS) (InGameInfos p1 p2 enemies) =
    let p = if isP1 then p1 else p2
        po = playerObject p
        picP = objectPicture po
        (px, py) = centerHitbox (objectHitbox po)
        
        newPo = initPlayerObject picP px py newDir newOS
        newP = initAlivePlayer newPo (playerLifes p) (playerHealth p) (playerScore p)
    in 
        if isP1 then ((), initInGameInfos newP p2 enemies)
        else ((), initInGameInfos p1 newP enemies)

prop_pre_updatePlayerDirectionSpeed :: Bool -> (Direction, ObjectSpeed) -> InGameInfos -> Bool
prop_pre_updatePlayerDirectionSpeed isP1 _ (InGameInfos p1 p2 _) =
    let p = if isP1 then p1 else p2
    in not (isPlayerDead p)

-- Moves the player, according to its current object direction and speed
movePlayer :: Bool -> InGameInfos -> ((), InGameInfos)
movePlayer isP1 igi@(InGameInfos p1 p2 enemies) =
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

        -- tests X independantly, X direction can become 0 if it brings out of screen bounds
        newDirX = if newPX >= leftBound && newPX <= rightBound then dirX else 0
        -- tests Y independantly, Y direction can become 0 if it brings out of screen bounds
        newDirY = if newPY >= bottomBound && newPY <= topBound then dirY else 0

    in if (newDirX /= 0 || newDirY /= 0) 
        then
            let newD = (initDirection newDirX newDirY)
                newPo1 = (initMovableObject (objectPicture po) (objectHitbox po) newD (objectSpeed po)) -- player object with new direction
                newPo2 = moveObject newPo1 screenDefaultSpeed -- player object with its hitbox having a new position
                newP = initAlivePlayer newPo2 (playerLifes p) (playerHealth p) (playerScore p)
            in 
                if isP1 then ((), initInGameInfos newP p2 enemies)
                else ((), initInGameInfos p1 newP enemies)
        else ((), igi)

prop_pre_movePlayer :: Bool -> InGameInfos -> Bool
prop_pre_movePlayer isP1 (InGameInfos p1 p2 _) =
    let p = if isP1 then p1 else p2
    in not (isPlayerDead p)

-- Detects if there is a collision between a player and an ennemy
collisionPlayerWithEnemy :: Player -> Enemy -> Bool
collisionPlayerWithEnemy player enemy =
    let po = playerObject player
        eo = enemyObject enemy
    in collisionObject po eo

-- Sorts ennemies by keeping those alive, if they have collided with the given player + returns the number of collisions
keepAliveEnemies :: Player -> [Enemy] -> Int -> ([Enemy], Int)
keepAliveEnemies _ [] nbColls = ([], nbColls)
keepAliveEnemies player (enemy:xs) nbColls
    | collisionPlayerWithEnemy player enemy && (enemyHealth enemy) == 1 = (keepAliveEnemies player xs (nbColls+1)) -- if the collided ennemy has 1 health -> HE IS DEAD
    | collisionPlayerWithEnemy player enemy = -- if the collided ennemy has more than 1 health, he is kept whith a decreased health
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
handleCollisionPlayerWithEnemies  isP1 (InGameInfos p1 p2 listEnemies) = 
    let player = if isP1 then p1 else p2
        (newEnemies, collisions) = keepAliveEnemies player listEnemies 0
        po = (playerObject player)

        newHealth = (playerHealth player) - collisions*10
        newScore = (playerScore player) + collisions*47
        
        -- if the new health reaches 0 or less, a life is decreased and the health is reseted at 100
        newLifes = if newHealth <= 0 then ((playerLifes player)-1) else (playerLifes player)
        newHealth2 = if newHealth <= 0 && newLifes > 0 then 100 else newHealth

        -- if the new life counter is strictly negative : the player becomes dead
        newP = if newLifes <= 0 then (initDeadPlayer po newScore 1) else (initAlivePlayer po newLifes newHealth2 newScore)
    in 
        if isP1 then ((), initInGameInfos newP p2 newEnemies)
        else ((), initInGameInfos p1 newP newEnemies)

prop_pre_handleCollisionPlayerWithEnemies :: Bool -> InGameInfos -> Bool
prop_pre_handleCollisionPlayerWithEnemies isP1 (InGameInfos p1 p2 _) =
    let p = if isP1 then p1 else p2
    in not (isPlayerDead p)

-- ============================================================
-- ====================== IN GAME STATE =======================
-- ============================================================

type InGameState a = St.State InGameInfos a -- IN GAME STATE MONAD

-- MAIN IN GAME UPDATE FUNCTION
updateInGame :: Keyboard -> Float -> InGameState ()
updateInGame kbd deltaTime = do
    igi <- St.get

    let p1Dead = isPlayerDead (gamePlayer1 igi)
        p2Dead = isPlayerDead (gamePlayer2 igi)

    if not p1Dead
      then do
        -- update player1 direction and speed
        updatePlayerDirectionSpeedSt True (player1NewDirectionSpeed kbd deltaTime)
        -- move player1
        movePlayerSt True
        -- handle collisions between player1 and enemies
        handleCollisionPlayerWithEnemiesSt True
      else return ()

    if not p2Dead
      then do
        -- update player2 direction and speed
        updatePlayerDirectionSpeedSt False (player2NewDirectionSpeed kbd deltaTime)
        -- move player2
        movePlayerSt False
        -- handle collisions between player2 and enemies
        handleCollisionPlayerWithEnemiesSt False
      else return ()
    

updatePlayerDirectionSpeedSt :: Bool -> (Direction, ObjectSpeed) -> InGameState ()
updatePlayerDirectionSpeedSt isP1 ds = St.state (updatePlayerDirectionSpeed isP1 ds)

movePlayerSt :: Bool -> InGameState ()
movePlayerSt isP1 = St.state (movePlayer isP1)

handleCollisionPlayerWithEnemiesSt :: Bool -> InGameState ()
handleCollisionPlayerWithEnemiesSt isP1 = St.state (handleCollisionPlayerWithEnemies isP1)