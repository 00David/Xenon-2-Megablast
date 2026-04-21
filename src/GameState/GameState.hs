module GameState.GameState (module GameState.GameState) where

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
    state :: GameState, -- StartMenu or InGame
    assets :: GameAssets,
    background :: Background
} deriving Show

initStartGame :: IO Game
initStartGame = do 
    assts <- initGameAssets
    bgnd <- initStartBackground
    return $ Game initKeyboard (initStartMenu Start) assts bgnd

-- ============================================================
-- ====================== GAMESTATE ===========================
-- ============================================================

data GameState = 
    StartMenu StartMenuOption
    | InGame InGameInfos
    deriving (Show)

prop_inv_gamestate :: GameState -> Bool
prop_inv_gamestate (StartMenu _) = True
prop_inv_gamestate (InGame igi) = prop_inv_ingameinfos igi

data StartMenuOption = Start | Option2
    deriving (Show, Eq)

data InGameInfos = InGameInfos {
        gamePlayer1 :: Player,
        gamePlayer2 :: Player,
        gameEnemies :: [Enemy]
    } deriving (Show)

type InGameState a = St.State InGameInfos a

prop_inv_ingameinfos :: InGameInfos -> Bool
prop_inv_ingameinfos (InGameInfos p1 p2 enemies) = prop_inv_player p1 && prop_inv_player p2
    && foldr (\e acc -> prop_inv_enemy e && acc) True enemies

initStartMenu :: StartMenuOption -> GameState
initStartMenu option = StartMenu option

-- tests TODO
initInGame :: InGameInfos -> GameState
initInGame gameInfos = InGame gameInfos

-- tests TODO
initInGameInfos :: Player -> Player -> [Enemy] -> InGameInfos
initInGameInfos player1 player2 enemies = InGameInfos player1 player2 enemies

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
            newP1 = initPlayer newP1o 3 100 0
            newP2o = initPlayerObject picPlayer xP2 yP2 (initDirection 0 0) (initObjectSpeed 0)
            newP2 = initPlayer newP2o 0 100 0
            newVo = initStaticEnemyRectangleObject picVirus xVirus yVirus
            newV = initEnemy newVo 1
            listEnemies = [newV]
        in initInGame (initInGameInfos newP1 newP2 listEnemies)

updateInGame :: Keyboard -> Float -> InGameState ()
updateInGame kbd deltaTime = do
    -- update player1 direction and speed
    updatePlayerDirectionSpeedSt True (player1NewDirectionSpeed kbd deltaTime)

    -- move player1
    movePlayerSt True

    -- handle collisions between player1 and enemies
    handleCollisionPlayerWithEnemiesSt True
    

updatePlayerDirectionSpeedSt :: Bool -> (Direction, ObjectSpeed) -> InGameState ()
updatePlayerDirectionSpeedSt isP1 ds = St.state (updatePlayerDirectionSpeed isP1 ds)

-- tests TODO
-- Sets player new given direction and speed
updatePlayerDirectionSpeed :: Bool -> (Direction, ObjectSpeed) -> InGameInfos -> ((), InGameInfos)
updatePlayerDirectionSpeed isP1 (newDir, newOS) (InGameInfos p1 p2 enemies) =
    let p = if isP1 then p1 else p2
        po = playerObject p
        picP = objectPicture po
        (px, py) = centerHitbox (objectHitbox po)
        
        newPo = initPlayerObject picP px py newDir newOS
        newP = initPlayer newPo (playerLifes p) (playerHealth p) (playerScore p)
    in 
        if isP1 then ((), initInGameInfos newP p2 enemies)
        else ((), initInGameInfos p1 newP enemies)

movePlayerSt :: Bool -> InGameState ()
movePlayerSt isP1 = St.state (movePlayer isP1)

-- tests TODO
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
                newP = initPlayer newPo2 (playerLifes p) (playerHealth p) (playerScore p)
            in 
                if isP1 then ((), initInGameInfos newP p2 enemies)
                else ((), initInGameInfos p1 newP enemies)
        else ((), igi)

-- tests TODO
-- Detects if there is a collision between a player and an ennemy
collisionPlayerWithEnemy :: Player -> Enemy -> Bool
collisionPlayerWithEnemy player enemy =
    let po = playerObject player
        eo = enemyObject enemy
    in collisionObject po eo

-- tests TODO
-- Sorts ennemies by keeping those alive, if they have collided with the given player
keepAliveEnemies :: Player -> [Enemy] -> [Enemy]
keepAliveEnemies _ [] = []
keepAliveEnemies player (enemy:xs)
    | collisionPlayerWithEnemy player enemy && (enemyHealth enemy) == 1 = (keepAliveEnemies player xs) -- if the collided ennemy has 1 health -> HE IS DEAD
    | collisionPlayerWithEnemy player enemy =
        let newEnemy = initEnemy (enemyObject enemy) ((enemyHealth enemy)-1)
        in newEnemy:(keepAliveEnemies player xs) -- if the collided ennemy has more than 1 health, he is kept whith a decreased health
    | otherwise = enemy:(keepAliveEnemies player xs)

handleCollisionPlayerWithEnemiesSt :: Bool -> InGameState ()
handleCollisionPlayerWithEnemiesSt isP1 = St.state (handleCollisionPlayerWithEnemies isP1)

-- tests TODO
-- Decreases enemies health if they collide with the player, once an enemy has no health (=0), he is deleted from the game infos
handleCollisionPlayerWithEnemies :: Bool -> InGameInfos -> ((), InGameInfos)
handleCollisionPlayerWithEnemies  isP1 (InGameInfos p1 p2 listEnemies) = 
    let player = if isP1 then p1 else p2
        newEnemies = keepAliveEnemies player listEnemies
        enemiesCollided = (length listEnemies) - (length newEnemies)
        po = (playerObject player)
        newLifes = (playerLifes player)
        newHealth = (playerHealth player) - enemiesCollided*10
        newScore = (playerScore player) + enemiesCollided*47
    in 
        if isP1 then ((), initInGameInfos (initPlayer po newLifes newHealth newScore) p2 newEnemies)
        else ((), initInGameInfos p1 (initPlayer po newLifes newHealth newScore) newEnemies)
        