module Model (module Model) where

import Graphics.Gloss

import Hitbox
import GameSetup
import Objects

import Debug.Trace

-- ============================================================
-- ========================= PLAYER ===========================
-- ============================================================

data Player = Player {
    playerObject :: Object, -- graphical representation of the player
    playerLifes :: Int, -- player remaining lifes, inside of [0, 3]
    playerHealth :: Int, -- health for the current player life, inside of [0, 100]
    playerScore :: Int -- player current score
} deriving (Show)

initPlayerObject :: Picture -> Float -> Float -> Direction -> ObjectSpeed -> Object
initPlayerObject pic x y dir speed = 
    (initMovableObject 
        pic 
        (initHitboxRectangle (x-(widthPlayer / 2)) (y-(heightPlayer / 2)) widthPlayer heightPlayer)
        dir
        speed
    )

initPlayer :: Object -> Int -> Int -> Int -> Player
initPlayer po lifes health score
    | lifes < 0 || lifes > 3 = error "number of lifes outside of [0, 3], must be inside it"
    | health < 0 || health > 100 = error "current life health outside of [0, 100], must be inside it"
    | otherwise = Player po lifes health score

-- Updates the player informations
updatePlayer :: Player -> Player
updatePlayer p =
    --trace (show (objectDirection p1o)) $
    movePlayer p

-- Sets player new given direction and speed
setPlayerDirectionSpeed :: Player -> (Direction, ObjectSpeed) -> Player
setPlayerDirectionSpeed p (newDir, newOS) =
    let po = playerObject p
        picP = objectPicture po
    in case centerHitbox (objectHitbox po) of
        Just (px, py) -> 
            let newPo = initPlayerObject picP px py newDir newOS
            in initPlayer newPo (playerLifes p) (playerHealth p) (playerScore p)
        Nothing -> error "player must have a center"

-- Moves the player, according to its current object direction and speed
movePlayer :: Player -> Player
movePlayer p@(Player po _ _ _) =
    let d@(Direction dirx diry) = objectDirection po
        pic = objectPicture po
        os@(ObjectSpeed s) = objectSpeed po
        dxp = (fromIntegral dirx)*s -- player movement, for x, got from direction and speed
        dyp = (fromIntegral diry)*s -- player movement, for y, got from direction and speed
    in case centerHitbox (objectHitbox po) of
        Just (px, py) -> 
            let newPX = px + dxp
                newPY = py + dyp
                leftBound = leftXScreenBound + (widthPlayer / 2)
                rightBound = rightXScreenBound - (widthPlayer / 2)
                topBound = topYScreenBound - (heightPlayer / 2)
                bottomBound = bottomYScreenBound + (heightPlayer / 2)
            in if newPX >= leftBound && newPX <= rightBound &&
                    newPY >= bottomBound && newPY <= topBound
                then 
                    let newPo = moveObject po screenDefaultSpeed
                    in initPlayer newPo (playerLifes p) (playerHealth p) (playerScore p)
                else p
        Nothing -> error "player must have a center"

-- ============================================================
-- ========================= ENNEMY ===========================
-- ============================================================

data Ennemy = Ennemy {
    ennemyObject :: Object, -- graphical representation of the ennemy
    ennemyHealth :: Int -- ennemy remaining health, > 0
} deriving (Show)

initStaticEnnemyRectangleObject :: Picture -> Float -> Float -> Object
initStaticEnnemyRectangleObject pic x y = 
    (initStaticObject 
        pic
        (initHitboxRectangle (x-(widthVirus / 2)) (y-(heightVirus / 2)) widthVirus heightVirus)
    )

initEnnemy :: Object -> Int -> Ennemy
initEnnemy eo health
    | health <= 0 = error "ennemy health must be strictly positive"
    | otherwise = Ennemy eo health

-- ============================================================
-- ====================== GAMESTATE ===========================
-- ============================================================

data GameState = 
    StartMenu StartMenuOption
    | InGame InGameInfos
    deriving (Show)

data StartMenuOption = Start | Option2
    deriving (Show, Eq)

data InGameInfos = InGameInfos {
        gamePlayer1 :: Player,
        gameEnemies :: [Ennemy]
    } deriving (Show)

initStartMenu :: StartMenuOption -> GameState
initStartMenu option = StartMenu option

initInGame :: InGameInfos -> GameState
initInGame gameInfos = InGame gameInfos

initInGameInfos :: Player -> [Ennemy] -> InGameInfos
initInGameInfos player enemies = InGameInfos player enemies

startInitInGame :: Picture -> Picture -> Float -> Float -> Float -> Float -> GameState
startInitInGame picPlayer picVirus xVirus yVirus xP1 yP1
    | xP1 - (widthPlayer / 2) < leftXScreenBound
        || xP1 + (widthPlayer / 2) > rightXScreenBound  = error "player1 x out of screen"
    | yP1 - (heightPlayer / 2) < bottomYScreenBound
        || yP1 + (heightPlayer / 2) > topYScreenBound = error "player1 y out of screen"
    | xVirus - (widthVirus / 2) < leftXScreenBound
        || xVirus + (widthVirus / 2) > rightXScreenBound  = error "virus x out of screen"
    | yVirus - (heightVirus / 2) < bottomYScreenBound
        || yVirus + (heightVirus / 2) > topYScreenBound = error "virus y out of screen"
    | otherwise = 
        let newPo = initPlayerObject picPlayer xP1 yP1 (initDirection 0 0) (initObjectSpeed 0)
            newP = initPlayer newPo 3 100 0
            newVo = initStaticEnnemyRectangleObject picVirus xVirus yVirus
            newV = initEnnemy newVo 1
            listEnemies = [newV]
        in initInGame (initInGameInfos newP listEnemies)

-- Detects if there is a collision between a player and an ennemy
collisionPlayerWithEnemy :: Player -> Ennemy -> Bool
collisionPlayerWithEnemy player enemy =
    let po = playerObject player
        eo = ennemyObject enemy
    in collisionObject po eo

-- Sorts ennemies by keeping those alive, if they have collided with the given player
keepAliveEnemies :: Player -> [Ennemy] -> [Ennemy]
keepAliveEnemies _ [] = []
keepAliveEnemies player (enemy:xs)
    | collisionPlayerWithEnemy player enemy && (ennemyHealth enemy) == 1 = (keepAliveEnemies player xs) -- if the collided ennemy has 1 health -> HE IS DEAD
    | collisionPlayerWithEnemy player enemy =
        let newEnemy = initEnnemy (ennemyObject enemy) ((ennemyHealth enemy)-1)
        in newEnemy:(keepAliveEnemies player xs) -- if the collided ennemy has more than 1 health, he is kept whith a decreased health
    | otherwise = enemy:(keepAliveEnemies player xs)

handleCollisionP1WithEnemies :: InGameInfos -> InGameInfos
handleCollisionP1WithEnemies (InGameInfos player1 listEnemies) = 
    let newEnemies = keepAliveEnemies player1 listEnemies
        enemiesCollided = (length listEnemies) - (length newEnemies)
        po = (playerObject player1)
        newLifes = (playerLifes player1)
        newHealth = (playerHealth player1) - enemiesCollided*10
        newScore = (playerScore player1) + enemiesCollided*47
    in (initInGameInfos (initPlayer po newLifes newHealth newScore) newEnemies)