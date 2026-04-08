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
    playerScore :: Int -- player current score, positive
} deriving (Show)

prop_inv_player :: Player -> Bool
prop_inv_player (Player po lifes health score) = prop_inv_object po && lifes >= 0 && lifes <= 3
    && health >= 0 && health <= 100 && score >= 0

initPlayerObject :: Picture -> Float -> Float -> Direction -> ObjectSpeed -> Object
initPlayerObject pic x y dir speed = 
    (initMovableObject 
        pic 
        (initHitboxRectangle (x-(widthPlayer / 2)) (y-(heightPlayer / 2)) widthPlayer heightPlayer)
        dir
        speed
    )

-- tests TODO
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

-- tests TODO
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

-- tests TODO
-- Moves the player, according to its current object direction and speed
movePlayer :: Player -> Player
movePlayer p@(Player po _ _ _) =
    let d@(Direction dirX dirY) = objectDirection po
        pic = objectPicture po
        os@(ObjectSpeed s) = objectSpeed po
        dxp = (fromIntegral dirX)*s -- player movement, for x, got from direction and speed
        dyp = (fromIntegral dirY)*s -- player movement, for y, got from direction and speed
    in case centerHitbox (objectHitbox po) of
        Just (px, py) -> 
            let newPX = px + dxp
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
                    in initPlayer newPo2 (playerLifes p) (playerHealth p) (playerScore p)
                else p

        Nothing -> error "player must have a center"

-- ============================================================
-- ========================= ENNEMY ===========================
-- ============================================================

data Enemy = Enemy {
    enemyObject :: Object, -- graphical representation of the enemy
    enemyHealth :: Int -- enemy remaining health, > 0
} deriving (Show)

prop_inv_enemy :: Enemy -> Bool
prop_inv_enemy (Enemy eo health) = prop_inv_object eo && health > 0

initStaticEnemyRectangleObject :: Picture -> Float -> Float -> Object
initStaticEnemyRectangleObject pic x y = 
    (initStaticObject 
        pic
        (initHitboxRectangle (x-(widthVirus / 2)) (y-(heightVirus / 2)) widthVirus heightVirus)
    )

-- tests TODO
initEnemy :: Object -> Int -> Enemy
initEnemy eo health
    | health <= 0 = error "ennemy health must be strictly positive"
    | otherwise = Enemy eo health

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

-- tests TODO
-- Decreases enemies health if they collide with the player, once an enemy has no health (=0), he is deleted from the game infos
handleCollisionP1WithEnemies :: InGameInfos -> InGameInfos
handleCollisionP1WithEnemies (InGameInfos player1 player2 listEnemies) = 
    let newEnemies = keepAliveEnemies player1 listEnemies
        enemiesCollided = (length listEnemies) - (length newEnemies)
        po = (playerObject player1)
        newLifes = (playerLifes player1)
        newHealth = (playerHealth player1) - enemiesCollided*10
        newScore = (playerScore player1) + enemiesCollided*47
    in (initInGameInfos (initPlayer po newLifes newHealth newScore) player2 newEnemies)

handleCollisionP2WithEnemies :: InGameInfos -> InGameInfos
handleCollisionP2WithEnemies (InGameInfos player1 player2 listEnemies) = 
    let newEnemies = keepAliveEnemies player2 listEnemies
        enemiesCollided = (length listEnemies) - (length newEnemies)
        po = (playerObject player2)
        newLifes = (playerLifes player2)
        newHealth = (playerHealth player2) - enemiesCollided*10
        newScore = (playerScore player2) + enemiesCollided*47
    in (initInGameInfos player1 (initPlayer po newLifes newHealth newScore) newEnemies)