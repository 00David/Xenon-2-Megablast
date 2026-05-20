{-# LANGUAGE InstanceSigs #-}
module GameState.Game (module GameState.Game) where

import Graphics.Gloss

import Data.Maybe
import System.Random
import qualified Control.Monad.State as St

import Keyboard
import GameSetup
import GameState.Enemy
import GameState.Player
import GameState.Projectile
import GameState.Wall
import Graphics.Assets
import Graphics.Background
import Graphics.Explosion
import Objects.Objects
import Objects.Hitbox
import Typeclasses.Damageable
import Typeclasses.Invariant
import Typeclasses.Movable

-- ============================================================
-- =================== GAME INITIALISATION ====================
-- ============================================================

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
    return $ initGame initKeyboard (initStartMenu OnePlayer) assts bgnd 0

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

-- Initializes the game at start, with the given number of players
startInitInGame :: StdGen -> Int -> GameState
startInitInGame gen nbPlayers
    | nbPlayers < 1 || nbPlayers > 2  = error "only 1 or 2 players"
    | otherwise =
        let p1 = startInitAlivePlayer 1
            p2 = if nbPlayers == 2 then startInitAlivePlayer 2 else startInitDeadPlayer 2
            --newE = placeEnemy 200 200
            --listEnemies = [newE]
            walls = startInitGameWalls gen
        in initInGame (initInGameInfos screenDefaultSpeed p1 p2 [] walls [] [])

prop_pre_startInitInGame:: StdGen -> Int -> Bool
prop_pre_startInitInGame _ nbPlayers = nbPlayers == 1 || nbPlayers == 2

-- ============================================================
-- =================== START MENU OPTION ======================
-- ============================================================

data StartMenuOption = OnePlayer | TwoPlayers
    deriving (Show, Eq)

instance Renderable StartMenuOption where
    getTranslatedAssets :: GameAssets -> StartMenuOption -> [Picture]
    getTranslatedAssets _ OnePlayer = 
        let (xSelect1, xSelect2, ySelect) = (-125, 100, 0)
        in [
            Translate (-80) (0) (Scale 0.3 0.3 (Color white (Text "1 Player"))),
            Translate (-88) (-100) (Scale 0.3 0.3 (Color white (Text "2 Players"))),
            Translate xSelect1 ySelect (Scale 0.3 0.3 (Color white (Text ">"))),
            Translate xSelect2 ySelect (Scale 0.3 0.3 (Color white (Text "<")))
        ]
    getTranslatedAssets _ TwoPlayers = 
        let (xSelect1, xSelect2, ySelect) = (-133, 114, -100) 
        in [
            Translate (-80) (0) (Scale 0.3 0.3 (Color white (Text "1 Player"))),
            Translate (-88) (-100) (Scale 0.3 0.3 (Color white (Text "2 Players"))),
            Translate xSelect1 ySelect (Scale 0.3 0.3 (Color white (Text ">"))),
            Translate xSelect2 ySelect (Scale 0.3 0.3 (Color white (Text "<")))
        ]

-- ============================================================
-- ====================== IN GAME INFOS =======================
-- ============================================================

data InGameInfos = InGameInfos {
        gameScreenSpeed :: ScreenScrollingSpeed,
        gamePlayer1 :: Player,
        gamePlayer2 :: Player,
        gameEnemies :: [Enemy],
        gameWalls :: GameWalls,
        gameProjectiles :: [Projectile],
        gameHitExplosions :: [Explosion]
    } deriving (Show)

prop_inv_ingameinfos :: InGameInfos -> Bool
prop_inv_ingameinfos (InGameInfos screenSpeed p1 p2 enemies walls projs expl) = screenSpeed > 0 
    && prop_inv_player p1 && prop_inv_player p2
    && foldr (\e acc -> prop_inv_enemy e && acc) True enemies
    && prop_inv_gameWalls walls && all prop_inv_projectile projs
    && all prop_inv_explosion expl

instance Invariant InGameInfos where
    prop_inv :: InGameInfos -> Bool
    prop_inv = prop_inv_ingameinfos 

instance Renderable InGameInfos where
    getTranslatedAssets :: GameAssets -> InGameInfos -> [Picture]
    getTranslatedAssets ga (InGameInfos _ player1 player2 enemies walls projs expl) =
        (getTranslatedAssets ga walls) ++ (getTranslatedAssets ga player1) ++ (getTranslatedAssets ga player2) ++
        concatMap (getTranslatedAssets ga) enemies ++ 
        concatMap (getTranslatedAssets ga) projs ++
        concatMap (getTranslatedAssets ga) expl

initInGameInfos :: ScreenScrollingSpeed -> Player -> Player -> [Enemy] -> GameWalls -> [Projectile] -> [Explosion] -> InGameInfos
initInGameInfos screenSpeed player1 player2 enemies walls projs expl = 
    InGameInfos screenSpeed player1 player2 enemies walls projs expl

-- Indicates if everything else than a player is the same for 2 given InGameInfos 
sameInGameInfosExceptPlayer :: Bool -> InGameInfos -> InGameInfos -> Bool
sameInGameInfosExceptPlayer isP1 (InGameInfos screenSpeed p1 p2 enemies gw projs expl) (InGameInfos screenSpeed' p1' p2' enemies' gw' projs' expl') =
    if isP1 
        then p2 == p2' && screenSpeed == screenSpeed' && enemies == enemies' && gw == gw' && projs == projs' && expl == expl'
        else p1 == p1' && screenSpeed == screenSpeed' && enemies == enemies' && gw == gw' && projs == projs' && expl == expl'


-- Sets player new given direction and speed
updatePlayerDirectionSpeed :: Bool -> (Direction, ObjectSpeed) -> InGameInfos -> ((), InGameInfos)
updatePlayerDirectionSpeed isP1 (newDir, newOS) (InGameInfos ss p1 p2 enemies gw projs expl) =
    let p = if isP1 then p1 else p2
        po = playerObject p
        (px, py) = centerHitbox (objectHitbox po)
        
        newPo = initPlayerObject px py newDir newOS
        newP = updatePlayerObject p newPo
    in 
        if isP1 then ((), initInGameInfos ss newP p2 enemies gw projs expl)
        else ((), initInGameInfos ss p1 newP enemies gw projs expl)

prop_pre_updatePlayerDirectionSpeed :: Bool -> (Direction, ObjectSpeed) -> InGameInfos -> Bool
prop_pre_updatePlayerDirectionSpeed isP1 _ (InGameInfos _ p1 p2 _ _ _ _) =
    let p = if isP1 then p1 else p2
    in not (isPlayerDead p)

prop_post_updatePlayerDirectionSpeed :: Bool -> (Direction, ObjectSpeed) -> InGameInfos -> Bool
prop_post_updatePlayerDirectionSpeed isP1 (dir, os) igi@(InGameInfos _ _ _ _ _ _ _) =
    let (_, igi'@(InGameInfos _ p1' p2' _ _ _ _)) = (updatePlayerDirectionSpeed isP1 (dir, os) igi)
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


-- Moves the player, according to its current object direction and speed, by taking into account walls, eventually another player, and finally borders
movePlayerInsideScreen :: Bool -> InGameInfos -> ((), InGameInfos)
movePlayerInsideScreen isP1 igi@(InGameInfos ss p1 p2 enemies walls projs expl) =
    let 
        -- First, we need to handle eventual collisions with walls or another player after movement
        p = if isP1 then p1 else p2
        anotherP = if isP1 then p2 else p1
        po = playerObject p
        (Direction dirX dirY) = objectDirection po

        -- try indepedently to move its object in X and Y
        objMoveX = initMovableObject (objectHitbox po) (Direction dirX 0) (objectSpeed po)
        objMoveY = initMovableObject (objectHitbox po) (Direction 0 dirY) (objectSpeed po)

        collideWallsX = willCollide objMoveX walls ss
        collideWallsY = willCollide objMoveY walls ss
        collidePlayerX = if isPlayerDead anotherP then False else willCollide objMoveX anotherP ss
        collidePlayerY = if isPlayerDead anotherP then False else willCollide objMoveY anotherP ss


        -- X and/or Y directions can become 0 if it leads to a wall or player collision
        newDirX = if not (collideWallsX || collidePlayerX) then dirX else 0
        newDirY = if not (collideWallsY || collidePlayerY) then dirY else 0

    in if (newDirX /= 0 || newDirY /= 0) 
        then
            let 
                -- Then, moves the player by keeping him inside the screen (by taking into account updated directions)
                newD = (initDirection newDirX newDirY)
                newPo = (initMovableObject (objectHitbox po) newD (objectSpeed po))
                pWithNewDirection = updatePlayerObject p newPo
                newP = movePlayer pWithNewDirection ss
            in 
                if isP1 then ((), initInGameInfos ss newP p2 enemies walls projs expl)
                else ((), initInGameInfos ss p1 newP enemies walls projs expl)
        else ((), igi)

prop_pre_movePlayerInsideScreen :: Bool -> InGameInfos -> Bool
prop_pre_movePlayerInsideScreen isP1 (InGameInfos _ p1 p2 _ _ _ _) = 
    let p = if isP1 then p1 else p2
    in not (isPlayerDead p)

prop_post_movePlayerInsideScreen :: Bool -> InGameInfos -> Bool
prop_post_movePlayerInsideScreen isP1 igi@(InGameInfos _ _ _ _ _ _ _) =
    let (_, igi'@(InGameInfos _ p1' p2' _ _ _ _)) = movePlayerInsideScreen isP1 igi
    in (sameInGameInfosExceptPlayer isP1 igi igi') && 
        if isP1
            then not (isPlayerDead p1') -- insideScreenPlayer p1' checked by the player invariant
            else not (isPlayerDead p2') -- insideScreenPlayer p2' checked by the player invariant


-- Decreases enemies health if they collide with the player, once an enemy has no health (=0), he is deleted from the game infos
handleCollisionPlayerWithEnemies :: Bool -> InGameInfos -> ((), InGameInfos)
handleCollisionPlayerWithEnemies  isP1 (InGameInfos ss p1 p2 enemies gw projs expl) = 
    let player = if isP1 then p1 else p2

        -- First, we consider giving damages on enemies colliding with the player
        (newEnemies, scoreFromEnemies) = foldr
            (\e (accEnemies, accScore) ->
                if collision player e
                    then case takeDamage 1 e of
                        Just e' -> (e':accEnemies, accScore)
                        Nothing -> (accEnemies, accScore + enemyScoreGiven e)
                    else (e:accEnemies, accScore)
                ) ([], 0) enemies

        -- Then, we consider giving damages on the player colliding with enemies
        newMaybePlayer = foldr
                (\e maybePlayer ->
                    if collision e player
                        then maybePlayer >>= takeDamage (enemyCollisionDamage e)
                        else maybePlayer
                ) (Just player) enemies
        newPlayer = case newMaybePlayer of
            Just p -> (addScore scoreFromEnemies p)
            Nothing -> error "player cannot be Nothing"
    in 
        if isP1 then ((), initInGameInfos ss newPlayer p2 newEnemies gw projs expl)
        else ((), initInGameInfos ss p1 newPlayer newEnemies gw projs expl)

prop_pre_handleCollisionPlayerWithEnemies :: Bool -> InGameInfos -> Bool
prop_pre_handleCollisionPlayerWithEnemies isP1 (InGameInfos _ p1 p2 _ _ _ _) =
    let p = if isP1 then p1 else p2
    in not (isPlayerDead p)

prop_post_handleCollisionPlayerWithEnemies :: Bool -> InGameInfos -> Bool
prop_post_handleCollisionPlayerWithEnemies isP1 igi@(InGameInfos ss p1 p2 enemies gw projs expl) =
    let (_, (InGameInfos ss' p1' p2' enemies' gw' projs' expl')) = (handleCollisionPlayerWithEnemies isP1 igi)
    in if isP1 
        then (length enemies') <= (length enemies) && (playerLifes p1') <= (playerLifes p1) 
            && (playerScore p1') >= (playerScore p1) && ss == ss' && p2 == p2' && gw == gw' && projs == projs' && expl == expl'
        else (length enemies') <= (length enemies) && (playerLifes p2') <= (playerLifes p2) 
            && (playerScore p2') >= (playerScore p2) && ss == ss' && p1 == p1' && gw == gw' && projs == projs' && expl == expl'


-- Moves walls according to current screen scrolling speed and keep those inside the screen
moveWalls :: InGameInfos -> ((), InGameInfos)
moveWalls (InGameInfos ss p1 p2 enemies (GameWalls left1 left2 right1 right2 walls) projs expl) =
    let 
        newLeft1 = partialFilterWall insideScreen (partialMapWall (flip move ss) left1)
        newLeft2 = partialFilterWall insideScreen (partialMapWall (flip move ss) left2)
        newRight1 = partialFilterWall insideScreen (partialMapWall (flip move ss) right1)
        newRight2 = partialFilterWall insideScreen (partialMapWall (flip move ss) right2)
        newWalls = filter (not . null) (fmap (\w -> filterWall insideScreen (fmap (flip move ss) w)) walls)

        newGameWalls = (initGameWalls newLeft1 newLeft2 newRight1 newRight2 newWalls)
    in 
        ((), initInGameInfos ss p1 p2 enemies newGameWalls projs expl)

prop_post_moveWalls :: InGameInfos -> Bool
prop_post_moveWalls igi@(InGameInfos ss p1 p2 ennemies _ projs expl) =
    let (_, (InGameInfos ss' p1' p2' ennemies' gw' projs' expl')) = moveWalls igi
    in 
        insideScreen gw' && ss == ss' && p1 == p1' && p2 == p2' && ennemies == ennemies' && projs == projs' && expl == expl'


-- Bumps horizontally the player to the nearest valid position (left or right),
-- prioritizing the direction that keeps the player closest to the original x position.
-- Invalid positions (outside screen or colliding) are skipped.
keepBumpin :: ScreenScrollingSpeed -> Object -> GameWalls -> Player -> Object
keepBumpin ss obj gw anotherP =
    let (xStart, y) = centerHitbox (objectHitbox obj)
        
        -- Screen bounds for player
        leftBound = leftXScreenBound + (widthPlayer / 2)
        rightBound = rightXScreenBound - (widthPlayer / 2)
        
        -- Generate candidate positions: alternating left and right from start
        -- Distance 1: [xStart - ss, xStart + ss]
        -- Distance 2: [xStart - 2*ss, xStart + 2*ss], etc.
        candidates = concatMap (\distance ->
            let offset = distance * ss
                xLeft = xStart - offset
                xRight = xStart + offset
            in [(xLeft, distance), (xRight, distance)]
            ) [1..1000] -- Large enough to cover the screen width
        
        -- Check if a position is valid (inside screen and no collision)
        isValid x = 
            let objAtX = initPlayerObject x y (objectDirection obj) (objectSpeed obj)
            in x >= leftBound && x <= rightBound 
                && not (collision objAtX gw) 
                && not (collision objAtX anotherP)
        
        -- Find the first valid position
        findValidPos [] = obj -- Should never happen, but keep original if no valid position
        findValidPos ((x, _):xs) = 
            if isValid x 
                then initPlayerObject x y (objectDirection obj) (objectSpeed obj)
                else findValidPos xs
                
    in findValidPos candidates

prop_pre_keepBumpin :: ScreenScrollingSpeed -> Object -> GameWalls -> Player -> Bool
prop_pre_keepBumpin ss _ _ _ = ss > 0

prop_post_keepBumpin :: ScreenScrollingSpeed -> Object -> GameWalls -> Player -> Bool
prop_post_keepBumpin ss obj gw anotherP =
    let (_, y) = centerHitbox (objectHitbox obj)
        obj' = keepBumpin ss obj gw anotherP
        (x', y') = centerHitbox (objectHitbox obj')
        
        leftBound = leftXScreenBound + (widthPlayer / 2)
        rightBound = rightXScreenBound - (widthPlayer / 2)
    in 
        not (collision obj' gw) && not (collision obj' anotherP)
        && y' == y 
        && x' >= leftBound && x' <= rightBound


-- Bumps the player from a wall
bumpPlayerFromWalls :: Bool -> InGameInfos -> ((), InGameInfos)
bumpPlayerFromWalls isP1 igi@(InGameInfos ss p1 p2 enemies gw projs expl) =
    let p = if isP1 then p1 else p2
        anotherP = if isP1 then p2 else p1
        po = playerObject p
        h = objectHitbox po
        (x,y) = centerHitbox h

        bottomYPlayer = y-(heightPlayer/2)
        -- we use the static version of the player object, because this one moves down according to screen scrolling speed,
        -- so it tells us if the player will then collide with another player after a bump
        staticPo = initStaticObject h
    in 
        -- if there is no collision, no state change
        if not (collision p gw) then ((), igi)
            
        -- if there is a collision, 2 cases :
        else 
            -- if the player, after the vertical bump, would still be in the screen
            -- AND will not collide with another player : vertical bump
            if (bottomYPlayer-ss) >= bottomYScreenWithBarBound && not (willCollide staticPo anotherP ss) then 
                let newPo = initPlayerObject x (y-ss) (objectDirection po) (objectSpeed po)
                    newP = updatePlayerObject p newPo
                in 
                    if isP1 then ((), initInGameInfos ss newP p2 enemies gw projs expl)
                    else ((), initInGameInfos ss p1 newP enemies gw projs expl)

            -- otherwise : horizontal bump
            else 
                let newPo = keepBumpin ss po gw anotherP
                    newP = updatePlayerObject p newPo
                in 
                    if isP1 then ((), initInGameInfos ss newP p2 enemies gw projs expl)
                    else ((), initInGameInfos ss p1 newP enemies gw projs expl)

prop_pre_bumpPlayerFromWalls :: Bool -> InGameInfos -> Bool
prop_pre_bumpPlayerFromWalls isP1 (InGameInfos _ p1 p2 _ _ _ _) =
    let p = if isP1 then p1 else p2
    in not (isPlayerDead p)

prop_post_bumpPlayerFromWalls :: Bool -> InGameInfos -> Bool
prop_post_bumpPlayerFromWalls isP1 igi@(InGameInfos ss p1 p2 enemies gw projs expl) =
    let (_, (InGameInfos ss' p1' p2' enemies' gw' projs' expl')) = (bumpPlayerFromWalls isP1 igi)
    in if isP1 
        then not (isPlayerDead p1') &&  not (collision p1' gw')
            && ss == ss' && p2 == p2' && enemies == enemies' && gw == gw' && projs == projs' && expl == expl'
        else not (isPlayerDead p2') &&  not (collision p2' gw')
            && ss == ss' && p1 == p1' && enemies == enemies' && gw == gw' && projs == projs' && expl == expl'


-- Moves game projectiles according to their direction and speed. Those becoming outside of the screen are deleted.
moveProjectiles :: InGameInfos -> ((), InGameInfos)
moveProjectiles (InGameInfos ss p1 p2 enemies walls projs expl) =
    let 
        newProjectiles = filter insideScreen (fmap (flip move ss) projs)
    in 
        ((), initInGameInfos ss p1 p2 enemies walls newProjectiles expl)

prop_pre_moveProjectiles :: InGameInfos -> Bool
prop_pre_moveProjectiles (InGameInfos _ _ _ _ _ projs _) = all insideScreenProjectile projs

prop_post_moveProjectiles :: InGameInfos -> Bool
prop_post_moveProjectiles igi@(InGameInfos ss p1 p2 enemies walls _ expl) = 
    let (_, (InGameInfos ss' p1' p2' enemies' walls' projs' expl')) = moveProjectiles igi
    in (all insideScreenProjectile projs')
        && ss == ss' && p1 == p1' && p2 == p2' && enemies == enemies' && walls == walls' && expl == expl'


-- Considers a projectile, and an enemy with player1 and 2 accumulated scores given by kills.
-- Eventually increment one of those counters if the enemy is killed by the projectile.
applyProjectileToEnemy :: Projectile -> (Maybe Enemy, Score, Score) -> (Maybe Enemy, Score, Score)
applyProjectileToEnemy _ (Nothing, scoreP1, scoreP2) = (Nothing, scoreP1, scoreP2)
applyProjectileToEnemy proj (Just enemy, scoreP1, scoreP2)
    | not (collision proj enemy) = (Just enemy, scoreP1, scoreP2)
    | otherwise = 
        case takeDamage (projectileDamage proj) enemy of
            Just e' -> (Just e', scoreP1, scoreP2)
            Nothing ->
                let score = enemyScoreGiven enemy
                in case projectilePlayerId proj of
                    1 -> (Nothing, scoreP1 + score, scoreP2)
                    2 -> (Nothing, scoreP1, scoreP2 + score)
                    _ -> (Nothing, scoreP1, scoreP2)

prop_pre_applyProjectileToEnemy :: Projectile -> (Maybe Enemy, Int, Int) -> Bool
prop_pre_applyProjectileToEnemy _ (_, p1Kills, p2Kills) = p1Kills >= 0 && p2Kills >= 0

prop_post_applyProjectileToEnemy :: Projectile -> (Maybe Enemy, Int, Int) -> Bool
prop_post_applyProjectileToEnemy proj (maybeEnemy, p1Kills, p2Kills) =
    let (_, p1Kills', p2Kills') = applyProjectileToEnemy proj (maybeEnemy, p1Kills, p2Kills)
    in p1Kills' == (p1Kills+1) || p2Kills' == (p2Kills+1)


-- If a player projectile collides with an enemy : decreases its health. Once an enemy has no health (=0), he is deleted from the game infos.
-- If an enemy projectile collides with a player : decreases its health and potentially its number of lifes. Once a player has no lifes (=0), he becomes dead.
handleCollisionProjectilesWithPlayersEnemies :: InGameInfos -> ((), InGameInfos)
handleCollisionProjectilesWithPlayersEnemies (InGameInfos ss p1 p2 enemies gw projs expl) = 
    let
        playerProjectiles = filter isPlayerShot projs
        enemyProjectiles  = filter (not . isPlayerShot) projs

        -- First, we consider giving damages on enemies colliding with player projectiles. We keep counters for player kills.
        maybeEnemiesWithKills = fmap (\enemy -> foldr applyProjectileToEnemy (Just enemy, 0, 0) playerProjectiles) enemies

        (maybeEnemies, p1ScoreToAddList, p2ScoreToAddList) = unzip3 maybeEnemiesWithKills
        newEnemies = mapMaybe id maybeEnemies
        p1ScoreToAdd = foldl' (+) 0 p1ScoreToAddList
        p2ScoreToAdd = foldl' (+) 0 p2ScoreToAddList

        
        -- We remove player projectiles that have collided
        remainingPlayerProjectiles = filter (\proj -> not (any (collision proj) enemies)) playerProjectiles
        -- And create the corresponding explosions
        playerProjectileExplosions = getExplosions playerProjectiles remainingPlayerProjectiles

        -- Secondly, we consider giving damages on the player1 colliding with enemies projectiles
        newMaybeP1 = if isPlayerDead p1 then Just p1
                    else foldl
                        (\maybePlayer proj ->
                            if collision proj p1
                                then maybePlayer >>= takeDamage (projectileDamage proj)
                                else maybePlayer
                        ) (Just p1) enemyProjectiles
        newP1 = case newMaybeP1 of
            Just p -> (addScore p1ScoreToAdd p)
            Nothing -> error "player1 cannot be Nothing"

        -- Thirdly, we consider giving damages on the player2 colliding with enemies projectiles
        newMaybeP2 = if isPlayerDead p2 then Just p2
                    else foldl
                        (\maybePlayer proj ->
                            if collision proj p2
                                then maybePlayer >>= takeDamage (projectileDamage proj)
                                else maybePlayer
                        ) (Just p2) enemyProjectiles
        newP2 = case newMaybeP2 of
            Just p -> (addScore p2ScoreToAdd p)
            Nothing -> error "player2 cannot be Nothing"

        -- We remove enemy projectiles that collided with at least one alive player
        remainingEnemyProjectiles = filter (\proj ->
            not (
                (not (isPlayerDead p1) && collision proj p1)
                || (not (isPlayerDead p2) && collision proj p2)
                )
            ) enemyProjectiles
        -- And create the corresponding explosions
        enemyProjectileExplosions = getExplosions enemyProjectiles remainingEnemyProjectiles

        -- Final projectiles
        newProjectiles = remainingPlayerProjectiles ++ remainingEnemyProjectiles
        -- and their explosions
        newExpl = expl ++ playerProjectileExplosions ++ enemyProjectileExplosions

    in ((), initInGameInfos ss newP1 newP2 newEnemies gw newProjectiles newExpl)

prop_post_handleCollisionProjectilesWithPlayersEnemies :: InGameInfos -> Bool
prop_post_handleCollisionProjectilesWithPlayersEnemies igi@(InGameInfos ss p1 p2 enemies gw projs expl) =
    let (_, (InGameInfos ss' p1' p2' enemies' gw' projs' expl')) = (handleCollisionProjectilesWithPlayersEnemies igi)
    in 
        (length enemies') <= (length enemies) && (length projs') <= (length projs)
        && (playerLifes p1') <= (playerLifes p1) && (playerScore p1') >= (playerScore p1) 
        && (playerLifes p2') <= (playerLifes p2) && (playerScore p2') >= (playerScore p2)
        && ss == ss' && gw == gw' && expl == expl'


-- Runs all enemies: makes them shoot, then move them, and eventually delete them if they become out of the screen
runEnemies :: InGameInfos -> ((), InGameInfos)
runEnemies (InGameInfos ss p1 p2 enemies gw projs expl) =
    let
        -- First, make all enemies shoot and collect new projectiles
        (newProjectiles, enemiesAfterShooting) = foldr
            (\enemy (accProjs, accEnemies) ->
                let (maybeProj, enemy') = shootEnemy enemy
                in case maybeProj of
                    Just proj -> (proj:accProjs, enemy':accEnemies)
                    Nothing   -> (accProjs, enemy':accEnemies)
            ) ([], []) enemies
        
        -- Then, move all enemies
        movedEnemies = fmap (flip move ss) enemiesAfterShooting

        -- keep enemies inside the screen (considering that they are still inside the screen when they are above it)
        insideScreenEnemies = filter insideScreen movedEnemies
        
        -- Combine old projectiles with new ones
        allProjectiles = projs ++ newProjectiles
        
    in ((), initInGameInfos ss p1 p2 insideScreenEnemies gw allProjectiles expl)

prop_pre_runEnemies :: InGameInfos -> Bool
prop_pre_runEnemies (InGameInfos ss _ _ enemies _ _ _) = 
    ss > 0 && all (\e -> prop_pre_moveEnemy e ss) enemies

prop_post_runEnemies :: InGameInfos -> Bool
prop_post_runEnemies igi@(InGameInfos ss p1 p2 enemies gw projs expl) =
    let (_, (InGameInfos ss' p1' p2' enemies' gw' projs' expl')) = runEnemies igi
    in 
        ss == ss' && p1 == p1' && p2 == p2' && gw == gw' && expl == expl'
        && length enemies' == length enemies
        && length projs' >= length projs
        -- All enemies (that can move) have been moved
        && all (\(oldE, newE) -> 
            let oldPos = centerHitbox (objectHitbox (enemyObject oldE))
                newPos = centerHitbox (objectHitbox (enemyObject newE))
            in oldPos /= newPos || objectSpeed (enemyObject oldE) == initObjectSpeed 0
        ) (zip enemies enemies')


-- Generates periodically enemies, by respecting a maximum number of enemies
generateEnemies :: FrameCounter -> StdGen -> InGameInfos -> (StdGen, InGameInfos)
generateEnemies currentFrameCounter gen igi@(InGameInfos ss p1 p2 enemies gw projs expl)
    | length enemies >= maxEnemies = (gen, igi) -- too much enemies : don't generate more
    | currentFrameCounter `mod` 240 /= 0 || currentFrameCounter == 0 = (gen, igi) -- generate only every 240 frames (4 seconds), otherwise don't
    | otherwise = 
        let
            -- nnumber to generate : 1 / 3 / 5
            (choice, gen1) = randomR (0::Int, 2) gen
            nbEnemies =
                case choice of
                    0 -> 1
                    1 -> 3
                    _ -> 5
            maxToSpawn = min nbEnemies (maxEnemies - length enemies) -- don't go beyond the maximum number
            (genFinal,newEnemies) = generateListEnemies maxToSpawn gen1
        in
            (genFinal, initInGameInfos ss p1 p2 (enemies ++ newEnemies) gw projs expl)


-- Runs all hit explosions: update their animation
runExplosions :: InGameInfos -> ((), InGameInfos)
runExplosions (InGameInfos ss p1 p2 enemies gw projs expl) =
    let
        newExpl = mapMaybe runExplosion expl
    in
        ((), initInGameInfos ss p1 p2 enemies gw projs newExpl)

prop_post_runExplosions :: InGameInfos -> Bool
prop_post_runExplosions igi@(InGameInfos ss p1 p2 enemies gw projs expl) =
    let
        (_, InGameInfos ss' p1' p2' enemies' gw' projs' expl') = runExplosions igi
    in
        ss == ss' && p1 == p1' && p2 == p2' && enemies == enemies' && gw == gw' && projs == projs'
        && length expl' <= length expl


-- Runs all current players animations
runPlayersAnimation :: InGameInfos -> ((), InGameInfos)
runPlayersAnimation (InGameInfos ss p1 p2 enemies gw projs expl) =
    let
        newP1AfterAnim = runPlayerAnimation p1
        newP2AfterAnim = runPlayerAnimation p2
    in
        ((), initInGameInfos ss newP1AfterAnim newP2AfterAnim enemies gw projs expl)

prop_post_runPlayersAnimation :: InGameInfos -> Bool
prop_post_runPlayersAnimation igi@(InGameInfos ss _ _ enemies gw projs expl) =
    let (_, (InGameInfos ss' _ _ enemies' gw' projs' expl')) = runPlayersAnimation igi
    in  ss == ss' && enemies == enemies' && gw == gw' && projs == projs' && expl == expl'


-- Generates periodically finite walls of rocks, each 'generateWallInterval' frames
generateWall :: FrameCounter -> StdGen -> InGameInfos -> (StdGen, InGameInfos)
generateWall currentFrameCounter gen igi@(InGameInfos ss p1 p2 enemies gw projs expl)
    | currentFrameCounter `mod` generateWallInterval /= 0 || currentFrameCounter == 0 = (gen, igi) -- no generation
    | otherwise = 
        let
            (newWall, gen2) = startFiniteWall gen
            newGW = addFiniteWall gw newWall
        in
            (gen2, initInGameInfos ss p1 p2 enemies newGW projs expl)

-- ============================================================
-- ====================== IN GAME STATE =======================
-- ============================================================

type InGameState a = St.State InGameInfos a -- IN GAME STATE MONAD

-- MAIN IN GAME UPDATE FUNCTION
updateInGame :: FrameCounter -> StdGen -> Keyboard -> Float -> InGameState ()
updateInGame nbFrames gen kbd deltaTime = do
    igi <- St.get

    -- update players animation (red spaceship on damage, or death explosion)
    runPlayersAnimationSt

    -- potentially generate a random finite wall
    gen2 <- generateWallSt nbFrames gen

    -- potentially generate enemies
    _ <- generateEnemiesSt nbFrames gen2

    -- make ennemies shoot and move them
    runEnemiesSt

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
        movePlayerInsideScreenSt True
        -- handle collisions between player1 and enemies
        handleCollisionPlayerWithEnemiesSt True
      else return ()

    if not p2Dead
      then do
        -- bump the player2 from walls if needed
        bumpPlayerFromWallsSt False
        -- update player2 direction and speed
        updatePlayerDirectionSpeedSt False (player2NewDirectionSpeed kbd deltaTime)
        -- move player2
        movePlayerInsideScreenSt False
        -- handle collisions between player2 and enemies
        handleCollisionPlayerWithEnemiesSt False
      else return ()

    -- update hit explosion animations
    runExplosionsSt

    -- move projectiles
    moveProjectilesSt

    -- handle projectile collisions
    handleCollisionProjectilesWithPlayersEnemiesSt

prop_pre_updateInGame :: Keyboard -> Float -> Bool
prop_pre_updateInGame _ deltaTime = deltaTime >= 0

runPlayersAnimationSt :: InGameState ()
runPlayersAnimationSt = St.state runPlayersAnimation

generateWallSt :: FrameCounter -> StdGen -> InGameState StdGen
generateWallSt nbFrames gen = St.state (generateWall nbFrames gen)

generateEnemiesSt :: FrameCounter -> StdGen -> InGameState StdGen
generateEnemiesSt nbFrames gen = St.state (generateEnemies nbFrames gen)

runEnemiesSt :: InGameState ()
runEnemiesSt = St.state runEnemies

moveWallsSt :: InGameState ()
moveWallsSt = St.state moveWalls

bumpPlayerFromWallsSt :: Bool -> InGameState ()
bumpPlayerFromWallsSt isP1 = St.state (bumpPlayerFromWalls isP1)

updatePlayerDirectionSpeedSt :: Bool -> (Direction, ObjectSpeed) -> InGameState ()
updatePlayerDirectionSpeedSt isP1 ds = St.state (updatePlayerDirectionSpeed isP1 ds)

movePlayerInsideScreenSt :: Bool -> InGameState ()
movePlayerInsideScreenSt isP1 = St.state (movePlayerInsideScreen isP1)

runExplosionsSt :: InGameState ()
runExplosionsSt = St.state runExplosions

handleCollisionPlayerWithEnemiesSt :: Bool -> InGameState ()
handleCollisionPlayerWithEnemiesSt isP1 = St.state (handleCollisionPlayerWithEnemies isP1)

moveProjectilesSt :: InGameState ()
moveProjectilesSt = St.state moveProjectiles

handleCollisionProjectilesWithPlayersEnemiesSt :: InGameState ()
handleCollisionProjectilesWithPlayersEnemiesSt = St.state handleCollisionProjectilesWithPlayersEnemies