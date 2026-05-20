{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-partial-fields #-} -- for partial fields of EnemyScripts
module GameState.Enemy (module GameState.Enemy) where

import Graphics.Gloss (Picture(Translate))

import System.Random
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
-- ========================= ENNEMY ===========================
-- ============================================================

data Enemy = Enemy {
    enemyObject :: Object, -- graphical representation of the enemy
    enemyHealth :: Health, -- enemy remaining health, > 0
    enemyCollisionDamage :: Damage, -- enemy collision damage, > 0
    enemyScoreGiven :: Score, -- score given to a player killing it, > 0
    enemyScript :: EnemyScript -- the script folowed by an enemy
} deriving (Eq, Show)

prop_inv_enemy :: Enemy -> Bool
prop_inv_enemy (Enemy eo health dmg score script) = prop_inv_object eo && health > 0 && dmg > 0 && score > 0
    && prop_inv_enemyScript script

initEnemy :: Object -> Health -> Damage -> Score -> EnemyScript -> Enemy
initEnemy eo health dmg score script
    | health <= 0 = error "ennemy health must be strictly positive"
    | dmg <= 0 = error "ennemy collision damage must be strictly positive"
    | score <= 0 = error "ennemy given score must be strictly positive"
    | otherwise = Enemy eo health dmg score script

noMoveButBoomEnemyHitbox :: Float -> Float -> Hitbox
noMoveButBoomEnemyHitbox x y =
    let width = Seq.index widthEnemies 0
        height = Seq.index heightEnemies 0
    in initHitboxes x y
        [ 
            initHitboxRectangle (x-8) (y-(height/2)) 16 height,
            initHitboxRectangle (x-22) (y-25) 44 57,
            initHitboxRectangle (x-27) (y-22) 54 41,
            initHitboxRectangle (x-30) (y-11) width 22
        ]

leftRightShootEnemyHitbox :: Float -> Float -> Hitbox
leftRightShootEnemyHitbox x y =
    let width = Seq.index widthEnemies 1
        height = Seq.index heightEnemies 1
    in initHitboxes x y
        [ 
            initHitboxRectangle (x-10) (y-32) 20 height,
            initHitboxRectangle (x-17) (y-10) 34 43,
            initHitboxRectangle (x-30) (y-2) width 32
        ]

loopEnemyHitbox :: Float -> Float -> Hitbox
loopEnemyHitbox x y =
    let width = Seq.index widthEnemies 2
        height = Seq.index heightEnemies 2
    in initHitboxes x y
        [ 
            initHitboxRectangle (x-36) (y-32) width 70,
            initHitboxRectangle (x-29) (y-36) 58 77,
            initHitboxRectangle (x-21) (y-40) 42 85,
            initHitboxRectangle (x-13) (y-44) 26 93,
            initHitboxRectangle (x-5) (y-49) 10 (height-1)
        ]

startInitNoMoveButBoomEnemy :: Float -> Float -> Enemy
startInitNoMoveButBoomEnemy x y =
    let eo = (initStaticObject (noMoveButBoomEnemyHitbox x y))
    in (initEnemy eo 
        noMoveButBoomEnemyHealth 
        noMoveButBoomEnemyCollisionDamage 
        noMoveButBoomEnemyScore 
        startInitNoMoveButBoomEnemyScript)

startInitLeftRightShootEnemy :: Float -> Float -> Float -> Float -> Enemy
startInitLeftRightShootEnemy x y xTarget yTarget =
    let eo = (initMovableObject 
                (leftRightShootEnemyHitbox x y)
                (initDirection 0 (-1))
                (initObjectSpeed leftRightShootEnemySpeed)
            )
    in (initEnemy eo 
        leftRightShootEnemyHealth 
        leftRightShootEnemyCollisionDamage 
        leftRightShootEnemyScore 
        (startInitLeftRightShootEnemyScript xTarget yTarget))

startInitLoopEnemy :: Float -> Float -> Float -> Enemy
startInitLoopEnemy x y yTarget =
    let eo = (initMovableObject 
                (loopEnemyHitbox x y)
                (initDirection 0 (-1))
                (initObjectSpeed loopEnemySpeed)
            )
    in (initEnemy eo 
        loopEnemyHealth
        loopEnemyCollisionDamage 
        loopEnemyScore 
        (startInitLoopEnemyScript yTarget))

-- Creates an Enemy shot, if possible to shoot
enemyShot :: Enemy -> Maybe Projectile
enemyShot (Enemy eo _ _ _ script) =
    let (x,y) = centerHitbox (objectHitbox eo)
    in case script of
        (LeftRightShootEnemy _ _ shootD) -> 
            if shootD == 1 
                then 
                    let 
                        dir = (initDirection 0 (-1))
                        s = initObjectSpeed leftRightShootEnemyShotSpeed
                        assetIndex = 0
                        projO = (enemyShotObject dir s x (y-50) assetIndex)
                    in Just (initEnemyShot projO assetIndex 
                        leftRightShootEnemyShotDamage)
                else
                    Nothing
        _ -> Nothing


-- Moves an enemy according to its script
moveEnemy :: Enemy -> ScreenScrollingSpeed -> Enemy
moveEnemy (Enemy eo health dmg score script) screenSpeed =
    let newEo = (moveObject eo screenSpeed) -- moves the enemy
    in case script of
        (NoMoveButBoomEnemy) -> (initEnemy newEo health dmg score initNoMoveButBoomEnemyScript)
        (LeftRightShootEnemy xTarget yTarget shootD) -> 
            let
                (Direction xDir yDir) = objectDirection newEo
                (x, y) = centerHitbox (objectHitbox newEo)

                -- switch X target direction if reached
                newXTarget = if (xDir == -1 && x <= xTarget) || (xDir == 1 && x >= xTarget) then (-xTarget) else xTarget
                newXDir = if (y <= yTarget) -- only if Y target already reached
                    then if (x < newXTarget) then 1 else -1 
                    else 0

                -- set Y direction at 0 if Y target reached
                newYDir = if y <= yTarget then 0 else yDir

                -- update Enemy movements with new direction
                newDirection = (initDirection newXDir newYDir)
                newEo2 = (initMovableObject (objectHitbox newEo) newDirection (objectSpeed newEo))

                -- update script
                newScript = initLeftRightShootEnemyScript newXTarget yTarget shootD
            in (initEnemy newEo2 health dmg score newScript)
        (LoopEnemy yTarget blSteps lSteps tlSteps tSteps trSteps rSteps brSteps goBottom) -> 
            let
                (_, y) = centerHitbox (objectHitbox newEo)
                d = objectDirection newEo
                
                (newDirection, newBlSteps, newLSteps, newTlSteps, newTSteps, newTrSteps, newRSteps, newBrSteps, newGoBottom) =
                    
                    if (y > yTarget && blSteps == defaultNbSteps) then
                        -- If above yTarget, and the loop movement has not yet started, don't change nothing
                        (d, blSteps, lSteps, tlSteps, tSteps, trSteps, rSteps, brSteps, goBottom)
                    else if goBottom then
                        -- If the loop has been done, keep going towards the bottom
                        (initDirection 0 (-1), 0, 0, 0, 0, 0, 0, 0, True)
                    else
                        -- If the yTarget has been reached, we start/keep looping
                        if blSteps > 0 then
                            -- Phase 1: Bottom-left
                            (initDirection (-1) (-1), blSteps - 1, lSteps, tlSteps, tSteps, trSteps, rSteps, brSteps, False)
                        else if lSteps > 0 then
                            -- Phase 2: Left
                            (initDirection (-1) 0, 0, lSteps - 1, tlSteps, tSteps, trSteps, rSteps, brSteps, False)
                        else if tlSteps > 0 then
                            -- Phase 3: Top-left
                            (initDirection (-1) 1, 0, 0, tlSteps - 1, tSteps, trSteps, rSteps, brSteps, False)
                        else if tSteps > 0 then
                            -- Phase 4: Top
                            (initDirection 0 1, 0, 0, 0, tSteps - 1, trSteps, rSteps, brSteps, False)
                        else if trSteps > 0 then
                            -- Phase 5: Top-Right
                            (initDirection 1 1, 0, 0, 0, 0, trSteps - 1, rSteps, brSteps, False)
                        else if rSteps > 0 then
                            -- Phase 6: Right
                            (initDirection 1 0, 0, 0, 0, 0, 0, rSteps - 1, brSteps, False)
                        else if brSteps > 0 then
                            -- Phase 7: Bottom-Right
                            (initDirection 1 (-1), 0, 0, 0, 0, 0, 0, brSteps - 1, False)
                        else
                            -- The loop has been completely done : goBottom activated
                            (initDirection 0 (-1), 0, 0, 0, 0, 0, 0, 0, True)
                
                -- update Enemy movements with new direction
                newEo2 = (initMovableObject (objectHitbox newEo) newDirection (objectSpeed newEo))
                
                -- update script
                newScript = initLoopEnemyScript yTarget newBlSteps newLSteps newTlSteps newTSteps newTrSteps newRSteps newBrSteps newGoBottom
            in (initEnemy newEo2 health dmg score newScript)

prop_pre_moveEnemy :: Enemy -> ScreenScrollingSpeed -> Bool
prop_pre_moveEnemy (Enemy eo _ _ _ script) screenSpeed =
    let positiveScreenSpeed = screenSpeed > 0
        (Direction dirX dirY) = objectDirection eo
        (x,y) = centerHitbox (objectHitbox eo)
        (ObjectSpeed speed) = objectSpeed eo
    in case script of
        (NoMoveButBoomEnemy) -> positiveScreenSpeed && speed == 0 && dirX == 0 && dirY == 0
        (LeftRightShootEnemy xTarget yTarget _) -> 
            let
                movingBottomIfAbove = if y > yTarget then dirY == -1 else True
                movingLeftIfAtRightOfTarget = if y <= yTarget && x > xTarget then dirX == -1 else True
                movingRightIfAtLeftOfTarget = if y <= yTarget && x < xTarget then dirX == 1 else True
            in positiveScreenSpeed && speed > 0 && movingBottomIfAbove
                && movingLeftIfAtRightOfTarget && movingRightIfAtLeftOfTarget
        (LoopEnemy yTarget blSteps lSteps tlSteps tSteps trSteps rSteps brSteps goBottom) -> 
            let
                hasSpeed = speed > 0
                
                -- Verification according to the state of the loop
                correctDirectionForPhase = 
                    if y > yTarget && blSteps == defaultNbSteps then
                        -- If above yTarget, and the loop movement has not yet started : must go towards the bottom
                        dirY == -1
                    else if goBottom then
                        -- If the loop has been done : must go towards the bottom
                        dirX == 0 && dirY == -1
                    else
                        -- Verify the coherence of the direction during loop phases 
                        if blSteps > 0 then
                            dirX == -1 && dirY == -1  -- Bottom-Left
                        else if lSteps > 0 then
                            dirX == -1 && dirY == 0   -- Left
                        else if tlSteps > 0 then
                            dirX == -1 && dirY == 1   -- Top-Left
                        else if tSteps > 0 then
                            dirX == 0 && dirY == 1    -- Top
                        else if trSteps > 0 then
                            dirX == 1 && dirY == 1    -- Top-Right
                        else if rSteps > 0 then
                            dirX == 1 && dirY == 0    -- Right
                        else if brSteps > 0 then
                            dirX == 1 && dirY == -1   -- Bottom-Right
                        else
                            -- Every steps at 0 but goBottom not yet activated : impossible
                            False
                
                onlyOnePhaseActive = 
                    if y > yTarget && blSteps == defaultNbSteps then
                        -- Before the loop : every step counters must be at defaultNbSteps
                        blSteps == defaultNbSteps && lSteps == defaultNbSteps && 
                        tlSteps == defaultNbSteps && tSteps == defaultNbSteps && 
                        trSteps == defaultNbSteps && rSteps == defaultNbSteps && 
                        brSteps == defaultNbSteps && not goBottom
                    else if goBottom then
                        -- After the loop : every step counters must be at 0
                        blSteps == 0 && lSteps == 0 && tlSteps == 0 && tSteps == 0 && 
                        trSteps == 0 && rSteps == 0 && brSteps == 0
                    else
                        -- While looping : exactly one non null counter, the rest at 0 in the order
                        let allStepsList = [blSteps, lSteps, tlSteps, tSteps, trSteps, rSteps, brSteps]
                            -- Find the index of the first non null counter
                            firstNonZeroIndex = length (takeWhile (== 0) allStepsList)
                            -- Every other counters before must be at 0, this one > 0, every counter after at defaultNbSteps
                        in firstNonZeroIndex < 7 && 
                           all (== 0) (take firstNonZeroIndex allStepsList) &&
                           (allStepsList !! firstNonZeroIndex) > 0 &&
                           all (== defaultNbSteps) (drop (firstNonZeroIndex + 1) allStepsList)

            in positiveScreenSpeed && hasSpeed && correctDirectionForPhase && onlyOnePhaseActive

prop_post_moveEnemy :: Enemy -> ScreenScrollingSpeed -> Bool
prop_post_moveEnemy e@(Enemy eo health dmg score script) screenSpeed =
    let (Enemy eo' health' dmg' score' script') = (moveEnemy e screenSpeed)
        eveythingElseSame = health' == health && dmg' == dmg && score' == score
        objectHasCorrectlyMoved = prop_post_moveObject eo screenSpeed

        (Direction dirX dirY) = objectDirection eo
        (Direction dirX' dirY') = objectDirection eo'
        (x, y) = centerHitbox (objectHitbox eo)
        (x', y') = centerHitbox (objectHitbox eo')
        (ObjectSpeed speed) = objectSpeed eo
        (ObjectSpeed speed') = objectSpeed eo'
        speedUnchanged = speed' == speed

    in eveythingElseSame && objectHasCorrectlyMoved && speedUnchanged &&
        case script of
            (NoMoveButBoomEnemy) -> 
                -- Script and direction have not changed
                script' == NoMoveButBoomEnemy && dirX' == 0 && dirY' == 0
                
            (LeftRightShootEnemy xTarget yTarget shootD) -> 
                case script' of
                    (LeftRightShootEnemy xTarget' yTarget' shootD') ->
                        let
                            -- yTarget never change
                            yTargetUnchanged = yTarget' == yTarget
                            
                            -- shootD never change in moveEnemy (changed in shootEnemy function)
                            shootDUnchanged = shootD' == shootD
                            
                            -- xTarget sign change if reached
                            xTargetCorrect = 
                                if (dirX == -1 && x <= xTarget) || (dirX == 1 && x >= xTarget)
                                then xTarget' == -xTarget  -- Swap
                                else xTarget' == xTarget    -- No change
                            
                            -- Y direction stays correct after movement
                            dirYCorrect = 
                                if y' <= yTarget 
                                then dirY' == 0           -- Reached : vertical stop
                                else dirY' == -1          -- Not yet reached : keeps going towards bottom
                            
                            -- X direction correct after movement
                            dirXCorrect = 
                                if y' <= yTarget
                                then if x' < xTarget' then dirX' == 1 else dirX' == -1
                                else dirX' == dirX  -- Keeps the original direction if ytarget not yet reached
                                
                        in yTargetUnchanged && shootDUnchanged && xTargetCorrect && dirYCorrect && dirXCorrect
                    _ -> False  -- The script must not change
                    
            (LoopEnemy yTarget blSteps lSteps tlSteps tSteps trSteps rSteps brSteps goBottom) ->
                case script' of
                    (LoopEnemy yTarget' blSteps' lSteps' tlSteps' tSteps' trSteps' rSteps' brSteps' goBottom') ->
                        let
                            -- yTarget never change
                            yTargetUnchanged = yTarget' == yTarget
                            
                            -- Verification according to the phase
                            transitionCorrect =
                                if y > yTarget && blSteps == defaultNbSteps then
                                    -- Stiil above : nothing changes
                                    blSteps' == defaultNbSteps && lSteps' == defaultNbSteps &&
                                    tlSteps' == defaultNbSteps && tSteps' == defaultNbSteps &&
                                    trSteps' == defaultNbSteps && rSteps' == defaultNbSteps &&
                                    brSteps' == defaultNbSteps && goBottom' == False &&
                                    dirX' == dirX && dirY' == dirY
                                    
                                else if goBottom then
                                    -- Mode goBottom : everything at 0, direction towards bottom
                                    blSteps' == 0 && lSteps' == 0 && tlSteps' == 0 && tSteps' == 0 &&
                                    trSteps' == 0 && rSteps' == 0 && brSteps' == 0 && goBottom' == True &&
                                    dirX' == 0 && dirY' == -1
                                    
                                else
                                    -- While looping : verify phases transitions
                                    if blSteps > 0 then
                                        -- Phase 1 : blSteps decrement
                                        blSteps' == blSteps - 1 && lSteps' == lSteps &&
                                        tlSteps' == tlSteps && tSteps' == tSteps &&
                                        trSteps' == trSteps && rSteps' == rSteps &&
                                        brSteps' == brSteps && goBottom' == False &&
                                        dirX' == -1 && dirY' == -1
                                        
                                    else if lSteps > 0 then
                                        -- Phase 2 : lSteps decrement
                                        blSteps' == 0 && lSteps' == lSteps - 1 &&
                                        tlSteps' == tlSteps && tSteps' == tSteps &&
                                        trSteps' == trSteps && rSteps' == rSteps &&
                                        brSteps' == brSteps && goBottom' == False &&
                                        dirX' == -1 && dirY' == 0
                                        
                                    else if tlSteps > 0 then
                                        -- Phase 3 : tlSteps decrement
                                        blSteps' == 0 && lSteps' == 0 &&
                                        tlSteps' == tlSteps - 1 && tSteps' == tSteps &&
                                        trSteps' == trSteps && rSteps' == rSteps &&
                                        brSteps' == brSteps && goBottom' == False &&
                                        dirX' == -1 && dirY' == 1
                                        
                                    else if tSteps > 0 then
                                        -- Phase 4 : tSteps decrement
                                        blSteps' == 0 && lSteps' == 0 &&
                                        tlSteps' == 0 && tSteps' == tSteps - 1 &&
                                        trSteps' == trSteps && rSteps' == rSteps &&
                                        brSteps' == brSteps && goBottom' == False &&
                                        dirX' == 0 && dirY' == 1
                                        
                                    else if trSteps > 0 then
                                        -- Phase 5 : trSteps decrement
                                        blSteps' == 0 && lSteps' == 0 &&
                                        tlSteps' == 0 && tSteps' == 0 &&
                                        trSteps' == trSteps - 1 && rSteps' == rSteps &&
                                        brSteps' == brSteps && goBottom' == False &&
                                        dirX' == 1 && dirY' == 1
                                        
                                    else if rSteps > 0 then
                                        -- Phase 6 : rSteps decrement
                                        blSteps' == 0 && lSteps' == 0 &&
                                        tlSteps' == 0 && tSteps' == 0 &&
                                        trSteps' == 0 && rSteps' == rSteps - 1 &&
                                        brSteps' == brSteps && goBottom' == False &&
                                        dirX' == 1 && dirY' == 0
                                        
                                    else if brSteps > 0 then
                                        -- Phase 7 : brSteps decrement
                                        blSteps' == 0 && lSteps' == 0 &&
                                        tlSteps' == 0 && tSteps' == 0 &&
                                        trSteps' == 0 && rSteps' == 0 &&
                                        brSteps' == brSteps - 1 && goBottom' == False &&
                                        dirX' == 1 && dirY' == -1
                                        
                                    else
                                        -- Transition towards goBottom mode
                                        blSteps' == 0 && lSteps' == 0 &&
                                        tlSteps' == 0 && tSteps' == 0 &&
                                        trSteps' == 0 && rSteps' == 0 &&
                                        brSteps' == 0 && goBottom' == True &&
                                        dirX' == 0 && dirY' == -1
                                        
                        in yTargetUnchanged && transitionCorrect
                    _ -> False  -- The script must not change

-- Indicates if an enemy is inside of the screen, or above. It is considered outside when COMPLETELY outside.
insideScreenOrAboveEnemy :: Enemy -> Bool
insideScreenOrAboveEnemy (Enemy eo _ _ _ _) = insideScreenOrAboveHitbox (objectHitbox eo)

-- Makes an enemy potentially shoot, according to its script
shootEnemy :: Enemy -> (Maybe Projectile, Enemy)
shootEnemy e@(Enemy eo health dmg score script) =
    case script of
        (LeftRightShootEnemy xTarget yTarget shootD) -> 
            let 
                maybeProj = enemyShot e
                newShootD = if shootD == 1 then leftRightShootEnemyShootDelay else (shootD-1) -- shoot delay reseted or decremented
                newScript = (initLeftRightShootEnemyScript xTarget yTarget newShootD)
            in (maybeProj, (initEnemy eo health dmg score newScript))
        _ -> (Nothing, e)

prop_post_shootEnemy :: Enemy -> Bool
prop_post_shootEnemy e@(Enemy eo health dmg score script) =
    let (maybeProj, (Enemy eo' health' dmg' score' script')) = shootEnemy e
        -- Shooting should not modify enemy stats or object data
        everythingElseSame = eo' == eo && health' == health && dmg' == dmg && score' == score
    in everythingElseSame && case script of
        -- Shooting script case
        (LeftRightShootEnemy xTarget yTarget shootD) ->
            case script' of
                (LeftRightShootEnemy xTarget' yTarget' shootD') ->
                    -- Target positions should remain unchanged
                    xTarget' == xTarget && yTarget' == yTarget

                    -- Shoot delay should reset after shooting, otherwise remain unchanged
                    && (if shootD > 1
                        then shootD' == shootD
                        else shootD' == leftRightShootEnemyShootDelay)

                    -- A shooting enemy should create a projectile
                    && case maybeProj of
                        Nothing -> False
                        Just _ -> True
                _ -> False

        -- Non-shooting scripts should stay identical
        _ ->
            script == script'
            -- No projectile should be created
            && case maybeProj of
                Nothing -> True
                Just _ -> False

-- Generates randomly a list of enemies, represented by a formation
generateListEnemies :: Int -> StdGen -> (StdGen,[Enemy])
generateListEnemies n gen
    | n <= 0 = (gen,[])
    | otherwise =
        let
            -- Common enemy type for whole wave
            (enemyType, gen1) = randomR (0 :: Int, 2) gen

            -- Center X of formation
            (centerX, gen2) = randomR (leftXScreenBound+300, rightXScreenBound-300) gen1

            -- Same Y for everybody
            (offsetY, gen3) = randomR (50,300) gen2
            y = topYScreenBound + offsetY

            -- Same target Y for everybody
            (targetOffset, gen4) = randomR (100,300) gen3
            yTarget = topYScreenBound - targetOffset

            -- Same X target for whole formation
            (xTarget, gen5) = randomR (150,rightXScreenBound-150) gen4

            enemies = generateFormation enemyType n centerX y xTarget yTarget
        in
            (gen5,enemies)

prop_post_generateListEnemies :: Int -> StdGen -> Bool
prop_post_generateListEnemies n gen =
    let (gen', newEnemies) = generateListEnemies n gen
    in gen' /= gen && all insideScreen newEnemies -- for an Enemy, he is considered inside even above screen

-- Generates a formation of enemies, for the given parameters
generateFormation :: Int -> Int -> Float -> Float -> Float -> Float -> [Enemy]
generateFormation enemyType n centerX y xTarget yTarget =
    let
        spacing = 100
        startX = centerX - fromIntegral (n-1) * spacing / 2
        xs = [startX + fromIntegral i * spacing | i <- [0..n-1]]
    in
        fmap (\x ->
                case enemyType of
                    0 -> startInitNoMoveButBoomEnemy x y
                    1 -> startInitLeftRightShootEnemy x y xTarget yTarget
                    _ -> startInitLoopEnemy x y yTarget
            ) xs

prop_pre_generateFormation :: Int -> Int -> Float -> Float -> Float -> Float -> Bool
prop_pre_generateFormation enemyType n centerX y xTarget yTarget =
    (enemyType >= 0 && enemyType <= 2) && (n >= 1 && n <= 5) -- verifies the type and number of enemies
    -- given the spacing (100) and the max number of enemies in the formation (5), ensures that the center of it will show all enemies
    && centerX >= (leftXScreenBound+200) && centerX <= (rightXScreenBound-200)
    && y > topYScreenBound -- y above screen because enemies spawn above
    && (yTarget >= bottomYScreenBound && yTarget <= topYScreenBound) -- y and x target inside the screen
    && (xTarget >= leftXScreenBound && xTarget <= rightXScreenBound) 

prop_post_generateFormation :: Int -> Int -> Float -> Float -> Float -> Float -> Bool
prop_post_generateFormation enemyType n centerX y xTarget yTarget =
    let newEnemies = generateFormation enemyType n centerX y xTarget yTarget
    in (length newEnemies) > 0 && all insideScreen newEnemies -- for an Enemy, he is considered inside even above screen


-- ============================================================
-- =================== ENNEMY INVARIANT =======================
-- ============================================================

instance Invariant Enemy where
    prop_inv :: Enemy -> Bool
    prop_inv = prop_inv_enemy 

-- ============================================================
-- ==================== ENEMY RENDERABLE ======================
-- ============================================================

instance Renderable Enemy where
    getTranslatedAssets :: GameAssets -> Enemy -> [Picture]
    getTranslatedAssets ga enemy = getTranslatedEnemyAsset ga enemy

-- Returns a list of translated enemy assets.
getTranslatedEnemyAsset :: GameAssets -> Enemy -> [Picture]
getTranslatedEnemyAsset ga (Enemy eo _ _ _ script) = 
    let (ex, ey) = centerHitbox (objectHitbox eo)
    in case script of
        (NoMoveButBoomEnemy) -> [Translate ex ey (Seq.index (enemiesPics ga) 0)]
        (LeftRightShootEnemy _ _ _) -> [Translate ex ey (Seq.index (enemiesPics ga) 1)]
        (LoopEnemy _ _ _ _ _ _ _ _ _) -> [Translate ex ey (Seq.index (enemiesPics ga) 2)]

-- ============================================================
-- ===================== ENEMY MOVABLE =======================
-- ============================================================

instance Movable Enemy where
    move :: Enemy -> ScreenScrollingSpeed -> Enemy
    move = moveEnemy

    insideScreen :: Enemy -> Bool
    insideScreen = insideScreenOrAboveEnemy

-- ============================================================
-- ==================== ENEMY COLLIDABLE ======================
-- ============================================================

instance Collidable Enemy where
    getObjects :: Enemy -> [Object]
    getObjects enemy = [enemyObject enemy]

    collision :: Collidable b => Enemy -> b -> Bool
    collision enemy other =
        let objs1 = getObjects enemy
            objs2 = getObjects other
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) objs1

    willCollide :: Collidable b => Enemy -> b -> ScreenScrollingSpeed -> Bool  
    willCollide enemy other screenSpeed =
        let objs1 = getObjects enemy
            objs2 = getObjects other
            movedObjs1 = map (\o -> moveObject o screenSpeed) objs1
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) movedObjs1

-- ============================================================
-- ==================== ENEMY DAMAGEABLE ======================
-- ============================================================

instance Damageable Enemy where
    currentHealth :: Enemy -> Maybe Health
    currentHealth e = Just (enemyHealth e) -- if it exists, an enemy cannot be dead

    takeDamage :: Damage -> Enemy -> Maybe Enemy
    takeDamage d (Enemy obj health dmg score script) = 
        let newHealth = health-d
        in
            if newHealth > 0 then Just (initEnemy obj newHealth dmg score script)
            else Nothing

-- ============================================================
-- ===================== ENNEMY SCRIPT=========================
-- ============================================================

type NbStepsRemaining = Int
defaultNbSteps :: NbStepsRemaining
defaultNbSteps = 10

type DelayRemaining = Int

data EnemyScript =
    NoMoveButBoomEnemy
    | LeftRightShootEnemy{ 
            xTargetEnemyScript :: Float,  -- the X that the enemy must reach while going left or right. Once it is reached, its sign is inverted. 
            yTargetEnemyScript :: Float, -- the Y that the enemy must reach before going left/right
            shootDelayEnemyScript :: Int -- shoots every time this delay reaches 1, then reseted.
        }
    | LoopEnemy{ 
            yTargetEnemyScript :: Float,
            bottomLeftStepsEnemyScript :: NbStepsRemaining,
            leftStepsEnemyScript :: NbStepsRemaining,
            topLeftStepsEnemyScript :: NbStepsRemaining,
            topStepsEnemyScript :: NbStepsRemaining,
            topRightStepsEnemyScript :: NbStepsRemaining,
            rightStepsEnemyScript :: NbStepsRemaining,
            bottomRightStepsEnemyScript :: NbStepsRemaining,
            goBottomEnemyScript :: Bool
        }
    deriving (Eq, Show)

prop_inv_enemyScript :: EnemyScript -> Bool
prop_inv_enemyScript (NoMoveButBoomEnemy) = True
prop_inv_enemyScript (LeftRightShootEnemy xTarget yTarget shootD) = 
    xTarget > leftXScreenBound && xTarget < rightXScreenBound
    && yTarget > bottomYScreenWithBarBound 
    && shootD > 0
prop_inv_enemyScript (LoopEnemy yTarget blSteps lSteps tlSteps tSteps trSteps rSteps brSteps _) = 
    yTarget > bottomYScreenWithBarBound
    && blSteps >= 0 && blSteps <= defaultNbSteps
    && lSteps >= 0 && lSteps <= defaultNbSteps
    && tlSteps >= 0 && tlSteps <= defaultNbSteps
    && tSteps >= 0 && tSteps <= defaultNbSteps
    && trSteps >= 0 && trSteps <= defaultNbSteps
    && rSteps >= 0 && rSteps <= defaultNbSteps
    && brSteps >= 0 && brSteps <= defaultNbSteps

initNoMoveButBoomEnemyScript :: EnemyScript
initNoMoveButBoomEnemyScript = NoMoveButBoomEnemy

initLeftRightShootEnemyScript :: Float -> Float -> DelayRemaining -> EnemyScript
initLeftRightShootEnemyScript xTarget yTarget shootDelay
    | xTarget <= leftXScreenBound || xTarget >= rightXScreenBound = error "xTarget out of screen"
    | yTarget <= bottomYScreenWithBarBound = error "yTarget bellow screen"
    | shootDelay < 1 = error "invalid shootDelay"
    | otherwise = LeftRightShootEnemy xTarget yTarget shootDelay

initLoopEnemyScript :: Float -> NbStepsRemaining -> NbStepsRemaining -> NbStepsRemaining -> NbStepsRemaining -> 
    NbStepsRemaining -> NbStepsRemaining -> NbStepsRemaining -> Bool -> EnemyScript
initLoopEnemyScript yTarget blSteps lSteps tlSteps tSteps trSteps rSteps brSteps goBottom
    | yTarget <= bottomYScreenWithBarBound = error "yTarget bellow screen"
    | blSteps < 0 || blSteps > defaultNbSteps = error "invalid bottomLeftSteps"
    | lSteps < 0 || lSteps > defaultNbSteps = error "invalid leftSteps"
    | tlSteps < 0 || tlSteps > defaultNbSteps = error "invalid topLeftSteps"
    | tSteps < 0 || tSteps > defaultNbSteps = error "invalid topSteps"
    | trSteps < 0 || trSteps > defaultNbSteps = error "invalid topRightSteps"
    | rSteps < 0 || rSteps > defaultNbSteps = error "invalid rightSteps"
    | brSteps < 0 || brSteps > defaultNbSteps = error "invalid bottomRightSteps"
    | otherwise = LoopEnemy yTarget blSteps lSteps tlSteps tSteps trSteps rSteps brSteps goBottom

startInitNoMoveButBoomEnemyScript :: EnemyScript
startInitNoMoveButBoomEnemyScript = initNoMoveButBoomEnemyScript

startInitLeftRightShootEnemyScript :: Float -> Float -> EnemyScript
startInitLeftRightShootEnemyScript xTarget yTarget = (initLeftRightShootEnemyScript xTarget yTarget leftRightShootEnemyShootDelay)

startInitLoopEnemyScript :: Float -> EnemyScript
startInitLoopEnemyScript yTarget = 
    (initLoopEnemyScript yTarget defaultNbSteps defaultNbSteps defaultNbSteps defaultNbSteps defaultNbSteps defaultNbSteps defaultNbSteps False)

-- ============================================================
-- ================== ENEMY SCRIPT INVARIANT ==================
-- ============================================================

instance Invariant EnemyScript where
    prop_inv :: EnemyScript -> Bool
    prop_inv = prop_inv_enemyScript 