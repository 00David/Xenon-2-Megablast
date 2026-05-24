{-# LANGUAGE InstanceSigs #-}
module GameState.Bonus (module GameState.Bonus) where

import Graphics.Gloss ( Picture(Translate))

import System.Random
import qualified Data.Sequence as Seq

import GameSetup
import GameState.Enemy
import Graphics.Assets
import Objects.Objects
import Objects.Hitbox
import Typeclasses.Invariant
import Typeclasses.Movable

-- ============================================================
-- ======================== BONUS =============================
-- ============================================================

data Bonus = PlayerBonus { -- we could imagine having more types of bonuses ...
    bonusObject :: Object, -- graphical representation of the bonus, while he has not yet been picked up
    getBonus :: PlayerShootBonus -- the bonus itself
} deriving (Eq, Show)

prop_inv_bonus :: Bonus -> Bool
prop_inv_bonus (PlayerBonus bo psb) = prop_inv_object bo && prop_inv_playerShootBonus psb

-- ============================================================
-- =================== BONUS CONSTRUCTORS =====================
-- ============================================================

initPlayerShootBonus :: Object -> PlayerShootBonus -> Bonus
initPlayerShootBonus bo psb = PlayerBonus bo psb

startInitPlayerShootBonus :: XCoord -> YCoord -> PlayerShootBonus -> Bonus
startInitPlayerShootBonus x y psb =
    let h = (initHitboxCircle x y (radiusBonus))
        bo = (initStaticObject h)
    in (initPlayerShootBonus bo psb)

-- ============================================================
-- ==================== BONUS OPERATIONS ======================
-- ============================================================

-- Randomly possibly generates a new bonus where the given enemy died
generateBonusForEnemy :: Enemy -> (StdGen, [Bonus]) -> (StdGen, [Bonus])
generateBonusForEnemy enemy (gen, bonusList) =
    let -- Get enemy position
        (x, y) = centerHitbox (objectHitbox (enemyObject enemy))
        
        -- Random check if bonus should drop (based on bonusDropChance (in [0, 1]))
        (dropChance, gen1) = randomR (0.0, 1.0) gen :: (Float, StdGen)
        
        -- Random bonus type selection (0 to 3 for the 4 types)
        (bonusTypeIndex, gen2) = randomR (0, (nbPlayerBonusAssets-1)) gen1 :: (Int, StdGen)
        
        bonusType = case bonusTypeIndex of
            0 -> ShootFaster
            1 -> DelayDecreased
            2 -> MoreDamages
            3 -> BiggerShots
            _ -> error "should never happen"
    in
        if dropChance <= bonusDropChance
            then (gen2, (startInitPlayerShootBonus x y bonusType) : bonusList)
            else (gen2, bonusList)

prop_post_generateBonusForEnemy :: Enemy -> (StdGen, [Bonus]) -> Bool
prop_post_generateBonusForEnemy enemy (gen, bonusList) =
    let (gen', bonusList') = generateBonusForEnemy enemy (gen, bonusList)
        (ex, ey) = centerHitbox (objectHitbox (enemyObject enemy))
    in
        -- The generator has changed
        gen /= gen'
        -- Either no bonus was added (dropChance > percentageBonusDrop)
        && (length bonusList' == length bonusList

        -- Or exactly one bonus was added (dropChance <= percentageBonusDrop)
        || (length bonusList' == ((length bonusList) + 1)
            && any (\bonus ->
                let h = objectHitbox (bonusObject bonus)
                    (x, y) = centerHitbox h
                in 
                    -- The new bonus is at the enemy's position
                    x == ex && y == ey
                    -- The new bonus has a valid type (not NoBonus)
                    && case getBonus bonus of 
                        NoBonus -> False
                        _ -> True
                ) bonusList'
        ))

-- Randomly possibly generates new bonuses where enemies died
generateNewBonuses :: StdGen -> [Enemy] -> [Enemy] -> (StdGen, [Bonus])
generateNewBonuses gen enemiesBefore enemiesAfter =
    let -- Find enemies that died (present in enemiesBefore but not in enemiesAfter)
        deadEnemies = filter (\e1 -> 
            let h1 = objectHitbox (enemyObject e1)
                (ex1, ey1) = centerHitbox h1
                in not (any (\e2 ->
                    let h2 = objectHitbox (enemyObject e2)
                        (ex2, ey2) = centerHitbox h2
                    in ex1 == ex2 && ey1 == ey2
                    ) enemiesAfter
                )
            ) enemiesBefore
        -- For each dead enemy, possibly generate a bonus
        (finalGen, bonuses) = foldr generateBonusForEnemy (gen, []) deadEnemies
    in (finalGen, bonuses)
    
prop_pre_generateNewBonuses :: StdGen -> [Enemy] -> [Enemy] -> Bool
prop_pre_generateNewBonuses _ enemiesBefore enemiesAfter =
    length enemiesAfter <= length enemiesBefore

prop_post_generateNewBonuses :: StdGen -> [Enemy] -> [Enemy] -> Bool
prop_post_generateNewBonuses gen enemiesBefore enemiesAfter =
    let (_, bonuses) = generateNewBonuses gen enemiesBefore enemiesAfter
        deadEnemies = filter (\e -> e `notElem` enemiesAfter) enemiesBefore
    in
        -- Number of bonuses is at most the number of dead enemies
        length bonuses <= length deadEnemies
        -- Each bonus position corresponds to a dead enemy position
        && all (\bonus -> 
            let (bx, by) = centerHitbox (objectHitbox (bonusObject bonus))
            in any (\enemy -> 
                let (ex, ey) = centerHitbox (objectHitbox (enemyObject enemy))
                in bx == ex && by == ey
            ) deadEnemies
        ) bonuses
        -- Each bonus has a valid type (not NoBonus)
        && all (\bonus -> case getBonus bonus of
            NoBonus -> False
            _ -> True
        ) bonuses

-- Moves a bonus
moveBonus :: Bonus -> ScreenScrollingSpeed -> Bonus
moveBonus (PlayerBonus bo psb) ss = initPlayerShootBonus (moveObject bo ss) psb

prop_pre_moveBonus :: Bonus -> ScreenScrollingSpeed -> Bool
prop_pre_moveBonus _ ss = ss >= 0 -- positive screen scrolling speed

prop_post_moveBonus :: Bonus -> ScreenScrollingSpeed -> Bool
prop_post_moveBonus b@(PlayerBonus _ psb) ss =
    let (PlayerBonus _ psb') = moveBonus b ss
    in psb == psb' -- ensures that the player bonus stays the same

-- Indicates if a bonus is inside of the screen, or above 
insideScreenOrAboveBonus :: Bonus -> Bool
insideScreenOrAboveBonus (PlayerBonus bo _) = insideScreenOrAboveHitbox (objectHitbox bo)

-- ============================================================
-- ==================== BONUS INVARIANT =======================
-- ============================================================

instance Invariant Bonus where
    prop_inv :: Bonus -> Bool
    prop_inv = prop_inv_bonus 

-- ============================================================
-- ==================== BONUS RENDERABLE ======================
-- ============================================================

instance Renderable Bonus where
    getTranslatedAssets :: GameAssets -> Bonus -> [Picture]
    getTranslatedAssets ga bonus = getTranslatedBonusAsset ga bonus

-- Returns a list of translated bonus assets (at most one).
getTranslatedBonusAsset :: GameAssets -> Bonus -> [Picture]
getTranslatedBonusAsset ga (PlayerBonus bo psb) = 
    let (x, y) = centerHitbox (objectHitbox bo)
    in case psb of
        NoBonus ->  []
        ShootFaster ->  [Translate x y (Seq.index (playerShootBonusPics ga) 0)]
        DelayDecreased ->  [Translate x y (Seq.index (playerShootBonusPics ga) 1)]
        MoreDamages ->  [Translate x y (Seq.index (playerShootBonusPics ga) 2)]
        BiggerShots ->  [Translate x y (Seq.index (playerShootBonusPics ga) 3)]

prop_post_getTranslatedBonusAsset :: GameAssets -> Bonus -> Bool
prop_post_getTranslatedBonusAsset ga bonus = length (getTranslatedBonusAsset ga bonus) <= 1 -- at most one bonus asset

-- ============================================================
-- ===================== BONUS MOVABLE ========================
-- ============================================================

instance Movable Bonus where
    move :: Bonus -> ScreenScrollingSpeed -> Bonus
    move = moveBonus

    insideScreen :: Bonus -> Bool
    insideScreen = insideScreenOrAboveBonus

-- ============================================================
-- ==================== BONUS COLLIDABLE ======================
-- ============================================================

instance Collidable Bonus where
    getObjects :: Bonus -> [Object]
    getObjects bonus = [bonusObject bonus]

    collision :: Collidable b => Bonus -> b -> Bool
    collision bonus other =
        let objs1 = getObjects bonus
            objs2 = getObjects other
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) objs1

    willCollide :: Collidable b => Bonus -> b -> ScreenScrollingSpeed -> Bool  
    willCollide bonus other screenSpeed =
        let bonusMoved = move bonus screenSpeed
        in collision bonusMoved other

-- ============================================================
-- ================== PLAYER SHOOT BONUS ======================
-- ============================================================

data PlayerShootBonus = NoBonus | ShootFaster | DelayDecreased | MoreDamages | BiggerShots
    deriving (Eq, Show)

prop_inv_playerShootBonus :: PlayerShootBonus -> Bool
prop_inv_playerShootBonus _ = True

-- ============================================================
-- ================= PLAYER SHOOT INVARIANT ===================
-- ============================================================

instance Invariant PlayerShootBonus where
    prop_inv :: PlayerShootBonus -> Bool
    prop_inv = prop_inv_playerShootBonus 