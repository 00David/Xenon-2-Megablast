{-# LANGUAGE InstanceSigs #-}
module GameState.Enemy (module GameState.Enemy) where

import Graphics.Gloss (Picture(Translate))

import GameSetup
import Graphics.Assets
import Invariant
import Objects.Hitbox
import Objects.Objects

-- ============================================================
-- ========================= ENNEMY ===========================
-- ============================================================

type EnemyHealth = Int -- enemy remaining health, > 0

data Enemy = Enemy {
    enemyObject :: Object, -- graphical representation of the enemy
    enemyHealth :: EnemyHealth
} deriving (Eq, Show)

prop_inv_enemy :: Enemy -> Bool
prop_inv_enemy (Enemy eo health) = prop_inv_object eo && health > 0

initStaticEnemyRectangleObject :: Float -> Float -> Object
initStaticEnemyRectangleObject x y = 
    (initStaticObject 
        (initHitboxRectangle (x-(widthVirus / 2)) (y-(heightVirus / 2)) widthVirus heightVirus)
    )

initEnemy :: Object -> EnemyHealth -> Enemy
initEnemy eo health
    | health <= 0 = error "ennemy health must be strictly positive"
    | otherwise = Enemy eo health

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
getTranslatedEnemyAsset ga enemy = 
    let eo = enemyObject enemy
        (ex, ey) = centerHitbox (objectHitbox eo)
        h = objectHitbox eo
    in [Translate ex ey (virusPic ga)] ++ (translateHitbox h)

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

    willCollide :: Collidable b => Enemy -> b -> Float -> Bool  
    willCollide enemy other screenSpeed =
        let objs1 = getObjects enemy
            objs2 = getObjects other
            movedObjs1 = map (\o -> moveObject o screenSpeed) objs1
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) movedObjs1