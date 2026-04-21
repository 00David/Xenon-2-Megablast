module GameState.Enemy (module GameState.Enemy) where

import Graphics.Gloss

import Objects.Hitbox
import Objects.Objects
import GameSetup

-- ============================================================
-- ========================= ENNEMY ===========================
-- ============================================================

data Enemy = Enemy {
    enemyObject :: Object, -- graphical representation of the enemy
    enemyHealth :: Int -- enemy remaining health, > 0
} deriving (Eq, Show)

prop_inv_enemy :: Enemy -> Bool
prop_inv_enemy (Enemy eo health) = prop_inv_object eo && health > 0

-- ?
initStaticEnemyRectangleObject :: Picture -> Float -> Float -> Object
initStaticEnemyRectangleObject pic x y = 
    (initStaticObject 
        pic
        (initHitboxRectangle (x-(widthVirus / 2)) (y-(heightVirus / 2)) widthVirus heightVirus)
    )

initEnemy :: Object -> Int -> Enemy
initEnemy eo health
    | health <= 0 = error "ennemy health must be strictly positive"
    | otherwise = Enemy eo health