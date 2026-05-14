{-# LANGUAGE InstanceSigs #-}
module GameState.Projectile (module GameState.Projectile) where

import Graphics.Gloss (Picture(Translate))

import qualified Data.Sequence as Seq

import GameSetup
import Graphics.Assets
import Invariant
import Objects.Hitbox
import Objects.Objects

-- ============================================================
-- ====================== PROJECTILE ==========================
-- ============================================================

type ProjectileAsset = Int -- 0 for a player shot. 0 or 1 for an enemy shot
type Damage = Int -- shot damage >= 0
type Range = Float -- shot range > 0
type DistanceTraveled = Float -- shot distance traveled since creation >= 0

data Projectile = 
    PlayerShot Object ProjectileAsset Damage Range DistanceTraveled
    | EnemyShot Object ProjectileAsset Damage Range DistanceTraveled
    deriving (Eq, Show)

prop_inv_projectile :: Projectile -> Bool
prop_inv_projectile (PlayerShot po sI d r dt) = prop_inv_object po && sI == 0 && d >= 1
    && r > 0 && dt >= 0
prop_inv_projectile (EnemyShot po sI d r dt) = prop_inv_object po && (sI == 0 || sI == 1) && d >= 1
    && r > 0 && dt >= 0

playerShotObject :: Direction -> ObjectSpeed -> Float -> Float -> ProjectileAsset-> Object
playerShotObject dir os x y sprite
    | sprite >= 0 && sprite <= (nbPlayerShotAssets-1) = 
        let h = (initHitboxCircle x y (((Seq.index widthPlayerAssets sprite)+(Seq.index heightPlayerAssets sprite))/2.0))
            in (initMovableObject h dir os)
    | otherwise = error "unknown player asset"

enemyShotObject :: Direction -> ObjectSpeed -> Float -> Float -> ProjectileAsset -> Object
enemyShotObject dir os x y sprite
    | sprite >= 0 && sprite <= (nbEnemyShotAssets-1) = 
        let h = (initHitboxCircle x y (((Seq.index widthEnemyAssets sprite)+(Seq.index heightEnemyAssets sprite))/2.0))
            in (initMovableObject h dir os)
    | otherwise = error "unknown enemy asset"

initPlayerShot :: Direction -> ObjectSpeed -> Float -> Float -> ProjectileAsset -> Damage -> Range -> DistanceTraveled -> Projectile
initPlayerShot dir speed x y asset d r dt  = 
    let po = (playerShotObject dir speed x y asset)
    in (PlayerShot po asset d r dt)

initEnemyShot :: Direction -> ObjectSpeed -> Float -> Float -> ProjectileAsset -> Damage -> Range -> DistanceTraveled -> Projectile
initEnemyShot dir speed x y asset d r dt  = 
    let po = (enemyShotObject dir speed x y asset)
    in (PlayerShot po asset d r dt)

projectileObject :: Projectile -> Object
projectileObject (PlayerShot obj _ _ _ _) = obj
projectileObject (EnemyShot obj _ _ _ _) = obj

projectileAsset :: Projectile -> ProjectileAsset
projectileAsset (PlayerShot _ asset _ _ _) = asset
projectileAsset (EnemyShot _ asset _ _ _) = asset

-- Indicates if a projectile was fired by a player. Otherwise it was by an enemy.
isPlayerShot :: Projectile -> Bool
isPlayerShot (PlayerShot _ _ _ _ _) = True
isPlayerShot (EnemyShot _ _ _ _ _) = False

-- Indicates if a projectile is inside the screen
insideScreenProjectile :: Projectile -> Bool
insideScreenProjectile proj = 
    let h = objectHitbox (projectileObject proj)
    in (insideScreenHitbox h)

-- ============================================================
-- ================= PROJECTILE INVARIANT =====================
-- ============================================================

instance Invariant Projectile where
    prop_inv :: Projectile -> Bool
    prop_inv = prop_inv_projectile

-- ============================================================
-- ================= PROJECTILE RENDERABLE ====================
-- ============================================================

instance Renderable Projectile where
    getTranslatedAssets :: GameAssets -> Projectile -> [Picture]
    getTranslatedAssets ga player = getTranslatedProjectileAssets ga player

-- Returns a list of translated projectile assets.
getTranslatedProjectileAssets :: GameAssets -> Projectile -> [Picture]
getTranslatedProjectileAssets ga proj = 
    let po = projectileObject proj
        (px, py) = centerHitbox (objectHitbox po)
        h = objectHitbox po
    in 
        if (isPlayerShot proj)
            then [Translate px py (Seq.index (playerShotPics ga) (projectileAsset proj))] ++ (translateHitbox h)
            else [Translate px py (Seq.index (enemyShotPics ga) (projectileAsset proj))] ++ (translateHitbox h)
        
-- ============================================================
-- ================= PROJECTILE COLLIDABLE ====================
-- ============================================================

instance Collidable Projectile where
    getObjects :: Projectile -> [Object]
    getObjects projectile = [projectileObject projectile]

    collision :: Collidable b => Projectile -> b -> Bool
    collision projectile other =
        let objs1 = getObjects projectile
            objs2 = getObjects other
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) objs1

    willCollide :: Collidable b => Projectile -> b -> Float -> Bool  
    willCollide projectile other screenSpeed =
        let objs1 = getObjects projectile
            objs2 = getObjects other
            movedObjs1 = map (\o -> moveObject o screenSpeed) objs1
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) movedObjs1