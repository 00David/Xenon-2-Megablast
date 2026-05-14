{-# LANGUAGE InstanceSigs #-}
module GameState.Projectile (module GameState.Projectile) where

import Graphics.Gloss (Picture(Translate))

import qualified Data.Sequence as Seq

import Damageable
import GameSetup
import Graphics.Assets
import Invariant
import Objects.Hitbox
import Objects.Objects

-- ============================================================
-- ====================== PROJECTILE ==========================
-- ============================================================

type ProjectileAsset = Int -- 0 for a player shot. 0 or 1 for an enemy shot
-- type Damage = Int -- shot damage >= 0
type Range = Float -- shot range > 0
type DistanceTraveled = Float -- shot distance traveled since creation >= 0
--type PlayerId = Int -- player id, 1 or 2

data Projectile = 
    PlayerShot Object ProjectileAsset Damage Range DistanceTraveled PlayerId
    | EnemyShot Object ProjectileAsset Damage Range DistanceTraveled
    deriving (Eq, Show)

prop_inv_projectile :: Projectile -> Bool
prop_inv_projectile (PlayerShot po sI d r dt pId) = prop_inv_object po && sI == 0 && d >= 1
    && r > 0 && dt >= 0 && (pId == 1 || pId == 2)
prop_inv_projectile (EnemyShot po sI d r dt) = prop_inv_object po && (sI == 0 || sI == 1) && d >= 1
    && r > 0 && dt >= 0

playerShotObject :: Direction -> ObjectSpeed -> Float -> Float -> ProjectileAsset-> Object
playerShotObject dir os x y asset = 
    let h = (initHitboxCircle x y (((Seq.index widthPlayerShotAssets asset)+(Seq.index heightPlayerShotAssets asset))/2.0))
        in (initMovableObject h dir os)

prop_pre_playerShotObject :: Direction -> ObjectSpeed -> Float -> Float -> ProjectileAsset-> Bool
prop_pre_playerShotObject _ _ _ _ asset
    | asset >= 0 && asset <= (nbPlayerShotAssets-1) = True
    | otherwise = False

enemyShotObject :: Direction -> ObjectSpeed -> Float -> Float -> ProjectileAsset -> Object
enemyShotObject dir os x y asset
    | asset == 0 = 
        let r = ((Seq.index widthEnemyShotAssets asset)
              + (Seq.index heightEnemyShotAssets asset)) / 2.0
            h = initHitboxCircle x y r
        in initMovableObject h dir os
    | asset == 1 =
        let w = Seq.index widthEnemyShotAssets asset
            hgt = Seq.index heightEnemyShotAssets asset
            xBL = x - (w / 2)
            yBL = y - (hgt / 2)
            h = initHitboxRectangle xBL yBL w hgt
        in initMovableObject h dir os
    | otherwise = error "unknown enemy shot asset"

prop_pre_enemyShotObject :: Direction -> ObjectSpeed -> Float -> Float -> ProjectileAsset-> Bool
prop_pre_enemyShotObject _ _ _ _ asset
    | asset >= 0 && asset <= (nbEnemyShotAssets-1) = True
    | otherwise = False

initPlayerShot :: Object -> ProjectileAsset -> Damage -> Range -> DistanceTraveled -> PlayerId -> Projectile
initPlayerShot po asset d r dt pId
    | not (asset >= 0 && asset <= (nbPlayerShotAssets-1)) = error "invalid asset index"
    | d < 1 = error "damage must be >= 1"
    | r <= 0 = error "range must be > 0"
    | dt < 0 = error "distance traveled must be >= 0"
    | not (pId == 1 || pId == 2) = error "invalid player id"
    | otherwise = PlayerShot po asset d r dt pId

initEnemyShot :: Object -> ProjectileAsset -> Damage -> Range -> DistanceTraveled -> Projectile
initEnemyShot po asset d r dt
    | not (asset >= 0 && asset <= (nbEnemyShotAssets-1)) = error "invalid asset index"
    | d < 1 = error "damage must be >= 1"
    | r <= 0 = error "range must be > 0"
    | dt < 0 = error "distance traveled must be >= 0"
    | otherwise = EnemyShot po asset d r dt

projectileObject :: Projectile -> Object
projectileObject (PlayerShot obj _ _ _ _ _) = obj
projectileObject (EnemyShot obj _ _ _ _) = obj

projectileAsset :: Projectile -> ProjectileAsset
projectileAsset (PlayerShot _ asset _ _ _ _) = asset
projectileAsset (EnemyShot _ asset _ _ _) = asset

projectileDamage :: Projectile -> Damage
projectileDamage (PlayerShot _ _ d _ _ _) = d
projectileDamage (EnemyShot _ _ d _ _) = d

projectileRange :: Projectile -> Range
projectileRange (PlayerShot _ _ _ r _ _) = r
projectileRange (EnemyShot _ _ _ r _) = r

projectileDistanceTraveled :: Projectile -> DistanceTraveled
projectileDistanceTraveled (PlayerShot _ _ _ _ dt _) = dt
projectileDistanceTraveled (EnemyShot _ _ _ _ dt) = dt

projectilePlayerId :: Projectile -> PlayerId
projectilePlayerId (PlayerShot _ _ _ _ _ pId) = pId
projectilePlayerId (EnemyShot _ _ _ _ _) = 0

-- Indicates if a projectile was fired by a player. Otherwise it was by an enemy.
isPlayerShot :: Projectile -> Bool
isPlayerShot (PlayerShot _ _ _ _ _ _) = True
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
            then [Translate px py (Seq.index (playerShotPics ga) (projectileAsset proj))]-- ++ (translateHitbox h)
            else [Translate px py (Seq.index (enemyShotPics ga) (projectileAsset proj))]-- ++ (translateHitbox h)
        
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

    willCollide :: Collidable b => Projectile -> b -> ScreenScrollingSpeed -> Bool  
    willCollide projectile other screenSpeed =
        let objs1 = getObjects projectile
            objs2 = getObjects other
            movedObjs1 = map (\o -> moveObject o screenSpeed) objs1
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) movedObjs1