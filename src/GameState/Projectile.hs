{-# LANGUAGE InstanceSigs #-}
module GameState.Projectile (module GameState.Projectile) where

import Graphics.Gloss (Picture(Translate))

import qualified Data.Sequence as Seq

import GameSetup
import Graphics.Assets
import Objects.Hitbox
import Objects.Objects
import Typeclasses.Invariant
import Typeclasses.Movable

-- ============================================================
-- ====================== PROJECTILE ==========================
-- ============================================================

type ProjectileAsset = Int -- 0 or 1 for a player shot. 0 for an enemy shot
-- type Damage = Int -- shot damage >= 0

-- The contained Object must move by itself, not static
data Projectile = 
    PlayerShot Object ProjectileAsset Damage PlayerId
    | EnemyShot Object ProjectileAsset Damage
    deriving (Eq, Show)

prop_inv_projectile :: Projectile -> Bool
prop_inv_projectile (PlayerShot po asset d pId) = isMovable po && prop_inv_object po && (asset >= 0 && asset <= (nbPlayerShotAssets-1)) && d >= 1
                        && (pId == 1 || pId == 2)
prop_inv_projectile (EnemyShot po asset d) = isMovable po && prop_inv_object po && (asset >= 0 && asset <= (nbPlayerShotAssets-1)) && d >= 1

-- ============================================================
-- ================ PROJECTILE CONSTRUCTORS ===================
-- ============================================================

initPlayerShot :: Object -> ProjectileAsset -> Damage -> PlayerId -> Projectile
initPlayerShot po asset d pId
    | not (isMovable po) = error "projectile Object must move by itself"
    | not (asset >= 0 && asset <= (nbPlayerShotAssets-1)) = error "invalid asset index"
    | d < 1 = error "damage must be >= 1"
    | not (pId == 1 || pId == 2) = error "invalid player id"
    | otherwise = PlayerShot po asset d pId

startInitPlayerShot :: XCoord -> YCoord -> ObjectSpeed -> ProjectileAsset -> Damage -> PlayerId -> Projectile
startInitPlayerShot x y os asset d pId =
    let 
        h = (initHitboxCircle x y (((Seq.index widthPlayerShotAssets asset) + (Seq.index heightPlayerShotAssets asset))/4.0))
        projO = (initMovableObject h (initDirection 0 1) os)
    in (initPlayerShot projO asset d pId)

prop_pre_startInitPlayerShot :: XCoord -> YCoord -> ObjectSpeed -> ProjectileAsset -> Damage -> PlayerId -> Bool
prop_pre_startInitPlayerShot _ _ _ asset d pId = asset >= 0 && asset <= (nbPlayerShotAssets-1) && d > 0 && (pId == 1 || pId == 2)

initEnemyShot :: Object -> ProjectileAsset -> Damage -> Projectile
initEnemyShot po asset d
    | not (isMovable po) = error "projectile Object must move by itself"
    | not (asset >= 0 && asset <= (nbEnemyShotAssets-1)) = error "invalid asset index"
    | d < 1 = error "damage must be >= 1"
    | otherwise = EnemyShot po asset d

startInitEnemyShot :: XCoord -> YCoord -> ObjectSpeed -> ProjectileAsset -> Damage -> Projectile
startInitEnemyShot x y os asset d =
    let 
        h = (initHitboxCircle x y (((Seq.index widthEnemyShotAssets asset) + (Seq.index heightEnemyShotAssets asset)) / 4.0))
        projO = (initMovableObject h (initDirection 0 (-1)) os)
    in (initEnemyShot projO asset d)

prop_pre_startInitEnemyShot :: XCoord -> YCoord -> ObjectSpeed -> ProjectileAsset -> Damage -> Bool
prop_pre_startInitEnemyShot _ _ _ asset d = asset >= 0 && asset <= (nbEnemyShotAssets-1) && d > 0

-- ============================================================
-- ================= PROJECTILE OPEARATIONS ===================
-- ============================================================

projectileObject :: Projectile -> Object
projectileObject (PlayerShot obj _ _ _) = obj
projectileObject (EnemyShot obj _ _) = obj

projectileAsset :: Projectile -> ProjectileAsset
projectileAsset (PlayerShot _ asset _ _) = asset
projectileAsset (EnemyShot _ asset _) = asset

projectileDamage :: Projectile -> Damage
projectileDamage (PlayerShot _ _ d _) = d
projectileDamage (EnemyShot _ _ d) = d

projectilePlayerId :: Projectile -> PlayerId
projectilePlayerId (PlayerShot _ _ _ pId) = pId
projectilePlayerId (EnemyShot _ _ _) = 0

-- Indicates if a projectile was fired by a player. Otherwise it was by an enemy.
isPlayerShot :: Projectile -> Bool
isPlayerShot (PlayerShot _ _ _ _) = True
isPlayerShot (EnemyShot _ _ _) = False

-- Moves a projectile
moveProjectile :: Projectile -> ScreenScrollingSpeed -> Projectile
moveProjectile proj ss =
    let newPo = moveObject (projectileObject proj) ss
    in 
        if (isPlayerShot proj) then (initPlayerShot newPo (projectileAsset proj) (projectileDamage proj)(projectilePlayerId proj))
        else (initEnemyShot newPo (projectileAsset proj) (projectileDamage proj))

prop_pre_moveProjectile :: Projectile -> ScreenScrollingSpeed -> Bool
prop_pre_moveProjectile _ ss = ss > 0 -- screen scrolling speed strictly positive

prop_post_moveProjectile :: Projectile -> ScreenScrollingSpeed -> Bool
prop_post_moveProjectile proj ss = 
    let proj' = moveProjectile proj ss
    in case (proj, proj') of -- ensures that all other attributes than the object stay the same, as well as the projectile type
        ((PlayerShot _ asset d pId), (PlayerShot _ asset' d' pId')) -> asset == asset' && d == d' && pId == pId'
        ((EnemyShot _ asset d), (EnemyShot _ asset' d')) -> asset == asset' && d == d'
        _ -> False

-- Indicates if a projectile is inside the screen.
-- For enemy projectiles, it can also be above (considering that they can go only towards the bottom).
insideScreenProjectile :: Projectile -> Bool
insideScreenProjectile proj = 
    if isPlayerShot proj then insideScreenHitbox (objectHitbox (projectileObject proj))
    else insideScreenOrAboveHitbox (objectHitbox (projectileObject proj))

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
    getTranslatedAssets ga player = getTranslatedProjectileAsset ga player

-- Returns a translated projectile asset.
getTranslatedProjectileAsset :: GameAssets -> Projectile -> [Picture]
getTranslatedProjectileAsset ga (PlayerShot po asset _ pId) = 
    let (px, py) = centerHitbox (objectHitbox po)
    in 
        if pId == 1 then [Translate px py (Seq.index (player1ShotPics ga) asset)]
        else [Translate px py (Seq.index (player2ShotPics ga) asset)]
getTranslatedProjectileAsset ga (EnemyShot po asset _) =
    let (px, py) = centerHitbox (objectHitbox po)
    in [Translate px py (Seq.index (enemyShotPics ga) asset)]

prop_post_getTranslatedProjectileAsset :: GameAssets -> Projectile -> Bool
prop_post_getTranslatedProjectileAsset ga proj = length (getTranslatedProjectileAsset ga proj) == 1 -- exactly one projectile asset

-- ============================================================
-- =================== PROJECTILE MOVABLE =====================
-- ============================================================

instance Movable Projectile where
    move :: Projectile -> ScreenScrollingSpeed -> Projectile
    move = moveProjectile

    insideScreen :: Projectile -> Bool
    insideScreen = insideScreenProjectile

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
        let projectileMoved = move projectile screenSpeed
        in collision projectileMoved other