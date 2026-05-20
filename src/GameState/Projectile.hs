{-# LANGUAGE InstanceSigs #-}
module GameState.Projectile (module GameState.Projectile) where

import Graphics.Gloss (Picture(Translate))

import qualified Data.Sequence as Seq

import GameSetup
import Graphics.Assets
import Objects.Hitbox
import Objects.Objects
import Typeclasses.Damageable
import Typeclasses.Invariant
import Typeclasses.Movable

-- ============================================================
-- ====================== PROJECTILE ==========================
-- ============================================================

type ProjectileAsset = Int -- 0 for a player shot. 0 or 1 for an enemy shot
-- type Damage = Int -- shot damage >= 0

data Projectile = 
    PlayerShot Object ProjectileAsset Damage PlayerId
    | EnemyShot Object ProjectileAsset Damage
    deriving (Eq, Show)

prop_inv_projectile :: Projectile -> Bool
prop_inv_projectile (PlayerShot po sI d pId) = prop_inv_object po && sI == 0 && d >= 1
    && (pId == 1 || pId == 2)
prop_inv_projectile (EnemyShot po sI d) = prop_inv_object po && (sI == 0 || sI == 1) && d >= 1

playerShotObject :: Direction -> ObjectSpeed -> XCoord -> YCoord -> ProjectileAsset-> Object
playerShotObject dir os x y asset = 
    let h = (initHitboxCircle x y (((Seq.index widthPlayerShotAssets asset)+(Seq.index heightPlayerShotAssets asset))/4.0))
        in (initMovableObject h dir os)

prop_pre_playerShotObject :: Direction -> ObjectSpeed -> XCoord -> YCoord -> ProjectileAsset-> Bool
prop_pre_playerShotObject _ _ _ _ asset
    | asset >= 0 && asset <= (nbPlayerShotAssets-1) = True
    | otherwise = False

enemyShotObject :: Direction -> ObjectSpeed -> XCoord -> YCoord -> ProjectileAsset -> Object
enemyShotObject dir os x y asset
    | asset == 0 = 
        let r = ((Seq.index widthEnemyShotAssets asset)
              + (Seq.index heightEnemyShotAssets asset)) / 4.0
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

prop_pre_enemyShotObject :: Direction -> ObjectSpeed -> XCoord -> YCoord -> ProjectileAsset-> Bool
prop_pre_enemyShotObject _ _ _ _ asset
    | asset >= 0 && asset <= (nbEnemyShotAssets-1) = True
    | otherwise = False

initPlayerShot :: Object -> ProjectileAsset -> Damage -> PlayerId -> Projectile
initPlayerShot po asset d pId
    | not (asset >= 0 && asset <= (nbPlayerShotAssets-1)) = error "invalid asset index"
    | d < 1 = error "damage must be >= 1"
    | not (pId == 1 || pId == 2) = error "invalid player id"
    | otherwise = PlayerShot po asset d pId

initEnemyShot :: Object -> ProjectileAsset -> Damage -> Projectile
initEnemyShot po asset d
    | not (asset >= 0 && asset <= (nbEnemyShotAssets-1)) = error "invalid asset index"
    | d < 1 = error "damage must be >= 1"
    | otherwise = EnemyShot po asset d

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

-- Indicates if a projectile is inside the screen. It is considered outside when COMPLETELY outside.
insideScreenProjectile :: Projectile -> Bool
insideScreenProjectile proj = insideScreenOrAboveHitbox (objectHitbox (projectileObject proj))

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
getTranslatedProjectileAssets ga (PlayerShot po asset _ pId) = 
    let (px, py) = centerHitbox (objectHitbox po)
    in 
        if pId == 1 then [Translate px py (Seq.index (player1ShotPics ga) asset)]-- ++ (translateHitbox h)
        else [Translate px py (Seq.index (player2ShotPics ga) asset)]-- ++ (translateHitbox h)
getTranslatedProjectileAssets ga (EnemyShot po asset _) =
    let (px, py) = centerHitbox (objectHitbox po)
    in [Translate px py (Seq.index (enemyShotPics ga) asset)]-- ++ (translateHitbox h)

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
        let objs1 = getObjects projectile
            objs2 = getObjects other
            movedObjs1 = map (\o -> moveObject o screenSpeed) objs1
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) movedObjs1