{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module Graphics.Explosion (module Graphics.Explosion) where

import Graphics.Gloss (Picture(Translate))

import qualified Data.Sequence as Seq

import GameSetup
import Graphics.Assets
import Objects.Objects
import Objects.Hitbox
import Typeclasses.Invariant

-- ============================================================
-- =========== EXPLOSION ANIMATION ON PROJECTILE HIT ==========
-- ============================================================

data Explosion = Explosion {
    explosionX :: XCoord, -- X center of the explosion
    explosionY :: YCoord, -- Y center of the explosion
    explosionPhaseCounter :: FrameCounter, -- frame counter of the current explosion phase, inside of [1, nbFramesPerExplosionPhase]
    explosionPhase :: AnimationPhase -- the current explosion phase (= explosion animation), inside of [0, nbHitAssets-1]
} deriving (Show, Eq)

prop_inv_explosion :: Explosion -> Bool
prop_inv_explosion (Explosion _ _ cpt phase) =
    cpt >= 1 && cpt <= nbFramesPerExplosionPhase
    && phase >= 0 && phase <= (nbHitAssets-1)

-- ============================================================
-- ================= EXPLOSION CONSTRUCTORS ===================
-- ============================================================

initExplosion :: XCoord -> YCoord -> FrameCounter -> AnimationPhase -> Explosion
initExplosion x y cpt phase
    | cpt < 1 || cpt > nbFramesPerExplosionPhase = error "invalid explosion frames counter, must be inside [1, nbFramesPerExplosionPhase]"
    | phase < 0 || phase > (nbHitAssets-1) = error "invalid number of explosion phase, must be inside [0, nbHitAssets-1]"
    | otherwise = (Explosion x y cpt phase)

startInitExplosion :: XCoord -> YCoord -> Explosion
startInitExplosion x y = (initExplosion x y 1 0) -- frame counter at 1, explosion phase at 0

-- ============================================================
-- ================== EXPLOSION OPERATIONS ====================
-- ============================================================

-- Run the explosion animation : either returns the updated explosion animation, or Nothing if it has finished
runExplosion :: Explosion -> Maybe Explosion
runExplosion (Explosion x y cpt phase)
    | cpt < nbFramesPerExplosionPhase = Just (initExplosion x y (cpt+1) phase) -- increments the frames counter if limit not reached (nbFramesPerExplosionPhase)
    | cpt == nbFramesPerExplosionPhase = -- once the frames limit is reached
        if phase == (nbHitAssets-1) then Nothing -- if during last phase, returns Nothing : the animation just finished
        else Just (initExplosion x y 1 (phase+1)) -- otherwise, reset the frame counter and go to the next explosion phase
    | otherwise = error $ "impossible case "++(show cpt)++" "++(show phase)

-- Get explosions, by veryfing if a collidable has disapeared from the original list
getExplosions :: forall a. (Collidable a, Eq a) => [a] -> [a] -> [Explosion]
getExplosions beforeCollisions afterCollisions =
    let 
        -- Get the disapeared collidables (those in 'beforeCollisions', not anymore in 'afterCollisions')
        disappeared =
            filter (\collBefore ->
                not (any (\collAfter -> collBefore == collAfter)
                        afterCollisions))
                beforeCollisions
    in
        concatMap createExplosions disappeared
        where
            -- Create explosions for each collidable given
            createExplosions :: a -> [Explosion]
            createExplosions coll =
                let 
                    objs = getObjects coll -- in general, only one object is got here
                in map (\obj ->
                    let (x, y) = centerHitbox (objectHitbox obj)
                    in (startInitExplosion x y)) objs -- start an explosion at the center of each object of the disapeared collidable

prop_pre_getExplosions :: [a] -> [a] -> Bool
prop_pre_getExplosions beforeCollisions afterCollisions = length afterCollisions <= length beforeCollisions

-- There is 1 explosion created per collidable disapearing, but the collidable can
-- be represented by 0 or more objects (like walls) (in general exactly one, but no garantees), so it is impossible to have a
-- post condition on the resulting explosions list when there is a change between beforeCollisions and afterCollisions

prop_post_getExplosions :: (Collidable a, Eq a) => [a] -> [a] -> Bool
prop_post_getExplosions beforeCollisions afterCollisions =
    let expls = getExplosions beforeCollisions afterCollisions
    in
        if beforeCollisions == afterCollisions then (length expls) == 0 -- no explosion created if both given lists are the same
        else True -- otherwise, no garantees on length of expls like explained before

-- ============================================================
-- =================== EXPLOSION INVARIANT =======================
-- ============================================================

instance Invariant Explosion where
    prop_inv :: Explosion -> Bool
    prop_inv = prop_inv_explosion 

-- ============================================================
-- ==================== ENEMY RENDERABLE ======================
-- ============================================================

instance Renderable Explosion where
    getTranslatedAssets :: GameAssets -> Explosion -> [Picture]
    getTranslatedAssets ga explosion = getTranslatedExplosionAsset ga explosion

-- Returns the translated explosion asset
getTranslatedExplosionAsset :: GameAssets -> Explosion -> [Picture]
getTranslatedExplosionAsset ga (Explosion x y _ phase) = [Translate x y (Seq.index (hitPics ga) phase)]

prop_post_getTranslatedExplosionAsset :: GameAssets -> Explosion -> Bool
prop_post_getTranslatedExplosionAsset ga expl = length (getTranslatedExplosionAsset ga expl) == 1