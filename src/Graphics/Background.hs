{-# LANGUAGE InstanceSigs #-}
module Graphics.Background (module Graphics.Background) where

import Graphics.Gloss

import Data.Fixed

import Graphics.Assets
import GameSetup
import Typeclasses.Invariant

-- ============================================================
-- ======================== BACKGROUND ========================
-- ============================================================

-- While rendering, a second background picture is added above the current background picture
-- position, and a third one is added above the current background picture.
data Background = Background{
    backgroundPicture :: Picture,  -- the background picture
    backgroundScrollingSpeed :: ScreenScrollingSpeed,
    backgroundY :: YCoord -- the Y coordinate of the center background picture
} deriving (Eq, Show)

-- Ensures that background pictures always cover entirely screen height, by having Y part of [0, heightBackgroundPicture[
prop_inv_background :: Background -> Bool
prop_inv_background (Background _ _ y) = y >= 0 && y < heightBackgroundPicture

-- ============================================================
-- ================= BACKGROUND CONSTRUCTORS ==================
-- ============================================================

initBackground :: Picture -> ScreenScrollingSpeed -> YCoord -> Background
initBackground pic scrollingSpeed bgndY = 
    (Background pic scrollingSpeed bgndY)

initStartBackground :: IO Background
initStartBackground = do
    bgnd <- loadPNG "./assets/Starfield.png"
    return (Background bgnd backgroundDefaultScrollingSpeed 0)

-- ============================================================
-- ================== BACKGROUND OPERATIONS ===================
-- ============================================================

-- Updates background position of background pictures
-- First argument : renderIO delta time, must be positive
updateBackground :: Float -> Background -> Background
updateBackground dt (Background pic speed y) =
    let dy = speed * dt
        newY = y - dy
    in (initBackground pic speed (mod' newY heightBackgroundPicture))-- mod' : modulo generalized to Real type

prop_pre_updateBackground :: Float -> Background -> Bool
prop_pre_updateBackground dt _ = dt >= 0 -- renderIO delta time, must be positive

prop_post_updateBackground :: Float -> Background -> Bool
prop_post_updateBackground dt bgnd@(Background pic speed _) =
    let (Background pic' speed' _) = updateBackground dt bgnd
    in pic == pic' && speed == speed' -- the new Y is verified by the invariant, the rest must be kept unchanged

-- Gets background translated assets on the screen (3 background pictures placed on 3 different Y).
getTranslatedBackgrounds :: Background -> [Picture]
getTranslatedBackgrounds (Background pic _ y) =
    let baseY = y - (heightBackgroundPicture / 2)
    in [Translate 0 (baseY - heightBackgroundPicture) pic 
        , Translate 0 baseY pic
        , Translate 0 (baseY + heightBackgroundPicture) pic]

prop_post_getTranslatedBackgrounds :: Background -> Bool
prop_post_getTranslatedBackgrounds bgnd =
    let pics = getTranslatedBackgrounds bgnd
    in length pics== 3 -- exactly 3 background pictures

-- ============================================================
-- =================== BACKGROUND INVARIANT ===================
-- ============================================================

instance Invariant Background where
    prop_inv :: Background -> Bool
    prop_inv = prop_inv_background 

-- ============================================================
-- ================== BACKGROUND RENDERABLE ===================
-- ============================================================

instance Renderable Background where
    getTranslatedAssets :: GameAssets -> Background -> [Picture]
    getTranslatedAssets _ bgnd = getTranslatedBackgrounds bgnd