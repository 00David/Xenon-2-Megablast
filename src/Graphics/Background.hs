{-# LANGUAGE InstanceSigs #-}
module Graphics.Background (module Graphics.Background) where

import Graphics.Gloss

import Data.Fixed

import Graphics.Assets
import GameSetup
import Typeclasses.Invariant

heightBackgroundPicture:: Float
heightBackgroundPicture = 1100

data Background = Background{
    backgroundPicture :: Picture, 
    backgroundScrollingSpeed :: Float,
    backgroundY :: YCoord
} deriving (Eq, Show)

-- Ensures that background pictures always cover entirely screen height, by having y part of [0, heightBackgroundPicture[
prop_inv_background :: Background -> Bool
prop_inv_background (Background _ _ y) = y >= 0 && y < heightBackgroundPicture

initStartBackground :: IO Background
initStartBackground = do
    bgnd <- loadPNG "./assets/Starfield.png"
    return (Background bgnd backgroundDefaultScrollingSpeed 0)

initBackground :: Picture -> Float -> YCoord -> Background
initBackground pic scrollingSpeed bgndY = 
    (Background pic scrollingSpeed bgndY)

-- Updates background position of background pictures
-- First argument : renderIO delta time, must be positive
updateBackground :: Float -> Background -> Background
updateBackground dt (Background pic speed y) =
    let dy = speed * dt
        newY = y - dy
    in (initBackground pic speed (mod' newY heightBackgroundPicture))

prop_pre_updateBackground :: Float -> Background -> Bool
prop_pre_updateBackground dt _ = dt >= 0

prop_post_updateBackground :: Float -> Background -> Bool
prop_post_updateBackground dt bgnd@(Background pic speed _) =
    let (Background picRes speedRes yRes) = updateBackground dt bgnd
    in pic == picRes && speed == speedRes && yRes >= 0 && yRes < heightBackgroundPicture

-- Gets background translated assets on the screen (3 same background pictures).
getTranslatedBackgrounds :: Background -> [Picture]
getTranslatedBackgrounds (Background pic _ y) =
    let baseY = y - (heightBackgroundPicture / 2)
    in [Translate 0 (baseY - heightBackgroundPicture) pic 
        , Translate 0 baseY pic
        , Translate 0 (baseY + heightBackgroundPicture) pic]

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