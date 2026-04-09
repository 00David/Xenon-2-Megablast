module Graphics.Background (module Graphics.Background) where

import Graphics.Gloss

import Graphics.Assets
import GameSetup
import Data.Fixed

heightBackgroundPicture:: Float
heightBackgroundPicture = 1100

data Background = Background{
    backgroundPicture :: Picture, 
    backgroundScrollingSpeed :: Float,
    backgroundY :: Float
} deriving Show

-- Ensures that background pictures always cover entirely screen height, by having y part of [0, heightBackgroundPicture[
prop_inv_background :: Background -> Bool
prop_inv_background (Background _ _ y) = y >= 0 && y < heightBackgroundPicture

-- tests TODO
initStartBackground :: IO Background
initStartBackground = do
    bgnd <- loadPNG "./assets/Starfield.png"
    return (Background bgnd backgroundDefaultScrollingSpeed 0)

-- tests TODO
initBackground :: Picture -> Float -> Float -> Background
initBackground pic scrollingSpeed bgndY = 
    (Background pic scrollingSpeed (mod' bgndY heightBackgroundPicture))

-- tests TODO
-- Updates background position o background pictures
-- First argument : renderIO delta time
updateBackground :: Float -> Background -> Background
updateBackground dt (Background pic speed y) =
    let dy = speed * dt
        newY = y - dy
    in (initBackground pic speed newY)

-- Gets background translated assets on the screen (3 same background pictures).
getTranslatedBackgrounds :: Background -> [Picture]
getTranslatedBackgrounds (Background pic _ y) =
    let baseY = y - (heightBackgroundPicture / 2)
    in [Translate 0 (baseY - heightBackgroundPicture) pic 
        , Translate 0 baseY pic
        , Translate 0 (baseY + heightBackgroundPicture) pic]