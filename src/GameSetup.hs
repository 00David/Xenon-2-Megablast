module GameSetup (module GameSetup) where
    
import Graphics.Gloss
import Data.Sequence

-- ============================================================
-- ========================= ASSETS ===========================
-- ============================================================

widthScreen :: Int
widthScreen = 1100
heightScreen :: Int
heightScreen = 700

leftXScreenBound :: Float
leftXScreenBound = -((fromIntegral widthScreen) / 2)
rightXScreenBound :: Float
rightXScreenBound = ((fromIntegral widthScreen) / 2)
topYScreenBound :: Float
topYScreenBound = ((fromIntegral heightScreen) / 2)
bottomYScreenBound :: Float
bottomYScreenBound = -((fromIntegral heightScreen) / 2)
bottomYScreenWithBarBound :: Float
bottomYScreenWithBarBound = bottomYScreenBound+33 -- bottomYScreenBound counting bottom score bar

widthVirus :: Float
widthVirus = 65
heightVirus :: Float
heightVirus = 64

loadVirus :: IO Picture
loadVirus = do
    virus <- loadBMP "./assets/virus.bmp"
    return virus

widthPlayer :: Float
widthPlayer = 110
heightPlayer :: Float
heightPlayer = 76

widthRocks :: Seq Float
widthRocks = fromList [90, 90, 87, 84]
heightRocks :: Seq Float
heightRocks = fromList [42, 42, 44, 42]

nbRockAssets :: Int
nbRockAssets = 4

-- ============================================================
-- ========================= SPEEDS ===========================
-- ============================================================

screenDefaultSpeed :: Float
screenDefaultSpeed = 3

playerDefaultSpeed :: Float
playerDefaultSpeed = 300 -- pixels / second

backgroundDefaultScrollingSpeed :: Float
backgroundDefaultScrollingSpeed = 100 -- pixels / second

framesPerSecond :: Int
framesPerSecond = 60