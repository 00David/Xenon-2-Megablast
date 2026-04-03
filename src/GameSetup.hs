module GameSetup (module GameSetup) where
import Graphics.Gloss

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

-- ============================================================
-- ========================= SPEEDS ===========================
-- ============================================================

screenDefaultSpeed :: Float
screenDefaultSpeed = 3

playerDefaultSpeed :: Float
playerDefaultSpeed = 300 -- pixels / second

framesPerSecond :: Int
framesPerSecond = 60