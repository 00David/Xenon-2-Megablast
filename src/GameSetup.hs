module GameSetup (module GameSetup) where
import Graphics.Gloss
import Graphics.Gloss.Juicy

-- ============================================================
-- ========================= ASSETS ===========================
-- ============================================================

widthScreen :: Int
widthScreen = 1100
heightScreen :: Int
heightScreen = 700

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

loadPNG :: String -> IO Picture
loadPNG path = do
    maybePNG <- loadJuicyPNG path
    case maybePNG of
        Nothing  -> error ("Impossible to load "++path)
        Just png -> return png

-- ============================================================
-- ========================= SPEEDS ===========================
-- ============================================================

screenSpeed :: Float
screenSpeed = 3

playerSpeed :: Float
playerSpeed = 300 -- pixels / second

framesPerSecond :: Int
framesPerSecond = 60