module GameSetup (module GameSetup) where

import Damageable
import Graphics.Gloss
import Data.Sequence

-- ============================================================
-- ===================== COMMON TYPES =========================
-- ============================================================

type PlayerId = Int
type Score = Int

-- ============================================================
-- ======================= GAME EVENTS ========================
-- ============================================================

maxFramesToConsider :: Int
maxFramesToConsider = framesPerSecond*10

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

widthEnemyShotAssets :: Seq Float
widthEnemyShotAssets = fromList [8, 8]
heightEnemyShotAssets :: Seq Float
heightEnemyShotAssets = fromList [8, 8]

nbEnemyShotAssets :: Int
nbEnemyShotAssets = 2

loadVirus :: IO Picture
loadVirus = do
    virus <- loadBMP "./assets/virus.bmp"
    return virus

widthPlayer :: Float
widthPlayer = 110
heightPlayer :: Float
heightPlayer = 76

widthPlayerShotAssets :: Seq Float
widthPlayerShotAssets = fromList [8]
heightPlayerShotAssets :: Seq Float
heightPlayerShotAssets = fromList [8]

nbPlayerShotAssets :: Int
nbPlayerShotAssets = 1

widthRockAssets :: Seq Float
widthRockAssets = fromList [90, 90, 87, 84]
heightRockAssets :: Seq Float
heightRockAssets = fromList [42, 42, 44, 42]

nbRockAssets :: Int
nbRockAssets = 4

-- Vertical spacing between wall segments (rocks).
cell :: Float
cell = index heightRockAssets 0

-- ============================================================
-- ========================= SPEEDS ===========================
-- ============================================================

type ScreenScrollingSpeed = Float
screenDefaultSpeed :: ScreenScrollingSpeed
screenDefaultSpeed = 3

playerDefaultSpeed :: Float
playerDefaultSpeed = 300 -- pixels / second

backgroundDefaultScrollingSpeed :: Float
backgroundDefaultScrollingSpeed = 100 -- pixels / second

framesPerSecond :: Int
framesPerSecond = 60

-- ============================================================
-- ======================== ENEMIES ===========================
-- ============================================================

enemyDefaultHealth :: Health
enemyDefaultHealth = 1

enemyDefaultCollisionDamage :: Damage
enemyDefaultCollisionDamage = 10

-- ============================================================
-- ========================= SHOTS ============================
-- ============================================================

playerDefaultShootDelay :: Int
playerDefaultShootDelay = 60 -- in frames

playerDefaultShotSpeed :: Float
playerDefaultShotSpeed = 6

playerDefaultShotDamage :: Int
playerDefaultShotDamage = 1

playerDefaultShotRange :: Float
playerDefaultShotRange = 500