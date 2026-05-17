module GameSetup (module GameSetup) where

import Graphics.Gloss
import Data.Sequence

import Typeclasses.Damageable

-- ============================================================
-- ===================== COMMON TYPES =========================
-- ============================================================

type PlayerId = Int
type Score = Int
type ShootDelay = Int
type ScreenScrollingSpeed = Float

-- ============================================================
-- ======================= GAME EVENTS ========================
-- ============================================================

maxFramesToConsider :: Int
maxFramesToConsider = framesPerSecond*1000000

maxEnemies :: Int
maxEnemies = 20

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
widthEnemyShotAssets = fromList [32, 32]
heightEnemyShotAssets :: Seq Float
heightEnemyShotAssets = fromList [32, 32]

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
widthPlayerShotAssets = fromList [16]
heightPlayerShotAssets :: Seq Float
heightPlayerShotAssets = fromList [16]

nbPlayerShotAssets :: Int
nbPlayerShotAssets = 1

widthRockAssets :: Seq Float
widthRockAssets = fromList [90, 90, 87, 84]
heightRockAssets :: Seq Float
heightRockAssets = fromList [42, 42, 44, 42]

nbRockAssets :: Int
nbRockAssets = 4

-- Vertical spacing between wall segments (rocks).
rockCell :: Float
rockCell = index heightRockAssets 0

-- ============================================================
-- ========================= SPEEDS ===========================
-- ============================================================

screenDefaultSpeed :: ScreenScrollingSpeed
screenDefaultSpeed = 3 -- pixels / frame

playerDefaultSpeed :: Float
playerDefaultSpeed = 300 -- pixels / second

backgroundDefaultScrollingSpeed :: Float
backgroundDefaultScrollingSpeed = 100 -- pixels / second

framesPerSecond :: Int
framesPerSecond = 60

leftRightShootEnemySpeed :: Float
leftRightShootEnemySpeed = 2 -- pixels / frame

loopEnemySpeed :: Float
loopEnemySpeed = 6 -- pixels / frame

-- ============================================================
-- ======================== ENEMIES ===========================
-- ============================================================

noMoveButBiteEnemyHealth :: Health
noMoveButBiteEnemyHealth = 1

leftRightShootEnemyHealth :: Health
leftRightShootEnemyHealth = 1

loopEnemyHealth :: Health
loopEnemyHealth = 3

noMoveButBiteEnemyScore :: Score
noMoveButBiteEnemyScore = 10

leftRightShootEnemyScore :: Score
leftRightShootEnemyScore = 25

loopEnemyScore :: Score
loopEnemyScore = 50

noMoveButBiteEnemyCollisionDamage :: Damage
noMoveButBiteEnemyCollisionDamage = 20

leftRightShootEnemyCollisionDamage :: Damage
leftRightShootEnemyCollisionDamage = 10

loopEnemyCollisionDamage :: Damage
loopEnemyCollisionDamage = 10

leftRightShootEnemyShootDelay :: ShootDelay
leftRightShootEnemyShootDelay = 60 -- in frames / second

leftRightShootEnemyShotSpeed :: Float
leftRightShootEnemyShotSpeed = 10

leftRightShootEnemyShotDamage :: Damage
leftRightShootEnemyShotDamage = 10

-- ============================================================
-- ========================= SHOTS ============================
-- ============================================================

playerDefaultShootDelay :: ShootDelay
playerDefaultShootDelay = 60 -- in frames / second

playerDefaultShotSpeed :: Float
playerDefaultShotSpeed = 6

playerDefaultShotDamage :: Damage
playerDefaultShotDamage = 1