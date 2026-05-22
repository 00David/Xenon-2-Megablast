module GameSetup (module GameSetup) where

import Data.Sequence

import Typeclasses.Damageable

-- ============================================================
-- ===================== COMMON TYPES =========================
-- ============================================================

type XCoord = Float
type YCoord = Float
type PlayerId = Int
type Score = Int
type ShootDelay = Int
type ScreenScrollingSpeed = Float
type FrameCounter = Int
type AnimationPhase = Int

-- ============================================================
-- ======================= GAME EVENTS ========================
-- ============================================================

maxFramesToConsider :: FrameCounter
maxFramesToConsider = framesPerSecond*1000000 -- (game frame counter reseted at 0 when reached, for preventing overflow)

nbFramesPerExplosionPhase :: FrameCounter
nbFramesPerExplosionPhase = 10

nbFramesRedOnDamage :: FrameCounter
nbFramesRedOnDamage = 4

nbFramesInvincible :: FrameCounter
nbFramesInvincible = 120

invincibleIntervalBlink :: FrameCounter
invincibleIntervalBlink = 10 -- in number of frames

maxEnemies :: Int
maxEnemies = 20

generateWallInterval :: FrameCounter
generateWallInterval = 600 -- in number of frames <=> 10s

bonusDropChance :: Float
bonusDropChance = 0.1 -- inside of [0, 1]

-- ============================================================
-- ========================= ASSETS ===========================
-- ============================================================

widthScreen :: Int
widthScreen = 1100
heightScreen :: Int
heightScreen = 700

leftXScreenBound :: XCoord
leftXScreenBound = -((fromIntegral widthScreen) / 2)
rightXScreenBound :: XCoord
rightXScreenBound = ((fromIntegral widthScreen) / 2)
topYScreenBound :: YCoord
topYScreenBound = ((fromIntegral heightScreen) / 2)
bottomYScreenBound :: YCoord
bottomYScreenBound = -((fromIntegral heightScreen) / 2)
bottomYScreenWithBarBound :: YCoord
bottomYScreenWithBarBound = bottomYScreenBound+33 -- bottomYScreenBound counting bottom score bar

widthEnemies :: Seq Float
widthEnemies = fromList [60, 60, 72]
heightEnemies :: Seq Float
heightEnemies = fromList [60, 65, 100]

nbEnemiesAssets :: Int
nbEnemiesAssets = 3

widthEnemyShotAssets :: Seq Float
widthEnemyShotAssets = fromList [32, 32]
heightEnemyShotAssets :: Seq Float
heightEnemyShotAssets = fromList [32, 32]

nbEnemyShotAssets :: Int
nbEnemyShotAssets = 1

widthPlayer :: Float
widthPlayer = 110
heightPlayer :: Float
heightPlayer = 76

nbPlayerExplosionAssets :: Int
nbPlayerExplosionAssets = 6

widthPlayerShotAssets :: Seq Float
widthPlayerShotAssets = fromList [16, 32]
heightPlayerShotAssets :: Seq Float
heightPlayerShotAssets = fromList [16, 32]

nbPlayerShotAssets :: Int
nbPlayerShotAssets = 2

widthRocks :: Seq Float
widthRocks = fromList [90, 90, 87, 84]
heightRocks :: Seq Float
heightRocks = fromList [42, 42, 44, 42]

nbRockAssets :: Int
nbRockAssets = 4

-- Vertical spacing between wall segments (rocks).
rockCell :: Float
rockCell = index heightRocks 0

nbHitAssets :: Int
nbHitAssets = 7

widthBonus :: Float
widthBonus = 16
heightBonus :: Float
heightBonus = 16
radiusBonus :: Float
radiusBonus = (widthBonus + heightBonus) / 4.0

nbPlayerBonusAssets :: Int
nbPlayerBonusAssets = 4

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
-- ======================== PLAYERS ===========================
-- ============================================================

player1StartX :: XCoord
player1StartX = -200
player1StartY :: YCoord
player1StartY = 0

player2StartX :: XCoord
player2StartX = 200
player2StartY :: YCoord
player2StartY = 0

playerDefaultShootDelay :: ShootDelay
playerDefaultShootDelay = 60 -- in frames / second

playerBonusShootDelay :: ShootDelay
playerBonusShootDelay = 20 -- in frames / second

playerDefaultShotSpeed :: Float
playerDefaultShotSpeed = 6

playerBonusShotSpeed :: Float
playerBonusShotSpeed = 3*playerDefaultShotSpeed

playerDefaultShotDamage :: Damage
playerDefaultShotDamage = 1

playerBonusShotDamage :: Damage
playerBonusShotDamage = 3*playerDefaultShotDamage

-- ============================================================
-- ======================== ENEMIES ===========================
-- ============================================================

noMoveButBoomEnemyHealth :: Health
noMoveButBoomEnemyHealth = 1

leftRightShootEnemyHealth :: Health
leftRightShootEnemyHealth = 1

loopEnemyHealth :: Health
loopEnemyHealth = 3

noMoveButBoomEnemyScore :: Score
noMoveButBoomEnemyScore = 10

leftRightShootEnemyScore :: Score
leftRightShootEnemyScore = 25

loopEnemyScore :: Score
loopEnemyScore = 50

noMoveButBoomEnemyCollisionDamage :: Damage
noMoveButBoomEnemyCollisionDamage = 20

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