{-# LANGUAGE InstanceSigs #-}
module Graphics.Assets (module Graphics.Assets) where

import Graphics.Gloss ( Picture(Translate, Color), rectangleWire, circleSolid, red, blue )
import Graphics.Gloss.Juicy

import Data.Char
import Data.Sequence
import qualified Data.Sequence as Seq
import qualified Data.List as List

import GameSetup
import Objects.Hitbox

-- ============================================================
-- ================= RENDERABLE TYPECLASS =====================
-- ============================================================

class Renderable a where
    -- Get a list of translated assets representing the 'a'
    getTranslatedAssets :: GameAssets -> a -> [Picture]

law_renderable_does_not_modify :: (Renderable a, Eq a) => GameAssets -> a -> Bool
law_renderable_does_not_modify ga x =
    let _ = getTranslatedAssets ga x
    in x == x

law_renderable_finite :: Renderable a => GameAssets -> a -> Bool
law_renderable_finite ga x = Prelude.length (getTranslatedAssets ga x) < 100

-- ============================================================
-- ======================== GAME ASSETS =======================
-- ============================================================

-- Loads a PNG Picture, from a given path
loadPNG :: String -> IO Picture
loadPNG path = do
    maybePNG <- loadJuicyPNG path
    case maybePNG of
        Nothing  -> error ("Impossible to load "++path)
        Just png -> return png

data GameAssets = GameAssets {
    -- players
    p1Pic :: Picture,
    p2Pic :: Picture,
    p1ExplosionPics :: Seq Picture,
    p2ExplosionPics :: Seq Picture,
    pDamagedPic :: Picture,
    pInvinciblePic :: Picture,
    pBoosterPics :: Seq Picture,
    -- enemies
    enemiesPics :: Seq Picture,
    -- bottom bar
    bottomLeftPic :: Picture,
    bottomBarPic :: Picture,
    bottomRightPic :: Picture,
    digitPics :: DigitAssets,
    p1HealthBarPics :: HealtBarAssets,
    p2HealthBarPics :: HealtBarAssets,
    -- walls
    leftWallPics :: Seq Picture,
    rightWallPics :: Seq Picture,
    -- shots
    player1ShotPics :: Seq Picture,
    player2ShotPics :: Seq Picture,
    enemyShotPics :: Seq Picture,
    -- hit explosions
    hitPics :: Seq Picture
} deriving Show

initGameAssets :: IO GameAssets
initGameAssets = do
    p1 <- loadPNG "./assets/spaceship/player1/spaceship.png"
    p2 <- loadPNG "./assets/spaceship/player2/spaceship.png"
    p1Explosions <- initPlayerExplosionAssets True
    p2Explosions <- initPlayerExplosionAssets False
    pDamaged <- loadPNG "./assets/spaceship/spaceship_damaged.png"
    pInvincible <- loadPNG "./assets/spaceship/spaceship_invincible.png"
    boosters <- initBoosterAssets
    enemies <- initEnemiesAssets
    bottomLeft <- loadPNG "./assets/bottom_score/bottom_left_bar.png"
    bottomBar <- loadPNG "./assets/bottom_score/bottom_center_bar/bottom_bar.png"
    bottomRight <- loadPNG "./assets/bottom_score/bottom_right_bar.png"
    ds <- initDigitAssets
    p1Health <- initHealthP1Assets
    p2Health <- initHealthP2Assets
    leftWalls <- initWallAssets True
    rightWalls <- initWallAssets False
    p1Shots <- initPlayerShotAssets True
    p2Shots <- initPlayerShotAssets False
    eShots <- initEnemyShotAssets
    hits <- initHitAssets
    return $ GameAssets p1 p2 p1Explosions p2Explosions pDamaged pInvincible boosters enemies
        bottomLeft bottomBar bottomRight ds p1Health p2Health leftWalls rightWalls 
        p1Shots p2Shots eShots hits

-- Translates a hitbox into its visible borders, for debug purpose
translateHitbox :: Hitbox -> [Picture]
translateHitbox (Circle x y r) = 
    [Translate x y $
        Color red $
        circleSolid r
    ]
translateHitbox (Rectangle x y w h) = 
    let centerX = x + (w / 2)
        centerY = y + (h / 2)
    in [Translate centerX centerY $
        Color blue $
        rectangleWire w h
    ]
translateHitbox (Hitboxes _ _ l) = foldr (\h acc -> (translateHitbox h) <> acc) [] l

-- ============================================================
-- ======================= BOTTOM BAR =========================
-- ============================================================

-- Gets bottom bar translated assets on the screen, for given players.
getTranslatedBottomBar :: GameAssets -> Int -> Int -> Int -> Int -> Int -> Int -> [Picture]
getTranslatedBottomBar ga scoreP1 healthP1 lifesP1 scoreP2 healthP2 lifesP2  =
    let picturesDigits = digitPics ga 
    in
        -- bottom left corner assets
        [(Translate (leftXScreenBound+37.5) (bottomYScreenBound+16.5) (bottomLeftPic ga)),
        (Translate (leftXScreenBound+14) (bottomYScreenBound+17) (letterPBlackPic (picturesDigits))),
        (Translate (leftXScreenBound+36) (bottomYScreenBound+17) (digit1BlackPic picturesDigits))]
        -- player 1 score assets
        ++(getTranslatedScoreAssets ga True scoreP1)++
        -- bottom center assets
        [(Translate 0 (bottomYScreenBound+16.5) (bottomBarPic ga))]
        ++(getTranslatedHealthAssets ga healthP1 healthP2)++
        [(Translate (-68) (bottomYScreenBound+16.5) (getDigitAsset ga lifesP1)),
        (Translate 68 (bottomYScreenBound+16.5) (getDigitAsset ga lifesP2))]
        -- player 2 score assets
        ++(getTranslatedScoreAssets ga False scoreP2)++
        -- bottom right corner assets
        [(Translate (rightXScreenBound-37.5) (bottomYScreenBound+16.5) (bottomRightPic ga)),
        (Translate (rightXScreenBound-36) (bottomYScreenBound+17) (letterPBlackPic picturesDigits)),
        (Translate (rightXScreenBound-14) (bottomYScreenBound+17) (digit2BlackPic picturesDigits))]

-- Completes a score String, by adding at the start of the string '0's until having a string of 7 digits
completeScoreString :: String -> String
completeScoreString scoreStr = aux (List.length scoreStr) scoreStr
    where
        aux :: Int -> String -> String
        aux 7 acc = acc
        aux i acc = "0"++(aux (i+1) acc)

prop_pre_completeScoreString :: String -> Bool
prop_pre_completeScoreString scoreStr
    | not (all isDigit scoreStr) = False -- score String must contain only digits
    | List.length scoreStr > 7 = False -- score String cannot have more than 7 digits
    | otherwise = True

prop_post_completeScoreString :: String -> Bool
prop_post_completeScoreString scoreStr = 
    let resScoreStr = completeScoreString scoreStr 
    in ((all isDigit resScoreStr) &&  (List.length resScoreStr == 7))

-- Transforms an Int to its corresponding string of 7 digits, beeing completed by '0's at the beginning if needed,
-- and beeing capped at a maximum value of 9999999 (7 digits)
scoreToString :: Int -> String
scoreToString score
    | l > 7 = "9999999"
    | l == 7 = scoreStr
    | otherwise = completeScoreString scoreStr
    where
        scoreStr = show score
        l = List.length scoreStr

prop_post_scoreToString :: Int -> Bool
prop_post_scoreToString score =
    let scoreStr = scoreToString score
    in List.length scoreStr == 7 && scoreStr <= "9999999"

-- Gets score digit assets translated on the screen, for a given player score
-- First argument : indicates if it is about player1's score (or player2's score) (if assets must be translated on the left or right part of the bottom bar)
-- Second argument : the player's score
getTranslatedScoreAssets :: GameAssets -> Bool -> Int -> [Picture]
getTranslatedScoreAssets ga isP1 score =
    let xPadding = if isP1 then leftXScreenBound+100 else rightXScreenBound-100-27*6
        scoreStr = scoreToString score

        -- builds the array of digit Picture, iterating over the digit caracters
        aux :: Int -> String -> [Picture]
        aux _ [] = []
        aux i (d:ds) = ((Translate (xPadding+(27*(fromIntegral i)))) (bottomYScreenBound+16.5) (getDigitAsset ga (digitToInt d))):(aux (i+1) ds)
    in aux 0 scoreStr


prop_post_getTranslatedScoreAssets :: GameAssets -> Bool -> Int -> Bool
prop_post_getTranslatedScoreAssets ga isP1 score = (List.length (getTranslatedScoreAssets ga isP1 score)) == 7

-- ============================================================
-- ==================== HEALTH BAR ASSETS =====================
-- ============================================================

data HealtBarAssets = HealtBarAssets {
    healthPics :: Seq Picture -- size of 11
} deriving Show

initHealthP1Assets :: IO HealtBarAssets
initHealthP1Assets = do
    healths <- sequence 
        ([loadPNG $ "./assets/bottom_score/bottom_center_bar/healthP1/health" ++ (show n) ++ "0_1.png" | n <- [1..9 :: Int]]
        ++ [loadPNG "./assets/bottom_score/bottom_center_bar/health100.png"])
    return $ HealtBarAssets (Seq.fromList healths)

initHealthP2Assets :: IO HealtBarAssets
initHealthP2Assets = do
    healths <- sequence 
        ([loadPNG $ "./assets/bottom_score/bottom_center_bar/healthP2/health" ++ (show n) ++ "0_2.png" | n <- [1..9 :: Int]]
        ++ [loadPNG "./assets/bottom_score/bottom_center_bar/health100.png"])
    return $ HealtBarAssets (Seq.fromList healths)

-- Returns the correct health asset for the player (player 1 or 2, according to first argument), 
-- for a given health (second argument, part of ]0, 100])
getHealthAsset :: GameAssets -> Bool -> Int -> Picture
getHealthAsset ga isP1 health = 
    let pHealthBarPics = if isP1 then p1HealthBarPics ga else p2HealthBarPics ga
    in Seq.index (healthPics pHealthBarPics) ((health - 1) `div` 10)

prop_pre_getHealthAsset :: GameAssets -> Int -> Bool
prop_pre_getHealthAsset _ health
    | health <= 0 || health > 100 = False
    | otherwise = True

-- Returns the correct health assets for both players 1 and 2 given healths. 
-- If a player has exactly 0 health, no asset is returned for him.
getTranslatedHealthAssets :: GameAssets -> Int -> Int  -> [Picture]
getTranslatedHealthAssets ga p1Health p2Health
    | p1Health < 0 = error "player1 health must be positive"
    | p1Health > 100 = error "player1 health cannot be greater than 100"
    | p2Health < 0 = error "player2 health must be positive"
    | p2Health > 100 = error "player1 health cannot be greater than 100"
    | otherwise = 
            (if p1Health == 0 then [] else [(Translate (-169) (bottomYScreenBound+17) (getHealthAsset ga True p1Health))])
            ++
            (if p2Health == 0 then [] else [(Translate 169 (bottomYScreenBound+17) (getHealthAsset ga True p2Health))])

prop_pre_getTranslatedHealthAssets :: GameAssets -> Int -> Int  -> Bool
prop_pre_getTranslatedHealthAssets _ p1Health p2Health
    | p1Health < 0 || p1Health > 100 = False
    | p2Health < 0 || p2Health > 100 = False
    | otherwise = True

prop_post_getTranslatedHealthAssets :: GameAssets -> Int -> Int -> Bool
prop_post_getTranslatedHealthAssets ga p1Health p2Health =
    let healthAssets = getTranslatedHealthAssets ga p1Health p2Health
    in case (p1Health, p2Health) of
        (0, 0) -> List.length healthAssets == 0
        (0, _) -> List.length healthAssets == 1
        (_, 0) -> List.length healthAssets == 1
        _ -> List.length healthAssets == 2

-- ============================================================
-- =============== BOTTOM DIGITS/P LETTER ASSETS ==============
-- ============================================================

data DigitAssets = DigitAssets {
    digits :: Seq Picture, -- size of 10
    letterPPic :: Picture,
    -- for players bottom left/right annotations
    digit1BlackPic :: Picture,
    digit2BlackPic :: Picture,
    letterPBlackPic :: Picture
} deriving Show

initDigitAssets :: IO DigitAssets
initDigitAssets = do
    ds <- sequence [loadPNG $ "./assets/bottom_score/digits/" ++ (show n) ++ ".png" | n <- [0..9 :: Int]]
    letterP <- loadPNG "./assets/bottom_score/digits/p.png"
    digit1Black <- loadPNG "./assets/bottom_score/digits/1_black.png"
    digit2Black <- loadPNG "./assets/bottom_score/digits/2_black.png"
    letterPBlack <- loadPNG "./assets/bottom_score/digits/p_black.png"
    return $ DigitAssets (Seq.fromList ds) letterP digit1Black digit2Black letterPBlack

-- Returns the non-black digit asset for the given Int
getDigitAsset :: GameAssets -> Int -> Picture
getDigitAsset ga i
    | i < 0 = error "Int cannot be strictly negative"
    | i > 9 = error "Int cannot be strictly greater than 9"
    | otherwise = Seq.index (digits (digitPics ga)) i

prop_pre_getDigitAsset :: GameAssets -> Int -> Bool
prop_pre_getDigitAsset _ i
    | i < 0 || i > 9 = False
    | otherwise = True

-- ============================================================
-- =============== PLAYER EXPLOSION ASSET =====================
-- ============================================================

initPlayerExplosionAssets :: Bool -> IO (Seq Picture)
initPlayerExplosionAssets isP1 =
    if isP1 then do
        expl <- sequence [loadPNG ("./assets/spaceship/player1/explosion/spaceship_expl" ++ show n ++ ".png") | n <- [0..(nbPlayerExplosionAssets-1) :: Int]]
        return (Seq.fromList expl)
    else do
        expl <- sequence [loadPNG ("./assets/spaceship/player2/explosion/spaceship_expl" ++ show n ++ ".png") | n <- [0..(nbPlayerExplosionAssets-1) :: Int]]
        return (Seq.fromList expl)

-- ============================================================
-- ==================== BOOSTER ASSETS ========================
-- ============================================================

initBoosterAssets :: IO (Seq Picture)
initBoosterAssets = do
    boosters <- sequence 
        [ loadPNG "./assets/spaceship/booster_left.png"
        , loadPNG "./assets/spaceship/booster_right.png"
        , loadPNG "./assets/spaceship/booster_top_left.png"
        , loadPNG "./assets/spaceship/booster_top_right.png"]
    return (Seq.fromList boosters)

-- ============================================================
-- ==================== ENEMIES ASSETS ========================
-- ============================================================

initEnemiesAssets :: IO (Seq Picture)
initEnemiesAssets = do
    enemies <- sequence [loadPNG ("./assets/enemies/enemy" ++ show n ++ ".png") | n <- [0..(nbEnemiesAssets-1) :: Int]]
    return (Seq.fromList enemies)

-- ============================================================
-- ====================== WALL ASSETS =========================
-- ============================================================

initWallAssets :: Bool -> IO (Seq Picture)
initWallAssets left = do
    let wallSide = if left then "left_rock" else "right_rock"
    imgs <- sequence [loadPNG ("./assets/walls/" ++ wallSide ++ show n ++ ".png") | n <- [0..3 :: Int]]
    return (Seq.fromList imgs)

-- ============================================================
-- ====================== SHOT ASSETS =========================
-- ============================================================

initPlayerShotAssets :: Bool -> IO (Seq Picture)
initPlayerShotAssets isP1 =
    if isP1 then do
        pShots <- sequence [loadPNG ("./assets/shots/player1Shot" ++ show n ++ ".png") | n <- [0..(nbPlayerShotAssets-1) :: Int]]
        return (Seq.fromList pShots)
    else do
        pShots <- sequence [loadPNG ("./assets/shots/player2Shot" ++ show n ++ ".png") | n <- [0..(nbPlayerShotAssets-1) :: Int]]
        return (Seq.fromList pShots)

initEnemyShotAssets :: IO (Seq Picture)
initEnemyShotAssets = do
    eShots <- sequence [loadPNG ("./assets/shots/enemyShot" ++ show n ++ ".png") | n <- [0..(nbEnemyShotAssets-1) :: Int]]
    return (Seq.fromList eShots)

-- ============================================================
-- ====================== HIT ASSETS =========================
-- ============================================================

initHitAssets :: IO (Seq Picture)
initHitAssets = do
    hits <- sequence [loadPNG ("./assets/hit/expl" ++ show n ++ ".png") | n <- [0..(nbHitAssets-1) :: Int]]
    return (Seq.fromList hits)