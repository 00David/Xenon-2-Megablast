module Graphics.Assets (module Graphics.Assets) where

import Graphics.Gloss ( Picture(Translate) )
import Graphics.Gloss.Juicy

import Data.Char
import Data.Sequence
import qualified Data.Sequence as Seq
import qualified Data.List as List

import GameState.Player
import GameState.Enemy
import GameSetup
import Objects.Objects
import Objects.Hitbox
import Objects.Wall

-- Loads a PNG Picture, from a given path
loadPNG :: String -> IO Picture
loadPNG path = do
    maybePNG <- loadJuicyPNG path
    case maybePNG of
        Nothing  -> error ("Impossible to load "++path)
        Just png -> return png

-- ============================================================
-- ======================== GAME ASSETS =======================
-- ============================================================

data GameAssets = GameAssets {
    -- players
    p1Pic :: Picture,
    p2Pic :: Picture,
    pBoosterPics :: [Picture],
    -- enemies
    virusPic :: Picture,
    -- bottom bar
    bottomLeftPic :: Picture,
    bottomBarPic :: Picture,
    bottomRightPic :: Picture,
    digitPics :: DigitAssets,
    p1HealthBarPics :: HealtBarAssets,
    p2HealthBarPics :: HealtBarAssets,
    -- walls
    leftWallPics :: [Picture],
    rightWallPics :: [Picture]
} deriving Show

initGameAssets :: IO GameAssets
initGameAssets = do
    p1 <- loadPNG "./assets/spaceship/spaceship_norm.png"
    p2 <- loadPNG "./assets/spaceship/spaceship_norm.png"
    -- spaceship boosters are loaded into an array
    boosters <- sequence 
        [ loadPNG "./assets/spaceship/booster_left.png"
        , loadPNG "./assets/spaceship/booster_right.png"
        , loadPNG "./assets/spaceship/booster_top_left.png"
        , loadPNG "./assets/spaceship/booster_top_right.png"]
    v <- loadBMP "./assets/virus.bmp"
    bottomLeft <- loadPNG "./assets/bottom_score/bottom_left_bar.png"
    bottomBar <- loadPNG "./assets/bottom_score/bottom_center_bar/bottom_bar.png"
    bottomRight <- loadPNG "./assets/bottom_score/bottom_right_bar.png"
    ds <- initDigitAssets
    p1Health <- initHealthP1Assets
    p2Health <- initHealthP2Assets
    leftWalls <- initWallAssets True
    rightWalls <- initWallAssets False
    return $ GameAssets p1 p2 boosters v bottomLeft bottomBar bottomRight ds p1Health p2Health leftWalls rightWalls

-- ============================================================
-- ======================= BOTTOM BAR =========================
-- ============================================================

-- Gets bottom bar translated assets on the screen, for given players.
getTranslatedBottomBar :: Player -> Player -> GameAssets -> [Picture]
getTranslatedBottomBar p1 p2 assts =
    let picturesDigits = digitPics assts 
    in
        -- bottom left corner assets
        [(Translate (leftXScreenBound+37.5) (bottomYScreenBound+16.5) (bottomLeftPic assts)),
        (Translate (leftXScreenBound+14) (bottomYScreenBound+17) (letterPBlackPic (picturesDigits))),
        (Translate (leftXScreenBound+36) (bottomYScreenBound+17) (digit1BlackPic picturesDigits))]
        -- player 1 score assets
        ++(getTranslatedScoreAssets True (playerScore p1) assts)++
        -- bottom center assets
        [(Translate 0 (bottomYScreenBound+16.5) (bottomBarPic assts))]
        ++(getTranslatedHealthAssets (playerHealth p1) (playerHealth p2) assts)++
        [(Translate (-68) (bottomYScreenBound+16.5) (getDigitAsset (playerLifes p1) assts)),
        (Translate 68 (bottomYScreenBound+16.5) (getDigitAsset (playerLifes p2) assts))]
        -- player 2 score assets
        ++(getTranslatedScoreAssets False (playerScore p2) assts)++
        -- bottom right corner assets
        [(Translate (rightXScreenBound-37.5) (bottomYScreenBound+16.5) (bottomRightPic assts)),
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
getTranslatedScoreAssets :: Bool -> Int -> GameAssets -> [Picture]
getTranslatedScoreAssets isP1 score assts =
    let xPadding = if isP1 then leftXScreenBound+100 else rightXScreenBound-100-27*6
        scoreStr = scoreToString score

        -- builds the array of digit Picture, iterating over the digit caracters
        aux :: Int -> String -> [Picture]
        aux _ [] = []
        aux i (d:ds) = ((Translate (xPadding+(27*(fromIntegral i)))) (bottomYScreenBound+16.5) (getDigitAsset (digitToInt d) assts)):(aux (i+1) ds)
    in aux 0 scoreStr


prop_post_getTranslatedScoreAssets :: Bool -> Int -> GameAssets -> Bool
prop_post_getTranslatedScoreAssets isP1 score assts = (List.length (getTranslatedScoreAssets isP1 score assts)) == 7

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
getHealthAsset :: Bool -> Int -> GameAssets -> Picture
getHealthAsset isP1 health gameAssets = 
    let pHealthBarPics = if isP1 then p1HealthBarPics gameAssets else p2HealthBarPics gameAssets
    in Seq.index (healthPics pHealthBarPics) ((health - 1) `div` 10)

prop_pre_getHealthAsset :: Int -> GameAssets -> Bool
prop_pre_getHealthAsset health _
    | health <= 0 || health > 100 = False
    | otherwise = True

-- Returns the correct health assets for both players 1 and 2 given healths. 
-- If a player has exactly 0 health, no asset is returned for him.
getTranslatedHealthAssets :: Int -> Int -> GameAssets -> [Picture]
getTranslatedHealthAssets p1Health p2Health assts
    | p1Health < 0 = error "player1 health must be positive"
    | p1Health > 100 = error "player1 health cannot be greater than 100"
    | p2Health < 0 = error "player2 health must be positive"
    | p2Health > 100 = error "player1 health cannot be greater than 100"
    | otherwise = 
            (if p1Health == 0 then [] else [(Translate (-169) (bottomYScreenBound+17) (getHealthAsset True p1Health assts))])
            ++
            (if p2Health == 0 then [] else [(Translate 169 (bottomYScreenBound+17) (getHealthAsset True p2Health assts))])

prop_pre_getTranslatedHealthAssets :: Int -> Int -> GameAssets -> Bool
prop_pre_getTranslatedHealthAssets p1Health p2Health _
    | p1Health < 0 || p1Health > 100 = False
    | p2Health < 0 || p2Health > 100 = False
    | otherwise = True

prop_post_getTranslatedHealthAssets :: Int -> Int -> GameAssets -> Bool
prop_post_getTranslatedHealthAssets p1Health p2Health assts =
    let healthAssets = getTranslatedHealthAssets p1Health p2Health assts
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
getDigitAsset :: Int -> GameAssets -> Picture
getDigitAsset i assts
    | i < 0 = error "Int cannot be strictly negative"
    | i > 9 = error "Int cannot be strictly greater than 9"
    | otherwise = Seq.index (digits (digitPics assts)) i

prop_pre_getDigitAsset :: Int -> GameAssets -> Bool
prop_pre_getDigitAsset i _
    | i < 0 || i > 9 = False
    | otherwise = True

-- ============================================================
-- ====================== BOOSTER ASSETS ======================
-- ============================================================

-- Returns a list of translated booster assets for boosters only enabled when moving with the right player direction.
getTranslatedBoosterAssets :: Player -> GameAssets -> [Picture]
-- (pBoosterPics assts)[0] = booster_left
-- (pBoosterPics assts)[1] = booster_right
-- (pBoosterPics assts)[2] = booster_top_left
-- (pBoosterPics assts)[3] = booster_top_right
getTranslatedBoosterAssets player assts = 
    let po = playerObject player
        (Direction dx dy) = objectDirection po
        (px, py) = centerHitbox (objectHitbox po)

        aux :: [Picture] -> Int -> [Picture]
        aux [] _ = []
        aux (pic_booster:xs) i
            | i == 0 = if dy > 0 then (Translate (px-16) (py-50) pic_booster):(aux xs (i+1)) else (aux xs (i+1))
            | i == 1 = if dy > 0 then (Translate (px+16) (py-50) pic_booster):(aux xs (i+1)) else (aux xs (i+1))
            | i == 2 = if dy < 0 then (Translate (px-25) (py+17) pic_booster):(aux xs (i+1)) else (aux xs (i+1))
            | i == 3 = if dy < 0 then (Translate (px+25) (py+17) pic_booster):(aux xs (i+1)) else (aux xs (i+1))
            | otherwise = error "cannot have more than 4 blaster pictures in the initial Picture array"
    in aux (pBoosterPics assts) 0

prop_post_getTranslatedBoosterAssets :: Player -> GameAssets -> Bool
prop_post_getTranslatedBoosterAssets player assts = 
    let boosterPics = getTranslatedBoosterAssets player assts
    in (List.length boosterPics) == 2 || (List.length boosterPics) == 0

-- ============================================================
-- ====================== ENEMIES ASSETS ======================
-- ============================================================

-- Returns a list of translated enemies assets.
getTranslatedEnemiesAssets :: [Enemy] -> [Picture]
getTranslatedEnemiesAssets [] = []
getTranslatedEnemiesAssets (enemy:xs) = 
    let eo = enemyObject enemy
        pic = objectPicture eo
        h = objectHitbox eo
    in (translateHitbox h pic) ++ getTranslatedEnemiesAssets xs where
        translateHitbox :: Hitbox -> Picture -> [Picture]
        translateHitbox (Circle x y _) p = [Translate x y p]
        translateHitbox (Rectangle x y w h) p = 
            let centerX = x + (w / 2)
                centerY = y + (h / 2)
            in [Translate centerX centerY p]
        translateHitbox (Hitboxes _ _ l) p = foldr (\h acc -> (translateHitbox h p) <> acc) [] l

-- ============================================================
-- ==================== WALL ASSETS =====================
-- ============================================================

initWallAssets :: Bool -> IO [Picture]
initWallAssets left = do
    let wallSide = if left then "left_wall" else "right_wall"
    imgs <- sequence [loadPNG ("./assets/walls/" ++ wallSide ++ show n ++ ".png") | n <- [0..3]]
    return imgs

-- Returns a list of translated wall assets.
getTranslatedWallAssets :: FiniteWall -> [Picture]
getTranslatedWallAssets (FiniteWall []) = []
getTranslatedWallAssets (FiniteWall (wall:xs)) = 
    let pic = objectPicture wall
        h = objectHitbox wall
    in (translateHitbox h pic) ++ getTranslatedWallAssets (FiniteWall xs) where
        translateHitbox :: Hitbox -> Picture -> [Picture]
        translateHitbox (Circle x y _) p = [Translate x y p]
        translateHitbox (Rectangle x y w h) p = 
            let centerX = x + (w / 2)
                centerY = y + (h / 2)
            in [Translate centerX centerY p]
        translateHitbox (Hitboxes _ _ l) p = foldr (\h acc -> (translateHitbox h p) <> acc) [] l

-- Returns a list of translated game wall assets. For infinite walls, it only translates a finite sub-part of them.
getTranslatedGameWallAssets :: GameWalls -> [Picture]
getTranslatedGameWallAssets (GameWalls leftWall leftWall2 rightWall rightWall2 walls) = 
    getTranslatedWallAssets (infiniteToFiniteWall leftWall) ++ 
    getTranslatedWallAssets (infiniteToFiniteWall leftWall2) ++ 
    getTranslatedWallAssets (infiniteToFiniteWall rightWall) ++ 
    getTranslatedWallAssets (infiniteToFiniteWall rightWall2) ++ 
    foldr (\w acc -> (getTranslatedWallAssets w) <> acc) [] walls