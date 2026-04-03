module Assets (module Assets) where

import Data.Char
import Data.Sequence
import qualified Data.Sequence as Seq

import Graphics.Gloss
import Graphics.Gloss.Juicy

import GameSetup
import Utils
import qualified Data.List as List

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
    p2HealthBarPics :: HealtBarAssets
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
    return $ GameAssets p1 p2 boosters v bottomLeft bottomBar bottomRight ds p1Health p2Health

-- tests TODO
-- completes a score String, by adding at the start of the string '0's until having a string of 7 digits
completeScoreString :: String -> String
completeScoreString scoreStr = aux (List.length scoreStr) scoreStr
    where
        aux :: Int -> String -> String
        aux 7 acc = acc
        aux i acc = "0"++(aux (i+1) acc)

-- tests TODO
prop_pre_completeScoreString :: String -> Bool
prop_pre_completeScoreString scoreStr
    | not (all isDigit scoreStr) = False -- score String must contain only digits
    | List.length scoreStr > 7 = False -- score String cannot have more than 7 digits
    | otherwise = True

-- tests TODO
prop_post_completeScoreString :: String -> Bool
prop_post_completeScoreString scoreStr = 
    (prop_pre_completeScoreString scoreStr) ==> 
    let resScoreStr = completeScoreString scoreStr 
    in ((all isDigit resScoreStr) &&  (List.length resScoreStr == 9))

-- tests TODO
scoreToString :: Int -> Maybe String
scoreToString score
    | l > 7 = Nothing
    | l == 7 = Just scoreStr
    | otherwise = Just (completeScoreString scoreStr)
    where
        scoreStr = show score
        l = List.length scoreStr

getTranslatedScoreAssets :: Bool -> Int -> GameAssets -> [Picture]
getTranslatedScoreAssets isP1 score assts =
    let xPadding = if isP1 then leftXScreenBound+100 else rightXScreenBound-100-27*6
    in case scoreToString score of
        Nothing -> [(Translate (xPadding+(27*i))) (bottomYScreenBound+16.5) (getDigitAsset 0 assts) | i <- [0..6]]
        Just str -> aux 0 str
            where
                -- builds the array of digit Picture, iterating over the digit caracters
                aux :: Int -> String -> [Picture]
                aux _ [] = []
                aux i (d:ds) = ((Translate (xPadding+(27*(fromIntegral i)))) (bottomYScreenBound+16.5) (getDigitAsset (digitToInt d) assts)):(aux (i+1) ds)

-- tests TODO
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
-- for a given health (seconde argument, part of ]0, 100])
getHealthAsset :: Bool -> Int -> GameAssets -> Picture
getHealthAsset isP1 health gameAssets
    | health <= 0 = error "player health must be strictly positive"
    | health > 100 = error "player health cannot be greater than 100"
    | otherwise = Seq.index (healthPics pHealthBarPics) ((health - 1) `div` 10)
    where
        pHealthBarPics = if isP1 then p1HealthBarPics gameAssets else p2HealthBarPics gameAssets

-- tests TODO
prop_pre_getHealthAsset :: Int -> GameAssets -> Bool
prop_pre_getHealthAsset health _
    | health <= 0 || health > 100 = False
    | otherwise = True

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

-- tests TODO
prop_pre_getDigitAsset :: Int -> GameAssets -> Bool
prop_pre_getDigitAsset i _
    | i < 0 || i > 9 = False
    | otherwise = True