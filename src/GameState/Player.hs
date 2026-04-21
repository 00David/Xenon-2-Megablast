module GameState.Player (module GameState.Player) where

import Graphics.Gloss

import Objects.Hitbox
import Objects.Objects
import GameSetup

-- ============================================================
-- ========================= PLAYER ===========================
-- ============================================================

data Player = Player {
    playerObject :: Object, -- graphical representation of the player
    playerLifes :: Int, -- player remaining lifes, inside of [0, 3]
    playerHealth :: Int, -- health for the current player life, inside of [0, 100]
    playerScore :: Int -- player current score, positive
} deriving (Eq, Show)

prop_inv_player :: Player -> Bool
prop_inv_player (Player po lifes health score) = prop_inv_object po && lifes >= 0 && lifes <= 3
    && health >= 0 && health <= 100 && score >= 0

-- ?
initPlayerObject :: Picture -> Float -> Float -> Direction -> ObjectSpeed -> Object
initPlayerObject pic x y dir speed = 
    (initMovableObject 
        pic 
        (initHitboxRectangle (x-(widthPlayer / 2)) (y-(heightPlayer / 2)) widthPlayer heightPlayer)
        dir
        speed
    )

initPlayer :: Object -> Int -> Int -> Int -> Player
initPlayer po lifes health score
    | lifes < 0 || lifes > 3 = error "number of lifes outside of [0, 3], must be inside it"
    | health < 0 || health > 100 = error "current life health outside of [0, 100], must be inside it"
    | otherwise = Player po lifes health score