module GameState.Player (module GameState.Player) where

import Graphics.Gloss

import Objects.Hitbox
import Objects.Objects
import GameSetup

-- ============================================================
-- ========================= PLAYER ===========================
-- ============================================================

data Player = AliveP Object Int Int Int
    | DeadP Object Int Int
    deriving (Eq, Show)

{-- 
When Alive, it has :
- a graphical representation of the player
- player remaining lifes, inside of [1, 3]
- health for the current player life, inside of ]0, 100]
- player current score, positive
Whe Dead, it has :
- a graphical representation of the player
- player current score, positive
- player current explosion animation, inside of [1, 7]
--}

prop_inv_player :: Player -> Bool
prop_inv_player (AliveP po lifes health score) = prop_inv_object po && lifes >= 1 && lifes <= 3
    && health >= 1 && health <= 100 && score >= 0
prop_inv_player (DeadP po score anim) = prop_inv_object po && score >= 0
    && anim >= 1 && anim <= 7

-- ?
initPlayerObject :: Picture -> Float -> Float -> Direction -> ObjectSpeed -> Object
initPlayerObject pic x y dir speed = 
    (initMovableObject 
        pic 
        (initHitboxRectangle (x-(widthPlayer / 2)) (y-(heightPlayer / 2)) widthPlayer heightPlayer)
        dir
        speed
    )

initAlivePlayer :: Object -> Int -> Int -> Int -> Player
initAlivePlayer po lifes health score
    | lifes < 0 || lifes > 3 = error "number of lifes outside of [0, 3], must be inside it"
    | health < 0 || health > 100 = error "current life health outside of [0, 100], must be inside it"
    | score < 0 = error "score must be positive"
    | otherwise = AliveP po lifes health score

initDeadPlayer :: Object -> Int -> Int -> Player
initDeadPlayer po score anim
    | score < 0 = error "score must be positive"
    | anim < 1 || anim > 7 = error "animation number must be inside of [1, 7]" 
    | otherwise = DeadP po score anim

playerObject :: Player -> Object
playerObject (AliveP o _ _ _) = o
playerObject (DeadP o _ _) = o

playerLifes :: Player -> Int
playerLifes (AliveP _ l _ _) = l
playerLifes (DeadP _ _ _) = 0

playerHealth :: Player -> Int
playerHealth (AliveP _ _ h _) = h
playerHealth (DeadP _ _ _) = 0

playerScore :: Player -> Int
playerScore (AliveP _ _ _ s) = s
playerScore (DeadP _ s _) = s

playerExplAnimation :: Player -> Int
playerExplAnimation (AliveP _ _ _ _) = 0
playerExplAnimation (DeadP _ _ anim) = anim

-- Indicates if a player is dead
isPlayerDead :: Player -> Bool
isPlayerDead (AliveP _ _ _ _) = False
isPlayerDead (DeadP _ _ _) = True