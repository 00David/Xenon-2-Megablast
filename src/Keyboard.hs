module Keyboard (module Keyboard) where
import Graphics.Gloss.Interface.IO.Interact

import Data.Set

import GameSetup
import Objects.Objects

type Keyboard = Set Key

initKeyboard :: Keyboard
initKeyboard = empty

handleKeyEvent :: Event -> Keyboard -> Keyboard
handleKeyEvent (EventKey key Down _ _) kbd = insert key kbd
handleKeyEvent (EventKey key Up _ _) kbd = delete key kbd
handleKeyEvent _ kbd = kbd   

isKeyDown :: Key -> Keyboard -> Bool
isKeyDown = member

-- Gives a new direction and object speed for player1 (up/left/down/right keys)
-- Float argument : delta time between each frame, got by updateIO, must be positive
player1NewDirectionSpeed :: Keyboard -> Float -> (Direction, ObjectSpeed)
player1NewDirectionSpeed kbd deltaTime =
    let newXDir = case (isKeyDown (SpecialKey KeyLeft) kbd, isKeyDown (SpecialKey KeyRight) kbd) of
            (True, False) -> -1
            (False, True) -> 1
            _ -> 0
        newYDir = case (isKeyDown (SpecialKey KeyUp) kbd, isKeyDown (SpecialKey KeyDown) kbd) of
            (True, False) -> 1
            (False, True) -> -1
            _ -> 0
        newDir = (initDirection newXDir newYDir)
        newSpeed = if newXDir /= 0 || newYDir /= 0 then playerDefaultSpeed*deltaTime else 0
        newObjectSpeed = (initObjectSpeed newSpeed)
        --trace (show (Direction newXDir newYDir)) $
    in (newDir, newObjectSpeed)

-- Gives a new direction and object speed for player2 (AZERTY (z/q/s/d keys) or QWERTY (w/a/s/d keys))
-- Float argument : delta time between each frame, got by updateIO, must be positive
player2NewDirectionSpeed :: Keyboard -> Float -> (Direction, ObjectSpeed)
player2NewDirectionSpeed kbd deltaTime =
    let isLeft  = isKeyDown (Char 'q') kbd || isKeyDown (Char 'a') kbd || isKeyDown (Char 'Q') kbd || isKeyDown (Char 'A') kbd
        isRight = isKeyDown (Char 'd') kbd || isKeyDown (Char 'D') kbd
        isUp    = isKeyDown (Char 'z') kbd || isKeyDown (Char 'w') kbd || isKeyDown (Char 'Z') kbd || isKeyDown (Char 'W') kbd
        isDown  = isKeyDown (Char 's') kbd || isKeyDown (Char 'S') kbd
        
        newXDir = case (isLeft, isRight) of
            (True, False) -> -1
            (False, True) -> 1
            _ -> 0
        newYDir = case (isUp, isDown) of
            (True, False) -> 1
            (False, True) -> -1
            _ -> 0
        newDir = (initDirection newXDir newYDir)
        newSpeed = if newXDir /= 0 || newYDir /= 0 then playerDefaultSpeed*deltaTime else 0
        newObjectSpeed = (initObjectSpeed newSpeed)
        --trace (show (Direction newXDir newYDir)) $
    in (newDir, newObjectSpeed)

prop_pre_playerNewDirectionSpeed :: Keyboard -> Float -> Bool
prop_pre_playerNewDirectionSpeed _ dt = dt >=0