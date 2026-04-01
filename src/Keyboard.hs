module Keyboard (module Keyboard) where
import Graphics.Gloss.Interface.IO.Interact

import Data.Set (Set)
import qualified Data.Set as S

import Debug.Trace

import GameSetup
import Objects

type Keyboard = Set Key

initKeyboard :: Keyboard
initKeyboard = S.empty

handleKeyEvent :: Event -> Keyboard -> Keyboard
handleKeyEvent (EventKey key Down _ _) kbd = 
    S.insert key kbd
handleKeyEvent (EventKey key Up _ _) kbd = 
    S.delete key kbd
handleKeyEvent _ kbd = kbd   

isKeyDown :: Key -> Keyboard -> Bool
isKeyDown = S.member

-- Gives a new direction and object
-- Float argument : delta time between each frame, got by updateIO
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