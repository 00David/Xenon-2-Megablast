module Keyboard (module Keyboard) where
import Graphics.Gloss.Interface.IO.Interact

import Data.Set (Set)
import qualified Data.Set as S

import Debug.Trace

import Model

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