module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Debug.Trace

import Model
import Keyboard
import Objects
import Hitbox
import GameSetup
import RandomGenerations

data GameControl = GameControl { 
    keyboard :: Keyboard,
    state :: GameState
} deriving Show

initGame :: IO GameControl
initGame = do 
    (vx, vy) <- generateVirusCoordinates
    spaceshipP1 <- loadPNG "./assets/spaceship/spaceship_norm.png"
    return GameControl {
        keyboard = initKeyboard,
        state = initGameState spaceshipP1 vx vy
    } 
  

renderIO :: Picture -> Picture -> GameControl -> IO Picture
renderIO bgnd virus gc = return $ render bgnd virus gc

render :: Picture -> Picture -> GameControl -> Picture
render bgnd virus (GameControl _ (GameState p1 vx vy)) =
    let p1o = playerObject p1 in
    case centerHitbox (objectHitbox p1o) of
        Just (p1x, p1y) -> Pictures [bgnd, Translate p1x p1y (objectPicture p1o) , 
            Translate vx vy virus]
        Nothing -> error "player must have a center"
  

handleEventsIO :: Event -> GameControl -> IO GameControl
handleEventsIO ev gc = return $ handleEvents ev gc

handleEvents :: Event -> GameControl -> GameControl
handleEvents ev (GameControl kbd gs) = 
    -- trace ("event received: " <> show ev) 
    GameControl (handleKeyEvent ev kbd) gs

updateIO :: Float -> GameControl -> IO GameControl
updateIO dt gc = do 
    let newGC@(GameControl _ st) = update dt gc
    if collisionWithVirus st then trace "VIRUS DETRUIT !" $ do
        (newVX, newVY) <- generateVirusCoordinates
        return newGC{state = st{virusX = newVX, virusY = newVY}}
    else
        return newGC

update :: Float -> GameControl -> GameControl
update deltaTime (GameControl kbd gs@(GameState p1@(Player p1o _) _ _)) =
    let xdirp1 = case (isKeyDown (SpecialKey KeyLeft) kbd, isKeyDown (SpecialKey KeyRight) kbd) of
                    (True, False) -> -1
                    (False, True) -> 1
                    _ -> 0
      
        ydirp1 = case (isKeyDown (SpecialKey KeyUp) kbd, isKeyDown (SpecialKey KeyDown) kbd) of
                    (True, False) -> 1
                    (False, True) -> -1
                    _ -> 0
        picp1 = objectPicture p1o
        hp1 = objectHitbox p1o
        sp1 = if xdirp1 /= 0 || ydirp1 /= 0 then playerSpeed*deltaTime else 0
    in 
        trace (show (Direction xdirp1 ydirp1)) $
        let gs2 = gs{ player1=p1{ playerObject = MovableO picp1 hp1 (Direction xdirp1 ydirp1) sp1 } }
            gs3 = movePlayer1 gs2
        in GameControl kbd gs3

-- Game loop
main :: IO ()
main = do
    bgnd <- loadPNG "./assets/Starfield.png"
    virus <- loadBMP "./assets/virus.bmp"
    initCtrl <- initGame
    playIO 
        (InWindow "Xenon 2 : Megablast" (widthScreen, heightScreen) (10, 10)) 
        black 
        framesPerSecond
        initCtrl
        (renderIO bgnd virus)
        handleEventsIO
        updateIO