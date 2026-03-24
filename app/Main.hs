module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import System.Random
import Debug.Trace

import Model
import Keyboard

data GameControl = GameControl { 
    keyboard :: Keyboard,
    state :: GameState
  }
  deriving Show

generateCoordinates :: Int -> Int ->  IO (Float, Float)
generateCoordinates w h = do
    x <- randomRIO (fromIntegral (((-widthScreen) `div` 2) + w), fromIntegral ((widthScreen `div` 2) - w))
    y <- randomRIO (fromIntegral (((-heigthScreen) `div` 2) + h), fromIntegral ((heigthScreen `div` 2) - h))
    return (x, y)

initGame :: IO GameControl
initGame = do 
  (vx, vy) <- generateCoordinates widthVirus heigthVirus
  return GameControl {
                  keyboard = initKeyboard,
                  state = initGameState vx vy
  }

renderIO :: Picture -> Picture -> Picture -> GameControl -> IO Picture
renderIO bgnd perso virus gc = return $ render bgnd perso virus gc

render :: Picture -> Picture -> Picture -> GameControl -> Picture
render bgnd perso virus (GameControl _ (GameState px py vx vy _)) =
  Pictures [bgnd, Translate px py perso, Translate vx vy virus]

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
    (newVX, newVY) <- generateCoordinates widthVirus heigthVirus
    return newGC{state = st{virusX = newVX, virusY = newVY}}
  else
    return newGC

update :: Float -> GameControl -> GameControl
update _ (GameControl kbd gs) =
  let gs1 = if isKeyDown (SpecialKey KeyLeft) kbd then moveLeft gs else gs in
  let gs2 = if isKeyDown (SpecialKey KeyRight) kbd then moveRight gs1 else gs1 in
  let gs3 = if isKeyDown (SpecialKey KeyUp) kbd then moveUp gs2 else gs2 in
  let gs4 = if isKeyDown (SpecialKey KeyDown) kbd then moveDown gs3 else gs3 in
  GameControl kbd gs4

-- Game loop
main :: IO ()
main = do
  maybeBgnd <- loadJuicyPNG "./assets/Starfield.png"
  maybeSpaceshipP1 <- loadJuicyPNG "./assets/spaceship/spaceship_norm.png"
  virus <- loadBMP "./assets/virus.bmp"
  initCtrl <- initGame
  case (maybeBgnd, maybeSpaceshipP1) of
        (Nothing, _)  -> putStrLn "Imossible to load background"
        (_ , Nothing)  -> putStrLn "Imossible to load player1 spaceship"
        (Just bgnd, Just spaceshipP1) -> playIO 
                      (InWindow "Xenon 2 : Megablast" (widthScreen, heigthScreen) (10, 10)) 
                      black 
                      60
                      initCtrl
                      (renderIO bgnd spaceshipP1 virus)
                      handleEventsIO
                      updateIO

  