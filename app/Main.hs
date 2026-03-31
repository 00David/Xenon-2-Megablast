module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game

import Debug.Trace

import Model
import Keyboard
import Objects
import Hitbox
import GameSetup
import RandomGenerations
import PictureUtils
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

-- GameControl initialisation

data GameControl = GameControl { 
    keyboard :: Keyboard,
    state :: GameState -- StartMenu or InGame
} deriving Show

initGame :: IO GameControl
initGame = do 
    return GameControl {
        keyboard = initKeyboard,
        state = (initStartMenu Start)
    }

-- Rendering
renderIO :: Picture -> Picture -> [Picture] -> GameControl -> IO Picture
renderIO bgnd virus blasters (GameControl _ gs) = 
    case gs of
        -- Start menu
        StartMenu option ->
            -- displays information texts
            let subtitle = Translate (-180) (200) (Scale 0.3 0.3 (Color white (Text "An inspiration of ...")))
                title = Translate (-290) (100) (Scale 0.4 0.4 (Color white (Text "XENON 2 : MEGABLAST")))
                textOption1 = Translate (-43) (0) (Scale 0.3 0.3 (Color white (Text "Start")))
                textOption2 = Translate (-63) (-100) (Scale 0.3 0.3 (Color white (Text "Option2")))
                (xSelect1, xSelect2, ySelect) = case option of
                    Start -> (-90, 65, 0)
                    Option2 -> (-110, 90, -100) 
                select1 = Translate xSelect1 ySelect (Scale 0.3 0.3 (Color white (Text ">")))
                select2 = Translate xSelect2 ySelect (Scale 0.3 0.3 (Color white (Text "<")))
            in return (Pictures [bgnd, subtitle, title, textOption1, textOption2, select1, select2])
        -- In game
        InGame (InGameInfos p1 vx vy) ->
            let p1o = playerObject p1
                (Direction xdirp1 ydirp1) = objectDirection p1o 
            in case centerHitbox (objectHitbox p1o) of
                Just (p1x, p1y) -> do
                    let blastersEnbled = blastersEnabled blasters p1 -- get the sprites of the enabled spaceship blasters
                    return (Pictures ([bgnd]++blastersEnbled++[Translate p1x p1y (objectPicture p1o), Translate vx vy virus]))
                Nothing -> error "player must have a center"
  
-- Event handling
handleEventsIO :: Event -> GameControl -> IO GameControl
handleEventsIO ev (GameControl kbd gs) = do
    -- trace ("event received: " <> show ev) 
    let newKBD = (handleKeyEvent ev kbd) -- keyboard update
    case gs of
        -- Start menu
        StartMenu option -> 
            -- if the space bar is pressed on "Start", launches the game
            if option == Start && (isKeyDown (SpecialKey KeySpace) newKBD)
                then do
                    (vx, vy) <- generateVirusCoordinates
                    spaceshipP1 <- loadPNG "./assets/spaceship/spaceship_norm.png"
                    return GameControl {
                        keyboard = initKeyboard,
                        state = initInGame spaceshipP1 vx vy 0 0
                    }
                else 
                    case (isKeyDown (SpecialKey KeyUp) newKBD, isKeyDown (SpecialKey KeyDown) newKBD, option) of
                    (True, True, _) -> return (GameControl newKBD gs)
                    (_, True, Start) -> return (GameControl newKBD (initStartMenu Option2))
                    (True, _, Option2) -> return (GameControl newKBD (initStartMenu Start))
                    _ -> return (GameControl newKBD gs)
        -- In game
        InGame _ -> 
            -- if the "Escape" key is pressed while in game, we're back to the start menu
            if (isKeyDown (SpecialKey KeyEsc) newKBD)
                then return GameControl {
                    keyboard = initKeyboard,
                    state = initStartMenu Start
                }
            else return (GameControl newKBD gs)

-- Updating
updateIO :: Float -> GameControl -> IO GameControl
updateIO deltaTime (GameControl kbd gs) = do
    case gs of
        -- Start menu
        StartMenu _ -> return (GameControl kbd gs)
        -- In game
        InGame ig1@(InGameInfos p1@(Player p1o _) _ _) ->
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
            
                --trace (show (Direction xdirp1 ydirp1)) $

                -- direction and speed player1 updates
                ig2 = ig1{ player1=p1{ playerObject = MovableO picp1 hp1 (Direction xdirp1 ydirp1) sp1 } }

                -- player1 position update
                ig3 = movePlayer1 ig2

            in
            -- in case of collision with the virus, moves it elsewhere
            if collisionWithVirus ig3 then trace "VIRUS DETRUIT !" $ do
                (newVX, newVY) <- generateVirusCoordinates
                return (GameControl kbd (InGame(ig3{virusX = newVX, virusY = newVY})))
            else
                return (GameControl kbd (InGame ig3))

-- Game loop
main :: IO ()
main = do
    bgnd <- loadPNG "./assets/Starfield.png"
    virus <- loadBMP "./assets/virus.bmp"
    -- spaceship blasters are loaded into an array
    blasters <- sequence 
        [ loadPNG "./assets/spaceship/blaster_left.png"
        , loadPNG "./assets/spaceship/blaster_right.png"
        , loadPNG "./assets/spaceship/blaster_top_left.png"
        , loadPNG "./assets/spaceship/blaster_top_right.png"
        ]
    initCtrl <- initGame
    playIO 
        (InWindow "Xenon 2 : Megablast" (widthScreen, heightScreen) (10, 10)) 
        black 
        framesPerSecond
        initCtrl
        (renderIO bgnd virus blasters)
        handleEventsIO
        updateIO