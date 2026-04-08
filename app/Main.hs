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
import Assets
import RandomGenerations
import Background

-- GameControl initialisation

data GameControl = GameControl { 
    keyboard :: Keyboard,
    state :: GameState, -- StartMenu or InGame
    assets :: GameAssets,
    background :: Background
} deriving Show

initGame :: IO GameControl
initGame = do 
    assts <- initGameAssets
    bgnd <- initStartBackground
    return GameControl {
        keyboard = initKeyboard,
        state = (initStartMenu Start),
        assets = assts,
        background = bgnd
    }

-- Rendering
renderIO :: GameControl -> IO Picture
renderIO (GameControl _ gs assts bgnd) = 
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
            in return (Pictures ((getTranslatedBackgrounds bgnd)++[subtitle, title, textOption1, textOption2, select1, select2]))
        -- In game
        InGame (InGameInfos p1 p2 enemies) ->
            let p1o = playerObject p1
            in case centerHitbox (objectHitbox p1o) of
                Just (p1x, p1y) -> do
                    let picturesP1Boosters = getTranslatedBoosterAssets p1 assts -- get the sprites of the enabled spaceship boosters
                        picturesEnemies = getTranslatedEnemiesAssets enemies -- get the sprites of the enemies
                        picturesBackground = getTranslatedBackgrounds bgnd
                    return (Pictures (picturesBackground++picturesP1Boosters++[Translate p1x p1y (objectPicture p1o)]++picturesEnemies
                        -- bottom bar pictures
                        ++ (getTranslatedBottomBar p1 p2 assts)
                        ))
                Nothing -> error "player must have a center"
  
-- Event handling
handleEventsIO :: Event -> GameControl -> IO GameControl
handleEventsIO ev (GameControl kbd gs assts bgnd) = do
    -- trace ("event received: " <> show ev) 
    let newKBD = (handleKeyEvent ev kbd) -- keyboard update
    case gs of
        -- Start menu
        StartMenu option -> 
            -- if the space bar is pressed on "Start", launches the game
            if option == Start && (isKeyDown (SpecialKey KeySpace) newKBD)
                then do
                    (vx, vy) <- generateVirusCoordinates
                    return GameControl {
                        keyboard = initKeyboard,
                        state = startInitInGame (p1Pic assts) (virusPic assts) vx vy 0 0 0 0,
                        assets = assts,
                        background = bgnd
                    }
                else 
                    if isKeyDown (SpecialKey KeyEsc) newKBD
                        then do
                            putStrLn "Exiting..."
                            error "EXIT"
                        else
                            case (isKeyDown (SpecialKey KeyUp) newKBD, isKeyDown (SpecialKey KeyDown) newKBD, option) of
                            (True, True, _) -> return (GameControl newKBD gs assts bgnd)
                            (_, True, Start) -> return (GameControl newKBD (initStartMenu Option2) assts bgnd)
                            (True, _, Option2) -> return (GameControl newKBD (initStartMenu Start) assts bgnd)
                            _ -> return (GameControl newKBD gs assts bgnd)
        -- In game
        InGame _ -> 
            -- if the "Escape" key is pressed while in game, we're back to the start menu
            if (isKeyDown (SpecialKey KeyEsc) newKBD)
                then return (GameControl initKeyboard (initStartMenu Start) assts bgnd)
                else return (GameControl newKBD gs assts bgnd)

-- Updating
updateIO :: Float -> GameControl -> IO GameControl
updateIO deltaTime (GameControl kbd gs assts bgnd) = do
    case gs of
        -- Start menu
        StartMenu _ -> return (GameControl kbd gs assts bgnd)
        -- In game
        InGame ig1@(InGameInfos p1 p2 _) ->
            let 
                -- Background postion updated
                newBgnd = updateBackground deltaTime bgnd

                -- player1 updated direction and speed
                (newDirP1, newObjectSpeedP1) = player1NewDirectionSpeed kbd deltaTime
                p1V2 = setPlayerDirectionSpeed p1 (newDirP1, newObjectSpeedP1)

                -- player1 updated position
                newPlayer1 = updatePlayer p1V2
                ig2 = ig1{ gamePlayer1=newPlayer1 }

                -- collided enemies deleted from the GameState
                ig3 = handleCollisionP1WithEnemies ig2
            in
            -- in case of collision with the virus, moves it elsewhere
            if length (gameEnemies ig3) == 0 then trace "VIRUS DETRUIT !" $ do
                (newVX, newVY) <- generateVirusCoordinates
                let newVo = initStaticEnemyRectangleObject (virusPic assts) newVX newVY
                    newV = initEnemy newVo 1
                    newEnemies = [newV]
                return (GameControl kbd (initInGame(initInGameInfos (gamePlayer1 ig3) p2 newEnemies)) assts newBgnd)
            else
                return (GameControl kbd (InGame ig3) assts newBgnd)

-- Game loop
main :: IO ()
main = do
    initCtrl <- initGame
    playIO 
        (InWindow "Xenon 2 : Megablast" (widthScreen, heightScreen) (0, 0)) 
        black 
        framesPerSecond
        initCtrl
        renderIO
        handleEventsIO
        updateIO