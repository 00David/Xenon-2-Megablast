module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game

import System.Random
import qualified Control.Monad.State as St

import Debug.Trace

import GameSetup
import GameState.Enemy
import GameState.Game
import GameState.Player
import Graphics.Assets
import Graphics.Background
import RandomGenerations
import Keyboard

-- Rendering
renderIO :: Game -> IO Picture

-- Start menu
renderIO (Game _ (StartMenu option) _ bgnd _) =
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
renderIO (Game _ (InGame igi) assts bgnd _) =
    let
        player1 = (gamePlayer1 igi)
        player2 = (gamePlayer2 igi)
    in
        return (Pictures (
            (getTranslatedAssets assts bgnd) ++
            (getTranslatedAssets assts igi) ++
            (getTranslatedBottomBar assts 
                    (playerScore player1) (playerHealth player1) (playerLifes player1)
                    (playerScore player2) (playerHealth player2) (playerLifes player2))
            ))  
  
-- Event handling
handleEventsIO :: Event -> Game -> IO Game

-- Start menu
handleEventsIO ev game@(Game kbd (StartMenu option) _ _ counter) = do
    let newKBD = (handleKeyEvent ev kbd) -- keyboard update

    -- if the space bar is pressed on "Start", launches the game
    if option == Start && (isKeyDown (SpecialKey KeySpace) newKBD)
        then do
            (vx, vy) <- generateVirusCoordinates
            gen <- newStdGen
            return game{
                keyboard = initKeyboard,
                state = startInitInGame gen vx vy 0 0 0 0
                }
        else 
            if isKeyDown (SpecialKey KeyEsc) newKBD
                then do
                    putStrLn "Exiting..."
                    error "EXIT"
                else
                    case (isKeyDown (SpecialKey KeyUp) newKBD, isKeyDown (SpecialKey KeyDown) newKBD, option) of
                    (True, True, _) -> return game{keyboard = newKBD}
                    (_, True, Start) -> return game{keyboard = newKBD, state = (initStartMenu Option2)}
                    (True, _, Option2) -> return game{keyboard = newKBD, state = (initStartMenu Start)}
                    _ -> return game{keyboard = newKBD}

-- In game
handleEventsIO ev game@(Game kbd (InGame (InGameInfos p1 p2 enemies walls projectiles)) _ _ _) = do
    -- trace ("event received: " <> show ev) 
    let newKBD = (handleKeyEvent ev kbd) -- keyboard update

    -- if the "Escape" key is pressed while in game, we're back to the start menu
    if (isKeyDown (SpecialKey KeyEsc) newKBD)
        then return game{keyboard = initKeyboard, state = (initStartMenu Start)}
        else  
            -- if the "Space" key is pressed while in game, player1 SHOOOT
            if (isKeyDown (SpecialKey KeySpace) newKBD)
                then case playerShot p1 of
                    Nothing -> return game{keyboard = newKBD}
                    Just shot -> 
                        let newProjectiles = shot : projectiles
                            newIgi = initInGameInfos p1 p2 enemies walls newProjectiles
                        in return game{keyboard = newKBD, state = (InGame newIgi)}
                else return game{keyboard = newKBD}

-- Updating
updateIO :: Float -> Game -> IO Game

-- Start menu
updateIO _ game@(Game _ (StartMenu _) _ _ _) = return game 

-- In game
updateIO deltaTime game@(Game kbd (InGame ig1@(InGameInfos _ _ _ _ _)) _ bgnd counter) = do 
    let 
        -- Background update
        newBgnd = updateBackground deltaTime bgnd

        -- In game informations update
        (_, ig2) = St.runState (updateInGame kbd deltaTime) ig1

        -- Counter update
        newCounter = (counter + 1) `mod` maxFramesToConsider

    -- in case of collision with the virus, moves it elsewhere
    if length (gameEnemies ig2) == 0 then trace "VIRUS DETRUIT !" $ do
        (newVX, newVY) <- generateVirusCoordinates
        let newVo = initStaticEnemyRectangleObject newVX newVY
            newV = initEnemy newVo 1
            newEnemies = [newV]
        return game{state = (initInGame(initInGameInfos (gamePlayer1 ig2) (gamePlayer2 ig2) newEnemies (gameWalls ig2) (gameProjectiles ig2))), 
            background = newBgnd, frameCounter = newCounter}
    else
        return game{state = (InGame ig2), background = newBgnd, frameCounter = newCounter}

-- Game loop
main :: IO ()
main = do
    initCtrl <- startInitGame
    playIO 
        (InWindow "Xenon 2 : Megablast" (widthScreen, heightScreen) (0, 0)) 
        black 
        framesPerSecond
        initCtrl
        renderIO
        handleEventsIO
        updateIO