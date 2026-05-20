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
import Keyboard

-- Rendering
renderIO :: Game -> IO Picture

-- Start menu
renderIO (Game _ (StartMenu option) ga bgnd _) =
    -- displays information texts
    let subtitle = Translate (-180) (200) (Scale 0.3 0.3 (Color white (Text "An inspiration of ...")))
        title = Translate (-290) (100) (Scale 0.4 0.4 (Color white (Text "XENON 2 : MEGABLAST")))
    in return (Pictures ((getTranslatedAssets ga bgnd)++[subtitle, title]++(getTranslatedAssets ga option)))

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

        launchPressed = isKeyDown (SpecialKey KeySpace) newKBD || isKeyDown (SpecialKey KeyEnter) newKBD

        keyDownPressed = isKeyDown (SpecialKey KeyDown) newKBD || isKeyDown (Char 's') newKBD || isKeyDown (Char 'S') newKBD
        keyUpPressed = isKeyDown (SpecialKey KeyUp) newKBD || isKeyDown (Char 'z') newKBD || isKeyDown (Char 'Z') newKBD
                        || isKeyDown (Char 'w') newKBD || isKeyDown (Char 'W') newKBD -- QWERTY friendly

    -- if the space or enter key is pressed on "1 Player", launches the game with one player
    if option == OnePlayer && launchPressed
        then do
            gen <- newStdGen
            return game{
                keyboard = initKeyboard,
                state = startInitInGame gen 1
                }
    -- if the space or enter key is pressed on "2 Players", launches the game with two players
    else if option == TwoPlayers && launchPressed
        then do
            gen <- newStdGen
            return game{
                keyboard = initKeyboard,
                state = startInitInGame gen 2
                }
        else 
            if isKeyDown (SpecialKey KeyEsc) newKBD
                then do
                    putStrLn "Exiting..."
                    error "EXIT"
                else
                    case (keyUpPressed, keyDownPressed, option) of
                    (True, True, _) -> return game{keyboard = newKBD}
                    (_, True, OnePlayer) -> return game{keyboard = newKBD, state = (initStartMenu TwoPlayers)}
                    (True, _, TwoPlayers) -> return game{keyboard = newKBD, state = (initStartMenu OnePlayer)}
                    _ -> return game{keyboard = newKBD}

-- In game
handleEventsIO ev game@(Game kbd (InGame (InGameInfos ss p1 p2 enemies walls projectiles expl)) _ _ _) = do
    -- trace ("event received: " <> show ev) 
    let newKBD = (handleKeyEvent ev kbd) -- keyboard update

    -- if the "Escape" key is pressed while in game, we're back to the start menu
    if (isKeyDown (SpecialKey KeyEsc) newKBD)
        then return game{keyboard = initKeyboard, state = (initStartMenu OnePlayer)}
        else  
            -- if the "Space" key is pressed while in game, player1 SHOOOT
            if (isKeyDown (SpecialKey KeySpace) newKBD)
                then case playerShot p1 of
                    Nothing -> return game{keyboard = newKBD}
                    Just shot -> 
                        let newProjectiles = shot : projectiles
                            newIgi = initInGameInfos ss p1 p2 enemies walls newProjectiles expl
                        in return game{keyboard = newKBD, state = (InGame newIgi)}
            -- if the "ENTER" key is pressed while in game, player2 SHOOOT
            else if (isKeyDown (SpecialKey KeyEnter) newKBD)
                then case playerShot p2 of
                    Nothing -> return game{keyboard = newKBD}
                    Just shot -> 
                        let newProjectiles = shot : projectiles
                            newIgi = initInGameInfos ss p1 p2 enemies walls newProjectiles expl
                        in return game{keyboard = newKBD, state = (InGame newIgi)}
                
                else return game{keyboard = newKBD}

-- Updating
updateIO :: Float -> Game -> IO Game

-- Start menu
updateIO _ game@(Game _ (StartMenu _) _ _ _) = return game 

-- In game
updateIO deltaTime game@(Game kbd (InGame ig1@(InGameInfos _ _ _ _ _ _ _)) _ bgnd counter) = do 
    gen <- newStdGen
    let 
        -- Background update
        newBgnd = updateBackground deltaTime bgnd

        -- In game informations update
        (_, ig2) = St.runState (updateInGame counter gen kbd deltaTime) ig1

        -- Counter update
        newCounter = (counter + 1) `mod` maxFramesToConsider

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