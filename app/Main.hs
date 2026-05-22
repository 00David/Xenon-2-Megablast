module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game

import System.Random
import qualified Control.Monad.State as St

import GameSetup
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
renderIO (Game _ (InGame igi) ga bgnd _) =
    let
        player1 = (gamePlayer1 igi)
        player2 = (gamePlayer2 igi)
    in
        return (Pictures (
            (getTranslatedAssets ga bgnd) ++
            (getTranslatedAssets ga igi) ++
            (getTranslatedBottomBar ga 
                    (playerScore player1) (playerHealth player1) (playerLifes player1)
                    (playerScore player2) (playerHealth player2) (playerLifes player2))
            ))  
  
-- Event handling
handleEventsIO :: Event -> Game -> IO Game

-- Start menu
handleEventsIO ev (Game kbd gs@(StartMenu option) ga bgnd nbFrames) = do
    let newKBD = (handleKeyEvent ev kbd) -- keyboard update

        launchPressed = isKeyDown (SpecialKey KeySpace) newKBD || isKeyDown (SpecialKey KeyEnter) newKBD

        keyDownPressed = isKeyDown (SpecialKey KeyDown) newKBD || isKeyDown (Char 's') newKBD || isKeyDown (Char 'S') newKBD
        keyUpPressed = isKeyDown (SpecialKey KeyUp) newKBD || isKeyDown (Char 'z') newKBD || isKeyDown (Char 'Z') newKBD
                        || isKeyDown (Char 'w') newKBD || isKeyDown (Char 'W') newKBD -- QWERTY friendly

    -- if the space or enter key is pressed on "1 Player", launches the game with one player
    if option == OnePlayer && launchPressed
        then do
            gen <- newStdGen
            return (initGame (initKeyboard) (startInitInGame gen 1) ga bgnd nbFrames)
    -- if the space or enter key is pressed on "2 Players", launches the game with two players
    else if option == TwoPlayers && launchPressed
        then do
            gen <- newStdGen
            return (initGame (initKeyboard) (startInitInGame gen 2) ga bgnd nbFrames)
        else 
            if isKeyDown (SpecialKey KeyEsc) newKBD
                then do
                    putStrLn "Exiting..."
                    error "EXIT"
                else
                    case (keyUpPressed, keyDownPressed, option) of
                    (True, True, _) -> return (initGame (newKBD) gs ga bgnd nbFrames)
                    (_, True, OnePlayer) -> return (initGame (newKBD) (initStartMenu TwoPlayers) ga bgnd nbFrames)
                    (True, _, TwoPlayers) -> return (initGame (newKBD) (initStartMenu OnePlayer) ga bgnd nbFrames)
                    _ -> return (initGame (newKBD) gs ga bgnd nbFrames)

-- In game
handleEventsIO ev game@(Game kbd gs@(InGame (InGameInfos ss p1 p2 enemies walls projectiles expl bns)) ga bgnd nbFrames) = do
    -- trace ("event received: " <> show ev) 
    let newKBD = (handleKeyEvent ev kbd) -- keyboard update

    -- if the "Escape" key is pressed while in game, we're back to the start menu
    if (isKeyDown (SpecialKey KeyEsc) newKBD)
        then return game{keyboard = initKeyboard, state = (initStartMenu OnePlayer)}
        else  
            -- if BOTH "Space" and "Enter" are pressed, both players SHOOOT (if possible)
            if isKeyDown (SpecialKey KeySpace) newKBD &&
                    isKeyDown (SpecialKey KeyEnter) newKBD
                then
                    case (playerShot p1, playerShot p2) of
                        ((Just shot1, newP1), (Just shot2, newP2)) ->
                            let newProjectiles = shot1 : shot2 : projectiles
                                newIgi = initInGameInfos ss newP1 newP2 enemies walls newProjectiles expl bns
                            in return (initGame newKBD (initInGame newIgi) ga bgnd nbFrames)
                        ((Just shot1, newP1), (Nothing, newP2)) ->
                            let newProjectiles = shot1 : projectiles
                                newIgi = initInGameInfos ss newP1 newP2 enemies walls newProjectiles expl bns
                            in return (initGame newKBD (initInGame newIgi) ga bgnd nbFrames)
                        ((Nothing, newP1), (Just shot2, newP2)) ->
                            let newProjectiles = shot2 : projectiles
                                newIgi = initInGameInfos ss newP1 newP2 enemies walls newProjectiles expl bns
                            in return (initGame newKBD (initInGame newIgi) ga bgnd nbFrames)
                        ((Nothing, newP1), (Nothing, newP2)) ->
                            let newIgi = initInGameInfos ss newP1 newP2 enemies walls projectiles expl bns
                            in return (initGame newKBD (initInGame newIgi) ga bgnd nbFrames)

            -- if the "Space" key is pressed while in game, player1 SHOOOT (if possible)
            else if isKeyDown (SpecialKey KeySpace) newKBD
                then
                    case playerShot p1 of
                        (Nothing, newP1) ->
                            let newIgi = initInGameInfos ss newP1 p2 enemies walls projectiles expl bns
                            in return (initGame newKBD (initInGame newIgi) ga bgnd nbFrames)
                        (Just shot, newP1) ->
                            let newProjectiles = shot : projectiles
                                newIgi = initInGameInfos ss newP1 p2 enemies walls newProjectiles expl bns
                            in return (initGame newKBD (initInGame newIgi) ga bgnd nbFrames)

            -- if the "ENTER" key is pressed while in game, player2 SHOOOT (if possible)
            else if isKeyDown (SpecialKey KeyEnter) newKBD
                then
                    case playerShot p2 of
                        (Nothing, newP2) ->
                            let newIgi = initInGameInfos ss p1 newP2 enemies walls projectiles expl bns
                            in return (initGame newKBD (initInGame newIgi) ga bgnd nbFrames)
                        (Just shot, newP2) ->
                            let newProjectiles = shot : projectiles
                                newIgi = initInGameInfos ss p1 newP2 enemies walls newProjectiles expl bns
                            in return (initGame newKBD (initInGame newIgi) ga bgnd nbFrames)

            else
                return (initGame newKBD gs ga bgnd nbFrames)

-- Updating
updateIO :: Float -> Game -> IO Game

-- Start menu
updateIO _ game@(Game _ (StartMenu _) _ _ _) = return game 

-- In game
updateIO deltaTime (Game kbd (InGame ig1@(InGameInfos _ _ _ _ _ _ _ _)) ga bgnd nbFrames) = do 
    gen <- newStdGen
    let 
        -- Background update
        newBgnd = updateBackground deltaTime bgnd

        -- In game informations update
        (_, ig2) = St.runState (updateInGame nbFrames gen kbd deltaTime) ig1

        -- Counter update
        newNbFrames = (nbFrames + 1) `mod` maxFramesToConsider

    return (initGame kbd (initInGame ig2) ga (newBgnd) (newNbFrames))

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