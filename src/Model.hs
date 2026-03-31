module Model (module Model) where

import Graphics.Gloss

import Debug.Trace
import Hitbox
import GameSetup

import Objects

data Player = Player {
    playerObject :: Object, -- graphical representation of the player
    playerLifes :: Int -- player remaining lifes
} deriving (Show)

playerPicture :: Player -> Picture
playerPicture (Player o _) = objectPicture o

initPlayer :: Picture -> Float -> Float -> Player
initPlayer pic x y = Player 
    (initMovableObject pic 
        (initHitboxRectangle (x-(widthPlayer / 2)) (y-(heightPlayer / 2)) widthPlayer heightPlayer)
        (Direction 0 0)
        0
    )
    3

data Ennemy = Ennemy {
    ennemyObject :: Object, -- graphical representation of the ennemy
    ennemyLifes :: Int -- ennemy remaining lifes
} deriving (Show)

ennemyPicture :: Ennemy -> Picture
ennemyPicture (Ennemy o _) = objectPicture o

data GameState = 
    StartMenu StartMenuOption
    | InGame InGameInfos
    deriving (Show)

data StartMenuOption = Start | Option2
    deriving (Show, Eq)

data InGameInfos = InGameInfos {
        player1 :: Player
        , virusX :: Float
        , virusY :: Float
    } deriving (Show)
  

initStartMenu :: StartMenuOption -> GameState
initStartMenu option = StartMenu option

initInGame :: Picture -> Float -> Float -> Float -> Float -> GameState
initInGame picPlayer xVirus yVirus xP1 yP1 = InGame (InGameInfos (initPlayer picPlayer xP1 yP1) xVirus yVirus)


movePlayer1 :: InGameInfos -> InGameInfos
movePlayer1 gi@(InGameInfos p1@(Player p1o _) _ _) =
    --trace (show (objectDirection p1o)) $
    let (Direction dirxp1 diryp1) = objectDirection p1o
        s = objectSpeed p1o
        dxp1 = (fromIntegral dirxp1)*s
        dyp1 = (fromIntegral diryp1)*s
    in case centerHitbox (objectHitbox p1o) of
        Just (p1x, p1y) -> 
            let newX = p1x + dxp1
                newY = p1y + dyp1
                leftBound = -((fromIntegral widthScreen) / 2) + (widthPlayer / 2)
                rightBound = ((fromIntegral widthScreen) / 2) - (widthPlayer / 2)
                bottomBound = -((fromIntegral heightScreen) / 2) + (heightPlayer / 2)
                topBound = ((fromIntegral heightScreen) / 2) - (heightPlayer / 2)
            in if newX >= leftBound && newX <= rightBound &&
                  newY >= bottomBound && newY <= topBound
               then gi { player1 = p1 { playerObject = moveObject p1o screenSpeed } }
               else gi
        Nothing -> error "player must have a center"

collisionWithVirus :: InGameInfos -> Bool
collisionWithVirus (InGameInfos (Player p1o _) vx vy) = 

    case centerHitbox (objectHitbox p1o) of
        Just (p1x, p1y) -> let
                persoHalfW = widthPlayer / 2
                persoHalfH = heightPlayer / 2
                virusHalfW = widthVirus / 2
                virusHalfH = heightVirus / 2
                
                persoLeft   = p1x - persoHalfW
                persoRight  = p1x + persoHalfW
                persoTop    = p1y + persoHalfH
                persoBottom = p1y - persoHalfH
                
                virusLeft   = vx - virusHalfW
                virusRight  = vx + virusHalfW
                virusTop    = vy + virusHalfH
                virusBottom = vy - virusHalfH
            in
                persoRight >= virusLeft &&
                persoLeft <= virusRight &&
                persoTop >= virusBottom &&
                persoBottom <= virusTop
        Nothing -> error "player must have a center"