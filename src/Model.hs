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

initPlayer :: Picture -> Player
initPlayer pic = Player (initMovableObject pic 
    (initHitboxRectangle (0-(widthPlayer / 2)) (0-(heightPlayer / 2)) widthPlayer heightPlayer)
    (Direction 0 0)
    0)
    3

data Ennemy = Ennemy {
    ennemyObject :: Object, -- graphical representation of the ennemy
    ennemyLifes :: Int -- ennemy remaining lifes
} deriving (Show)

data GameState = GameState {
  player1 :: Player
  , virusX :: Float
  , virusY :: Float}
  deriving (Show)

initGameState :: Picture -> Float -> Float -> GameState
initGameState picPlayer xVirus yVirus = GameState (initPlayer picPlayer) xVirus yVirus


movePlayer1 :: GameState -> GameState
movePlayer1 gs@(GameState p1@(Player p1o _) _ _) =
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
               then gs { player1 = p1 { playerObject = moveObject p1o screenSpeed } }
               else gs
        Nothing -> error "player must have a center"

collisionWithVirus :: GameState -> Bool
collisionWithVirus (GameState (Player p1o _) vx vy) = 

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