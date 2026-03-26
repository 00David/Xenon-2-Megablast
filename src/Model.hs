module Model (module Model) where

import Graphics.Gloss

import Debug.Trace

import Objects

data Player = Player {
    playerObject :: Object, -- graphical representation of the player
    playerLifes :: Int -- player remaining lifes
} deriving (Show)

initPlayer :: Picture -> Player
initPlayer pic = Player (initMovableObject pic 
    (initHitboxRectangle (0-(widthPerso `div` 2)) (0-(heightPerso `div` 2)) widthPerso heightPerso)
    (Direction 0 0)
    0)
    3

data Ennemy = Ennemy {
    ennemyObject :: Object, -- graphical representation of the ennemy
    ennemyLifes :: Int -- ennemy remaining lifes
} deriving (Show)

widthScreen :: Int
widthScreen = 1100
heightScreen :: Int
heightScreen = 700

widthVirus :: Int
widthVirus = 65
heightVirus :: Int
heightVirus = 64

widthPerso :: Int
widthPerso = 110
heightPerso :: Int
heightPerso = 76

data GameState = GameState {
  player1 :: Player
  , virusX :: Int
  , virusY :: Int
  , speed :: Int }
  deriving (Show)

initGameState :: Picture -> Int -> Int -> GameState
initGameState picPlayer xVirus yVirus = GameState (initPlayer picPlayer) xVirus yVirus 3


movePlayer1 :: GameState -> GameState
movePlayer1 gs@(GameState p1@(Player p1o _) _ _ screenSpeed) =
    --trace (show (objectDirection p1o)) $
    let (Direction dirxp1 diryp1) = objectDirection p1o
        s = objectSpeed p1o
        dxp1 = dirxp1*s
        dyp1 = diryp1*s
    in case centerHitbox (objectHitbox p1o) of
        Just (p1x, p1y) -> 
            let newX = p1x + dxp1
                newY = p1y + dyp1
                leftBound = -(widthScreen `div` 2) + (widthPerso `div` 2)
                rightBound = (widthScreen `div` 2) - (widthPerso `div` 2)
                bottomBound = -(heightScreen `div` 2) + (heightPerso `div` 2)
                topBound = (heightScreen `div` 2) - (heightPerso `div` 2)
            in if newX >= leftBound && newX <= rightBound &&
                  newY >= bottomBound && newY <= topBound
               then gs { player1 = p1 { playerObject = moveObject p1o screenSpeed } }
               else gs
        Nothing -> error "player must have a center"

collisionWithVirus :: GameState -> Bool
collisionWithVirus (GameState (Player p1o _) vx vy _) = 

    case centerHitbox (objectHitbox p1o) of
        Just (p1x, p1y) -> let
                persoHalfW = widthPerso `div` 2
                persoHalfH = heightPerso `div` 2
                virusHalfW = widthVirus `div` 2
                virusHalfH = heightVirus `div` 2
                
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