module Model (module Model) where

widthScreen :: Int
widthScreen = 1100
heigthScreen :: Int
heigthScreen = 700

widthVirus :: Int
widthVirus = 65
heigthVirus :: Int
heigthVirus = 64

widthPerso :: Int
widthPerso = 100
heigthPerso :: Int
heigthPerso = 100

data GameState = GameState {
  persoX :: Float
  , persoY :: Float
  , virusX :: Float
  , virusY :: Float
  , speed :: Float }
  deriving (Show)

initGameState :: Float -> Float -> GameState
initGameState xVirus yVirus = GameState 0 0 xVirus yVirus 3

moveLeft :: GameState -> GameState
moveLeft gs@(GameState px _ _ _ sp) | px > -275 = gs { persoX = px - sp }
                                | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState px _ _ _ sp) | px < 285 = gs { persoX = px + sp }
                                 | otherwise = gs
                              
moveDown :: GameState -> GameState
moveDown gs@(GameState _ py _ _ sp) | py > - 190 = gs { persoY = py - sp }
                              | otherwise = gs

moveUp :: GameState -> GameState
moveUp gs@(GameState _ py _ _ sp) | py < 190 = gs { persoY = py + sp }
                                | otherwise = gs

collisionWithVirus :: GameState -> Bool
collisionWithVirus (GameState px py vx vy _) = 
    let
        persoHalfW = fromIntegral (widthPerso `div` 2)
        persoHalfH = fromIntegral (heigthPerso `div` 2)
        virusHalfW = fromIntegral (widthVirus `div` 2)
        virusHalfH = fromIntegral (heigthVirus `div` 2)
        
        persoLeft   = px - persoHalfW
        persoRight  = px + persoHalfW
        persoTop    = py + persoHalfH
        persoBottom = py - persoHalfH
        
        virusLeft   = vx - virusHalfW
        virusRight  = vx + virusHalfW
        virusTop    = vy + virusHalfH
        virusBottom = vy - virusHalfH
    in
        persoRight >= virusLeft &&
        persoLeft <= virusRight &&
        persoTop >= virusBottom &&
        persoBottom <= virusTop