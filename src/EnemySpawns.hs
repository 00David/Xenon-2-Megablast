module EnemySpawns (module EnemySpawns) where

import GameSetup
import System.Random

generateCoordinates :: (Float, Float) -> (Float, Float) ->  IO (Float, Float)
generateCoordinates (minX, maxX) (minY, maxY) = do
    x <- randomRIO (minX,  maxX)
    y <- randomRIO (minY,  maxY)
    return (x, y)

generateVirusCoordinates :: IO (Float, Float)
generateVirusCoordinates = do
    (vx, vy) <- generateCoordinates ( (-(fromIntegral widthScreen) / 2) + widthVirus, ((fromIntegral widthScreen) / 2) - widthVirus) 
                                    ( (-(fromIntegral heightScreen) / 2) + heightVirus, ((fromIntegral heightScreen) / 2) - heightVirus)
    return (vx, vy)