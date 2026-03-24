import UtilsSpec
import ObjectsSpec

import Test.Hspec

main :: IO ()
main = hspec $ do

    -- UTILS
    clampSpec

    -- OBJECTS
    hitboxInitSpec
    collisionSpec
    assocCollisionSpec
    directionInitSpec
    objectInitSpec
    objectGetHitboxSpec
    objectGetDirectionSpec
    objectGetSpeedSpec
    wallInitSpec
