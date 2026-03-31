import Test.Hspec

import qualified UtilsSpec
import qualified HitboxSpec
import qualified ObjectsSpec

main :: IO ()
main = hspec $ do

    -- UTILS
    describe "Utils" UtilsSpec.spec

    -- HITBOX
    describe "Hitbox" HitboxSpec.spec

    -- OBJECTS
    describe "Objects" ObjectsSpec.spec