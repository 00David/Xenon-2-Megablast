import Test.Hspec

import qualified UtilsSpec
import qualified HitboxSpec
import qualified ObjectsSpec
import qualified AssetsSpec
import qualified PlayerSpec
import qualified EnemySpec
import qualified BackgroundSpec

main :: IO ()
main = hspec $ do

    -- UTILS
    describe "Utils" UtilsSpec.spec

    -- HITBOX
    describe "Hitbox" HitboxSpec.spec

    -- OBJECTS
    describe "Objects" ObjectsSpec.spec

    -- ASSETS
    describe "Assets" AssetsSpec.spec

    -- PLAYER
    describe "Player" PlayerSpec.spec

    -- ENEMY
    describe "Enemy" EnemySpec.spec

    -- BACKGROUND
    describe "Background" BackgroundSpec.spec