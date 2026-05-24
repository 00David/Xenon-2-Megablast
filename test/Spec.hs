import Test.Hspec
{--
import qualified EnemySpec
import qualified GameSpec
--}
import qualified KeyboardSpec
import qualified HitboxSpec
import qualified ObjectsSpec
import qualified AssetsSpec
import qualified BackgroundSpec
import qualified ExplosionSpec
import qualified RockSpec
import qualified WallSpec
import qualified ProjectileSpec
import qualified PlayerSpec

main :: IO ()
main = hspec $ do

    -- KEYBOARD
    describe "[Keyboard]" KeyboardSpec.spec

    -- HITBOX
    describe "[Hitbox]" HitboxSpec.spec

    -- OBJECTS
    describe "[Objects]" ObjectsSpec.spec

    -- ASSETS
    describe "[Assets]" AssetsSpec.spec

    -- BACKGROUND
    describe "[Background]" BackgroundSpec.spec

    -- EXPLOSION
    describe "[Explosion]" ExplosionSpec.spec

    -- ROCK
    describe "[Rock]" RockSpec.spec

    -- WALL
    describe "[Wall]" WallSpec.spec

    -- PROJECTILE
    describe "[Projectile]" ProjectileSpec.spec

    -- PLAYER
    describe "[Player]" PlayerSpec.spec

    -- ENEMY
    --describe "[Enemy]" EnemySpec.spec

    -- GAME
    --describe "[Game]" GameSpec.spec