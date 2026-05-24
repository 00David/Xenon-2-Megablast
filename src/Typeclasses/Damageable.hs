module Typeclasses.Damageable (module Typeclasses.Damageable) where

import GameSetup
import Test.QuickCheck

-- Typeclass for defining a behavior on damages reception, while still being in the game once dead.
class Damageable a where
    -- Indicates if a Damageable is currently dead.
    isDead :: a -> Bool

    -- Gives a given amount of damages, possibly modifying the Damageable in return.
    takeDamage :: Damage -> a -> a

-- A dead Damageable stays dead after death
law_damageable_dead_stays_dead :: Damageable a => Damage -> a -> Bool
law_damageable_dead_stays_dead d a =
    let a' = takeDamage d a
    in if isDead a then isDead a' else True

-- Taking multiple damages after death don't change the entity receiving damages
law_damageable_dead_idempotent :: (Eq a, Damageable a) => Damage -> Damage -> a -> Bool
law_damageable_dead_idempotent d1 d2 a =
    let a1 = takeDamage d1 a
    in if isDead a1
        then takeDamage d2 a1 == a1
        else True

-- No damages = identity
law_damageable_zero_damage_identity :: (Eq a, Damageable a) => a -> Bool
law_damageable_zero_damage_identity a = takeDamage 0 a == a

-- Negative damages don't heal
law_damageable_no_heal_negative_damage :: (Eq a, Damageable a) => Damage -> a -> Property
law_damageable_no_heal_negative_damage d a = d < 0 ==> takeDamage d a == a