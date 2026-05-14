module Damageable (module Damageable) where

type Damage = Int
type Health = Int

-- Typeclass for defining a behavior on damages reception.
-- The 'a' receiving damages can die.
class Damageable a where
    -- Get the current health, or Nothing if dead
    currentHealth :: a -> Maybe Health

    -- Take a given amount of damage
    takeDamage :: Damage -> a -> Maybe a

law_damagable_stays_dead :: (Eq a, Damageable a) => a -> Damage -> Bool
law_damagable_stays_dead a d =
    case currentHealth a of
        Nothing -> takeDamage d a == Nothing
        Just _  -> True

law_damagable_health_consistency :: Damageable a => a -> Damage -> Bool
law_damagable_health_consistency a d =
    case takeDamage d a of
        Nothing -> currentHealth a == Nothing
        Just _  -> True

law_damagable_zero_damage_identity :: (Eq a, Damageable a) => a -> Bool
law_damagable_zero_damage_identity a = takeDamage 0 a == Just a