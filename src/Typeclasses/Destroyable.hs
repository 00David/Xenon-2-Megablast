module Typeclasses.Destroyable (module Typeclasses.Destroyable) where

import GameSetup

-- Typeclass for defining a behavior on damages reception, while not being in the game anymore once dead.
class Destroyable a where
    -- Gives a given amount of damages. The Destroyable receiving damages can die, and thus return Nothing.
    takeDamageMaybe :: Damage -> a -> Maybe a

-- A dead Destroyable stays dead after death
law_destroyable_dead_stays_dead :: (Eq a) => Destroyable a => Damage -> Damage -> a -> Bool
law_destroyable_dead_stays_dead d1 d2 a =
    case takeDamageMaybe d1 a of
        Nothing -> takeDamageMaybe d2 a == Nothing
        Just _  -> True

-- Damages are cumulative while alive
law_destroyable_damage_cumulative :: (Eq a, Destroyable a) => Damage -> Damage -> a -> Bool
law_destroyable_damage_cumulative d1 d2 a =
    case (takeDamageMaybe d1 a) of
        Just a' -> (takeDamageMaybe d2 a') == takeDamageMaybe (d1 + d2) a
        Nothing -> True

-- No damages = identity
law_destroyable_zero_damage_identity :: (Eq a, Destroyable a) => a -> Bool
law_destroyable_zero_damage_identity a = takeDamageMaybe 0 a == Just a