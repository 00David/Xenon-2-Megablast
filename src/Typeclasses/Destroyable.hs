module Typeclasses.Destroyable (module Typeclasses.Destroyable) where

import GameSetup
import Test.QuickCheck

-- Typeclass for defining a behavior on damages reception, while not being in the game anymore once dead.
class Destroyable a where
    -- Gives a given amount of damages. The Destroyable receiving damages can die, and thus return Nothing.
    takeDamageMaybe :: Damage -> a -> Maybe a

-- Damages are cumulative while alive
law_destroyable_damage_cumulative :: (Eq a, Destroyable a) => Damage -> Damage -> a -> Bool
law_destroyable_damage_cumulative d1 d2 a =
    case (takeDamageMaybe d1 a) of
        Just a' -> (takeDamageMaybe d2 a') == takeDamageMaybe (d1 + d2) a
        Nothing -> True

-- No damages = identity
law_destroyable_zero_damage_identity :: (Eq a, Destroyable a) => a -> Bool
law_destroyable_zero_damage_identity a = takeDamageMaybe 0 a == Just a

-- Negative damages don't heal
law_destroyable_no_heal_negative_damage :: (Eq a, Destroyable a) => Damage -> a -> Property
law_destroyable_no_heal_negative_damage d a = d < 0 ==> takeDamageMaybe 0 a == Just a