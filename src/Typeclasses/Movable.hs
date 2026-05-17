module Typeclasses.Movable (module Typeclasses.Movable) where

import GameSetup

-- Typeclass for defining a movement behavior.
class Movable a where
    -- Move the a according to either the given screen scrolling speed, or its own speed
    move :: a -> ScreenScrollingSpeed -> a

    -- Indicates if a is inside the screen
    insideScreen :: a -> Bool

law_move_commutative  :: (Movable a, Eq a) => a -> ScreenScrollingSpeed -> ScreenScrollingSpeed -> Bool
law_move_commutative  a s1 s2 = move (move a s1) s2 == move (move a s2) s1

law_positive_screenSpeed :: a -> ScreenScrollingSpeed -> Bool
law_positive_screenSpeed _ s = s >= 0