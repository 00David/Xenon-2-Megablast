module Typeclasses.Movable (module Typeclasses.Movable) where

import GameSetup

-- Typeclass for defining a movement behavior.
class Movable a where
    -- Move the a according to either the given screen scrolling speed, or its own speed
    move :: a -> ScreenScrollingSpeed -> a

    -- Indicates if a is inside the screen
    insideScreen :: a -> Bool

-- Commutativity cannot be kept as a law for move beacuse the Object instance implies having the ability to move
-- independantly of the given screen scrolling speed.

-- Law tested as a precondition before each move.
law_strict_positive_screenSpeed :: a -> ScreenScrollingSpeed -> Bool
law_strict_positive_screenSpeed _ s = s > 0