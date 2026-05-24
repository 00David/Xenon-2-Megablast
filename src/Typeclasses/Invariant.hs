module Typeclasses.Invariant (module Typeclasses.Invariant) where

import Test.QuickCheck

-- Utility typeclass mostly for veryfing that the invariant is satisfied while
-- using the a in another data type
class Invariant a where
    -- The invariant verified by 'a'
    prop_inv :: a -> Bool

law_invariant_stable :: Invariant a => a -> Property
law_invariant_stable x = prop_inv x ==> prop_inv x

law_invariant_idempotent :: Invariant a => a -> Bool
law_invariant_idempotent x = prop_inv x == prop_inv x

law_invariant_preserved :: (Invariant a, Invariant b) => (a -> b) -> a -> Property
law_invariant_preserved f x = prop_inv x ==> prop_inv (f x) -- naturally checked by my QuickCheck tests on operations