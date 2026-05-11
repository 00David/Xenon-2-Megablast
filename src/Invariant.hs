module Invariant (module Invariant) where

-- Utility typeclass mostly for veryfing that the invariant is satisfied while
-- using the a in another data type
class Invariant a where
    prop_inv :: a -> Bool