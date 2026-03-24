module Utils (module Utils) where

(==>) :: Bool -> Bool -> Bool
(==>) True False = False
(==>) _ _ = True
infixr 4 ==>

-- Returns a value inside the [minV, maxV] range
clamp :: Ord a => a -> a -> a -> a
clamp v minV maxV = max minV (min v maxV)