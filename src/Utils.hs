module Utils (module Utils) where

-- Returns a value inside the [minV, maxV] range
clamp :: Ord a => a -> a -> a -> a
clamp v minV maxV = max minV (min v maxV)