module PictureUtils (module PictureUtils) where
import Objects
import Graphics.Gloss
import Model
import Hitbox


blastersEnabled :: [Picture] -> Player -> [Picture]
-- pic_blasters[0] = blaster_left
-- pic_blasters[1] = blaster_right
-- pic_blasters[2] = blaster_top_left
-- pic_blasters[3] = blaster_top_right
blastersEnabled pic_blasters player = 
    let po = playerObject player
        (Direction dx dy) = objectDirection po
    in case centerHitbox (objectHitbox po) of
        Just (px, py) -> aux pic_blasters 0 
            where
                aux :: [Picture] -> Int -> [Picture]
                aux [] _ = []
                aux (pic_blaster:xs) i
                    | i == 0 = if dy > 0 then (Translate (px-16) (py-50) pic_blaster):(aux xs (i+1)) else (aux xs (i+1))
                    | i == 1 = if dy > 0 then (Translate (px+16) (py-50) pic_blaster):(aux xs (i+1)) else (aux xs (i+1))
                    | i == 2 = if dy < 0 then (Translate (px-25) (py+17) pic_blaster):(aux xs (i+1)) else (aux xs (i+1))
                    | i == 3 = if dy < 0 then (Translate (px+25) (py+17) pic_blaster):(aux xs (i+1)) else (aux xs (i+1))
                    | otherwise = error "there must be exactly 4 blaster pictures in the initial Picture array"
        Nothing -> error "player must have a center"


    