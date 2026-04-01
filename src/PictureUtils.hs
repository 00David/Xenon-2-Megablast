module PictureUtils (module PictureUtils) where
import Objects
import Graphics.Gloss ( Picture(Translate) )
import Model
import Hitbox

-- tests TODO
-- Sorts booster assets by returning a list of boosters only enabled when moving with the right player direction.
-- They are also positionned at the right place, compared with the player position.
boostersEnabled :: [Picture] -> Player -> [Picture]
-- pic_boosters[0] = booster_left
-- pic_boosters[1] = booster_right
-- pic_boosters[2] = booster_top_left
-- pic_boosters[3] = booster_top_right
boostersEnabled pic_boosters player = 
    let po = playerObject player
        (Direction dx dy) = objectDirection po
    in case centerHitbox (objectHitbox po) of
        Just (px, py) -> aux pic_boosters 0 
            where
                aux :: [Picture] -> Int -> [Picture]
                aux [] _ = []
                aux (pic_booster:xs) i
                    | i == 0 = if dy > 0 then (Translate (px-16) (py-50) pic_booster):(aux xs (i+1)) else (aux xs (i+1))
                    | i == 1 = if dy > 0 then (Translate (px+16) (py-50) pic_booster):(aux xs (i+1)) else (aux xs (i+1))
                    | i == 2 = if dy < 0 then (Translate (px-25) (py+17) pic_booster):(aux xs (i+1)) else (aux xs (i+1))
                    | i == 3 = if dy < 0 then (Translate (px+25) (py+17) pic_booster):(aux xs (i+1)) else (aux xs (i+1))
                    | otherwise = error "there must be exactly 4 blaster pictures in the initial Picture array"
        Nothing -> error "player must have a center"

-- tests TODO
-- all 4 pictures must be the booster pictures, but there is no way to also verify it
prop_pre_boostersEnabled :: [Picture] -> Player -> Bool
prop_pre_boostersEnabled pic_boosters _ = length pic_boosters == 4
    

-- tests TODO, list of hitboxes to investigate
translateEnemyPictures :: [Ennemy] -> [Picture]
translateEnemyPictures [] = []
translateEnemyPictures (enemy:xs) = 
    let eo = ennemyObject enemy
        pic = objectPicture eo
        h = objectHitbox eo
    in (translateHitbox h pic) ++ translateEnemyPictures xs where
        translateHitbox :: Hitbox -> Picture -> [Picture]
        translateHitbox (Circle x y _) p = [Translate x y p]
        translateHitbox (Rectangle x y w h) p = 
            let centerX = x + (w / 2)
                centerY = y + (h / 2)
            in [Translate centerX centerY p]
        translateHitbox (Hitboxes l) p = foldr (\h acc -> (translateHitbox h p) <> acc) [] l
