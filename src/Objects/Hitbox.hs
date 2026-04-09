module Objects.Hitbox (module Objects.Hitbox) where

import Utils

-- ============================================================
-- ====================== OBJECT HITBOXES =====================
-- ============================================================

data Hitbox = Circle Float Float Float -- x y center coordinates + radius. Radius >= 0.
    | Rectangle Float Float Float Float  -- x y bottom-left coordinates + width + heigth. Width > 0 and heigth > 0.
    | Hitboxes [Hitbox] -- a list of hitboxes. length > 0.
    deriving (Eq, Show)

prop_inv_hitbox :: Hitbox -> Bool
prop_inv_hitbox (Circle _ _ r) = r >= 0 -- if r == 0 : it is a point
prop_inv_hitbox (Rectangle _ _ w h) = w > 0 && h > 0
prop_inv_hitbox (Hitboxes l) = length l > 0 && foldr (\h acc -> prop_inv_hitbox h && acc) True l

initHitboxCircle :: Float -> Float -> Float -> Hitbox
initHitboxCircle x y r = if r < 0 then error "radius cannot be strictly negative" else (Circle x y r)

initHitboxRectangle :: Float -> Float -> Float -> Float -> Hitbox
initHitboxRectangle x y w h
    | w <= 0 = error "weigth cannot be negative or null"
    | h <= 0 = error "heigth cannot be negative or null"
    | otherwise = (Rectangle x y w h)

initHitboxes :: [Hitbox]-> Hitbox
initHitboxes l 
    | length l == 0 = error "must have at least 1 hitbox"
    | otherwise = (Hitboxes l)

-- Gives the (x,y) center of the Hitbox, or nothing if it is for 'Hitboxes'
centerHitbox :: Hitbox -> Maybe (Float, Float)
centerHitbox (Circle x y _) = Just (x,y)
centerHitbox (Rectangle x y w h) = Just (x + (w / 2), y + (h / 2))
centerHitbox _ = Nothing

-- Detects if there is a collision between 2 hitboxes
collisionHitbox :: Hitbox -> Hitbox -> Bool
collisionHitbox (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) =
    not (x1 + w1 < x2 ||  -- r1 completely at left of r2
         x2 + w2 < x1 ||  -- r2 completely at rigth of r1
         y1 + h1 < y2 ||  -- r1 completely under r2
         y2 + h2 < y1)
collisionHitbox (Circle x1 y1 r1) (Circle x2 y2 r2) = 
    let
        dx = x1 - x2
        dy = y1 - y2
        r = r1 + r2
    in
        dx*dx + dy*dy <= r*r
collisionHitbox (Circle x1 y1 r1) (Rectangle x2 y2 w2 h2) = 
    let
        closestX = clamp x1 x2 (x2 + w2)
        closestY = clamp y1 y2 (y2 + h2)
        dx = x1 - closestX
        dy = y1 - closestY
    in
        (x1 - closestX) * dx + dy * dy <= r1 * r1
collisionHitbox (Rectangle x2 y2 w2 h2) (Circle x1 y1 r1) = 
    let
        closestX = clamp x1 x2 (x2 + w2)
        closestY = clamp y1 y2 (y2 + h2)
        dx = x1 - closestX
        dy = y1 - closestY
    in
        (x1 - closestX) * dx + dy * dy <= r1 * r1
collisionHitbox (Hitboxes l) h2 = foldr (\h1 acc -> (collisionHitbox h1 h2) || acc) False l
collisionHitbox h2 (Hitboxes l) = foldr (\h1 acc -> (collisionHitbox h1 h2) || acc) False l

prop_commutativity_collisionHitbox :: Hitbox -> Hitbox -> Bool
prop_commutativity_collisionHitbox h1 h2 = (prop_inv_hitbox h1 && prop_inv_hitbox h2) ==> (collisionHitbox h1 h2 == collisionHitbox h2 h1)

-- Moves a hitbox, depending on x and y components of a vector, given as second argument
moveHitbox :: Hitbox -> (Float, Float) -> Hitbox
moveHitbox (Circle x y r) (dx,dy) = (initHitboxCircle (x+dx) (y+dy) r)
moveHitbox (Rectangle x y w h) (dx,dy) = (initHitboxRectangle (x+dx) (y+dy) w h)
moveHitbox (Hitboxes l) (dx,dy) = (initHitboxes (map (\h -> moveHitbox h (dx,dy)) l))

prop_post_moveHitbox :: Hitbox -> (Float, Float) -> Bool
prop_post_moveHitbox (Circle x y r) (dx,dy) =
    case (moveHitbox (Circle x y r) (dx,dy)) of
        (Circle x2 y2 r2) -> x2 == x+dx && y2 == y+dy && r2 == r
        _ -> False
prop_post_moveHitbox (Rectangle x y w h) (dx,dy) =
    case (moveHitbox (Rectangle x y w h) (dx,dy)) of
        (Rectangle x2 y2 w2 h2) -> x2 == x+dx && y2 == y+dy && w2 == w && h2 == h
        _ -> False
prop_post_moveHitbox (Hitboxes l) (dx,dy) =
    foldr (\h acc -> prop_post_moveHitbox h (dx,dy) && acc) True l