{-# LANGUAGE InstanceSigs #-}
module Objects.Hitbox (module Objects.Hitbox) where

import GameSetup
import Utils
import Typeclasses.Invariant

-- ============================================================
-- ====================== OBJECT HITBOXES =====================
-- ============================================================

type XCenter = Float
type YCenter = Float
type Radius = Float -- >= 0

type XBottomLeft = Float
type YBottomLeft = Float
type Width = Float -- > 0
type Heigth = Float -- > 0

data Hitbox = Circle XCenter YCenter Radius 
    | Rectangle XBottomLeft YBottomLeft Width Heigth
    | Hitboxes XCenter YCenter [Hitbox] -- x y center coordinates + length list of hitboxes > 0. The center must be part of at least 1 hitbox.
    deriving (Eq, Show)

prop_inv_hitbox :: Hitbox -> Bool
prop_inv_hitbox (Circle _ _ r) = r >= 0 -- if r == 0 : it is a point
prop_inv_hitbox (Rectangle _ _ w h) = w > 0 && h > 0
prop_inv_hitbox (Hitboxes x y l) =
    length l > 0
    && all prop_inv_hitbox l
    && any (partOfHitbox x y) l

initHitboxCircle :: XCenter -> YCenter -> Radius -> Hitbox
initHitboxCircle x y r = if r < 0 then error "radius cannot be strictly negative" else (Circle x y r)

initHitboxRectangle :: XBottomLeft -> YBottomLeft -> Width -> Heigth -> Hitbox
initHitboxRectangle x y w h
    | w <= 0 = error "width cannot be negative or null"
    | h <= 0 = error "heigth cannot be negative or null"
    | otherwise = (Rectangle x y w h)

initHitboxes :: XCenter -> YCenter -> [Hitbox]-> Hitbox
initHitboxes x y l 
    | length l == 0 = error "must have at least 1 hitbox"
    | not (all prop_inv_hitbox l) = error "each hitbox must verify its invariant"
    | not (any (partOfHitbox x y) l) = error "(x,y) must be part of at least 1 contained hitbox"
    | otherwise = (Hitboxes x y l)

-- Indicates if the given (x,y) coordinates are part of the given hitbox 
partOfHitbox :: XCoord -> YCoord -> Hitbox -> Bool
partOfHitbox x y (Circle xh yh r) = ((x - xh)*(x - xh)) + ((y - yh)*(y - yh)) <= r*r
partOfHitbox x y (Rectangle xh yh w h) =
    x >= xh &&
    x <= xh + w &&
    y >= yh &&
    y <= yh + h
partOfHitbox x y (Hitboxes _ _ ll) = any (partOfHitbox x y) ll

-- Gives the (x,y) center of the Hitbox
centerHitbox :: Hitbox -> (XCenter, YCenter)
centerHitbox (Circle x y _) = (x,y)
centerHitbox (Rectangle x y w h) = (x + (w / 2), y + (h / 2))
centerHitbox (Hitboxes x y _) = (x,y)

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
collisionHitbox (Hitboxes _ _ l) h2 = foldr (\h1 acc -> (collisionHitbox h1 h2) || acc) False l
collisionHitbox h2 (Hitboxes _ _ l) = foldr (\h1 acc -> (collisionHitbox h1 h2) || acc) False l

prop_commutativity_collisionHitbox :: Hitbox -> Hitbox -> Bool
prop_commutativity_collisionHitbox h1 h2 = (collisionHitbox h1 h2 == collisionHitbox h2 h1)

-- Moves a hitbox, depending on x and y components of a vector, given as second argument
moveHitbox :: Hitbox -> (XCoord, YCoord) -> Hitbox
moveHitbox (Circle x y r) (dx,dy) = (initHitboxCircle (x+dx) (y+dy) r)
moveHitbox (Rectangle x y w h) (dx,dy) = (initHitboxRectangle (x+dx) (y+dy) w h)
moveHitbox (Hitboxes x y l) (dx,dy) = (initHitboxes (x+dx) (y+dy) (map (\h -> moveHitbox h (dx,dy)) l))

prop_post_moveHitbox :: Hitbox -> (XCoord, YCoord) -> Bool
prop_post_moveHitbox hit@(Circle x y r) d@(dx,dy) =
    case (moveHitbox hit d) of
        (Circle x2 y2 r2) -> x2 == x+dx && y2 == y+dy && r2 == r
        _ -> False
prop_post_moveHitbox hit@(Rectangle x y w h) d@(dx,dy) =
    case (moveHitbox hit d) of
        (Rectangle x2 y2 w2 h2) -> x2 == x+dx && y2 == y+dy && w2 == w && h2 == h
        _ -> False
prop_post_moveHitbox hit@(Hitboxes x y l) d@(dx,dy) =
    case (moveHitbox hit d) of
        (Hitboxes x2 y2 l2) -> x2 == x+dx && y2 == y+dy && foldr (\h acc -> prop_post_moveHitbox h (dx,dy) && acc) True l && length l == length l2
        _ -> False

-- Indicates if a hitbox is exactly inside the screen.
insideScreenHitbox :: Hitbox -> Bool
insideScreenHitbox (Circle x y r) =
    let leftBound = leftXScreenBound + r
        rightBound = rightXScreenBound - r
        topBound = topYScreenBound - r
        bottomBound = bottomYScreenBound + r
    in x >= leftBound && x <= rightBound && y >= bottomBound && y <= topBound
insideScreenHitbox (Rectangle xBL yBL w h) =  
    let rightX = xBL + w
        topY = yBL + h
    in xBL >= leftXScreenBound 
        && rightX <= rightXScreenBound
        && yBL >= bottomYScreenBound
        && topY <= topYScreenBound
insideScreenHitbox (Hitboxes _ _ hitboxes) = all insideScreenHitbox hitboxes

-- Indicates if a hitbox is inside the screen or above it. 
-- Here it considers the Hitbox still inside when partially inside.
insideScreenOrAboveHitbox :: Hitbox -> Bool
insideScreenOrAboveHitbox (Circle x y _) =
    let leftBound = leftXScreenBound
        rightBound = rightXScreenBound
        bottomBound = bottomYScreenBound
    in x >= leftBound && x <= rightBound && y >= bottomBound
insideScreenOrAboveHitbox (Rectangle xBL yBL w h) =  
    xBL + w >= leftXScreenBound 
    && xBL <= rightXScreenBound
    && yBL + h >= bottomYScreenBound
insideScreenOrAboveHitbox (Hitboxes _ _ hitboxes) = all insideScreenOrAboveHitbox hitboxes

-- ============================================================
-- ===================== HITBOX INVARIANT =====================
-- ============================================================

instance Invariant Hitbox where
    prop_inv :: Hitbox -> Bool
    prop_inv = prop_inv_hitbox 