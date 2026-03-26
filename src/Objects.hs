module Objects (module Objects) where

import Graphics.Gloss (Picture)

import Utils

-- ============================================================
-- ====================== OBJECT HITBOXES =====================
-- ============================================================

data Hitbox = Circle Int Int Int -- x y center coordinates + radius
    | Rectangle Int Int Int Int  -- x y bottom-left coordinates + width + heigth
    | Hitboxes [Hitbox] -- a list of hitboxes
    deriving (Eq, Show)

prop_inv_hitbox :: Hitbox -> Bool
prop_inv_hitbox (Circle _ _ r) = r >= 0 -- if r == 0 : it is a point
prop_inv_hitbox (Rectangle _ _ w h) = w > 0 && h > 0
prop_inv_hitbox (Hitboxes l) = foldr (\h acc -> prop_inv_hitbox h && acc) True l

initHitboxCircle :: Int -> Int -> Int -> Hitbox
initHitboxCircle x y r = if r < 0 then error "radius cannot be strictly negative" else (Circle x y r)

initHitboxRectangle :: Int -> Int -> Int -> Int -> Hitbox
initHitboxRectangle x y w h
    | w <= 0 = error "weigth cannot be negative or null"
    | h <= 0 = error "heigth cannot be negative or null"
    | otherwise = (Rectangle x y w h)

initHitboxes :: [Hitbox]-> Hitbox
initHitboxes l = (Hitboxes l)

-- TODO : tests
-- Gives the (x,y) center of the Hitbox, or nothing if it is for Hitboxes
centerHitbox :: Hitbox -> Maybe (Int, Int)
centerHitbox (Circle x y _) = Just (x,y)
centerHitbox (Rectangle x y w h) = Just (x + (w `div` 2), y + (h `div` 2))
centerHitbox _ = Nothing

-- Detects if there is a collision between 2 hitboxes
collision :: Hitbox -> Hitbox -> Bool
collision (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) =
    not (x1 + w1 < x2 ||  -- r1 completely at left of r2
         x2 + w2 < x1 ||  -- r2 completely at rigth of r1
         y1 + h1 < y2 ||  -- r1 completely under r2
         y2 + h2 < y1)
collision (Circle x1 y1 r1) (Circle x2 y2 r2) = 
    let
        dx = x1 - x2
        dy = y1 - y2
        r = r1 + r2
    in
        dx*dx + dy*dy <= r*r
collision (Circle x1 y1 r1) (Rectangle x2 y2 w2 h2) = 
    let
        closestX = clamp x1 x2 (x2 + w2)
        closestY = clamp y1 y2 (y2 + h2)
        dx = x1 - closestX
        dy = y1 - closestY
    in
        (x1 - closestX) * dx + dy * dy <= r1 * r1
collision (Rectangle x2 y2 w2 h2) (Circle x1 y1 r1) = 
    let
        closestX = clamp x1 x2 (x2 + w2)
        closestY = clamp y1 y2 (y2 + h2)
        dx = x1 - closestX
        dy = y1 - closestY
    in
        (x1 - closestX) * dx + dy * dy <= r1 * r1
collision (Hitboxes l) h2 = foldr (\h1 acc -> (collision h1 h2) || acc) False l
collision h2 (Hitboxes l) = foldr (\h1 acc -> (collision h1 h2) || acc) False l

prop_commutativity_collision :: Hitbox -> Hitbox -> Bool
prop_commutativity_collision h1 h2 = (prop_inv_hitbox h1 && prop_inv_hitbox h2) ==> (collision h1 h2 == collision h2 h1)

-- TODO : tests
-- Moves a hitbox, depending on x and y components of a vector, given as second and third arguments
moveHitbox :: Hitbox -> Int -> Int -> Hitbox
moveHitbox (Circle x y r) dx dy = (Circle (x+dx) (y+dy) r)
moveHitbox (Rectangle x y w h) dx dy = (Rectangle (x+dx) (y+dy) w h)
moveHitbox (Hitboxes l) dx dy = (Hitboxes (map (\h -> moveHitbox h dx dy) l))

prop_post_moveHitbox :: Hitbox -> Int -> Int -> Bool
prop_post_moveHitbox (Circle x y r) dx dy =
    case (moveHitbox (Circle x y r) dx dy) of
        (Circle x2 y2 r2) -> x2 == x+dx && y2 == y+dy && r2 == r
        _ -> False
prop_post_moveHitbox (Rectangle x y w h) dx dy =
    case (moveHitbox (Rectangle x y w h) dx dy) of
        (Rectangle x2 y2 w2 h2) -> x2 == x+dx && y2 == y+dy && w2 == w && h2 == h
        _ -> False
prop_post_moveHitbox (Hitboxes l) dx dy =
    foldr (\h acc -> prop_post_moveHitbox h dx dy && acc) True l

-- ============================================================
-- ====================== OBJECTS =============================
-- ============================================================

data Direction = Direction Int Int -- direction components must be part of {-1, 0, 1}
    deriving (Eq, Show)

prop_inv_direction :: Direction -> Bool
prop_inv_direction (Direction x y) = -1 <= x && x <= 1 && -1 <= y && y <= 1

initDirection :: Int -> Int -> Direction
initDirection x y
    | not(-1 <= x && x <= 1) = error "x must be part of {-1, 0, 1}"
    | not(-1 <= y && y <= 1) = error "y must be part of {-1, 0, 1}"
    | otherwise = (Direction x y)

data Object = MovableO Picture Hitbox Direction Int -- last Int : speed
    | StaticO Picture Hitbox
    deriving (Eq, Show)

prop_inv_object :: Object -> Bool 
prop_inv_object (MovableO _ h d s) = prop_inv_hitbox h && prop_inv_direction d && s >= 0
prop_inv_object (StaticO _ h) = prop_inv_hitbox h

initMovableObject :: Picture -> Hitbox -> Direction -> Int -> Object
initMovableObject p h d s = if s < 0 then error "speed cannot be strictly negative" else (MovableO p h d s)

initStaticObject :: Picture -> Hitbox -> Object
initStaticObject p h = (StaticO p h)

objectPicture :: Object -> Picture
objectPicture (MovableO p _ _ _) = p
objectPicture (StaticO p _) = p

objectHitbox :: Object -> Hitbox
objectHitbox (MovableO _ h _ _) = h
objectHitbox (StaticO _ h) = h

objectDirection :: Object -> Direction
objectDirection (MovableO _ _ d _) = d
objectDirection (StaticO _ _) = Direction 0 0

objectSpeed :: Object -> Int
objectSpeed (MovableO _ _ _ s) = s
objectSpeed (StaticO _ _) = 0

-- TODO : tests
-- Moves an object
-- For a movable object it is according to its direction and speed
-- For a static object it is according to the second given argument, being the screen scrolling speed
moveObject :: Object -> Int -> Object
moveObject (MovableO p h (Direction dirx diry) s) _ = 
    let dx = dirx*s
        dy = diry*s
    in (MovableO p (moveHitbox h dx dy) (Direction dirx diry) s)
moveObject (StaticO p h) screenS = 
    (StaticO p (moveHitbox h 0 (-screenS)))

prop_pre_moveObject :: Object -> Int -> Bool
prop_pre_moveObject _ screenS = screenS >= 0

prop_post_moveObject :: Object -> Int -> Bool
prop_post_moveObject (MovableO p h (Direction dirx diry) s) screenS = 
    let dx = dirx*s
        dy = diry*s
    in case moveObject (MovableO p h (Direction dirx diry) s) screenS of
        (MovableO p2 _ d2 s2) -> p == p2 && (prop_post_moveHitbox h dx dy) && d2 == (Direction dirx diry) && s2 == s
        _ -> False
prop_post_moveObject (StaticO p h) screenS = 
    case moveObject (StaticO p h) screenS of
        (StaticO p2 _) -> p == p2 && (prop_post_moveHitbox h 0 (-screenS))
        _ -> False

-- ============================================================
-- ====================== WALLS ===============================
-- ============================================================

-- a wall is a list of static objects, with each colliding with the next one
newtype Wall = Wall [Object]
    deriving (Eq, Show)

prop_wall_allStaticObject :: [Object] -> Bool
prop_wall_allStaticObject [] = True
prop_wall_allStaticObject (x:xs) = case x of
    (MovableO _ _ _ _) -> False
    (StaticO _ _) -> prop_wall_allStaticObject xs

prop_wall_allCollideWithNextObject :: [Object] -> Bool
prop_wall_allCollideWithNextObject [] = True
prop_wall_allCollideWithNextObject (_:[]) = True
prop_wall_allCollideWithNextObject (x:y:xs) = (collision (objectHitbox x) (objectHitbox y)) && prop_wall_allCollideWithNextObject (y:xs)

prop_inv_wall :: [Object] -> Bool
prop_inv_wall l = length l > 0 && prop_wall_allStaticObject l && prop_wall_allCollideWithNextObject l

initWall :: [Object] -> Wall
initWall l
    | length l == 0 = error "a wall must have at least one object"
    | not (prop_wall_allStaticObject l) = error "all wall objects must be static"
    | not (prop_wall_allCollideWithNextObject l) = error "each object must collide with the next in the list"
    | otherwise = Wall l