module Objects (module Objects) where

import Graphics.Gloss (Picture)

import Hitbox

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

newtype ObjectSpeed = ObjectSpeed Float -- speed >= 0
    deriving (Eq, Show)

prop_inv_objectSpeed :: ObjectSpeed -> Bool 
prop_inv_objectSpeed (ObjectSpeed s) = s >= 0

initObjectSpeed :: Float -> ObjectSpeed
initObjectSpeed s = if s < 0 then error "speed cannot be strictly negative" else (ObjectSpeed s)

data Object = MovableO Picture Hitbox Direction ObjectSpeed
    | StaticO Picture Hitbox
    deriving (Eq, Show)

prop_inv_object :: Object -> Bool 
prop_inv_object (MovableO _ h d s) = prop_inv_hitbox h && prop_inv_direction d && prop_inv_objectSpeed s
prop_inv_object (StaticO _ h) = prop_inv_hitbox h

initMovableObject :: Picture -> Hitbox -> Direction -> ObjectSpeed -> Object
initMovableObject p h d s = (MovableO p h d s)

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

objectSpeed :: Object -> ObjectSpeed
objectSpeed (MovableO _ _ _ s) = s
objectSpeed (StaticO _ _) = ObjectSpeed 0

-- Moves an object
-- For a movable object it is according to its direction and speed
-- For a static object it is according to the second given argument, being the screen scrolling speed
moveObject :: Object -> Float -> Object
moveObject (MovableO p h (Direction dirx diry) os@(ObjectSpeed s)) _ = 
    let dx = (fromIntegral dirx)*s
        dy = (fromIntegral diry)*s
    in (initMovableObject p (moveHitbox h (dx, dy)) (Direction dirx diry) os)
moveObject (StaticO p h) screenS = 
    (initStaticObject p (moveHitbox h (0, -screenS)))

prop_pre_moveObject :: Object -> Float -> Bool
prop_pre_moveObject _ screenS = screenS >= 0

prop_post_moveObject :: Object -> Float -> Bool
prop_post_moveObject (MovableO p h d@(Direction dirx diry) os@(ObjectSpeed s)) screenS = 
    let dx = (fromIntegral dirx)*s
        dy = (fromIntegral diry)*s
    in case moveObject (MovableO p h d os) screenS of
        (MovableO p2 _ d2 os2) -> p == p2 && (prop_post_moveHitbox h (dx, dy)) && d == d2 && os == os2
        _ -> False
prop_post_moveObject (StaticO p h) screenS = 
    case moveObject (StaticO p h) screenS of
        (StaticO p2 _) -> p == p2 && (prop_post_moveHitbox h (0, -screenS))
        _ -> False

-- Tests TODO
-- Detects if there is a collision between 2 objects (thanks to their hitboxes)
collisionObject :: Object -> Object -> Bool
collisionObject o1 o2 = collisionHitbox (objectHitbox o1) (objectHitbox o2)

-- ============================================================
-- ====================== WALLS ===============================
-- ============================================================

-- a wall is a non empty list of static objects, with each collides with the next one in the list
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
prop_wall_allCollideWithNextObject (x:y:xs) = (collisionHitbox (objectHitbox x) (objectHitbox y)) && prop_wall_allCollideWithNextObject (y:xs)

prop_inv_wall :: [Object] -> Bool
prop_inv_wall l = length l > 0 && prop_wall_allStaticObject l && prop_wall_allCollideWithNextObject l

initWall :: [Object] -> Wall
initWall l
    | length l == 0 = error "a wall must have at least one object"
    | not (prop_wall_allStaticObject l) = error "all wall objects must be static"
    | not (prop_wall_allCollideWithNextObject l) = error "each object must collide with the next in the list"
    | otherwise = Wall l