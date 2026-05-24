{-# LANGUAGE InstanceSigs #-}
module Objects.Objects (module Objects.Objects) where

import GameSetup
import Objects.Hitbox
import Typeclasses.Invariant
import Typeclasses.Movable

-- ============================================================
-- ================= COLLIDABLE TYPECLASS =====================
-- ============================================================

class (Movable a) => Collidable a where
    -- Get a list of objects representing a (in general only one)
    getObjects :: a -> [Object]

    -- Detects if there is a collision with current object positions
    collision :: Collidable b => a -> b -> Bool

    -- Detects if there is a collision after moving a. Float is the scrolling speed
    willCollide :: Collidable b => a -> b -> ScreenScrollingSpeed -> Bool

law_collidable_reflexive :: Collidable a => a -> Bool
law_collidable_reflexive x = collision x x

law_collidable_symmetric :: (Collidable a, Collidable b) => a -> b -> Bool
law_collidable_symmetric x y = collision x y == collision y x

law_collidable_will_collide :: Collidable a => Collidable b => a -> b -> ScreenScrollingSpeed -> Bool
law_collidable_will_collide x y s = willCollide x y s == collision (move x s) y

-- ============================================================
-- ======================= DIRECTION ==========================
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

-- ============================================================
-- ====================== OBJECTSPEED =========================
-- ============================================================

newtype ObjectSpeed = ObjectSpeed Float -- speed >= 0
    deriving (Eq, Show)

prop_inv_objectSpeed :: ObjectSpeed -> Bool 
prop_inv_objectSpeed (ObjectSpeed s) = s >= 0

initObjectSpeed :: Float -> ObjectSpeed
initObjectSpeed s = if s < 0 then error "speed cannot be strictly negative" else (ObjectSpeed s)

-- ============================================================
-- ========================= OBJECT ===========================
-- ============================================================

data Object = MovableO Hitbox Direction ObjectSpeed -- A movable object moves according to its own direction and speed
    | StaticO Hitbox -- A static object moves according to the screen scrolling speed (towards the bottom)
    deriving (Eq, Show)

prop_inv_object :: Object -> Bool
prop_inv_object (MovableO h d s) = prop_inv_hitbox h && prop_inv_direction d && prop_inv_objectSpeed s
prop_inv_object (StaticO h) = prop_inv_hitbox h

initMovableObject :: Hitbox -> Direction -> ObjectSpeed -> Object
initMovableObject h d s = (MovableO h d s)

initStaticObject :: Hitbox -> Object
initStaticObject h = (StaticO h)

-- ============================================================
-- =================== OBJECT OPERATIONS ======================
-- ============================================================

objectHitbox :: Object -> Hitbox
objectHitbox (MovableO h _ _) = h
objectHitbox (StaticO h) = h

objectDirection :: Object -> Direction
objectDirection (MovableO _ d _) = d
objectDirection (StaticO _) = Direction 0 0

objectSpeed :: Object -> ObjectSpeed
objectSpeed (MovableO _ _ s) = s
objectSpeed (StaticO _) = ObjectSpeed 0

-- Moves an object
-- For a movable object it is according to its direction and speed
-- For a static object it is according to the second given argument, being the screen scrolling speed
moveObject :: Object -> ScreenScrollingSpeed -> Object
moveObject (MovableO h (Direction dirx diry) os@(ObjectSpeed s)) _ = 
    let dx = (fromIntegral dirx)*s
        dy = (fromIntegral diry)*s
    in (initMovableObject (moveHitbox h (dx, dy)) (Direction dirx diry) os)
moveObject (StaticO h) screenS = 
    (initStaticObject (moveHitbox h (0, -screenS)))

prop_pre_moveObject :: Object -> ScreenScrollingSpeed -> Bool
prop_pre_moveObject _ screenS = screenS >= 0 -- The screen scrolling speed must be strictly positive

prop_post_moveObject :: Object -> ScreenScrollingSpeed -> Bool
prop_post_moveObject (MovableO h d os) screenS = 
    case moveObject (MovableO h d os) screenS of
        (MovableO _ d2 os2) -> d == d2 && os == os2 -- hitbox has changed position, verified by the moveHitbox postcondition, and direction and object speed must stay the same
        _ -> False
prop_post_moveObject (StaticO h) screenS = 
    case moveObject (StaticO h) screenS of
        (StaticO _) -> True -- hitbox has changed position, verified by the moveHitbox postcondition, so no more verification
        _ -> False

-- Indicates if an object is inside the screen, according to its hitbox
insideScreenObject :: Object -> Bool
insideScreenObject (MovableO h _ _)  = insideScreenHitbox h
insideScreenObject (StaticO h)  = insideScreenHitbox h

-- Detects if there is a collision between 2 objects (thanks to their hitboxes)
collisionObject :: Object -> Object -> Bool
collisionObject o1 o2 = collisionHitbox (objectHitbox o1) (objectHitbox o2)

prop_commutativity_collisionObject :: Object -> Object -> Bool
prop_commutativity_collisionObject o1 o2 = (collisionObject o1 o2 == collisionObject o2 o1)

-- ============================================================
-- ================== DIRECTION INVARIANT =====================
-- ============================================================

instance Invariant Direction where
    prop_inv :: Direction -> Bool
    prop_inv = prop_inv_direction

-- ============================================================
-- ================== OBJECTSPEED INVARIANT ===================
-- ============================================================

instance Invariant ObjectSpeed where
    prop_inv :: ObjectSpeed -> Bool
    prop_inv = prop_inv_objectSpeed

-- ============================================================
-- =================== OBJECTS INVARIANT ======================
-- ============================================================

instance Invariant Object where
    prop_inv :: Object -> Bool
    prop_inv = prop_inv_object

-- ============================================================
-- ===================== OBJECTS MOVABLE ======================
-- ============================================================

instance Movable Object where
    move :: Object -> ScreenScrollingSpeed -> Object
    move = moveObject

    insideScreen :: Object -> Bool
    insideScreen = insideScreenObject

-- ============================================================
-- ================== OBJECTS COLLIDABLE ======================
-- ============================================================

instance Collidable Object where
    getObjects :: Object -> [Object]
    getObjects object = [object]

    collision :: Collidable b => Object -> b -> Bool
    collision obj other =
        let objs2 = getObjects other
        in any (\o2 -> collisionObject obj o2) objs2

    willCollide :: Collidable b => Object -> b -> ScreenScrollingSpeed -> Bool  
    willCollide obj other screenSpeed =
        let objs2 = getObjects other
            movedObj = moveObject obj screenSpeed
        in any (\o2 -> collisionObject movedObj o2) objs2