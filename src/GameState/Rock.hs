{-# LANGUAGE InstanceSigs #-}
module GameState.Rock (module GameState.Rock) where

import Graphics.Gloss (Picture(Translate))

import qualified Data.Sequence as Seq

import GameSetup
import Graphics.Assets
import Objects.Hitbox
import Objects.Objects
import Typeclasses.Invariant
import Typeclasses.Movable

-- ============================================================
-- ================= ROCK (part of walls) =====================
-- ============================================================

type RockAsset = Int -- 0, 1, 2 or 3 (= (nbRockAssets-1))

data Rock = 
    LeftRock {
        rockObject :: Object, -- must be a static Object
        rockAsset :: RockAsset, -- must be inside [0, (nbRockAssets-1)]
        rockForward :: Bool -- if not forward, there is an additional offset to the bottom limit of the screen : a forward rock will be out of screen faster than a non forward rock
    } 
    | RightRock {
        rockObject :: Object, -- must be a static Object
        rockAsset :: RockAsset, -- must be inside [0, (nbRockAssets-1)]
        rockForward :: Bool -- if not forward, there is an additional offset to the bottom limit of the screen : a forward rock will be out of screen faster than a non forward rock
    } deriving (Show, Eq)

prop_inv_rock :: Rock -> Bool
prop_inv_rock rock =
    let obj =  rockObject rock
        asset = rockAsset rock
    in case obj of
        (MovableO _ _ _) -> False
        (StaticO _) -> prop_inv_object obj && asset >= 0 && asset <= (nbRockAssets-1)

-- ============================================================
-- =================== ROCK CONSTRUCTORS ======================
-- ============================================================

-- Sequence of rock hitboxes initializers
rockHitbox :: Seq.Seq (XCoord -> YCoord -> Bool -> Hitbox)
rockHitbox = Seq.fromList [rock0Hitbox, rock1Hitbox, rock2Hitbox, rock3Hitbox]

-- Mirrors a rectangle around the vertical axis x = xAxis.
-- The rectangle is defined using its bottom-left coordinates.
mirrorRect :: Float -> XBottomLeft -> YBottomLeft -> Width -> Heigth -> Hitbox
mirrorRect xAxis xBL yBL w h =
    let xCenter = xBL + w/2
        xCenter' = 2*xAxis - xCenter
        xBL' = xCenter' - w/2
    in initHitboxRectangle xBL' yBL w h

rock0Hitbox :: XCoord -> YCoord -> Bool -> Hitbox
rock0Hitbox x y leftSide =
    let width = Seq.index widthRocks 0
        height = Seq.index heightRocks 0
        initHitbox = if leftSide then initHitboxRectangle else (mirrorRect x)
    in 
        initHitboxes x y
        [ 
            initHitbox (x-(width/2)) (y) width 15,
            initHitbox (x-36) (y) 30 21,
            initHitbox (x-(width/2)) (y-9) 84 9,
            initHitbox (x-(width/2)) (y-15) 81 15,
            initHitbox (x-(width/2)) (y-(height/2)) 63 21
        ]

rock1Hitbox :: XCoord -> YCoord -> Bool -> Hitbox
rock1Hitbox x y leftSide =
    let width = Seq.index widthRocks 0
        height = Seq.index heightRocks 0
        initHitbox = if leftSide then initHitboxRectangle else (mirrorRect x)
    in 
        initHitboxes x y
        [ 
            initHitbox (x-(width/2)) (y-6) width 12,
            initHitbox (x-(width/2)) (y-12) 87 18,
            initHitbox (x-(width/2)) (y-18) 81 27,
            initHitbox (x-(width/2)) (y-18) 69 36,
            initHitbox (x-(width/2)) (y-(height/2)) 57 42
        ]

rock2Hitbox :: XCoord -> YCoord -> Bool -> Hitbox
rock2Hitbox x y leftSide =
    let width = Seq.index widthRocks 0
        height = Seq.index heightRocks 0
        initHitbox = if leftSide then initHitboxRectangle else (mirrorRect x)
    in 
        initHitboxes x y
        [ 
            initHitbox (x) (y+3) ((width/2)-3) 10,
            initHitbox (x+28) (y-9) 7 28,
            initHitbox (x-(width/2)) (y-13) 78 29,
            initHitbox (x-(width/2)) ((y-height/2)) 71 38,
            initHitbox (x-(width/2)) ((y-height/2)) 34 44
        ]

rock3Hitbox :: XCoord -> YCoord -> Bool -> Hitbox
rock3Hitbox x y leftSide =
    let width = Seq.index widthRocks 0
        height = Seq.index heightRocks 0
        initHitbox = if leftSide then initHitboxRectangle else (mirrorRect x)
    in 
        initHitboxes x y
        [ 
            initHitbox (x+30) ((y-height/2)) 9 height,
            initHitbox (x-(width/2)) (y-15) 78 29,
            initHitbox (x-(width/2)) (y-18) 36 39
        ]

initRock :: Object -> RockAsset -> Bool -> Bool -> Rock
initRock obj asset leftSide forward = 
    case obj of
    (MovableO _ _ _) -> error "a rock cannot be represented by a movable object"
    (StaticO _) -> 
        if asset >= 0 && asset <= (nbRockAssets-1) 
            then 
                if leftSide 
                    then (LeftRock obj asset forward)
                    else (RightRock obj asset forward)
            else error "invalid rock type"

startInitRock :: XCoord -> YCoord -> RockAsset -> Bool -> Bool -> Rock
startInitRock x y asset leftSide forward = -- x and y centers of the rock
    let h = (Seq.index rockHitbox asset) x y leftSide
        ro = initStaticObject h
    in initRock ro asset leftSide forward

-- ============================================================
-- =================== ROCK OPERATIONS =======================
-- ============================================================

-- Moves a rock
moveRock :: Rock -> ScreenScrollingSpeed -> Rock
moveRock (LeftRock ro asset frwd) ss = initRock (moveObject ro ss) asset True frwd
moveRock (RightRock ro asset frwd) ss = initRock (moveObject ro ss) asset False frwd

prop_pre_moveRock :: Rock -> ScreenScrollingSpeed -> Bool
prop_pre_moveRock _ ss = ss > 0 -- strictly positive screen scrolling speed

prop_post_moveRock :: Rock -> ScreenScrollingSpeed -> Bool
prop_post_moveRock r ss = 
    let r2 = moveRock r ss
    in case (r, r2) of -- ensures that all other attributes than the object stay the same, as well as the rock type
        ((LeftRock _ asset frwd), (LeftRock _ asset' frwd')) -> asset == asset' && frwd == frwd'
        ((RightRock _ asset frwd), (RightRock _ asset' frwd')) -> asset == asset' && frwd == frwd'
        _ -> False

-- Indicates if a rock is ~ inside the screen ~ : it must not be below a certain y coordinate to be inside
-- The X coodinate is not taken into account : it will allow us to have some rocks being partially outside of the screen, on left or right 
-- If not forward, a little offset for this y limit is added
insideYScreenRock :: Rock -> Bool
insideYScreenRock rock = 
    let obj = rockObject rock
        (_,y) = centerHitbox (objectHitbox obj)
        limit = if rockForward rock then (bottomYScreenBound-rockCell) else (bottomYScreenBound-rockCell+(rockCell/2))
    in  y >= limit

-- ============================================================
-- ===================== ROCK INVARIANT =======================
-- ============================================================

instance Invariant Rock where
    prop_inv :: Rock -> Bool
    prop_inv = prop_inv_rock 

-- ============================================================
-- ==================== ROCK RENDERABLE =======================
-- ============================================================

instance Renderable Rock where
    getTranslatedAssets :: GameAssets -> Rock -> [Picture]
    getTranslatedAssets ga rock = getTranslatedRockAsset ga rock

-- Returns a list of translated rock assets (only one).
getTranslatedRockAsset :: GameAssets -> Rock -> [Picture]
getTranslatedRockAsset ga (LeftRock ro sprite _) = 
    let rockPic = Seq.index (leftWallPics ga) sprite
        (rx, ry) = centerHitbox (objectHitbox ro)
    in [Translate rx ry rockPic]
getTranslatedRockAsset ga (RightRock ro sprite _) = 
    let rockPic = Seq.index (rightWallPics ga) sprite
        (rx, ry) = centerHitbox (objectHitbox ro)
    in [Translate rx ry rockPic]

prop_post_getTranslatedRockAsset :: GameAssets -> Rock -> Bool
prop_post_getTranslatedRockAsset ga rock = length (getTranslatedRockAsset ga rock) == 1 -- exactly one rock asset

-- ============================================================
-- ====================== ROCK MOVABLE ========================
-- ============================================================

instance Movable Rock where
    move :: Rock -> ScreenScrollingSpeed -> Rock
    move = moveRock

    insideScreen :: Rock -> Bool
    insideScreen = insideYScreenRock

-- ============================================================
-- =================== ROCK COLLIDABLE ======================
-- ============================================================

instance Collidable Rock where
    getObjects :: Rock -> [Object]
    getObjects rock = [rockObject rock]

    collision :: Collidable b => Rock -> b -> Bool
    collision rock other =
        let objs1 = getObjects rock
            objs2 = getObjects other
        in any (\o1 -> any (\o2 -> collisionObject o1 o2) objs2) objs1

    willCollide :: Collidable b => Rock -> b -> ScreenScrollingSpeed -> Bool  
    willCollide rock other screenSpeed =
        let rockMoved = move rock screenSpeed
        in collision rockMoved other