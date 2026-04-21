module ObjectsSpec (
    TestObject(..),
    spec
)
where

import Graphics.Gloss (Picture (Blank))

import Test.Hspec
import Test.QuickCheck

import Objects.Hitbox
import Objects.Objects
import HitboxSpec(TestHitbox(..))

spec :: Spec
spec = do
  initDirectionSpec
  initObjectSpeedSpec
  initObjectSpec
  objectGetPictureSpec
  objectGetHitboxSpec
  objectGetDirectionSpec
  objectGetSpeedSpec
  moveObjectSpec
  moveObjectQuickCheckSpec
  collisionObjectSpec
  commutativityCollisionObjectSpec
  wallInitSpec

-- ============================================================
-- ====================== OBJECTS =============================
-- ============================================================

newtype TestDirection = TestDirection { getDirection :: Direction } deriving (Eq, Show)
instance Arbitrary TestDirection where
    arbitrary = do
        x <- elements [-1,0,1]
        y <- elements [-1,0,1]
        return $ TestDirection (Direction x y)

prop_initDirection_preservesInvariant :: Property
prop_initDirection_preservesInvariant =
  forAll (elements [-1,0,1]) $ \x ->
  forAll (elements [-1,0,1]) $ \y ->
    prop_inv_direction (initDirection x y)

initDirectionSpec :: SpecWith ()
initDirectionSpec = do
    describe "initDirection" $ do
        it "preserves the Direction invariant for valid Directions" $
            property prop_initDirection_preservesInvariant


newtype TestObjectSpeed = TestObjectSpeed { getObjectSpeed :: ObjectSpeed } deriving (Eq, Show)
instance Arbitrary TestObjectSpeed where
    arbitrary = TestObjectSpeed . ObjectSpeed <$> arbitrary `suchThat` (>= 0)

prop_initObjectSpeed_preservesInvariant :: Float -> Property
prop_initObjectSpeed_preservesInvariant s =
    s >= 0 ==> prop_inv_objectSpeed (initObjectSpeed s)

initObjectSpeedSpec :: SpecWith ()
initObjectSpeedSpec = do
    describe "initObjectSpeed" $ do
        it "preserves the ObjectSpeed invariant for valid ObjectSpeeds" $
            property prop_initObjectSpeed_preservesInvariant


newtype TestObject = TestObject { getObject :: Object } deriving (Eq, Show)
instance Arbitrary TestObject where
    arbitrary = do
        h <- arbitrary :: Gen TestHitbox
        d <- arbitrary :: Gen TestDirection
        s <- arbitrary :: Gen TestObjectSpeed
        oneof
          [ return $ TestObject (MovableO Blank (getHitbox h) (getDirection d) (getObjectSpeed s)), 
          return $ TestObject (StaticO Blank (getHitbox h))]

prop_initMovableObject_preservesInvariant :: TestHitbox -> TestDirection -> TestObjectSpeed -> Property
prop_initMovableObject_preservesInvariant h d s =
    property $ prop_inv_object (initMovableObject Blank (getHitbox h) (getDirection d) (getObjectSpeed s)) -- Blank : dummy picture

prop_initStaticObject_preservesInvariant :: TestHitbox -> Property
prop_initStaticObject_preservesInvariant h = 
    property $ prop_inv_object (initStaticObject Blank (getHitbox h)) -- Blank : dummy picture

initObjectSpec :: SpecWith ()
initObjectSpec = do
    describe "initObject" $ do
        it "preserves the Movable Object invariant for valid Movable Objects" $
            property prop_initMovableObject_preservesInvariant

        it "preserves the Static Object invariant for valid Static Objects" $
            property prop_initStaticObject_preservesInvariant

objectGetPictureSpec :: Spec
objectGetPictureSpec = do
    describe "objectPicture" $ do
        it "objectPicture returns the correct picture for MovableO" $ do
            let h = Rectangle 0 0 10 10
                d = Direction 1 0
                s = (ObjectSpeed 5)
                obj = MovableO Blank h d s
            objectPicture obj `shouldBe` Blank

        it "objectPicture returns the correct picture for StaticO" $ do
            let h = Circle 5 5 3
                obj = StaticO Blank h
            objectPicture obj `shouldBe` Blank

objectGetHitboxSpec :: Spec
objectGetHitboxSpec = do
    describe "objectHitbx" $ do
        it "objectHitbx returns the correct hitbox for MovableO" $ do
            let h = Rectangle 0 0 10 10
                d = Direction 1 0
                s = (ObjectSpeed 5)
                obj = MovableO Blank h d s
            objectHitbox obj `shouldBe` h

        it "objectHitbx returns the correct hitbox for StaticO" $ do
            let h = Circle 5 5 3
                obj = StaticO Blank h
            objectHitbox obj `shouldBe` h

objectGetDirectionSpec :: Spec
objectGetDirectionSpec = do
    describe "objectDirection" $ do
        it "objectDirection returns the correct direction for MovableO" $ do
            let h = Rectangle 0 0 10 10
                d = Direction (-1) 1
                s = (ObjectSpeed 7)
                obj = MovableO Blank h d s
            objectDirection obj `shouldBe` d

        it "objectDirection returns (0,0) for StaticO" $ do
            let h = Circle 0 0 2
                obj = StaticO Blank h
            objectDirection obj `shouldBe` Direction 0 0

objectGetSpeedSpec :: Spec
objectGetSpeedSpec = do
    describe "objectSpeed" $ do
        it "objectSpeed returns the correct speed for MovableO" $ do
            let h = Rectangle 0 0 10 10
                d = Direction 0 1
                s = (ObjectSpeed 42)
                obj = MovableO Blank h d s
            objectSpeed obj `shouldBe` s

        it "objectSpeed returns 0 for StaticO" $ do
            let h = Circle 1 1 5
                obj = StaticO Blank h
            objectSpeed obj `shouldBe` (ObjectSpeed 0)

moveObjectSpec :: Spec
moveObjectSpec = do
    describe "moveObject (unit tests)" $ do
        it "moves MovableO according to direction and speed" $ do
            let h = Rectangle 0 0 10 10
                d = Direction 1 (-1)
                s = (ObjectSpeed 5)
                obj = MovableO Blank h d s
            moveObject obj 0 `shouldBe` (MovableO Blank (Rectangle 5 (-5) 10 10) d s)

        it "does not move MovableO if direction  is (0, 0)" $ do
            let h = Rectangle 0 0 10 10
                d = Direction 0 0
                s = (ObjectSpeed 5)
                obj = MovableO Blank h d s
            moveObject obj 0 `shouldBe` (MovableO Blank (Rectangle 0 0 10 10) d s)

        it "moves StaticO according to screen speed (downwards)" $ do
            let h = Rectangle 0 10 5 5
                obj = StaticO Blank h
                screenS = 3
            moveObject obj screenS `shouldBe` (StaticO Blank (Rectangle 0 7 5 5))

        it "does not move StaticO if screen speed is 0" $ do
            let h = Rectangle 0 10 5 5
                obj = StaticO Blank h
                screenS = 0
            moveObject obj screenS `shouldBe` (StaticO Blank (Rectangle 0 10 5 5))

moveObjectQuickCheckSpec :: Spec
moveObjectQuickCheckSpec = do
    describe "moveObject (generated samples)" $ do
        it "satisfies moveObject post-condition for all valid Objects" $
            property (\(TestObject o) screenS ->
                prop_inv_object o && prop_pre_moveObject o screenS
                ==> let postO = moveObject o screenS in
                    prop_inv_object postO && prop_post_moveObject o screenS
                )

collisionObjectSpec :: Spec
collisionObjectSpec = do
    describe "collisionObject" $ do
        -- Rectangle vs Rectangle
        it "detects collision between overlapping rectangles" $ do
            let o1 = StaticO Blank (Rectangle 0 0 10 10)
                o2 = StaticO Blank (Rectangle 5 5 10 10)
            collisionObject o1 o2 `shouldBe` True

        it "detects no collision when rectangles are apart" $ do
            let o1 = StaticO Blank (Rectangle 0 0 10 10)
                o2 = StaticO Blank (Rectangle 20 20 5 5)
            collisionObject o1 o2 `shouldBe` False

        -- Circle vs Circle
        it "detects collision between overlapping circles" $ do
            let o1 = StaticO Blank (Circle 0 0 5)
                o2 = StaticO Blank (Circle 3 4 5)
            collisionObject o1 o2 `shouldBe` True

        it "detects no collision between distant circles" $ do
            let o1 = StaticO Blank (Circle 0 0 5)
                o2 = StaticO Blank (Circle 20 0 5)
            collisionObject o1 o2 `shouldBe` False

        -- Circle vs Rectangle
        it "detects collision between circle and rectangle" $ do
            let o1 = StaticO Blank (Circle 5 5 5)
                o2 = StaticO Blank (Rectangle 8 8 10 10)
            collisionObject o1 o2 `shouldBe` True

        it "detects no collision between circle and rectangle" $ do
            let o1 = StaticO Blank (Circle 0 0 2)
                o2 = StaticO Blank (Rectangle 10 10 5 5)
            collisionObject o1 o2 `shouldBe` False

        -- Movable vs Static
        it "detects collision between MovableO and StaticO" $ do
            let o1 = MovableO Blank (Rectangle 0 0 10 10) (Direction 1 0) (ObjectSpeed 1)
                o2 = StaticO Blank (Rectangle 5 5 10 10)
            collisionObject o1 o2 `shouldBe` True

        -- Hitboxes (list)
        it "detects collision if one hitbox inside list collides" $ do
            let hlist = Hitboxes 0 0 [Rectangle 0 0 10 10, Circle 20 20 5]
                o1 = StaticO Blank hlist
                o2 = StaticO Blank (Circle 5 5 2)
            collisionObject o1 o2 `shouldBe` True

        it "detects no collision if none collide" $ do
            let hlist = Hitboxes 0 0 [Rectangle 0 0 10 10, Circle 20 20 5]
                o1 = StaticO Blank hlist
                o2 = StaticO Blank (Circle 100 100 2)
            collisionObject o1 o2 `shouldBe` False

commutativityCollisionObjectSpec :: Spec
commutativityCollisionObjectSpec = do
    describe "prop_commutativity_collisionObject" $ do
        it "is symmetric (collisionObject o1 o2 == collisionObject o2 o1)" $
            property (\(TestObject o1) (TestObject o2) ->
                (prop_inv_object o1 && prop_inv_object o2)
                ==> prop_commutativity_collisionObject o1 o2
                )

-- ============================================================
-- ====================== WALLS ===============================
-- ============================================================

newtype TestWall = TestWall { getWall :: Wall } deriving (Eq, Show)
instance Arbitrary TestWall where
    arbitrary = do
        n <- getPositive <$> arbitrary  -- number of objects (here only rectangles) part of the wall

        -- first rectangle position randomly generated
        x0 <- arbitrary
        y0 <- arbitrary

        objs <- mkRects n (x0,y0) []
        return $ TestWall (initWall objs)

        where
            -- Generates a wall of overlapping rectangles
            -- First parameter : the number of remaining rectangles to create
            -- Second parameter : (x, y) of the bottom left angle of the current Rectangle to create
            -- Third parameter : the list containing all objects of the wall so far created
            mkRects :: Int -> (Float, Float) -> [Object] -> Gen [Object]
            mkRects 0 _ acc = return (reverse acc)
            mkRects k (x,y) acc = do
                -- each rectangle of the wall has its width / height randomly generated (strictly positive) 
                w <- getPositive <$> arbitrary
                h <- getPositive <$> arbitrary
                let rect = Rectangle x y w h
                    xNext = x
                    yNext = y + h / 2 -- vertical overlap
                mkRects (k-1) (xNext,yNext) (StaticO Blank rect : acc)

prop_wall_preservesInvariant :: TestWall -> Bool
prop_wall_preservesInvariant (TestWall w) = prop_inv_wall w

wallInitSpec :: Spec
wallInitSpec = do
    describe "initWall" $ do
        it "preserves the Wall invariant for valid Walls" $
            property prop_wall_preservesInvariant