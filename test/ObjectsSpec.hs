module ObjectsSpec (
    hitboxInitSpec,
    collisionSpec,
    commutativityCollisionSpec,
    directionInitSpec,
    objectInitSpec,
    objectGetHitboxSpec,
    objectGetDirectionSpec,
    objectGetSpeedSpec,
    wallInitSpec
)
where

import Graphics.Gloss (Picture (Blank))

import Test.Hspec
import Test.QuickCheck

import Objects

-- ============================================================
-- ====================== OBJECT HITBOXES =====================
-- ============================================================

newtype TestHitbox = TestHitbox { getHitbox :: Hitbox } deriving (Eq, Show)
instance Arbitrary TestHitbox where
  arbitrary = oneof
    [ TestHitbox <$> (Circle <$> arbitrary <*> arbitrary <*> (abs <$> arbitrary)) -- radius >= 0
    , TestHitbox <$> (Rectangle <$> arbitrary <*> arbitrary <*> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary)) -- width > 0 && heigth > 0
    --, TestHitbox <$> (Hitboxes <$> listOf (getHitbox <$> arbitrary))
    ]

prop_initHitboxCircle_preservesInvariant :: Int -> Int -> Int -> Property
prop_initHitboxCircle_preservesInvariant x y r =
    r >= 0 ==> prop_inv_hitbox (initHitboxCircle x y r)

prop_initHitboxRectangle_preservesInvariant :: Int -> Int -> Int -> Int -> Property
prop_initHitboxRectangle_preservesInvariant x y w h =
    w > 0 && h > 0 ==> prop_inv_hitbox (initHitboxRectangle x y w h)

prop_initHitboxes_preservesInvariant :: [TestHitbox] -> Bool
prop_initHitboxes_preservesInvariant l = prop_inv_hitbox (initHitboxes (map getHitbox l))

hitboxInitSpec :: SpecWith ()
hitboxInitSpec = do
    describe "initHitbox" $ do
        it "preserves the Hitbox invariant for valid Circles" $
            property prop_initHitboxCircle_preservesInvariant

        it "preserves the Hitbox invariant for valid Rectangles" $
            property prop_initHitboxRectangle_preservesInvariant

        it "preserves the Hitbox invariant for a list of Hitboxes" $
            property prop_initHitboxes_preservesInvariant

collisionSpec :: Spec
collisionSpec = do
    describe "collision" $ do
        -- Rectangle vs Rectangle
        it "detects collision between overlapping rectangles" $ do
            let r1 = Rectangle 0 0 10 10
                r2 = Rectangle 5 5 10 10
            collision r1 r2 `shouldBe` True

        it "detects no collision when rectangles are apart" $ do
            let r1 = Rectangle 0 0 10 10
                r2 = Rectangle 20 20 5 5
            collision r1 r2 `shouldBe` False

        -- Circle vs Circle
        it "detects collision between overlapping circles" $ do
            let c1 = Circle 0 0 5
                c2 = Circle 3 4 5 -- distance = 5, sum of radii = 10
            collision c1 c2 `shouldBe` True

        it "detects no collision when circles are apart" $ do
            let c1 = Circle 0 0 5
                c2 = Circle 20 0 5
            collision c1 c2 `shouldBe` False

        -- Circle vs Rectangle
        it "detects collision when circle intersects rectangle" $ do
            let c = Circle 5 5 5
                r = Rectangle 8 8 10 10
            collision c r `shouldBe` True

        it "detects no collision when circle and rectangle are apart" $ do
            let c = Circle 0 0 2
                r = Rectangle 10 10 5 5
            collision c r `shouldBe` False

        -- Rectangle vs Circle (inverse order)
        it "detects collision when rectangle intersects circle (inverse order)" $ do
            let r = Rectangle 8 8 10 10
                c = Circle 5 5 5
            collision r c `shouldBe` True

        -- Hitboxes list
        it "detects collision when one hitbox in the list collides" $ do
            let hlist = Hitboxes [Rectangle 0 0 10 10, Circle 20 20 5]
                c = Circle 5 5 3
            collision hlist c `shouldBe` True

        it "detects no collision when none in the list collide" $ do
            let hlist = Hitboxes [Rectangle 0 0 10 10, Circle 20 20 5]
                c = Circle 50 50 3
            collision hlist c `shouldBe` False

test_prop_commutativity_collision :: TestHitbox -> TestHitbox -> Property
test_prop_commutativity_collision h1 h2 = property (prop_commutativity_collision (getHitbox h1) (getHitbox h2))

commutativityCollisionSpec :: Spec
commutativityCollisionSpec = do
    describe "prop_commutativity_collision" $ do
        it "is symmetric (collision h1 h2 == collision h2 h1)" $
            property test_prop_commutativity_collision



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

directionInitSpec :: SpecWith ()
directionInitSpec = do
    describe "directionInit" $ do
        it "preserves the Direction invariant for valid Directions" $
            property prop_initDirection_preservesInvariant

newtype TestObject = TestObject { getObject :: Object } deriving (Eq, Show)
instance Arbitrary TestObject where
    arbitrary = do
        h <- arbitrary :: Gen TestHitbox
        d <- arbitrary :: Gen TestDirection
        s <- getNonNegative <$> arbitrary
        oneof
          [ return $ TestObject (MovableO Blank (getHitbox h) (getDirection d) s), 
          return $ TestObject (StaticO Blank (getHitbox h))]

prop_initMovableObject_preservesInvariant :: TestHitbox -> TestDirection -> Int -> Property
prop_initMovableObject_preservesInvariant h d s =
    s >= 0 ==> prop_inv_object (initMovableObject Blank (getHitbox h) (getDirection d) s) -- Blank : dummy picture

prop_initStaticObject_preservesInvariant :: TestHitbox -> Property
prop_initStaticObject_preservesInvariant h = 
    property $ prop_inv_object (initStaticObject Blank (getHitbox h)) -- Blank : dummy picture

objectInitSpec :: SpecWith ()
objectInitSpec = do
    describe "initObject" $ do
        it "preserves the Movable Object invariant for valid Movable Objects" $
            property prop_initMovableObject_preservesInvariant

        it "preserves the Static Object invariant for valid Static Objects" $
            property prop_initStaticObject_preservesInvariant

objectGetHitboxSpec :: Spec
objectGetHitboxSpec = do
    describe "objectHitbox" $ do
        it "objectHitbox returns the correct hitbox for MovableO" $ do
            let h = Rectangle 0 0 10 10
                d = Direction 1 0
                obj = MovableO Blank h d 5
            objectHitbox obj `shouldBe` h

        it "objectHitbox returns the correct hitbox for StaticO" $ do
            let h = Circle 5 5 3
                obj = StaticO Blank h
            objectHitbox obj `shouldBe` h

objectGetDirectionSpec :: Spec
objectGetDirectionSpec = do
    describe "objectDirection" $ do
        it "objectDirection returns the correct direction for MovableO" $ do
            let h = Rectangle 0 0 10 10
                d = Direction (-1) 1
                obj = MovableO Blank h d 7
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
                obj = MovableO Blank h d 42
            objectSpeed obj `shouldBe` 42

        it "objectSpeed returns 0 for StaticO" $ do
            let h = Circle 1 1 5
                obj = StaticO Blank h
            objectSpeed obj `shouldBe` 0

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
            mkRects :: Int -> (Int, Int) -> [Object] -> Gen [Object]
            mkRects 0 _ acc = return (reverse acc)
            mkRects k (x,y) acc = do
                -- each rectangle of the wall has its width / height randomly generated (strictly positive) 
                w <- getPositive <$> arbitrary
                h <- getPositive <$> arbitrary
                let rect = Rectangle x y w h
                    xNext = x
                    yNext = y + h `div` 2 -- vertical overlap
                mkRects (k-1) (xNext,yNext) (StaticO Blank rect : acc)


-- Propriétés pour les tests QuickCheck
prop_wall_preservesInvariant :: TestWall -> Bool
prop_wall_preservesInvariant (TestWall (Wall objs)) = prop_inv_wall objs

-- Spec pour les murs
wallInitSpec :: Spec
wallInitSpec = do
    describe "initWall" $ do
        it "preserves the Wall invariant for valid Walls" $
            property prop_wall_preservesInvariant