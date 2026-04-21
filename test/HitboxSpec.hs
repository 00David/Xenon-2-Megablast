module HitboxSpec (
    TestHitbox(..),
    spec
)
where

import Test.Hspec
import Test.QuickCheck

import Objects.Hitbox

spec :: Spec
spec = do
  initHitboxSpec
  partOfHitboxSpec
  centerHitboxSpec
  collisionHitboxSpec
  commutativityCollisionHitboxSpec
  moveHitboxSpec
  moveHitboxQuickCheckSpec

-- ============================================================
-- ====================== OBJECT HITBOXES =====================
-- ============================================================

newtype TestHitbox = TestHitbox { getHitbox :: Hitbox } deriving (Eq, Show)
instance Arbitrary TestHitbox where
  arbitrary = oneof
    [ TestHitbox <$> (Circle <$> arbitrary <*> arbitrary <*> (abs <$> arbitrary)) -- radius >= 0
    , TestHitbox <$> (Rectangle <$> arbitrary <*> arbitrary <*> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary)) -- width > 0 && heigth > 0
    , TestHitbox <$> genValidHitboxes] -- verifies Hitboxes invariants

genValidHitboxes :: Gen Hitbox
genValidHitboxes = do
    x <- arbitrary
    y <- arbitrary

    -- we force the first hitbox to contain (x,y) (a Circle or a Rectangle)
    base <- oneof
        [ Circle x y . abs <$> arbitrary
        , pure (Rectangle (x - 1) (y - 1) 2 2)]

    -- max 9 more hitboxes => max 10 at total
    n <- choose (0, 9)
    let genAtomic = oneof
                [ Circle <$> arbitrary <*> arbitrary <*> (abs <$> arbitrary) -- radius >= 0
                , Rectangle <$> arbitrary <*> arbitrary <*> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary)] -- width > 0 && heigth > 0
    rest <- vectorOf n genAtomic
    return (Hitboxes x y (base : rest))

prop_initHitboxCircle_preservesInvariant :: Float -> Float -> Float -> Property
prop_initHitboxCircle_preservesInvariant x y r =
    r >= 0 ==> prop_inv_hitbox (initHitboxCircle x y r)

prop_initHitboxRectangle_preservesInvariant :: Float -> Float -> Float -> Float -> Property
prop_initHitboxRectangle_preservesInvariant x y w h =
    w > 0 && h > 0 ==> prop_inv_hitbox (initHitboxRectangle x y w h)

prop_initHitboxes_preservesInvariant :: Float -> Float -> [TestHitbox] -> Property
prop_initHitboxes_preservesInvariant x y l = length l > 0
    && all prop_inv_hitbox (map getHitbox l)
    && any (partOfHitbox x y) (map getHitbox l) 
    ==> prop_inv_hitbox (initHitboxes x y (map getHitbox l))

initHitboxSpec :: SpecWith ()
initHitboxSpec = do
    describe "initHitbox" $ do
        it "preserves the Hitbox invariant for valid Circles" $
            property prop_initHitboxCircle_preservesInvariant

        it "preserves the Hitbox invariant for valid Rectangles" $
            property prop_initHitboxRectangle_preservesInvariant

        it "preserves the Hitbox invariant for a list of Hitboxes" $
            property prop_initHitboxes_preservesInvariant

partOfHitboxSpec :: SpecWith ()
partOfHitboxSpec = do
    describe "partOfHitbox" $ do
        it "(1, 1.8) is part of (Rectangle 0 0 2 2)" $ do
            let r = Rectangle 0 0 2 2
            partOfHitbox 1 1.8 r `shouldBe` True

        it "(-3, 1.8) is not part of (Rectangle 0 0 2 2)" $ do
            let r = Rectangle 0 0 2 2
            partOfHitbox (-3) 1.8 r `shouldBe` False

        it "(0.5, 0.3) is part of (Circle 0 0 1)" $ do
            let c = Circle 0 0 1
            partOfHitbox 0.5 0.3 c `shouldBe` True

        it "(1.5, 0.3) is not part of (Circle 0 0 1)" $ do
            let c = Circle 0 0 1
            partOfHitbox 1.5 0.3 c `shouldBe` False

        it "(1, 1.8) is part of [(Rectangle 0 0 2 2), (Circle 0 0 0.5)]" $ do
            let h = Hitboxes 0 0 [Rectangle 0 0 2 2, Circle 0 0 0.5]
            partOfHitbox 1 1.8 h `shouldBe` True

        it "(1, 3) is not part of [(Rectangle 0 0 2 2), (Circle 0 0 0.5)]" $ do
            let h = Hitboxes 0 0 [Rectangle 0 0 2 2, Circle 0 0 0.5]
            partOfHitbox 1 3 h `shouldBe` False

centerHitboxSpec :: Spec
centerHitboxSpec = do
    describe "centerHitbox" $ do

        it "returns the center of a Circle" $ do
            let c = Circle 0 0 10
            centerHitbox c `shouldBe` (0, 0)

        it "returns the center of a Rectangle" $ do
            let r = Rectangle 0 0 10 10
            centerHitbox r `shouldBe` (5, 5)

        it "returns the arbitrary fixed center of Hitboxes list" $ do
            let h = Hitboxes 0.5 0.5 [Circle 0 0 1, Rectangle 0 0 2 2]
            centerHitbox h `shouldBe` (0.5, 0.5)

collisionHitboxSpec :: Spec
collisionHitboxSpec = do
    describe "collisionHitbox" $ do
        -- Rectangle vs Rectangle
        it "detects collision between overlapping rectangles" $ do
            let r1 = Rectangle 0 0 10 10
                r2 = Rectangle 5 5 10 10
            collisionHitbox r1 r2 `shouldBe` True

        it "detects no collision when rectangles are apart" $ do
            let r1 = Rectangle 0 0 10 10
                r2 = Rectangle 20 20 5 5
            collisionHitbox r1 r2 `shouldBe` False

        -- Circle vs Circle
        it "detects collision between overlapping circles" $ do
            let c1 = Circle 0 0 5
                c2 = Circle 3 4 5 -- distance = 5, sum of radii = 10
            collisionHitbox c1 c2 `shouldBe` True

        it "detects no collision when circles are apart" $ do
            let c1 = Circle 0 0 5
                c2 = Circle 20 0 5
            collisionHitbox c1 c2 `shouldBe` False

        -- Circle vs Rectangle
        it "detects collision when circle intersects rectangle" $ do
            let c = Circle 5 5 5
                r = Rectangle 8 8 10 10
            collisionHitbox c r `shouldBe` True

        it "detects no collision when circle and rectangle are apart" $ do
            let c = Circle 0 0 2
                r = Rectangle 10 10 5 5
            collisionHitbox c r `shouldBe` False

        -- Rectangle vs Circle (inverse order)
        it "detects collision when rectangle intersects circle (inverse order)" $ do
            let r = Rectangle 8 8 10 10
                c = Circle 5 5 5
            collisionHitbox r c `shouldBe` True

        -- Hitboxes list
        it "detects collision when one hitbox in the list collides" $ do
            let hlist = Hitboxes 5 5 [Rectangle 0 0 10 10, Circle 20 20 5]
                c = Circle 5 5 3
            collisionHitbox hlist c `shouldBe` True

        it "detects no collision when none in the list collide" $ do
            let hlist = Hitboxes 5 5 [Rectangle 0 0 10 10, Circle 20 20 5]
                c = Circle 50 50 3
            collisionHitbox hlist c `shouldBe` False

commutativityCollisionHitboxSpec :: Spec
commutativityCollisionHitboxSpec = do
    describe "prop_commutativity_collisionHitbox" $ do
        it "is symmetric (collisionHitbox h1 h2 == collisionHitbox h2 h1)" $
            property (\(TestHitbox h1) (TestHitbox h2) ->
                (prop_inv_hitbox h1 && prop_inv_hitbox h2)
                ==> prop_commutativity_collisionHitbox h1 h2
                )

moveHitboxSpec :: Spec
moveHitboxSpec = do
    describe "moveHitbox (unit tests)" $ do

        it "moves a Circle correctly" $ do
            let c = Circle 1 2 5
            moveHitbox c (3, 4) `shouldBe` Circle 4 6 5

        it "moves a Rectangle correctly" $ do
            let r = Rectangle 1 2 10 20
            moveHitbox r (3, 4) `shouldBe` Rectangle 4 6 10 20

        it "moves all hitboxes inside Hitboxes" $ do
            let h = Hitboxes 0.5 0.5 [Circle 0 0 1, Rectangle 0 0 2 2]
            moveHitbox h (10, 10)
                `shouldBe`
                Hitboxes 10.5 10.5 [Circle 10 10 1, Rectangle 10 10 2 2]

moveHitboxQuickCheckSpec :: Spec
moveHitboxQuickCheckSpec = do
    describe "moveHitbox (generated samples)" $ do
        it "satisfies moveHitbox post-condition for all valid Hitbox(es)" $
            property (\(TestHitbox h) (dx, dy) ->
                prop_inv_hitbox h 
                ==> let hPost = moveHitbox h (dx, dy)
                    in prop_inv_hitbox hPost && prop_post_moveHitbox h (dx, dy)
                )