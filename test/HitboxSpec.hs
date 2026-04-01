module HitboxSpec (
    TestHitbox(getHitbox),
    spec
)
where

import Test.Hspec
import Test.QuickCheck

import Hitbox

spec :: Spec
spec = do
  initHitboxSpec
  centerHitboxSpec
  collisionHitboxSpec
  commutativityCollisionHitboxSpec
  moveHitboxSpec
  postMoveHitboxSpec

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

prop_initHitboxCircle_preservesInvariant :: Float -> Float -> Float -> Property
prop_initHitboxCircle_preservesInvariant x y r =
    r >= 0 ==> prop_inv_hitbox (initHitboxCircle x y r)

prop_initHitboxRectangle_preservesInvariant :: Float -> Float -> Float -> Float -> Property
prop_initHitboxRectangle_preservesInvariant x y w h =
    w > 0 && h > 0 ==> prop_inv_hitbox (initHitboxRectangle x y w h)

prop_initHitboxes_preservesInvariant :: [TestHitbox] -> Property
prop_initHitboxes_preservesInvariant l = length l > 0 ==> prop_inv_hitbox (initHitboxes (map getHitbox l))

initHitboxSpec :: SpecWith ()
initHitboxSpec = do
    describe "initHitbox" $ do
        it "preserves the Hitbox invariant for valid Circles" $
            property prop_initHitboxCircle_preservesInvariant

        it "preserves the Hitbox invariant for valid Rectangles" $
            property prop_initHitboxRectangle_preservesInvariant

        it "preserves the Hitbox invariant for a list of Hitboxes" $
            property prop_initHitboxes_preservesInvariant

centerHitboxSpec :: Spec
centerHitboxSpec = do
    describe "centerHitbox" $ do

        it "returns the center of a Circle" $ do
            let c = Circle 0 0 10
            centerHitbox c `shouldBe` Just (0, 0)

        it "returns the center of a Rectangle" $ do
            let r = Rectangle 0 0 10 10
            centerHitbox r `shouldBe` Just (5, 5)

        it "returns Nothing for Hitboxes list" $ do
            let h = Hitboxes [Circle 0 0 1]
            centerHitbox h `shouldBe` Nothing

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
            let hlist = Hitboxes [Rectangle 0 0 10 10, Circle 20 20 5]
                c = Circle 5 5 3
            collisionHitbox hlist c `shouldBe` True

        it "detects no collision when none in the list collide" $ do
            let hlist = Hitboxes [Rectangle 0 0 10 10, Circle 20 20 5]
                c = Circle 50 50 3
            collisionHitbox hlist c `shouldBe` False

test_prop_commutativity_collisionHitbox :: TestHitbox -> TestHitbox -> Property
test_prop_commutativity_collisionHitbox h1 h2 = property (prop_commutativity_collisionHitbox (getHitbox h1) (getHitbox h2))

commutativityCollisionHitboxSpec :: Spec
commutativityCollisionHitboxSpec = do
    describe "prop_commutativity_collisionHitbox" $ do
        it "is symmetric (collisionHitbox h1 h2 == collisionHitbox h2 h1)" $
            property test_prop_commutativity_collisionHitbox

moveHitboxSpec :: Spec
moveHitboxSpec = do
    describe "moveHitbox" $ do

        it "moves a Circle correctly" $ do
            let c = Circle 1 2 5
            moveHitbox c (3, 4) `shouldBe` Circle 4 6 5

        it "moves a Rectangle correctly" $ do
            let r = Rectangle 1 2 10 20
            moveHitbox r (3, 4) `shouldBe` Rectangle 4 6 10 20

        it "moves all hitboxes inside Hitboxes" $ do
            let h = Hitboxes [Circle 0 0 1, Rectangle 0 0 2 2]
            moveHitbox h (10, 10)
                `shouldBe`
                Hitboxes [Circle 10 10 1, Rectangle 10 10 2 2]

test_prop_post_moveHitbox :: TestHitbox -> (Float, Float) -> Property
test_prop_post_moveHitbox (TestHitbox h) (dx, dy) = prop_post_moveHitbox h (dx, dy) === True

postMoveHitboxSpec :: Spec
postMoveHitboxSpec = do
    describe "prop_post_moveHitbox" $ do
        it "correctly moves a hitbox" $
            property test_prop_post_moveHitbox