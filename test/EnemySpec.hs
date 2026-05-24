{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EnemySpec (
    TestEnemy(..),
    TestEnemyScript(..),
    spec
)
where

import Test.Hspec
import Test.QuickCheck

import System.Random

import GameSetup
import GameState.Enemy
import Graphics.Assets
import Objects.Hitbox
import Objects.Objects
import Typeclasses.Destroyable
import Typeclasses.Invariant
import Typeclasses.Movable
import AssetsSpec(TestGameAssets(..))
import ObjectsSpec(TestObject(..))

spec :: Spec
spec = do
    initEnemySpec
    startInitNoMoveButBoomEnemySpec
    startInitLeftRightShootEnemySpec
    startInitLoopEnemySpec
    moveEnemySpec
    moveEnemyQuickCheckSpec
    insideScreenOrAboveEnemySpec
    enemyShotSpec
    shootEnemySpec
    shootEnemyQuickCheckSpec
    generateListEnemiesSpec
    generateListEnemiesQuickCheckSpec
    generateFormationSpec
    generateFormationQuickCheckSpec
    getTranslatedEnemyAssetQuickCheckSpec
    initEnemyScriptSpec
    startInitEnemyScriptSpec
    takeDamageMaybeEnemySpec
    takeDamageMaybeEnemyQuickCheckSpec
    invariantLawsSpec
    renderableLawSpec
    movableLawsSpec
    collidableLawsSpec
    destroyableLawsSpec

fixedGenSeed :: Int
fixedGenSeed = 42 -- for creating generators in unit tests

-- ============================================================
-- ========================= ENEMY ============================
-- ============================================================

genNoMoveButBoomEnemy :: Gen TestEnemy
genNoMoveButBoomEnemy = do
    x <- choose (leftXScreenBound + 50, rightXScreenBound - 50)
    y <- choose (bottomYScreenBound, topYScreenBound + 300) -- can be generated above screen, then would scroll down
    return $ TestEnemy (startInitNoMoveButBoomEnemy x y)

genLeftRightShootEnemy :: Gen TestEnemy
genLeftRightShootEnemy = do
    x <- choose (leftXScreenBound + 50, rightXScreenBound - 50)
    y <- choose (topYScreenBound, topYScreenBound + 300) -- can be generated above screen, then would go down
    xTarget <- choose (leftXScreenBound + 100, rightXScreenBound - 100)
    yTarget <- choose (bottomYScreenWithBarBound + 50, topYScreenBound - 50)
    return $ TestEnemy (startInitLeftRightShootEnemy x y xTarget yTarget)

genLoopEnemy :: Gen TestEnemy
genLoopEnemy = do
    x <- choose (leftXScreenBound + 50, rightXScreenBound - 50)
    y <- choose (topYScreenBound, topYScreenBound + 300) -- can be generated above screen, then would go down
    yTarget <- choose (bottomYScreenWithBarBound + 50, topYScreenBound - 50)
    return $ TestEnemy (startInitLoopEnemy x y yTarget)

-- Initializes Enemies veryfing their invariant
newtype TestEnemy = TestEnemy { getEnemy :: Enemy } deriving (Eq, Show)
instance Arbitrary TestEnemy where
    arbitrary :: Gen TestEnemy
    arbitrary = oneof [
            genNoMoveButBoomEnemy,
            genLeftRightShootEnemy,
            genLoopEnemy
        ]

minSS :: Float
minSS = 1
maxSS :: Float
maxSS = 10

prop_initEnemy_preservesInvariant :: TestObject -> Property
prop_initEnemy_preservesInvariant (TestObject obj) =
    forAll (choose (1, 10)) $ \health ->
    forAll (choose (1, 50)) $ \dmg ->
    forAll (choose (1, 1000)) $ \score ->
    forAll arbitrary $ \(TestEnemyScript script) ->
        prop_inv_object obj && prop_inv_enemyScript script
        ==> prop_inv_enemy (initEnemy obj health dmg score script)

prop_startInitNoMoveButBoomEnemy_preservesInvariant :: Property
prop_startInitNoMoveButBoomEnemy_preservesInvariant =
    forAll (choose (leftXScreenBound + 50, rightXScreenBound - 50)) $ \x ->
    forAll (choose (topYScreenBound, topYScreenBound + 300)) $ \y ->
        prop_inv_enemy (startInitNoMoveButBoomEnemy x y)

prop_startInitLeftRightShootEnemy_preservesInvariant :: Property
prop_startInitLeftRightShootEnemy_preservesInvariant =
    forAll (choose (leftXScreenBound + 50, rightXScreenBound - 50)) $ \x ->
    forAll (choose (topYScreenBound, topYScreenBound + 300)) $ \y ->
    forAll (choose (leftXScreenBound + 100, rightXScreenBound - 100)) $ \xTarget ->
    forAll (choose (bottomYScreenWithBarBound + 50, topYScreenBound - 50)) $ \yTarget ->
        prop_inv_enemy (startInitLeftRightShootEnemy x y xTarget yTarget)

prop_startInitLoopEnemy_preservesInvariant :: Property
prop_startInitLoopEnemy_preservesInvariant =
    forAll (choose (leftXScreenBound + 50, rightXScreenBound - 50)) $ \x ->
    forAll (choose (topYScreenBound, topYScreenBound + 300)) $ \y ->
    forAll (choose (bottomYScreenWithBarBound + 50, topYScreenBound - 50)) $ \yTarget ->
        prop_inv_enemy (startInitLoopEnemy x y yTarget)

initEnemySpec :: SpecWith ()
initEnemySpec = do
    describe "initEnemy (QuickCheck)" $ do
        it "preserves the Enemy invariant for valid Enemies" $
            property prop_initEnemy_preservesInvariant

startInitNoMoveButBoomEnemySpec :: SpecWith ()
startInitNoMoveButBoomEnemySpec = do
    describe "startInitNoMoveButBoomEnemy (QuickCheck)" $ do
        it "preserves the Enemy invariant for start NoMoveButBoom Enemies" $
            property prop_startInitNoMoveButBoomEnemy_preservesInvariant

startInitLeftRightShootEnemySpec :: SpecWith ()
startInitLeftRightShootEnemySpec = do
    describe "startInitLeftRightShootEnemy (QuickCheck)" $ do
        it "preserves the Enemy invariant for start LeftRightShoot Enemies" $
            property prop_startInitLeftRightShootEnemy_preservesInvariant

startInitLoopEnemySpec :: SpecWith ()
startInitLoopEnemySpec = do
    describe "startInitLoopEnemy (QuickCheck)" $ do
        it "preserves the Enemy invariant for start Loop Enemies" $
            property prop_startInitLoopEnemy_preservesInvariant

moveEnemySpec :: Spec
moveEnemySpec = do
    describe "moveEnemy (unit tests)" $ do
        it "moves NoMoveButBoomEnemy according to screen speed only" $ do
            let e = startInitNoMoveButBoomEnemy 0 100
                ss = 5
                e' = moveEnemy e ss
                (_, y) = centerHitbox (objectHitbox (enemyObject e))
                (_, y') = centerHitbox (objectHitbox (enemyObject e'))
            y' `shouldBe` (y - ss)
        it "moves LeftRightShootEnemy according to its direction towards the bottom and its speed, at start" $ do
            let e = startInitLeftRightShootEnemy 0 (topYScreenBound+200) 200 0
                ss = 3
                e' = moveEnemy e ss
                (_, y) = centerHitbox (objectHitbox (enemyObject e))
                (_, y') = centerHitbox (objectHitbox (enemyObject e'))
            y' `shouldBe` (y-leftRightShootEnemySpeed)
        it "moves LoopEnemy according to its direction towards the bottom and its speed, at start" $ do
            let e = startInitLoopEnemy 0 (topYScreenBound+200) 0
                ss = 4
                e' = moveEnemy e ss
                (_, y) = centerHitbox (objectHitbox (enemyObject e))
                (_, y') = centerHitbox (objectHitbox (enemyObject e'))
            y' `shouldBe` (y-loopEnemySpeed)

moveEnemyQuickCheckSpec :: Spec
moveEnemyQuickCheckSpec = do
    describe "moveEnemy (QuickCheck)" $ do
        it "satisfies moveEnemy post-condition for all valid parameters" $
            property (\(TestEnemy e) ->
                forAll (choose (minSS, maxSS)) $ \ss ->
                    (prop_inv_enemy e && prop_pre_moveEnemy e ss)
                    ==> let e' = moveEnemy e ss
                        in prop_inv_enemy e' && prop_post_moveEnemy e ss
            )

insideScreenOrAboveEnemySpec :: Spec
insideScreenOrAboveEnemySpec = do
    describe "insideScreenOrAboveEnemy (unit tests)" $ do
        it "enemy above screen is inside" $ do
            let e = startInitNoMoveButBoomEnemy 0 (topYScreenBound + 100)
            insideScreenOrAboveEnemy e `shouldBe` True
        it "enemy inside screen is inside" $ do
            let e = startInitLeftRightShootEnemy 0 0 200 (-200)
            insideScreenOrAboveEnemy e `shouldBe` True
        it "enemy below screen is outside" $ do
            let e = startInitLoopEnemy 0 (bottomYScreenBound - 200) (-400)
            insideScreenOrAboveEnemy e `shouldBe` False

enemyShotSpec :: Spec
enemyShotSpec = do
    describe "enemyShot (unit tests)" $ do
        it "NoMoveButBoomEnemy cannot shoot" $ do
            let e = startInitNoMoveButBoomEnemy 0 0
                maybeProj = enemyShot e
            maybeProj `shouldBe` Nothing
        it "LeftRightShootEnemy can shoot when delay is 1" $ do
            let (Enemy eo health dmg score _) = startInitLeftRightShootEnemy 0 0 100 50
                -- Manually set delay to 1 by creating the enemy with the right script
                scriptWithDelay1 = initLeftRightShootEnemyScript 100 50 1
                e = initEnemy eo health dmg score scriptWithDelay1
                maybeProj = enemyShot e
            case maybeProj of
                Just _ -> True `shouldBe` True
                Nothing -> expectationFailure "Expected a projectile"
        it "LeftRightShootEnemy don't shoot when delay is 10 (just not 1)" $ do
            let (Enemy eo health dmg score _) = startInitLeftRightShootEnemy 0 0 100 50
                -- Manually set delay to 10 by creating the enemy with the right script
                scriptWithDelay10 = initLeftRightShootEnemyScript 100 50 10
                e = initEnemy eo health dmg score scriptWithDelay10
                maybeProj = enemyShot e
            case maybeProj of
                Just _ -> expectationFailure "Did not expected a projectile"
                Nothing -> True `shouldBe` True
        it "LoopEnemy cannot shoot" $ do
            let e = startInitLoopEnemy 0 100 50
                maybeProj = enemyShot e
            maybeProj `shouldBe` Nothing

shootEnemySpec :: Spec
shootEnemySpec = do
    describe "shootEnemy (unit tests)" $ do
        it "NoMoveButBoomEnemy never shoots" $ do
            let e = startInitNoMoveButBoomEnemy 0 100
                (maybeProj, _) = shootEnemy e
            maybeProj `shouldBe` Nothing
        it "LeftRightShootEnemy shoots when delay reaches 1" $ do
            let (Enemy eo health dmg score _) = startInitLeftRightShootEnemy 0 100 100 50
                scriptWithDelay1 = initLeftRightShootEnemyScript 100 50 1
                e = initEnemy eo health dmg score scriptWithDelay1
                (maybeProj, (Enemy _ _ _ _ script')) = shootEnemy e
            case maybeProj of
                Just _ -> do
                    -- Delay should be reset
                    case script' of
                        LeftRightShootEnemy _ _ delay' -> 
                            delay' `shouldBe` leftRightShootEnemyShootDelay
                        _ -> expectationFailure "Expected LeftRightShootEnemy script"
                Nothing -> expectationFailure "Expected a projectile"
        it "LeftRightShootEnemy does not shoot when delay is at 10 (not reached 1), delay is decremented" $ do
            let (Enemy eo health dmg score _) = startInitLeftRightShootEnemy 0 100 100 50
                scriptWithDelay10 = initLeftRightShootEnemyScript 100 50 10
                e = initEnemy eo health dmg score scriptWithDelay10
                (maybeProj, (Enemy _ _ _ _ script')) = shootEnemy e
            case maybeProj of
                Just _ -> expectationFailure "Does not expected a projectile"
                Nothing -> case script' of
                            LeftRightShootEnemy _ _ delay' -> 
                                delay' `shouldBe` (10-1) -- delay decreased
                            _ -> expectationFailure "Expected LeftRightShootEnemy script"
        it "LoopEnemy never shoots" $ do
            let e = startInitLoopEnemy 0 100 50
                (maybeProj, _) = shootEnemy e
            maybeProj `shouldBe` Nothing

shootEnemyQuickCheckSpec :: Spec
shootEnemyQuickCheckSpec = do
    describe "shootEnemy (QuickCheck)" $ do
        it "satisfies shootEnemy post-condition for all valid parameters" $
            property (\(TestEnemy e) ->
                prop_inv_enemy e
                ==> let (_, e') = shootEnemy e
                    in prop_inv_enemy e' && prop_post_shootEnemy e
            )

generateListEnemiesSpec :: Spec
generateListEnemiesSpec = do
    describe "generateListEnemies (unit tests)" $ do
        it "generates no enemies when n <= 0" $ do
            let gen = mkStdGen fixedGenSeed
                (_, enemies) = generateListEnemies 0 gen
            length enemies `shouldBe` 0
        it "generates exactly 3 enemies when n = 3" $ do
            let gen = mkStdGen fixedGenSeed
                (_, enemies) = generateListEnemies 3 gen
            length enemies `shouldBe` 3

generateListEnemiesQuickCheckSpec :: Spec
generateListEnemiesQuickCheckSpec = do
    describe "generateListEnemies (QuickCheck)" $ do
        it "satisfies generateListEnemies post-condition for all valid parameters" $
            property (\seed -> do
                forAll (choose (0, 5)) $ \nbEnemies ->
                    let gen = mkStdGen seed
                    in  prop_pre_generateListEnemies nbEnemies gen
                        ==> prop_post_generateListEnemies nbEnemies gen
            )

generateFormationSpec :: Spec
generateFormationSpec = do
    describe "generateFormation (unit tests)" $ do
        it "generates correct number of 3 enemies" $ do
            let enemies = generateFormation 0 3 0 (topYScreenBound + 100) 150 50
            length enemies `shouldBe` 3
        it "all enemies of type 0 are NoMoveButBoomEnemy" $ do
            let enemies = generateFormation 0 3 0 (topYScreenBound + 100) 150 50
                isNoMoveButBoom (Enemy _ _ _ _ NoMoveButBoomEnemy) = True
                isNoMoveButBoom _ = False
            all isNoMoveButBoom enemies `shouldBe` True
        it "all enemies of type 1 are LeftRightShootEnemy" $ do
            let enemies = generateFormation 1 3 0 (topYScreenBound + 100) 150 50
                isLeftRightShoot (Enemy _ _ _ _ (LeftRightShootEnemy _ _ _)) = True
                isLeftRightShoot _ = False
            all isLeftRightShoot enemies `shouldBe` True
        it "all enemies of type 2 are LoopEnemy" $ do
            let enemies = generateFormation 2 3 0 (topYScreenBound + 100) 150 50
                isLoop (Enemy _ _ _ _ (LoopEnemy _ _ _ _ _ _ _ _ _)) = True
                isLoop _ = False
            all isLoop enemies `shouldBe` True

generateFormationQuickCheckSpec :: Spec
generateFormationQuickCheckSpec = do
    describe "generateFormation (QuickCheck)" $ do
        it "satisfies generateFormation post-condition for all valid parameters" $
            property (
                forAll (choose (0, (nbEnemiesAssets-1))) $ \enemyType ->
                forAll (choose (1, 5)) $ \nbEnemies ->
                forAll (choose (leftXScreenBound + 200, rightXScreenBound - 200)) $ \centerX ->
                forAll (choose (topYScreenBound + 50, topYScreenBound + 300)) $ \y ->
                forAll (choose (leftXScreenBound + 100, rightXScreenBound - 100)) $ \xTarget ->
                forAll (choose (bottomYScreenWithBarBound + 50, topYScreenBound - 50)) $ \yTarget ->
                    prop_pre_generateFormation enemyType nbEnemies centerX y xTarget yTarget
                    ==> prop_post_generateFormation enemyType nbEnemies centerX y xTarget yTarget
            )

getTranslatedEnemyAssetQuickCheckSpec :: Spec
getTranslatedEnemyAssetQuickCheckSpec = do
    describe "getTranslatedEnemyAsset (QuickCheck)" $ do
        it "satisfies getTranslatedEnemyAsset post-condition for all valid parameters" $
            property (\(TestGameAssets ga) (TestEnemy enemy) ->
                prop_inv_enemy enemy ==> prop_post_getTranslatedEnemyAsset ga enemy
            )

takeDamageMaybeEnemySpec :: Spec
takeDamageMaybeEnemySpec = do
    describe "takeDamageMaybe (unit tests)" $ do
        it "reduces health when enemy survives" $ do
            let e@(Enemy _ health _ _ _) = startInitNoMoveButBoomEnemy 0 0
                dmgTaken = (health-1)
            case takeDamageMaybe dmgTaken e of
                Just (Enemy _ _ _ _ _) -> True `shouldBe` True
                Nothing -> expectationFailure "Enemy should survive"
        it "returns Nothing when health reaches zero" $ do
            let e@(Enemy _ health _ _ _) = startInitNoMoveButBoomEnemy 0 0
                dmgTaken = health
            takeDamageMaybe dmgTaken e `shouldBe` Nothing
        it "returns Nothing when health becomes negative" $ do
            let e@(Enemy _ health _ _ _) = startInitNoMoveButBoomEnemy 0 0
                dmgTaken = (health+1)
            takeDamageMaybe dmgTaken e `shouldBe` Nothing

takeDamageMaybeEnemyQuickCheckSpec :: Spec
takeDamageMaybeEnemyQuickCheckSpec = do
    describe "takeDamageMaybe (QuickCheck)" $ do
        it "satisfies takeDamageMaybe post-condition for all valid parameters" $
            property (\(TestEnemy e) ->
                forAll (choose (-30, 200)) $ \damage ->
                prop_inv_enemy e
                ==> let e' = takeDamageMaybe damage e
                    in (prop_post_takeDamageMaybeEnemy damage e) &&
                        case e' of
                            Just e'' -> prop_inv_enemy e''
                            Nothing -> True
            )

-- ============================================================
-- =================== ENEMY SCRIPT ===========================
-- ============================================================

genNoMoveButBoomEnemyScript :: Gen TestEnemyScript
genNoMoveButBoomEnemyScript = do
    return $ TestEnemyScript NoMoveButBoomEnemy

genLeftRightShootEnemyScript :: Gen TestEnemyScript
genLeftRightShootEnemyScript = do
    xTarget <- choose (leftXScreenBound + 100, rightXScreenBound - 100)
    yTarget <- choose (bottomYScreenWithBarBound + 50, topYScreenBound - 50)
    shootD <- choose (1, leftRightShootEnemyShootDelay)
    return $ TestEnemyScript (LeftRightShootEnemy xTarget yTarget shootD)

genLoopEnemyScript :: Gen TestEnemyScript
genLoopEnemyScript = do
    yTarget <- choose (bottomYScreenWithBarBound + 50, topYScreenBound - 50)
    blSteps <- choose (0, defaultNbSteps)
    lSteps <- choose (0, defaultNbSteps)
    tlSteps <- choose (0, defaultNbSteps)
    tSteps <- choose (0, defaultNbSteps)
    trSteps <- choose (0, defaultNbSteps)
    rSteps <- choose (0, defaultNbSteps)
    brSteps <- choose (0, defaultNbSteps)
    goBottom <- arbitrary
    return $ TestEnemyScript (LoopEnemy yTarget blSteps lSteps tlSteps tSteps trSteps rSteps brSteps goBottom)

-- Initializes Enemy scripts veryfing their invariant
newtype TestEnemyScript = TestEnemyScript { getEnemyScript :: EnemyScript } deriving (Eq, Show)
instance Arbitrary TestEnemyScript where
    arbitrary :: Gen TestEnemyScript
    arbitrary = oneof [
            genNoMoveButBoomEnemyScript,
            genLeftRightShootEnemyScript,
            genLoopEnemyScript
        ]

prop_initNoMoveButBoomEnemyScript_preservesInvariant :: Property
prop_initNoMoveButBoomEnemyScript_preservesInvariant =
    property $ prop_inv_enemyScript initNoMoveButBoomEnemyScript

prop_initLeftRightShootEnemyScript_preservesInvariant :: Property
prop_initLeftRightShootEnemyScript_preservesInvariant =
    forAll (choose (leftXScreenBound + 100, rightXScreenBound - 100)) $ \xTarget ->
    forAll (choose (bottomYScreenWithBarBound + 50, topYScreenBound - 50)) $ \yTarget ->
    forAll (choose (1, leftRightShootEnemyShootDelay)) $ \shootD ->
        prop_inv_enemyScript (initLeftRightShootEnemyScript xTarget yTarget shootD)

prop_initLoopEnemyScript_preservesInvariant :: Property
prop_initLoopEnemyScript_preservesInvariant =
    forAll (choose (bottomYScreenWithBarBound + 50, topYScreenBound - 50)) $ \yTarget ->
    forAll (choose (0, defaultNbSteps)) $ \blSteps ->
    forAll (choose (0, defaultNbSteps)) $ \lSteps ->
    forAll (choose (0, defaultNbSteps)) $ \tlSteps ->
    forAll (choose (0, defaultNbSteps)) $ \tSteps ->
    forAll (choose (0, defaultNbSteps)) $ \trSteps ->
    forAll (choose (0, defaultNbSteps)) $ \rSteps ->
    forAll (choose (0, defaultNbSteps)) $ \brSteps ->
    forAll arbitrary $ \goBottom ->
        prop_inv_enemyScript (initLoopEnemyScript yTarget blSteps lSteps tlSteps tSteps trSteps rSteps brSteps goBottom)

prop_startInitNoMoveButBoomEnemyScript_preservesInvariant :: Property
prop_startInitNoMoveButBoomEnemyScript_preservesInvariant =
    property $ prop_inv_enemyScript startInitNoMoveButBoomEnemyScript

prop_startInitLeftRightShootEnemyScript_preservesInvariant :: Property
prop_startInitLeftRightShootEnemyScript_preservesInvariant =
    forAll (choose (leftXScreenBound + 100, rightXScreenBound - 100)) $ \xTarget ->
    forAll (choose (bottomYScreenWithBarBound + 50, topYScreenBound - 50)) $ \yTarget ->
        prop_inv_enemyScript (startInitLeftRightShootEnemyScript xTarget yTarget)

prop_startInitLoopEnemyScript_preservesInvariant :: Property
prop_startInitLoopEnemyScript_preservesInvariant =
    forAll (choose (bottomYScreenWithBarBound + 50, topYScreenBound - 50)) $ \yTarget ->
        prop_inv_enemyScript (startInitLoopEnemyScript yTarget)

initEnemyScriptSpec :: SpecWith ()
initEnemyScriptSpec = do
    describe "initEnemyScript (QuickCheck)" $ do
        it "preserves the EnemyScript invariant for NoMoveButBoomEnemy" $
            property prop_initNoMoveButBoomEnemyScript_preservesInvariant
        it "preserves the EnemyScript invariant for LeftRightShootEnemy" $
            property prop_initLeftRightShootEnemyScript_preservesInvariant
        it "preserves the EnemyScript invariant for LoopEnemy" $
            property prop_initLoopEnemyScript_preservesInvariant

startInitEnemyScriptSpec :: SpecWith ()
startInitEnemyScriptSpec = do
    describe "startInitEnemyScript (QuickCheck)" $ do
        it "preserves the EnemyScript invariant for start NoMoveButBoomEnemy" $
            property prop_startInitNoMoveButBoomEnemyScript_preservesInvariant
        it "preserves the EnemyScript invariant for start LeftRightShootEnemy" $
            property prop_startInitLeftRightShootEnemyScript_preservesInvariant
        it "preserves the EnemyScript invariant for start LoopEnemy" $
            property prop_startInitLoopEnemyScript_preservesInvariant

-- ============================================================
-- ======================== LAWS ==============================
-- ============================================================

invariantLawsSpec :: Spec
invariantLawsSpec = do
    describe "Invariant laws (QuickCheck)" $ do
        it "law_invariant_stable for Enemy" $
            property (
                \(TestEnemy enemy) -> law_invariant_stable enemy
            )
        it "law_invariant_idempotent for Enemy" $
            property (
                \(TestEnemy enemy) -> law_invariant_idempotent enemy
            )
        it "law_invariant_stable for EnemyScript" $
            property (
                \(TestEnemyScript script) -> law_invariant_stable script
            )
        it "law_invariant_idempotent for EnemyScript" $
            property (
                \(TestEnemyScript script) -> law_invariant_idempotent script
            )

renderableLawSpec :: Spec
renderableLawSpec = do
    describe "Renderable laws (QuickCheck)" $ do
        it "law_renderable_finite for Enemy" $
            property (\(TestGameAssets ga) (TestEnemy enemy) ->
                law_renderable_finite ga enemy
            )

movableLawsSpec :: Spec
movableLawsSpec = do
    describe "Movable laws (QuickCheck)" $ do
        it "law_movable_preserves_invariant for Enemy" $
            property (\(TestEnemy e) ->
                forAll (choose (minSS, maxSS)) $ \ss ->
                    prop_inv_enemy e && prop_pre_moveEnemy e ss
                    ==> let e' = move e ss in prop_inv_enemy e'
            )

collidableLawsSpec :: Spec
collidableLawsSpec = do
    describe "Collidable laws (QuickCheck)" $ do
        it "law_collidable_reflexive for Enemy" $
            property (\(TestEnemy e) ->
                prop_inv_enemy e 
                ==> law_collidable_reflexive e
            )
        it "law_collidable_symmetric for Enemy with another Enemy" $
            property (\(TestEnemy e1) (TestEnemy e2) ->
                prop_inv_enemy e1 && prop_inv_enemy e2 
                ==> law_collidable_symmetric e1 e2
            )
        it "law_collidable_symmetric for Enemy with another Object" $
            property (\(TestEnemy e) (TestObject o) ->
                prop_inv_enemy e && prop_inv_object o 
                ==> law_collidable_symmetric e o
            )
        it "law_collidable_will_collide for Enemy with another Enemy" $
            property (\(TestEnemy e1) (TestEnemy e2) ->
                prop_inv_enemy e1 && prop_inv_enemy e2 
                ==> law_collidable_will_collide e1 e2
            )
        it "law_collidable_will_collide for Enemy with another Object" $
            property (\(TestEnemy e) (TestObject o) ->
                prop_inv_enemy e && prop_inv_object o ==>
                law_collidable_will_collide e o
            )

destroyableLawsSpec :: Spec
destroyableLawsSpec = do
    describe "Destroyable laws (QuickCheck)" $ do
        it "law_destroyable_damage_cumulative for Enemy" $
            property (\(TestEnemy e) ->
                forAll (choose (1, 50)) $ \d1 ->
                forAll (choose (1, 50)) $ \d2 ->
                    prop_inv_enemy e 
                    ==> law_destroyable_damage_cumulative d1 d2 e
            )
        it "law_destroyable_zero_damage_identity for Enemy" $
            property (\(TestEnemy e) ->
                prop_inv_enemy e 
                ==> law_destroyable_zero_damage_identity e
            )
        it "law_destroyable_zero_damage_identity for Enemy" $
            property (\(TestEnemy e) ->
                prop_inv_enemy e 
                ==> law_destroyable_zero_damage_identity e
            )
        it "law_destroyable_no_heal_negative_damage for Enemy" $
            property (\(TestEnemy e) ->
                forAll (choose (-100, -1)) $ \damage ->
                    prop_inv_enemy e ==>
                    law_destroyable_no_heal_negative_damage damage e
            )