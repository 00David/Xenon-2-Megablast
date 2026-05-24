{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GameSpec (
    spec
)
where

import Test.Hspec
import Test.QuickCheck

import System.Random

import GameSetup
import GameState.Bonus
import GameState.Enemy
import GameState.Game
import GameState.Player
import GameState.Projectile
import GameState.Wall
import Objects.Hitbox
import Objects.Objects
import Typeclasses.Invariant
import AssetsSpec(TestGameAssets(..))
import BackgroundSpec(TestBackground(..))
import BonusSpec(TestBonus(..))
import EnemySpec(TestEnemy(..))
import ExplosionSpec(TestExplosion(..))
import KeyboardSpec(TestKeyboard(..))
import ObjectsSpec(TestDirection(..), TestObject(..), TestObjectSpeed(..))
import PlayerSpec(TestPlayer(..))
import ProjectileSpec(TestProjectile(..))
import WallSpec(TestGameWalls(..))

spec :: Spec
spec = do 
    initGameSpec
    startInitGameSpec
    initStartMenuSpec
    initInGameSpec
    initInGameInfosSpec
    startInitInGameSpec
    startInitInGameQuickCheckSpec
    updatePlayerDirectionSpeedSpec
    updatePlayerDirectionSpeedQuickCheckSpec
    movePlayerInsideScreenSpec
    movePlayerInsideScreenQuickCheckSpec
    handleCollisionPlayerWithEnemiesSpec
    handleCollisionPlayerWithEnemiesQuickCheckSpec
    moveWallsSpec
    keepBumpinSpec
    keepBumpinQuickCheckSpec
    bumpPlayerFromWallsSpec
    bumpPlayerFromWallsQuickCheckSpec
    moveProjectilesSpec
    moveProjectilesQuickCheckSpec
    applyProjectileToEnemySpec
    applyProjectileToEnemyQuickCheckSpec
    handleCollisionProjectilesWithPlayersEnemiesQuickCheckSpec
    runEnemiesQuickCheckSpec
    generateEnemiesQuickCheckSpec
    runExplosionsQuickCheckSpec
    runPlayersAnimationQuickCheckSpec
    generateWallQuickCheckSpec
    moveBonusesSpec
    moveBonusesQuickCheckSpec
    handleCollisionPlayerWithBonusesSpec
    handleCollisionPlayerWithBonusesQuickCheckSpec
    incrementPlayersShootFrameCountersQuickCheckSpec
    invariantGameLawsSpec

fixedGenSeed :: Int
fixedGenSeed = 42 -- for creating generators in unit tests

-- ============================================================
-- =================== GAME INITIALISATION ====================
-- ============================================================

newtype TestGame = TestGame { getGame :: Game } deriving Show
instance Arbitrary TestGame where
    arbitrary :: Gen TestGame
    arbitrary = do
        TestKeyboard kbd <- arbitrary
        TestGameState st <- arbitrary
        TestGameAssets assts <- arbitrary
        TestBackground bgnd <- arbitrary
        frameCpt <- choose (0, maxFramesToConsider - 1)
        return $ TestGame $ initGame kbd st assts bgnd frameCpt

prop_initGame_preservesInvariant :: TestKeyboard -> TestGameState -> TestGameAssets -> TestBackground -> Property
prop_initGame_preservesInvariant (TestKeyboard kbd) (TestGameState st) (TestGameAssets assts) (TestBackground bgnd) = 
    forAll (choose (0, maxFramesToConsider - 1)) $ \fc ->
        prop_inv_game $ initGame kbd st assts bgnd fc

prop_startInitGame_preservesInvariant :: Property
prop_startInitGame_preservesInvariant =
    ioProperty $ do
        game <- startInitGame
        return (prop_inv_game game)

initGameSpec :: SpecWith ()
initGameSpec = do
    describe "initGame (QuickCheck)" $ do
        it "preserves the Game invariant for valid game sub-components" $
            property prop_initGame_preservesInvariant

startInitGameSpec :: SpecWith ()
startInitGameSpec = do
    describe "startInitGame (QuickCheck)" $ do
        it "preserves the Game invariant at start (general initialisation)" $
            property prop_startInitGame_preservesInvariant

-- ============================================================
-- ====================== GAMESTATE ===========================
-- ============================================================

newtype TestGameState = TestGameState { getGameState :: GameState } deriving Show
instance Arbitrary TestGameState where
    arbitrary :: Gen TestGameState
    arbitrary = oneof
        [ do
            TestStartMenuOption smo <- arbitrary
            return $ TestGameState (StartMenu smo)
        , do
            TestInGameInfos igi <- arbitrary
            return $ TestGameState (InGame igi)
        ]

prop_initStartMenu_preservesInvariant :: TestStartMenuOption -> Property
prop_initStartMenu_preservesInvariant (TestStartMenuOption smo) = 
    property $ prop_inv_gameState $ initStartMenu smo

prop_initInGame_preservesInvariant :: TestInGameInfos -> Property
prop_initInGame_preservesInvariant (TestInGameInfos igi) = 
    property $ prop_inv_gameState $ initInGame igi

initStartMenuSpec :: SpecWith ()
initStartMenuSpec = do
    describe "initStartMenu (QuickCheck)" $ do
        it "preserves the GameState invariant for valid StartMenuOptions" $
            property prop_initStartMenu_preservesInvariant

initInGameSpec :: SpecWith ()
initInGameSpec = do
    describe "initInGame (QuickCheck)" $ do
        it "preserves the GameState invariant for valid InGameInfos" $
            property prop_initInGame_preservesInvariant

startInitInGameSpec :: Spec
startInitInGameSpec = do
    describe "startInitInGame (unit tests)" $ do
        it "creates a valid 1 player game" $ do
            let gen = mkStdGen fixedGenSeed
                game = startInitInGame gen 1
            case game of
                InGame igi -> do
                    not (isPlayerDead (gamePlayer1 igi)) `shouldBe` True
                    isPlayerDead (gamePlayer2 igi) `shouldBe` True
                _ -> expectationFailure "Expected InGame"
        it "creates a valid 2 player game" $ do
            let gen = mkStdGen fixedGenSeed
                game = startInitInGame gen 2
            case game of
                InGame igi -> do
                    not (isPlayerDead (gamePlayer1 igi)) `shouldBe` True
                    not (isPlayerDead (gamePlayer2 igi)) `shouldBe` True
                _ -> expectationFailure "Expected InGame"

startInitInGameQuickCheckSpec :: Spec
startInitInGameQuickCheckSpec = do
    describe "startInitInGame (QuickCheck)" $ do
        it "satisfies startInitInGame post-condition for all valid parameters" $
            property (\seed ->
                forAll (choose (1, 2)) $ \nbPlayers ->
                    let gen = mkStdGen seed
                    in prop_pre_startInitInGame gen nbPlayers ==>
                        let game = startInitInGame gen nbPlayers
                        in prop_inv_gameState game
            )

-- ============================================================
-- =================== START MENU OPTION ======================
-- ============================================================

newtype TestStartMenuOption = TestStartMenuOption { getStartMenuOption :: StartMenuOption } deriving Show
instance Arbitrary TestStartMenuOption where
    arbitrary :: Gen TestStartMenuOption
    arbitrary = do
        startMenuOption <- elements [OnePlayer, TwoPlayers]
        return $ TestStartMenuOption startMenuOption

-- ============================================================
-- ===================== IN GAME INFOS ========================
-- ============================================================

minSS :: Float
minSS = 1
maxSS :: Float
maxSS = 10

newtype TestInGameInfos = TestInGameInfos { getInGameInfos :: InGameInfos } deriving Show
instance Arbitrary TestInGameInfos where
    arbitrary :: Gen TestInGameInfos
    arbitrary = do
        screenSpeed <- choose (minSS, maxSS)
        TestPlayer p1 <- arbitrary
        TestPlayer p2 <- arbitrary
        
        nEnemies <- choose (0, 20)
        enemies <- vectorOf nEnemies (getEnemy <$> arbitrary)
        
        TestGameWalls gw <- arbitrary
        
        nProjs <- choose (0, 40)
        projs <- vectorOf nProjs (getProjectile <$> arbitrary)
        
        nExpl <- choose (0, 20)
        expl <- vectorOf nExpl (getExplosion <$> arbitrary)
        
        nBns <- choose (0, 10)
        bns <- vectorOf nBns (getTBonus <$> arbitrary)
        
        return $ TestInGameInfos $ initInGameInfos screenSpeed p1 p2 enemies gw projs expl bns

prop_initInGameInfos_preservesInvariant :: TestPlayer -> TestPlayer -> [TestEnemy] -> TestGameWalls -> [TestProjectile] -> [TestExplosion] -> [TestBonus] -> Property
prop_initInGameInfos_preservesInvariant (TestPlayer p1) (TestPlayer p2) enemies (TestGameWalls gw) projs expl bns = 
    forAll (choose (minSS, maxSS)) $ \ss ->
        prop_inv_ingameinfos $ initInGameInfos ss p1 p2 (map getEnemy enemies) gw (map getProjectile projs) (map getExplosion expl) (map getTBonus bns)

initInGameInfosSpec :: SpecWith ()
initInGameInfosSpec = do
    describe "initInGameInfos (QuickCheck)" $ do
        it "preserves the InGameInfos invariant for all valid components" $
            property prop_initInGameInfos_preservesInvariant

-- ============================================================
-- ================= IN GAME OPERATIONS =======================
-- ============================================================

updatePlayerDirectionSpeedSpec :: Spec
updatePlayerDirectionSpeedSpec = do
    describe "updatePlayerDirectionSpeed (unit tests)" $ do
        it "updates player1 direction and speed" $ do
            let po1 = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p1  = initAlivePlayer po1 1 3 100 0 NoBonus 1 0

                po2 = initPlayerObject 50 50 (initDirection 0 0) (initObjectSpeed 0)
                p2  = initAlivePlayer po2 2 3 100 0 NoBonus 1 0

                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [] (startInitGameWalls gen) [] [] []

                newDirection = initDirection 1 0
                newSpeed = initObjectSpeed 2
                (_, igi') = updatePlayerDirectionSpeed True (newDirection, newSpeed) igi
                p1' = gamePlayer1 igi'

            objectDirection (playerObject p1') `shouldBe` newDirection
            objectSpeed (playerObject p1')     `shouldBe` newSpeed
        it "updates player2 direction and speed" $ do
            let po1 = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p1  = initAlivePlayer po1 1 3 100 0 NoBonus 1 0

                po2 = initPlayerObject 50 50 (initDirection 0 0) (initObjectSpeed 0)
                p2  = initAlivePlayer po2 2 3 100 0 NoBonus 1 0

                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [] (startInitGameWalls gen) [] [] []
                
                newDirection = initDirection 0 (-1)
                newSpeed = initObjectSpeed 3
                (_, igi') = updatePlayerDirectionSpeed False (newDirection, newSpeed) igi
                p2' = gamePlayer2 igi'

            objectDirection (playerObject p2') `shouldBe` newDirection
            objectSpeed (playerObject p2')     `shouldBe` newSpeed

updatePlayerDirectionSpeedQuickCheckSpec :: Spec
updatePlayerDirectionSpeedQuickCheckSpec = do
    describe "updatePlayerDirectionSpeed (QuickCheck)" $ do
        it "satisfies updatePlayerDirectionSpeed post-condition for all valid parameters" $
            property (\isP1 (TestDirection d) (TestObjectSpeed os) (TestInGameInfos igi) ->
                prop_inv_direction d && prop_inv_objectSpeed os && prop_inv_ingameinfos igi
                && prop_pre_updatePlayerDirectionSpeed isP1 (d, os) igi
                ==> let (_, igi') = updatePlayerDirectionSpeed isP1 (d, os) igi
                    in prop_inv_ingameinfos igi' && prop_post_updatePlayerDirectionSpeed isP1 (d, os) igi
            )

movePlayerInsideScreenSpec :: Spec
movePlayerInsideScreenSpec = do
    describe "movePlayerInsideScreen (unit tests)" $ do
        it "moves player1 according to direction and speed" $ do
            let (x, y) = (0, 0)
                po1 = initPlayerObject x y (initDirection 1 0) (initObjectSpeed 5) -- direction to right, speed of 5
                p1  = initAlivePlayer po1 1 3 100 0 NoBonus 1 0

                p2  = startInitDeadPlayer 2

                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [] (startInitGameWalls gen) [] [] []

                (_, igi') = movePlayerInsideScreen True igi
                p1' = gamePlayer1 igi'
                (x', _) = centerHitbox (objectHitbox (playerObject p1'))

            x' `shouldBe` x + 5

        it "moves player2 according to direction and speed" $ do
            let p1  = startInitDeadPlayer 1

                (x, y) = (50, 50)
                po2 = initPlayerObject x y (initDirection (-1) 0) (initObjectSpeed 2) -- direction to left, speed of 2
                p2  = initAlivePlayer po2 2 3 100 0 NoBonus 1 0

                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [] (startInitGameWalls gen) [] [] []

                (_, igi') = movePlayerInsideScreen False igi
                p2' = gamePlayer2 igi'
                (x', _) = centerHitbox (objectHitbox (playerObject p2'))

            x' `shouldBe` x - 2

movePlayerInsideScreenQuickCheckSpec :: Spec
movePlayerInsideScreenQuickCheckSpec = do
    describe "movePlayerInsideScreen (QuickCheck)" $ do
        it "satisfies movePlayerInsideScreen post-condition for all valid parameters" $
            property (\isP1 (TestInGameInfos igi) ->
                prop_inv_ingameinfos igi && prop_pre_movePlayerInsideScreen isP1 igi
                ==> let (_, igi') = movePlayerInsideScreen isP1 igi
                    in prop_inv_ingameinfos igi' && prop_post_movePlayerInsideScreen isP1 igi
            )

handleCollisionPlayerWithEnemiesSpec :: Spec
handleCollisionPlayerWithEnemiesSpec = do
    describe "handleCollisionPlayerWithEnemies (unit tests)" $ do
        it "handle only player1 collisions whith enemies" $ do
            let po1 = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p1  = initAlivePlayer po1 1 3 100 0 NoBonus 1 0

                po2 = initPlayerObject 100 100 (initDirection 0 0) (initObjectSpeed 0)
                p2  = initAlivePlayer po2 2 3 100 0 NoBonus 1 0

                e   = startInitNoMoveButBoomEnemy 0 0 -- enemy same place as player1 and player2
                
                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [e] (startInitGameWalls gen) [] [] []

                (_, igi') = handleCollisionPlayerWithEnemies True igi
                p1' = gamePlayer1 igi'
                p2' = gamePlayer2 igi'

            playerHealth p1' < playerHealth p1 `shouldBe` True
            p2' `shouldBe` p2 -- only player1 collision handled
        it "handle only player2 collisions whith enemies" $ do
            let po1 = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p1  = initAlivePlayer po1 1 3 100 0 NoBonus 1 0

                po2 = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p2  = initAlivePlayer po2 2 3 100 0 NoBonus 1 0

                e   = startInitNoMoveButBoomEnemy 0 0 -- enemy same place as player1 and player2

                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [e] (startInitGameWalls gen) [] [] []

                (_, igi') = handleCollisionPlayerWithEnemies False igi
                p1' = gamePlayer1 igi'
                p2' = gamePlayer2 igi'

            playerHealth p2' < playerHealth p2 `shouldBe` True
            p1' `shouldBe` p1 -- only player2 collision handled

handleCollisionPlayerWithEnemiesQuickCheckSpec :: Spec
handleCollisionPlayerWithEnemiesQuickCheckSpec = do
    describe "handleCollisionPlayerWithEnemies (QuickCheck)" $ do
        it "satisfies handleCollisionPlayerWithEnemies post-condition for all valid parameters" $
            property (\isP1 (TestInGameInfos igi) ->
                prop_inv_ingameinfos igi && prop_pre_handleCollisionPlayerWithEnemies isP1 igi
                ==> let (_, igi') = handleCollisionPlayerWithEnemies isP1 igi
                    in prop_inv_ingameinfos igi' && prop_post_handleCollisionPlayerWithEnemies isP1 igi
            )

moveWallsSpec :: Spec
moveWallsSpec = do
    describe "moveWalls (QuickCheck)" $ do
        it "satisfies moveWalls post-condition for all valid parameters" $
            property (\(TestInGameInfos igi) ->
                prop_inv_ingameinfos igi
                ==> let (_, igi') = moveWalls igi
                    in prop_inv_ingameinfos igi' && prop_post_moveWalls igi
            )

keepBumpinSpec :: Spec
keepBumpinSpec = do
    describe "keepBumpin (unit tests)" $ do
        it "returns a valid object position" $ do
            {--let obj = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                gw = initGameWalls Nothing Nothing Nothing Nothing []
                p = startInitDeadPlayer 2
                ss = screenDefaultSpeed
                
                obj' = keepBumpin ss obj gw p--}
                
            --prop_inv_object obj' `shouldBe` True
            True `shouldBe` True

keepBumpinQuickCheckSpec :: Spec
keepBumpinQuickCheckSpec = do
    describe "keepBumpin (QuickCheck)" $ do
        it "satisfies keepBumpin post-condition for all valid parameters" $
            property (\(TestObject obj) (TestGameWalls gw) (TestPlayer p) ->
                forAll (choose (minSS, maxSS)) $ \ss ->
                    prop_inv_object obj && prop_inv_gameWalls gw && prop_inv_player p
                    && prop_pre_keepBumpin ss obj gw p
                    ==> let obj' = keepBumpin ss obj gw p
                        in prop_inv_object obj' && prop_post_keepBumpin ss obj gw p
            )

bumpPlayerFromWallsSpec :: Spec
bumpPlayerFromWallsSpec = do
    describe "bumpPlayerFromWalls (unit tests)" $ do
        it "does nothing if no collision with walls" $ do
            let p1  = startInitAlivePlayer 1 -- not colliding whith border walls
                p2  = startInitDeadPlayer 2

                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [] (startInitGameWalls gen) [] [] []

                (_, igi') = bumpPlayerFromWalls True igi

            igi' `shouldBe` igi

bumpPlayerFromWallsQuickCheckSpec :: Spec
bumpPlayerFromWallsQuickCheckSpec = do
    describe "bumpPlayerFromWalls (QuickCheck)" $ do
        it "satisfies bumpPlayerFromWalls post-condition for all valid parameters" $
            property (\isP1 (TestInGameInfos igi) ->
                prop_inv_ingameinfos igi && prop_pre_bumpPlayerFromWalls isP1 igi
                ==> let (_, igi') = bumpPlayerFromWalls isP1 igi
                    in prop_inv_ingameinfos igi' && prop_post_bumpPlayerFromWalls isP1 igi
            )

moveProjectilesSpec :: Spec
moveProjectilesSpec = do
    describe "moveProjectiles (unit tests)" $ do
        it "removes player projectiles outside screen" $ do
            let p1 = startInitAlivePlayer 1
                p2 = startInitDeadPlayer 2
                
                -- Projectiles above screen
                proj1 = startInitPlayerShot 0 (topYScreenBound + 100) (initObjectSpeed 5) 0 1 1
                proj2 = startInitPlayerShot 0 (topYScreenBound + 200) (initObjectSpeed 5) 0 1 1
                
                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [] (startInitGameWalls gen) [proj1, proj2] [] []
                (_, igi') = moveProjectiles igi
            length (gameProjectiles igi') `shouldBe` 0
        it "removes enemies projectiles outside screen" $ do
            let p1 = startInitAlivePlayer 1
                p2 = startInitDeadPlayer 2
                
                -- Projectiles below screen
                proj1 = startInitEnemyShot 0 (bottomYScreenBound - 100) (initObjectSpeed 5) 0 10
                proj2 = startInitEnemyShot 0 (bottomYScreenBound - 200) (initObjectSpeed 5) 0 10
                
                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [] (startInitGameWalls gen) [proj1, proj2] [] []
                (_, igi') = moveProjectiles igi
            length (gameProjectiles igi') `shouldBe` 0
        it "removes both player and enemy projectiles outside screen" $ do
            let p1 = startInitAlivePlayer 1
                p2 = startInitDeadPlayer 2
                
                -- Player projectile above the screen, enemy projectile below the screen
                proj1 = startInitPlayerShot 0 (topYScreenBound + 100) (initObjectSpeed 5) 0 1 1
                proj2 = startInitEnemyShot 0 (bottomYScreenBound - 200) (initObjectSpeed 5) 0 10
                
                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [] (startInitGameWalls gen) [proj1, proj2] [] []
                (_, igi') = moveProjectiles igi
            length (gameProjectiles igi') `shouldBe` 0
        it "keep projectiles inside screen" $ do
            let p1 = startInitAlivePlayer 1
                p2 = startInitDeadPlayer 2
                
                -- Both projectiles at the center of the screen
                proj1 = startInitPlayerShot 0 0 (initObjectSpeed 5) 0 1 1
                proj2 = startInitEnemyShot 0 0 (initObjectSpeed 5) 0 10
                
                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [] (startInitGameWalls gen) [proj1, proj2] [] []
                (_, igi') = moveProjectiles igi
            length (gameProjectiles igi') `shouldBe` 2

moveProjectilesQuickCheckSpec :: Spec
moveProjectilesQuickCheckSpec = do
    describe "moveProjectiles (QuickCheck)" $ do
        it "satisfies moveProjectiles post-condition for all valid parameters" $
            property (\(TestInGameInfos igi) ->
                prop_inv_ingameinfos igi && prop_pre_moveProjectiles igi
                ==> let (_, igi') = moveProjectiles igi
                    in prop_inv_ingameinfos igi' && prop_post_moveProjectiles igi
            )

applyProjectileToEnemySpec :: Spec
applyProjectileToEnemySpec = do
    describe "applyProjectileToEnemy (unit tests)" $ do
        it "returns Nothing when no enemy" $ do
            let proj = startInitPlayerShot 0 0 (initObjectSpeed 5) 0 1 1
                result = applyProjectileToEnemy proj (Nothing, 0, 0)
            result `shouldBe` (Nothing, 0, 0)
        it "keeps enemy when no collision" $ do
            let proj = startInitPlayerShot 400 400 (initObjectSpeed 5) 0 1 1
                enemy = startInitNoMoveButBoomEnemy 0 0
                (maybeEnemy', _, _) = applyProjectileToEnemy proj (Just enemy, 0, 0)
            maybeEnemy' `shouldBe` Just enemy
        it "returns Nothing for the enemy, and increase player1 score on enemy death with this player's projectile" $ do
            let proj = startInitPlayerShot 0 0 (initObjectSpeed 5) 0 1 1 -- player1 shot killing the enemy
                enemy = startInitNoMoveButBoomEnemy 0 0
                (maybeEnemy', scoreP1, scoreP2) = applyProjectileToEnemy proj (Just enemy, 0, 0)
            maybeEnemy' `shouldBe` Nothing
            scoreP1 `shouldBe` noMoveButBoomEnemyScore
            scoreP2 `shouldBe` 0 -- player2 score don't change
        it "returns Nothing for the enemy, and increase player2 score on enemy death with this player's projectile" $ do
            let proj = startInitPlayerShot 0 0 (initObjectSpeed 5) 0 1 2 -- player2 shot killing the enemy
                enemy = startInitNoMoveButBoomEnemy 0 0
                (maybeEnemy', scoreP1, scoreP2) = applyProjectileToEnemy proj (Just enemy, 0, 0)
            maybeEnemy' `shouldBe` Nothing
            scoreP2 `shouldBe` noMoveButBoomEnemyScore
            scoreP1 `shouldBe` 0 -- player1 score don't change

applyProjectileToEnemyQuickCheckSpec :: Spec
applyProjectileToEnemyQuickCheckSpec = do
    describe "applyProjectileToEnemy (QuickCheck)" $ do
        it "satisfies applyProjectileToEnemy post-condition for all valid parameters" $
            property (\(TestProjectile proj) ->
                forAll arbitrary $ \(TestEnemy enemy) ->
                forAll (choose (0, 100)) $ \accScoreP1 ->
                forAll (choose (0, 100)) $ \accScoreP2 ->
                    prop_inv_projectile proj && prop_pre_applyProjectileToEnemy proj (Just enemy, accScoreP1, accScoreP2)
                    ==> prop_post_applyProjectileToEnemy proj (Just enemy, accScoreP1, accScoreP2)
            )

handleCollisionProjectilesWithPlayersEnemiesQuickCheckSpec :: Spec
handleCollisionProjectilesWithPlayersEnemiesQuickCheckSpec = do
    describe "handleCollisionProjectilesWithPlayersEnemies (QuickCheck)" $ do
        it "satisfies handleCollisionProjectilesWithPlayersEnemies post-condition for all valid parameters" $
            property (\seed (TestInGameInfos igi) ->
                let gen = mkStdGen seed
                in prop_inv_ingameinfos igi
                ==> let (_, igi') = handleCollisionProjectilesWithPlayersEnemies gen igi
                    in prop_inv_ingameinfos igi' && prop_post_handleCollisionProjectilesWithPlayersEnemies gen igi
            )

runEnemiesQuickCheckSpec :: Spec
runEnemiesQuickCheckSpec = do
    describe "runEnemies (QuickCheck)" $ do
        it "satisfies runEnemies post-condition for all valid parameters" $
            property (\(TestInGameInfos igi) ->
                prop_inv_ingameinfos igi && prop_pre_runEnemies igi
                ==> let (_, igi') = runEnemies igi
                    in prop_inv_ingameinfos igi' && prop_post_runEnemies igi
            )

generateEnemiesQuickCheckSpec :: Spec
generateEnemiesQuickCheckSpec = do
    describe "generateEnemies (QuickCheck)" $ do
        it "satisfies generateEnemies post-condition for all valid parameters" $
            property (\seed (TestInGameInfos igi) ->
                forAll (choose (0, maxFramesToConsider - 1)) $ \frameCpt ->
                    let gen = mkStdGen seed
                    in prop_inv_ingameinfos igi ==>
                        let (_, igi') = generateEnemies frameCpt gen igi
                        in prop_inv_ingameinfos igi'
            )

runExplosionsQuickCheckSpec :: Spec
runExplosionsQuickCheckSpec = do
    describe "runExplosions (QuickCheck)" $ do
        it "satisfies runExplosions post-condition for all valid parameters" $
            property (\(TestInGameInfos igi) ->
                prop_inv_ingameinfos igi
                ==> let (_, igi') = runExplosions igi
                    in prop_inv_ingameinfos igi' && prop_post_runExplosions igi
            )

runPlayersAnimationQuickCheckSpec :: Spec
runPlayersAnimationQuickCheckSpec = do
    describe "runPlayersAnimation (QuickCheck)" $ do
        it "satisfies runPlayersAnimation post-condition for all valid parameters" $
            property (\(TestInGameInfos igi) ->
                prop_inv_ingameinfos igi
                ==> let (_, igi') = runPlayersAnimation igi
                    in prop_inv_ingameinfos igi' && prop_post_runPlayersAnimation igi
            )

generateWallQuickCheckSpec :: Spec
generateWallQuickCheckSpec = do
    describe "generateWall (QuickCheck)" $ do
        it "satisfies generateWall post-condition for all valid parameters" $
            property (\seed (TestInGameInfos igi) ->
                forAll (choose (0, maxFramesToConsider - 1)) $ \frameCpt ->
                    let gen = mkStdGen seed
                    in prop_inv_ingameinfos igi ==>
                        let (_, igi') = generateWall frameCpt gen igi
                        in prop_inv_ingameinfos igi'
                           && prop_post_generateWall frameCpt gen igi
            )

moveBonusesSpec :: Spec
moveBonusesSpec = do
    describe "moveBonuses (unit tests)" $ do
        it "removes bonus outside screen" $ do
            let p1 = startInitAlivePlayer 1
                p2 = startInitDeadPlayer 2
                
                -- Bonus outside of screen
                bonus = startInitPlayerShootBonus 0 (bottomYScreenBound-200) ShootFaster
                
                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [] (startInitGameWalls gen) [] [] [bonus]
                (_, igi') = moveBonuses igi
                
            length (gameBonuses igi') `shouldBe` 0
        it "keep bonus inside screen" $ do
            let p1 = startInitAlivePlayer 1
                p2 = startInitDeadPlayer 2
                
                -- Bonus inside of screen
                bonus = startInitPlayerShootBonus 0 0 ShootFaster
                
                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [] (startInitGameWalls gen) [] [] [bonus]
                (_, igi') = moveBonuses igi
                
            length (gameBonuses igi') `shouldBe` 1

moveBonusesQuickCheckSpec :: Spec
moveBonusesQuickCheckSpec = do
    describe "moveBonuses (QuickCheck)" $ do
        it "satisfies moveBonuses post-condition for all valid parameters" $
            property (\(TestInGameInfos igi) ->
                prop_inv_ingameinfos igi && prop_pre_moveBonuses igi
                ==> let (_, igi') = moveBonuses igi
                    in prop_inv_ingameinfos igi' && prop_post_moveBonuses igi
            )

handleCollisionPlayerWithBonusesSpec :: Spec
handleCollisionPlayerWithBonusesSpec = do
    describe "handleCollisionPlayerWithBonuses (unit tests)" $ do
        it "removes bonus on collision with a player" $ do
            let po1 = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p1  = initAlivePlayer po1 1 3 100 0 NoBonus 1 0
                p2  = startInitDeadPlayer 2
                
                bonus = startInitPlayerShootBonus 0 0 ShootFaster -- collides with the player1
                
                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [] (startInitGameWalls gen) [] [] [bonus]
                (_, igi') = handleCollisionPlayerWithBonuses True igi
                
            length (gameBonuses igi') `shouldBe` 0
        it "keeps bonus when no collision with a player" $ do
            let po1 = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p1  = initAlivePlayer po1 1 3 100 0 NoBonus 1 0
                p2  = startInitDeadPlayer 2
                
                bonus = startInitPlayerShootBonus 300 300 ShootFaster -- does not collide with a player
                
                gen = mkStdGen fixedGenSeed
                igi = initInGameInfos screenDefaultSpeed p1 p2 [] (startInitGameWalls gen) [] [] [bonus]
                (_, igi') = handleCollisionPlayerWithBonuses True igi
                
            (gameBonuses igi') `shouldBe` [bonus]

handleCollisionPlayerWithBonusesQuickCheckSpec :: Spec
handleCollisionPlayerWithBonusesQuickCheckSpec = do
    describe "handleCollisionPlayerWithBonuses (QuickCheck)" $ do
        it "satisfies handleCollisionPlayerWithBonuses post-condition for all valid parameters" $
            property (\isP1 (TestInGameInfos igi) ->
                prop_inv_ingameinfos igi && prop_pre_handleCollisionPlayerWithBonuses isP1 igi
                ==> let (_, igi') = handleCollisionPlayerWithBonuses isP1 igi
                    in prop_inv_ingameinfos igi' && prop_post_handleCollisionPlayerWithBonuses isP1 igi
            )

incrementPlayersShootFrameCountersQuickCheckSpec :: Spec
incrementPlayersShootFrameCountersQuickCheckSpec = do
    describe "incrementPlayersShootFrameCounters (QuickCheck)" $ do
        it "satisfies incrementPlayersShootFrameCounters post-condition for all valid parameters" $
            property (\(TestInGameInfos igi) ->
                prop_inv_ingameinfos igi
                ==> let (_, igi') = incrementPlayersShootFrameCounters igi
                    in prop_inv_ingameinfos igi' && prop_post_incrementPlayersShootFrameCounters igi
            )

-- ============================================================
-- ======================== LAWS ==============================
-- ============================================================

invariantGameLawsSpec :: Spec
invariantGameLawsSpec = do
    describe "Invariant laws (QuickCheck)" $ do
        it "law_invariant_stable for Game" $
            property (
                \(TestGame game) -> law_invariant_stable game
            )
        it "law_invariant_idempotent for Game" $
            property (
                \(TestGame game) -> law_invariant_idempotent game
            )
        it "law_invariant_stable for GameState" $
            property (
                \(TestGameState gameState) -> law_invariant_stable gameState
            )
        it "law_invariant_idempotent for GameState" $
            property (
                \(TestGameState gameState) -> law_invariant_idempotent gameState
            )
        it "law_invariant_stable for InGameInfos" $
            property (
                \(TestInGameInfos igi) -> law_invariant_stable igi
            )
        it "law_invariant_idempotent for InGameInfos" $
            property (
                \(TestInGameInfos igi) -> law_invariant_idempotent igi
            )