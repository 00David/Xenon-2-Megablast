module GameSpec (
    spec
)
where

import Test.Hspec
import Test.QuickCheck

import Graphics.Gloss (Picture (Blank))

import GameState.Game
import KeyboardSpec(TestKeyboard(..))
import AssetsSpec(TestGameAssets(..))
import BackgroundSpec(TestBackground(..))
import PlayerSpec(TestPlayer(..))
import EnemySpec(TestEnemy(..))
import ObjectsSpec(TestDirection(..), TestObjectSpeed(..))
import GameState.Enemy
import GameState.Player
import Objects.Objects
import Objects.Hitbox

spec :: Spec
spec = do 
    initGameSpec
    initStartGameSpec
    initStartMenuSpec
    initInGameSpec
    initInGameInfosSpec
    startInitInGameSpec
    startInitInGameQuickCheckSpec
    updatePlayerDirectionSpeedSpec
    updatePlayerDirectionSpeedQuickCheckSpec
    movePlayerSpec
    movePlayerQuickCheckSpec
    collisionPlayerWithEnemySpec
    collisionPlayerWithEnemyQuickCheckSpec
    keepAliveEnemiesSpec
    keepAliveEnemiesQuickCheckSpec
    handleCollisionPlayerWithEnemiesSpec
    handleCollisionPlayerWithEnemiesQuickCheckSpec

-- ============================================================
-- =================== GAME INITIALISATION ====================
-- ============================================================

newtype TestGame = TestGame { getGame :: Game } deriving Show
instance Arbitrary TestGame where
    arbitrary = do
        kbd <- arbitrary
        st <- arbitrary
        assts <- arbitrary
        bgnd <- arbitrary
        return $ TestGame $ Game (getKeyboard kbd) (getGameState st) (getGameAssets assts) (getBackground bgnd)

prop_initGame_preservesInvariant :: TestKeyboard -> TestGameState -> TestGameAssets -> TestBackground -> Property
prop_initGame_preservesInvariant kbd st assts bgnd = 
    property $ prop_inv_game $ initGame (getKeyboard kbd) (getGameState st) (getGameAssets assts) (getBackground bgnd)

prop_initStartGame_preservesInvariant :: Property
prop_initStartGame_preservesInvariant =
    ioProperty $ do
        game <- startInitGame
        return (prop_inv_game game)

initGameSpec :: SpecWith ()
initGameSpec = do
    describe "initGame (QuickCheck)" $ do
        it "preserves the Game invariant for valid game sub-components" $
            property prop_initGame_preservesInvariant

initStartGameSpec :: SpecWith ()
initStartGameSpec = do
    describe "initStartGame (QuickCheck)" $ do
        it "preserves the Game invariant at start (general initialisation)" $
            property prop_initStartGame_preservesInvariant

-- ============================================================
-- ====================== GAMESTATE ===========================
-- ============================================================

newtype TestGameState = TestGameState { getGameState :: GameState } deriving Show
instance Arbitrary TestGameState where
    arbitrary = oneof
        [do
            TestStartMenuOption smo <- arbitrary
            return $ TestGameState (StartMenu smo)
        ,do
            TestInGameInfos ig <- arbitrary
            return $ TestGameState (InGame ig)]

prop_initStartMenu_preservesInvariant :: TestStartMenuOption -> Property
prop_initStartMenu_preservesInvariant smo = property $ prop_inv_gameState $ initStartMenu (getStartMenuOption smo)

prop_initInGame_preservesInvariant :: TestInGameInfos -> Property
prop_initInGame_preservesInvariant igi = property $ prop_inv_gameState $ initInGame (getInGameInfos igi)

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
    it "creates a valid game when everything is in bounds" $ do
        let xP1 = 0; yP1 = 0
            xP2 = 100; yP2 = 0
            xV  = 200; yV  = 0
            game = startInitInGame Blank Blank xV yV xP1 yP1 xP2 yP2
        case game of
          StartMenu _ -> expectationFailure "Expected InGame"
          InGame _    -> return ()

    {--it "initializes 0 enemies at start" $ do
        let game = startInitInGame Blank Blank 200 0 0 0 100 0 0 0
        case game of
          InGame infos -> length (gameEnemies infos) `shouldBe` 0
          _ -> expectationFailure "Expected InGame"--}

    it "player1 position is preserved" $ do
        let xP1 = 10; yP1 = 20
            game = startInitInGame Blank Blank 200 0 xP1 yP1 100 0
        case game of
          InGame infos ->
            let p1 = gamePlayer1 infos
                po1 = playerObject p1
                ph1 = objectHitbox po1
            in centerHitbox ph1 `shouldBe` (xP1, yP1)
          _ -> expectationFailure "Expected InGame"

    it "player2 position is preserved" $ do
        let xP2 = -50; yP2 = 30
            game = startInitInGame Blank Blank 200 0 0 0 xP2 yP2
        case game of
          InGame infos ->
            let p2 = gamePlayer2 infos
                po2 = playerObject p2
                ph2 = objectHitbox po2
            in centerHitbox ph2 `shouldBe` (xP2, yP2)
          _ -> expectationFailure "Expected InGame"

startInitInGameQuickCheckSpec :: Spec
startInitInGameQuickCheckSpec = do
    describe "startInitInGame (QuickCheck)" $ do
        it "satisfies startInitInGame post-condition for all valid parameters" $
            property (\xV yV xP1 yP1 xP2 yP2 ->
                prop_pre_startInitInGame Blank Blank xV yV xP1 yP1 xP2 yP2 
                ==> let game = startInitInGame Blank Blank xV yV xP1 yP1 xP2 yP2
                    in prop_inv_gameState game
                )

-- ============================================================
-- =================== START MENU OPTION ======================
-- ============================================================

newtype TestStartMenuOption = TestStartMenuOption { getStartMenuOption :: StartMenuOption } deriving Show
instance Arbitrary TestStartMenuOption where
    arbitrary = do
        startMenuOption <- elements [Start, Option2]
        return $ TestStartMenuOption startMenuOption

-- ============================================================
-- ===================== IN GAME INFOS ========================
-- ============================================================

newtype TestInGameInfos = TestInGameInfos { getInGameInfos :: InGameInfos } deriving Show
instance Arbitrary TestInGameInfos where
    arbitrary = do
        p1 <- arbitrary
        p2 <- arbitrary
        n_enemies <- choose (0, 10) -- between 0 and 10 in game enemies
        enemies <- vectorOf n_enemies arbitrary
        return $ TestInGameInfos $ InGameInfos (getPlayer p1) (getPlayer p2) (map getEnemy enemies)

prop_initInGameInfos_preservesInvariant :: TestPlayer -> TestPlayer -> [TestEnemy] -> Property
prop_initInGameInfos_preservesInvariant p1 p2 enemies = 
    property $ prop_inv_ingameinfos $ initInGameInfos (getPlayer p1) (getPlayer p2) (map getEnemy enemies)

initInGameInfosSpec :: SpecWith ()
initInGameInfosSpec = do
    describe "initInGameInfos" $ do
        it "preserves the InGameInfos invariant for all valid pairs of Players, and Enemies" $
            property prop_initInGameInfos_preservesInvariant

updatePlayerDirectionSpeedSpec :: Spec
updatePlayerDirectionSpeedSpec = do
  describe "updatePlayerDirectionSpeed (unit tests)" $ do

    it "updates player1 direction and speed (independently)" $ do
        let po1 = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
            p1  = initAlivePlayer po1 3 100 0

            po2 = initPlayerObject Blank 50 50 (initDirection 0 0) (initObjectSpeed 0)
            p2  = initAlivePlayer po2 3 100 0

            igi = initInGameInfos p1 p2 []
            ds = (initDirection 1 0, initObjectSpeed 2)
            (_, InGameInfos p1' _ _ _) = updatePlayerDirectionSpeed True ds igi

        objectDirection (playerObject p1') `shouldBe` initDirection 1 0
        objectSpeed (playerObject p1')     `shouldBe` initObjectSpeed 2

    it "updates player2 direction and speed (independently)" $ do
        let po1 = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
            p1  = initAlivePlayer po1 3 100 0

            po2 = initPlayerObject Blank 50 50 (initDirection 0 0) (initObjectSpeed 0)
            p2  = initAlivePlayer po2 3 100 0

            igi = initInGameInfos p1 p2 []
            ds = (initDirection 0 (-1), initObjectSpeed 3)
            (_, InGameInfos _ p2' _ _) = updatePlayerDirectionSpeed False ds igi

        objectDirection (playerObject p2') `shouldBe` initDirection 0 (-1)
        objectSpeed (playerObject p2')     `shouldBe` initObjectSpeed 3

updatePlayerDirectionSpeedQuickCheckSpec :: Spec
updatePlayerDirectionSpeedQuickCheckSpec = do
    describe "updatePlayerDirectionSpeed (QuickCheck)" $ do
        it "satisfies updatePlayerDirectionSpeed post-condition for all valid parameters" $
            property (\isP1 (TestDirection d, TestObjectSpeed os) (TestInGameInfos igi) ->
                prop_inv_direction d && prop_inv_objectSpeed os && prop_inv_ingameinfos igi
                && prop_pre_updatePlayerDirectionSpeed isP1 (d, os) igi
                ==> let ((), igiPost) = updatePlayerDirectionSpeed isP1 (d, os) igi
                in prop_inv_ingameinfos igiPost
            )

movePlayerSpec :: Spec
movePlayerSpec = do
  describe "movePlayer (unit tests)" $ do
    it "moves player1 according to direction and speed (independently)" $ do
        let (x, y) = (0, 0)
            po1 = initPlayerObject Blank x y (initDirection 1 0) (initObjectSpeed 5)
            p1  = initAlivePlayer po1 3 100 0

            po2 = initPlayerObject Blank 100 100 (initDirection 0 0) (initObjectSpeed 0)
            p2  = initAlivePlayer po2 3 100 0

            igi = initInGameInfos p1 p2 []
            (_, InGameInfos p1' _ _ _) = movePlayer True igi
            (x', _) = centerHitbox (objectHitbox (playerObject p1'))

        x' `shouldBe` x + 5

    it "moves player2 according to direction and speed (independently)" $ do
        let po1 = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
            p1  = initAlivePlayer po1 3 100 0

            (x, y) = (50, 50)
            po2 = initPlayerObject Blank x y (initDirection (-1) 0) (initObjectSpeed 2)
            p2  = initAlivePlayer po2 3 100 0

            igi = initInGameInfos p1 p2 []
            (_, InGameInfos _ p2' _ _) = movePlayer False igi
            (x', _) = centerHitbox (objectHitbox (playerObject p2'))

        x' `shouldBe` x - 2

movePlayerQuickCheckSpec :: Spec
movePlayerQuickCheckSpec = do
    describe "movePlayer (QuickCheck)" $ do
        it "satisfies movePlayer post-condition for all valid parameters" $
            property (\isP1 (TestInGameInfos igi) ->
                prop_inv_ingameinfos igi && prop_pre_movePlayer isP1 igi
                ==> let (_, igiPost) = movePlayer isP1 igi
                    in prop_inv_ingameinfos igiPost
            )

collisionPlayerWithEnemySpec :: Spec
collisionPlayerWithEnemySpec = do
    describe "collisionPlayerWithEnemy (unit tests)" $ do
        it "detects collision when player and enemy overlap" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initAlivePlayer po 3 100 0
                eo = initStaticObject Blank (initHitboxRectangle 0 0 10 10)
                e  = initEnemy eo 1
            collisionPlayerWithEnemy p e `shouldBe` True

        it "detects no collision when far apart" $ do
            let po = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initAlivePlayer po 3 100 0
                eo = initStaticObject Blank (initHitboxRectangle 1000 1000 10 10)
                e  = initEnemy eo 1
            collisionPlayerWithEnemy p e `shouldBe` False

collisionPlayerWithEnemyQuickCheckSpec :: Spec
collisionPlayerWithEnemyQuickCheckSpec = do
    describe "collisionPlayerWithEnemy (QuickCheck)" $ do
        it "satisfies collisionPlayerWithEnemy post-condition for all valid parameters" $
            property (\(TestPlayer p) (TestEnemy e) ->
                prop_inv_player p && prop_inv_enemy e 
                ==> collisionPlayerWithEnemy p e == collisionObject (playerObject p) (enemyObject e)
            )

keepAliveEnemiesSpec :: Spec
keepAliveEnemiesSpec = do
  describe "keepAliveEnemies (unit tests)" $ do
    it "returns empty list when no enemies" $ do
        let po = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
            p  = initAlivePlayer po 3 100 0
        keepAliveEnemies p [] 0 `shouldBe` ([], 0)

    it "keeps enemies unchanged when no collision" $ do
        let po = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
            p  = initAlivePlayer po 3 100 0
            eo = initStaticObject Blank (initHitboxRectangle 1000 1000 10 10)
            e  = initEnemy eo 3
        keepAliveEnemies p [e] 0 `shouldBe` ([e], 0)

    it "decreases enemy health on collision when hp > 1" $ do
        let po = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
            p  = initAlivePlayer po 3 100 0
            eo = initStaticObject Blank (initHitboxRectangle 0 0 10 10)
            e  = initEnemy eo 3
            (result, collisions) = keepAliveEnemies p [e] 0
        case result of
            [e'] -> do
                    enemyHealth e' `shouldBe` 2
                    collisions `shouldBe` 1
            _    -> expectationFailure "Expected exactly one enemy"

    it "removes enemy on collision when hp == 1" $ do
        let po = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
            p  = initAlivePlayer po 3 100 0
            eo = initStaticObject Blank (initHitboxRectangle 0 0 10 10)
            e  = initEnemy eo 1
        keepAliveEnemies p [e] 0 `shouldBe` ([], 1)

newtype TestEnemyList = TestEnemyList {getEnemies :: [Enemy]} deriving (Eq, Show)
instance Arbitrary TestEnemyList where
    arbitrary = do
        n <- choose (0, 10) -- between 0 and 10 enemis in an enemy list
        es <- vectorOf n arbitrary
        return $ TestEnemyList (map getEnemy es)

keepAliveEnemiesQuickCheckSpec :: Spec
keepAliveEnemiesQuickCheckSpec = do
    describe "keepAliveEnemies (QuickCheck)" $ do
        it "satisfies keepAliveEnemies post-condition for all valid parameters" $
            property ( \(TestPlayer p) (TestEnemyList enemies) ->
                let nbColls = 0 in -- fixed the precondition
                prop_inv_player p && all prop_inv_enemy enemies
                ==> let (result, _) = keepAliveEnemies p enemies nbColls
                    in all prop_inv_enemy result && prop_post_keepAliveEnemies p enemies nbColls
            )

handleCollisionPlayerWithEnemiesSpec :: Spec
handleCollisionPlayerWithEnemiesSpec = do
  describe "handleCollisionPlayerWithEnemies (unit tests)" $ do
    it "updates only player1 when isP1 = True" $ do
        let po1 = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
            p1  = initAlivePlayer po1 3 100 0

            po2 = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
            p2  = initAlivePlayer po2 3 100 0

            eo  = initStaticObject Blank (initHitboxRectangle 0 0 10 10)
            e   = initEnemy eo 3

            igi = initInGameInfos p1 p2 [e]
            (_, InGameInfos p1' p2' enemies') = handleCollisionPlayerWithEnemies True igi

        playerScore p1' `shouldBe` 47
        playerHealth p1' `shouldBe` 90
        playerLifes p1' `shouldBe` 3

        p2' `shouldBe` p2
        length enemies' `shouldBe` 1

    it "updates only player2 when isP1 = False" $ do
        let po1 = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
            p1  = initAlivePlayer po1 3 100 0

            po2 = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
            p2  = initAlivePlayer po2 3 100 0

            eo  = initStaticObject Blank (initHitboxRectangle 0 0 10 10)
            e   = initEnemy eo 3

            igi = initInGameInfos p1 p2 [e]
            (_, InGameInfos p1' p2' enemies') = handleCollisionPlayerWithEnemies False igi

        playerScore p2' `shouldBe` 47
        playerHealth p2' `shouldBe` 90
        playerLifes p2' `shouldBe` 3

        p1' `shouldBe` p1
        length enemies' `shouldBe` 1

    it "removes enemy when health reaches 0" $ do
        let po = initPlayerObject Blank 0 0 (initDirection 0 0) (initObjectSpeed 0)
            p  = initAlivePlayer po 3 100 0

            eo = initStaticObject Blank (initHitboxRectangle 0 0 10 10)
            e  = initEnemy eo 1

            igi = initInGameInfos p p [e]
            (_, InGameInfos _ _ enemies') = handleCollisionPlayerWithEnemies True igi
        length enemies' `shouldBe` 0

handleCollisionPlayerWithEnemiesQuickCheckSpec :: Spec
handleCollisionPlayerWithEnemiesQuickCheckSpec = do
    describe "handleCollisionPlayerWithEnemies (QuickCheck)" $ do
        it "satisfies handleCollisionPlayerWithEnemies post-condition for all valid parameters" $
            property (\isP1 (TestInGameInfos igi) ->
                prop_inv_ingameinfos igi && prop_pre_handleCollisionPlayerWithEnemies isP1 igi
                ==> let (_, igiPost) = handleCollisionPlayerWithEnemies isP1 igi
                    in prop_inv_ingameinfos igiPost
            )