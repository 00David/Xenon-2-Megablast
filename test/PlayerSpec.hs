{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PlayerSpec (
    TestPlayer(..),
    spec
)
where

import Test.Hspec
import Test.QuickCheck

import GameSetup
import GameState.Bonus
import GameState.Player
import GameState.Projectile
import Graphics.Assets
import Objects.Hitbox
import Objects.Objects
import Typeclasses.Damageable
import Typeclasses.Invariant
import AssetsSpec(TestGameAssets(..))
import ObjectsSpec(TestObject(..), TestDirection(..), TestObjectSpeed(..))

spec :: Spec
spec = do
    initAlivePlayerSpec
    initInvinciblePlayerSpec
    initDeadPlayerSpec
    startInitAlivePlayerSpec
    startInitDeadPlayerSpec
    aliveToInvinciblePlayerSpec
    invincibleToAlivePlayerSpec
    playerObjectSpec
    playerIdSpec
    playerLifesSpec
    playerHealthSpec
    playerScoreSpec
    updatePlayerObjectSpec
    addScoreSpec
    isPlayerDeadSpec
    movePlayerSpec
    movePlayerQuickCheckSpec
    playerShotSpec
    runPlayerAnimationSpec
    updatePlayerShootBonusSpec
    incrementShootFrameCounterSpec
    getTranslatedPlayerAssetQuickCheckSpec
    getTranslatedBoosterAssetsQuickCheckSpec
    takeDamagePlayerSpec
    takeDamagePlayerQuickCheckSpec
    invariantLawsSpec
    renderableLawSpec
    collidableLawsSpec
    damageableLawsSpec

-- ============================================================
-- =================== TEST PLAYER ============================
-- ============================================================

newtype TestPlayer = TestPlayer { getPlayer :: Player } deriving (Eq, Show)

genAlivePlayer :: Gen TestPlayer
genAlivePlayer = do
    pId <- choose (1, 2)
    lifes <- choose (1, 3)
    health <- choose (1, 100)
    score <- abs <$> arbitrary
    frameShootCpt <- choose (1, 100)
    frameRedCpt <- choose (0, nbFramesRedOnDamage)

    -- Ensure player is inside screen bounds
    x <- choose ((leftXScreenBound + widthPlayer/2), (rightXScreenBound - widthPlayer/2))
    y <- choose ((bottomYScreenWithBarBound + heightPlayer/2), (topYScreenBound - heightPlayer/2))

    (TestDirection dir) <-arbitrary
    (TestObjectSpeed speed) <-arbitrary
    let 
        playerObj = initPlayerObject x y dir speed
        psb = NoBonus -- TMP
    
    return $ TestPlayer (initAlivePlayer playerObj pId lifes health score psb frameShootCpt frameRedCpt)

genInvinciblePlayer :: Gen TestPlayer
genInvinciblePlayer = do
    pId <- choose (1, 2)
    lifes <- choose (1, 3)
    health <- choose (1, 100)
    score <- abs <$> arbitrary
    frameShootCpt <- choose (1, 100)
    frameCpt <- choose (1, nbFramesInvincible)

    -- Ensure player is inside screen bounds
    x <- choose ((leftXScreenBound + widthPlayer/2), (rightXScreenBound - widthPlayer/2))
    y <- choose ((bottomYScreenWithBarBound + heightPlayer/2), (topYScreenBound - heightPlayer/2))

    (TestDirection dir) <-arbitrary
    (TestObjectSpeed speed) <-arbitrary
    let 
        playerObj = initPlayerObject x y dir speed
        psb = NoBonus -- TMP
    
    return $ TestPlayer (initInvinciblePlayer playerObj pId lifes health score psb frameShootCpt frameCpt)

genDeadPlayer :: Gen TestPlayer
genDeadPlayer = do
    pId <- elements [1, 2]
    score <- abs <$> arbitrary
    frameCpt <- choose (1, nbFramesPerExplosionPhase)
    phase <- choose (0, nbPlayerExplosionAssets)

    -- Ensure player is inside screen bounds
    x <- choose ((leftXScreenBound + widthPlayer/2), (rightXScreenBound - widthPlayer/2))
    y <- choose ((bottomYScreenWithBarBound + heightPlayer/2), (topYScreenBound - heightPlayer/2))
    
    (TestDirection dir) <-arbitrary
    (TestObjectSpeed speed) <-arbitrary
    let 
        playerObj = initPlayerObject x y dir speed
    
    return $ TestPlayer (initDeadPlayer playerObj pId score frameCpt phase)

-- Initializes Players veryfing their invariant
instance Arbitrary TestPlayer where
    arbitrary :: Gen TestPlayer
    arbitrary = oneof [
        genAlivePlayer,
        genInvinciblePlayer,
        genDeadPlayer
        ]

-- Corresponds to the exact Player state taken as an input of the aliveToInvinciblePlayerSpec
genAliveToInvinciblePlayer :: Gen TestPlayer
genAliveToInvinciblePlayer = do
    pId <- choose (1,2)
    lifes <- choose (2,3) -- more than 1
    let health = 0 -- exactly 0
    score <- abs <$> arbitrary
    frameShootCpt <- choose (1,100)
    frameRedCpt <- choose (0, nbFramesRedOnDamage)

    x <- choose (leftXScreenBound + widthPlayer/2, rightXScreenBound - widthPlayer/2)
    y <- choose (bottomYScreenWithBarBound + heightPlayer/2, topYScreenBound - heightPlayer/2)
    (TestDirection dir) <- arbitrary
    (TestObjectSpeed speed) <- arbitrary

    let playerObj = initPlayerObject x y dir speed

    return $ TestPlayer (AliveP playerObj pId lifes health score NoBonus frameShootCpt frameRedCpt)

-- ============================================================
-- ================= PLAYER CONSTRUCTORS ======================
-- ============================================================

prop_initAlivePlayer_preservesInvariant :: TestObject -> Property
prop_initAlivePlayer_preservesInvariant (TestObject obj) =
    forAll (choose (1,2)) $ \pId ->
    forAll (choose (1,3)) $ \lifes ->
    forAll (choose (1,100)) $ \health ->
    forAll (abs <$> arbitrary) $ \score ->
    --forAll arbitrary $ \psb ->
    forAll (choose (1, 100)) $ \frameShootCpt ->
    forAll (choose (0, nbFramesRedOnDamage)) $ \frameRedCpt ->
        let
            psb = NoBonus
            pTMP = AliveP obj pId lifes health score psb frameShootCpt frameRedCpt
        in insideScreenPlayer pTMP ==> -- filter by keeping only objects inside screen bounds
            prop_inv_player (initAlivePlayer obj pId lifes health score psb frameShootCpt frameRedCpt)

prop_initInvinciblePlayer_preservesInvariant :: TestObject -> Property
prop_initInvinciblePlayer_preservesInvariant (TestObject obj) =
    forAll (choose (1,2)) $ \pId ->
    forAll (choose (1,3)) $ \lifes ->
    forAll (choose (1,100)) $ \health ->
    forAll (abs <$> arbitrary) $ \score ->
    --forAll arbitrary $ \psb ->
    forAll (choose (1, 100)) $ \frameShootCpt ->
    forAll (choose (1, nbFramesInvincible)) $ \frameCpt ->
        let
            psb = NoBonus
            pTMP = InvincibleP obj pId lifes health score psb frameShootCpt frameCpt
        in insideScreenPlayer pTMP ==> -- filter by keeping only objects inside screen bounds
            prop_inv_player (initInvinciblePlayer obj pId lifes health score psb frameShootCpt frameCpt)

prop_initDeadPlayer_preservesInvariant :: TestObject -> Property
prop_initDeadPlayer_preservesInvariant (TestObject obj) =
    forAll (choose (1,2)) $ \pId ->
    forAll (abs <$> arbitrary) $ \score ->
    forAll (choose (1, nbFramesPerExplosionPhase)) $ \frameCpt ->
    forAll (choose (0, nbPlayerExplosionAssets)) $ \phase ->
        let
            pTMP = DeadP obj pId score frameCpt phase
        in insideScreenPlayer pTMP ==> -- filter by keeping only objects inside screen bounds
            prop_inv_player (initDeadPlayer obj pId score frameCpt phase)

prop_startInitAlivePlayer_preservesInvariant :: Property
prop_startInitAlivePlayer_preservesInvariant =
    forAll (choose (1,2)) $ \pId ->
        prop_inv_player (startInitAlivePlayer pId)

prop_startInitDeadPlayer_preservesInvariant :: Property
prop_startInitDeadPlayer_preservesInvariant =
    forAll (choose (1,2)) $ \pId ->
        prop_inv_player (startInitDeadPlayer pId)

initAlivePlayerSpec :: SpecWith ()
initAlivePlayerSpec = do
    describe "initAlivePlayer (QuickCheck)" $ do
        it "preserves the Player invariant for valid alive Players" $
            property prop_initAlivePlayer_preservesInvariant

initInvinciblePlayerSpec :: SpecWith ()
initInvinciblePlayerSpec = do
    describe "initInvinciblePlayer (QuickCheck)" $ do
        it "preserves the Player invariant for valid invincible Players" $
            property prop_initInvinciblePlayer_preservesInvariant

initDeadPlayerSpec :: SpecWith ()
initDeadPlayerSpec = do
    describe "initDeadPlayer (QuickCheck)" $ do
        it "preserves the Player invariant for valid dead Players" $
            property prop_initDeadPlayer_preservesInvariant

startInitAlivePlayerSpec :: SpecWith ()
startInitAlivePlayerSpec = do
    describe "startInitAlivePlayer (QuickCheck)" $ do
        it "preserves the Player invariant for start alive Players" $
            property prop_startInitAlivePlayer_preservesInvariant

startInitDeadPlayerSpec :: SpecWith ()
startInitDeadPlayerSpec = do
    describe "startInitDeadPlayer (QuickCheck)" $ do
        it "preserves the Player invariant for start dead Players" $
            property prop_startInitDeadPlayer_preservesInvariant

-- ============================================================
-- ================== PLAYER OPERATIONS =======================
-- ============================================================

aliveToInvinciblePlayerSpec :: Spec
aliveToInvinciblePlayerSpec = do
    describe "aliveToInvinciblePlayer (QuickCheck)" $ do
        it "satisfies aliveToInvinciblePlayer post-condition for all valid parameters" $
            forAll genAliveToInvinciblePlayer (
                \(TestPlayer p) ->
                    let p' = aliveToInvinciblePlayer p
                    in prop_inv_player p'
                    && prop_post_aliveToInvinciblePlayer p
            )

invincibleToAlivePlayerSpec :: Spec
invincibleToAlivePlayerSpec = do
    describe "invincibleToAlivePlayer (QuickCheck)" $ do
        it "satisfies invincibleToAlivePlayer post-condition for all valid parameters" $
            property (\(TestPlayer p) ->
                (prop_inv_player p && prop_pre_invincibleToAlivePlayer p)
                ==> let p' = invincibleToAlivePlayer p
                    in prop_inv_player p' && prop_post_invincibleToAlivePlayer p
            )

playerObjectSpec :: Spec
playerObjectSpec = do
    describe "playerObject (unit tests)" $ do
        it "returns correct Object for alive Player" $ do
            let po = initPlayerObject 10 20 (initDirection 1 0) (initObjectSpeed 3)
                p  = initAlivePlayer po 1 3 100 0 NoBonus 1 0
            playerObject p `shouldBe` po

        it "returns correct Object for invincible Player" $ do
            let po = initPlayerObject 10 20 (initDirection 0 1) (initObjectSpeed 2)
                p  = initInvinciblePlayer po 2 2 80 50 NoBonus 1 10
            playerObject p `shouldBe` po

        it "returns correct Object for dead Player" $ do
            let po = initPlayerObject 5 5 (initDirection 0 0) (initObjectSpeed 0)
                p  = initDeadPlayer po 1 100 1 0
            playerObject p `shouldBe` po

playerIdSpec :: Spec
playerIdSpec = do
    describe "playerId (unit tests)" $ do
        it "returns id 1 for alive Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initAlivePlayer po 1 2 100 0 NoBonus 1 0
            playerId p `shouldBe` 1

        it "returns id 2 for invincible Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initInvinciblePlayer po 2 3 100 0 NoBonus 1 1
            playerId p `shouldBe` 2

        it "returns id 2 for dead Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initDeadPlayer po 2 10 1 0
            playerId p `shouldBe` 2

playerLifesSpec :: Spec
playerLifesSpec = do
    describe "playerLifes (unit tests)" $ do
        it "returns lifes for alive Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initAlivePlayer po 1 2 100 0 NoBonus 1 0
            playerLifes p `shouldBe` 2

        it "returns lifes for invincible Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initInvinciblePlayer po 1 3 100 0 NoBonus 1 1
            playerLifes p `shouldBe` 3

        it "returns 0 for dead Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initDeadPlayer po 1 10 1 0
            playerLifes p `shouldBe` 0

playerHealthSpec :: Spec
playerHealthSpec = do
    describe "playerHealth (unit tests)" $ do
        it "returns health for alive Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initAlivePlayer po 1 3 75 0 NoBonus 1 0
            playerHealth p `shouldBe` 75

        it "returns health for invincible Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initInvinciblePlayer po 1 2 50 0 NoBonus 1 1
            playerHealth p `shouldBe` 50

        it "returns 0 for dead Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initDeadPlayer po 2 0 1 0
            playerHealth p `shouldBe` 0

playerScoreSpec :: Spec
playerScoreSpec = do
    describe "playerScore (unit tests)" $ do
        it "returns score for alive Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initAlivePlayer po 1 3 100 42 NoBonus 1 0
            playerScore p `shouldBe` 42

        it "returns score for invincible Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initInvinciblePlayer po 2 2 100 123 NoBonus 1 1
            playerScore p `shouldBe` 123

        it "returns score for dead Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initDeadPlayer po 2 99 1 0
            playerScore p `shouldBe` 99

updatePlayerObjectSpec :: Spec
updatePlayerObjectSpec = do
    describe "updatePlayerObject (QuickCheck)" $ do
        it "satisfies updatePlayerObject post-condition for all valid parameters" $
            property (\(TestPlayer p) (TestObject newObj) ->
                prop_inv_player p && prop_inv_object newObj
                ==> let p' = updatePlayerObject p newObj
                    in prop_inv_object (playerObject p') && prop_post_updatePlayerObject p newObj
            )

addScoreSpec :: Spec
addScoreSpec = do
    describe "addScore (QuickCheck)" $ do
        it "satisfies addScore post-condition for all valid parameters" $
            property (\(TestPlayer p) s  ->
                    prop_inv_player p && prop_pre_addScore s p
                    ==> let p' = addScore s p
                        in prop_inv_player p' && prop_post_addScore s p
            )

isPlayerDeadSpec :: Spec
isPlayerDeadSpec = do
    describe "isPlayerDead (unit tests)" $ do
        it "returns False for alive Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initAlivePlayer po 1 3 100 10 NoBonus 1 0
            isPlayerDead p `shouldBe` False

        it "returns False for invincible Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initInvinciblePlayer po 1 3 100 10 NoBonus 1 1
            isPlayerDead p `shouldBe` False

        it "returns True for dead Player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p  = initDeadPlayer po 2 10 1 0
            isPlayerDead p `shouldBe` True

movePlayerSpec :: Spec
movePlayerSpec = do
    describe "movePlayer (unit tests)" $ do
        it "moves alive player according to direction and speed" $ do
            let po = initPlayerObject 0 0 (initDirection 1 0) (initObjectSpeed 5)
                p = initAlivePlayer po 1 3 100 0 NoBonus 1 0
                p' = movePlayer p 0
                po' = playerObject p'
            centerHitbox (objectHitbox po') `shouldBe` (5, 0)

        it "moves invincible player according to direction and speed" $ do
            let po = initPlayerObject 0 0 (initDirection 0 (-1)) (initObjectSpeed 3)
                p = initDeadPlayer po 1 100 1 0
                p' = movePlayer p 0
                po' = playerObject p'
            centerHitbox (objectHitbox po') `shouldBe` (0, (-3))

movePlayerQuickCheckSpec :: Spec
movePlayerQuickCheckSpec = do
    describe "movePlayer (QuickCheck)" $ do
        it "satisfies movePlayer post-condition for all valid parameters" $
            property (\(TestPlayer p) ss ->
                (prop_inv_player p && prop_pre_movePlayer p ss)
                ==> let p' = movePlayer p ss
                    in prop_inv_player p' && prop_post_movePlayer p ss
            )

playerShotSpec :: Spec
playerShotSpec = do
    describe "playerShot (QuickCheck)" $ do
        it "satisfies playerShot post-condition for all valid parameters" $
            property (\(TestPlayer p) ->
                prop_inv_player p
                ==> let (maybeProj, p') = playerShot p
                    in prop_inv_player p' && prop_post_playerShot p
                        && case maybeProj of
                            Nothing -> True
                            (Just proj) -> prop_inv_projectile proj
            )

runPlayerAnimationSpec :: Spec
runPlayerAnimationSpec = do
    describe "runPlayerAnimation (QuickCheck)" $ do
        it "satisfies runPlayerAnimation post-condition for all valid parameters" $
            property (\(TestPlayer p) ->
                prop_inv_player p
                ==> let p' = runPlayerAnimation p
                    in prop_inv_player p' && prop_post_runPlayerAnimation p
            )

updatePlayerShootBonusSpec :: Spec
updatePlayerShootBonusSpec = do
    describe "updatePlayerShootBonus (QuickCheck)" $ do
        it "satisfies updatePlayerShootBonus post-condition for all valid parameters" $
            property (\(TestPlayer p) ->
                let newPsb = NoBonus in -- TMP
                prop_inv_player p && prop_inv_playerShootBonus newPsb
                ==> let p' = updatePlayerShootBonus p newPsb
                    in prop_inv_player p' && prop_post_updatePlayerShootBonus p newPsb
            )

incrementShootFrameCounterSpec :: Spec
incrementShootFrameCounterSpec = do
    describe "incrementShootFrameCounter (QuickCheck)" $ do
        it "satisfies incrementShootFrameCounter post-condition for all valid parameters" $
            property (\(TestPlayer p) ->
                prop_inv_player p
                ==> let p' = incrementShootFrameCounter p
                    in prop_inv_player p' && prop_post_incrementShootFrameCounter p
            )

getTranslatedPlayerAssetQuickCheckSpec :: Spec
getTranslatedPlayerAssetQuickCheckSpec = do
    describe "getTranslatedPlayerAsset (QuickCheck)" $ do
        it "satisfies getTranslatedPlayerAsset post-condition for all valid parameters" $
            property (\(TestGameAssets ga) (TestPlayer player) ->
                prop_inv_player player ==> prop_post_getTranslatedPlayerAsset ga player
            )

getTranslatedBoosterAssetsQuickCheckSpec :: Spec
getTranslatedBoosterAssetsQuickCheckSpec = do
    describe "getTranslatedBoosterAssets (QuickCheck)" $ do
        it "satisfies getTranslatedBoosterAssets post-condition for all valid players" $
            property (\(TestGameAssets ga) (TestPlayer player) ->
                prop_inv_player player
                ==> prop_post_getTranslatedBoosterAssets ga player
            )

takeDamagePlayerSpec :: Spec
takeDamagePlayerSpec = do
    describe "takeDamage (unit tests)" $ do
        it "does not modify alive player with zero damage" $ do
            let p = startInitAlivePlayer 1
            takeDamage 0 p `shouldBe` p
        it "decreases health for alive player without losing a life" $ do
            let p = startInitAlivePlayer 1 -- starts with 3 lifes, 100 health points
                p' = takeDamage 30 p -- take 30 damages
            case p' of
                AliveP _ _ lifes health _ _ _ _ -> do
                    lifes `shouldBe` 3
                    health `shouldBe` 70
                _ -> expectationFailure "Expected AliveP"
        it "switches alive player to invincible after losing a life" $ do
            let p = startInitAlivePlayer 1 -- starts with 3 lifes, 100 health points
                p' = takeDamage 120 p
            case p' of
                InvincibleP _ _ lifes health _ _ _ _ -> do
                    lifes `shouldBe` 2 -- now has 2 remaining lifes
                    health `shouldBe` 100 -- with health reseted at 100
                _ -> expectationFailure "Expected InvincibleP"
        it "kills player after losing last life" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p = initAlivePlayer po 1 1 100 0 NoBonus 1 0 -- alive with 1 life, 100 health points
                p' = takeDamage 100 p
            case p' of
                DeadP _ _ _ _ _ -> True `shouldBe` True -- player became dead
                _ -> expectationFailure "Expected DeadP"
        it "does not damage invincible player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p = initInvinciblePlayer po 1 2 100 0 NoBonus 1 1 -- invincible, with 2 lifes, 100 health points
            takeDamage 50 p `shouldBe` p -- does not change on damages reception
        it "does not damage dead player" $ do
            let po = initPlayerObject 0 0 (initDirection 0 0) (initObjectSpeed 0)
                p = initDeadPlayer po 1 0 1 0 -- dead
            takeDamage 50 p `shouldBe` p -- does not change on damages reception

takeDamagePlayerQuickCheckSpec :: Spec
takeDamagePlayerQuickCheckSpec = do
    describe "takeDamage (QuickCheck)" $ do
        it "satisfies takeDamage post-condition for all valid parameters" $
            property (\(TestPlayer p) ->
                forAll (choose (-30, 200)) $ \damage ->
                prop_inv_player p
                ==> let p' = takeDamage damage p
                    in prop_inv_player p' && prop_post_takeDamagePlayer damage p
            )

-- ============================================================
-- ======================== LAWS ==============================
-- ============================================================

invariantLawsSpec :: Spec
invariantLawsSpec = do
    describe "Invariant laws (QuickCheck)" $ do
        it "law_invariant_stable for Player" $
            property (
                \(TestPlayer player) -> law_invariant_stable player
            )

        it "law_invariant_idempotent for Player" $
            property (
                \(TestPlayer player) -> law_invariant_idempotent player
            )

renderableLawSpec :: Spec
renderableLawSpec = do
    describe "Renderable laws (QuickCheck)" $ do
        it "law_renderable_finite for Player" $
            property (\(TestGameAssets ga) (TestPlayer player) ->
                law_renderable_finite ga player
            )

collidableLawsSpec :: Spec
collidableLawsSpec = do
    describe "Collidable laws (QuickCheck)" $ do
        it "law_collidable_reflexive for Player" $
            property (\(TestPlayer p) ->
                prop_inv_player p 
                ==> law_collidable_reflexive p
            )
        it "law_collidable_symmetric for Player with another Player" $
            property (\(TestPlayer p1) (TestPlayer p2) ->
                prop_inv_player p1 && prop_inv_player p2 
                ==> law_collidable_symmetric p1 p2
            )
        it "law_collidable_symmetric for Player with another Object" $
            property (\(TestPlayer p) (TestObject o) ->
                prop_inv_player p && prop_inv_object o 
                ==> law_collidable_symmetric p o
            )
        it "law_collidable_will_collide for Player with another Player" $
            property (\(TestPlayer p1) (TestPlayer p2) ->
                prop_inv_player p1 && prop_inv_player p2 
                ==> law_collidable_will_collide p1 p2
            )
        it "law_collidable_will_collide for Player with another Object" $
            property (\(TestPlayer p) (TestObject o) ->
                prop_inv_player p && prop_inv_object o
                ==> law_collidable_will_collide p o
            )

damageableLawsSpec :: Spec
damageableLawsSpec = do
    describe "Damageable laws (QuickCheck)" $ do
        it "law_damageable_dead_stays_dead for Player" $
            property (\(TestPlayer p) ->
                forAll (choose (1, 100)) $ \damage ->
                    prop_inv_player p 
                    ==> law_damageable_dead_stays_dead damage p
            )
        it "law_damageable_dead_idempotent for Player" $
            property (\(TestPlayer p) ->
                forAll (choose (1, 100)) $ \damage1 ->
                forAll (choose (1, 100)) $ \damage2 ->
                    prop_inv_player p 
                    ==> law_damageable_dead_idempotent damage1 damage2 p
            )
        it "law_damageable_zero_damage_identity for Player" $
            property (\(TestPlayer p) ->
                prop_inv_player p 
                ==> law_damageable_zero_damage_identity p
            )
        it "law_damageable_no_heal_negative_damage for Player" $
            property (\(TestPlayer p) ->
                forAll (choose (-100, -1)) $ \damage ->
                    prop_inv_player p ==>
                    law_damageable_no_heal_negative_damage damage p
            )