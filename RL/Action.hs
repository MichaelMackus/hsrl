module RL.Action (module RL.Action, module Control.Monad.Reader) where

import RL.Event
import RL.Game
import RL.Random

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe (fromMaybe, fromJust, maybeToList, catMaybes, isJust)
import qualified Data.List as L

class Monad m => GameAction m where
    getEnv :: m Env
    insertEvent  :: Event -> m ()
    insertEvent ev = insertEvents [ev]
    insertEvents :: [Event] -> m ()
    insertEvents = mapM_ insertEvent

-- TODO miss chance if invis
attack :: (GameAction m, MonadRandom m) => Mob -> Maybe Item -> Mob -> m ()
attack attacker weap target = do
    let weapProp  = weaponProperties =<< weap
        weapBonus = fromMaybe 0 (bonus <$> weapProp)
    -- attack roll
    atk <- roll (1 `d` 20)
    if atk + weapBonus >= thac0 attacker - mobAC target || atk == 20 then do
        -- hit!
        let dmgDie  = maybe (baseDmg attacker) dmgd weapProp
            crit    = atk == (maybe 20 critRange weapProp)
            -- the ornate sword automatically kills enemies on crit
            critDmg = if (itemDescription <$> (wielding (equipment attacker))) == Just "Ornate Sword" then hp target else maxD dmgDie
        dmg <- (+ strength attacker) <$> if crit then return critDmg else roll dmgDie
        insertEvent $ Damaged attacker target dmg
        when crit   $ insertEvent (Crit attacker target)
        when (isDead (target { hp = hp target - dmg })) $ insertEvent (Died target)
    else
        insertEvent (Missed attacker target)

applyItem :: (GameAction m, MonadRandom m) => DLevel -> Mob -> Item -> m ()
applyItem lvl m i =
    if isEquippable i then equip m i
    else if isDrinkable i then drinkPotion m i
    else if isReadable i then readScroll lvl m i
    else if (itemType i == Bandage) then applyBandage m
    else if (itemType i == Draught) then drinkDraught m i
    else return ()

applyBandage :: (GameAction m, MonadRandom m) => Mob -> m ()
applyBandage m = when (hp m < mhp m) $ do
    healed <- roll (1 `d` 4)
    insertEvents [BandageApplied m, Healed m healed]

drinkDraught :: (GameAction m, MonadRandom m) => Mob -> Item -> m ()
drinkDraught m i = do
    healed <- roll (1 `d` 6)
    insertEvents [Drank m i, Healed m healed]

-- fire projectile toward the mob's target
-- TODO unable to fire in melee
fire :: (GameAction m, MonadRandom m) => DLevel -> Mob -> Item -> Mob -> m ()
fire lvl attacker proj m = when (isProjectile proj) $ do
    let eqp = equipment attacker
        isLaunching = maybe False (`launchesProjectile` proj) (launcher eqp)
    -- fire projectile if proper launcher equipped
    if isLaunching then do
        insertEvent (FiredProjectile attacker (fromJust (launcher eqp)) proj (at m))
        attack attacker (launcher eqp) m
    else do
        insertEvent (ThrownProjectile attacker proj (at m))
        attack attacker (Just proj) m

equip :: GameAction m => Mob -> Item -> m ()
-- TODO why is this removing armor?
equip m i = do
    let wield  = L.filter isTwoHanded (catMaybes [wielding (equipment m), launcher (equipment m)])
        shld   = fromJust (shield (equipment m))
    when (isTwoHanded i && isShielded m) $ insertEvent (EquipmentRemoved m shld)
    when (isShield    i && handsFull  m) $ insertEvents (map (EquipmentRemoved m) wield)
    insertEvent (Equipped m i)

drinkPotion :: (GameAction m, MonadRandom m) => Mob -> Item -> m ()
drinkPotion m i = do
    insertEvent (Drank m i)
    case potionType i of
         Just Healing -> do
             healed <- roll (1 `d` 8)
             insertEvent (Healed m healed)
         Just Life -> do
             healed <- roll (1 `d` 8)
             insertEvent (GainedLife m healed)
         Just Acid -> do
             dmg <- roll (1 `d` 6)
             insertEvents [DrankAcid m, Damaged m m dmg]
             when (isDead (m { hp = hp m - dmg })) $ insertEvent (Died m)
         Just Strength     -> insertEvent (GainedStrength m 1)
         Just Invisibility -> insertEvent (Vanished m)
         Just Confusion    -> insertEvent (Confused m)
         Just Darkness     -> insertEvent (Blinded m)
         otherwise         -> return ()

-- TODO change to be independent of player/mob
readScroll :: (GameAction m, MonadRandom m) => DLevel -> Mob -> Item -> m ()
readScroll lvl m i = do
    insertEvent (Read m i)
    case scrollType i of
         Just Fire -> do
             dmg <- roll (6 `d` 6)
             let p    = player lvl
                 ms   = L.filter (\m -> withinFov lvl p (at m)) (mobs lvl)
             insertEvent (CastFire m dmg)
             mapM_ (\m -> insertEvent (Damaged p m dmg)) ms
         Just Lightning -> do
             dmg <- roll (6 `d` 6)
             let p    = player lvl
                 ms   = L.filter (\m -> withinFov lvl p (at m)) (mobs lvl)
             insertEvent (CastLightning m dmg)
             mapM_ (\m -> insertEvent (Damaged p m dmg)) ms
         Just Teleport     -> do
            p <- randomPassable lvl
            insertEvents $ maybeToList (Teleported m <$> p)
         Just Mapping      -> insertEvent (Mapped lvl)
         Just Telepathy    -> insertEvent (GainedTelepathy m)
         otherwise         -> return ()


