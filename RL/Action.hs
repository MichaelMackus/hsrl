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
attack :: MonadRandom r => Mob -> Maybe Item -> Mob -> r [Event]
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
        let e = Damaged attacker target dmg
            critE = if crit then [Crit attacker target] else []
            deadE = if isDead (target { hp = hp target - dmg }) then [Died target] else []
        return $ [e] ++ critE ++ deadE
    else
        return [Missed attacker target]

applyItem :: MonadRandom r => DLevel -> Mob -> Item -> r [Event]
applyItem lvl m i =
    if isEquippable i then return (equip m i)
    else if isDrinkable i then drinkPotion m i
    else if isReadable i then readScroll lvl m i
    else if (itemType i == Bandage) then applyBandage m
    else if (itemType i == Draught) then drinkDraught m i
    else return []

applyBandage :: MonadRandom r => Mob -> r [Event]
applyBandage m = do
    if hp m < mhp m then do
        healed <- roll (1 `d` 4)
        return [BandageApplied m, Healed m healed]
    else return []

drinkDraught :: MonadRandom r => Mob -> Item -> r [Event]
drinkDraught m i = do
    healed <- roll (1 `d` 6)
    return [Drank m i, Healed m healed]

-- fire projectile toward the mob's target
-- TODO unable to fire in melee
fire :: MonadRandom r => DLevel -> Mob -> Item -> Mob -> r [Event]
fire lvl attacker proj m =
    if isProjectile proj then
        let eqp = equipment attacker
            isLaunching = maybe False (`launchesProjectile` proj) (launcher eqp)
        -- fire projectile if proper launcher equipped
        in if isLaunching then do
               atkE <- attack attacker (launcher eqp) m
               return $ [FiredProjectile attacker (fromJust (launcher eqp)) proj (at m)] ++ atkE
           else do
               atkE <- attack attacker (Just proj) m
               return $ [ThrownProjectile attacker proj (at m)] ++ atkE
    else return []

equip :: Mob -> Item -> [Event]
-- TODO why is this removing armor?
equip m i = let wield  = L.filter isTwoHanded (catMaybes [wielding (equipment m), launcher (equipment m)])
                shld   = fromJust (shield (equipment m))
                wieldE = if isTwoHanded i && isShielded m then [EquipmentRemoved m shld] else []
                shldE  = if isShield    i && handsFull  m then map (EquipmentRemoved m) wield else []
             in wieldE ++ shldE ++ [Equipped m i]

drinkPotion :: MonadRandom r => Mob -> Item -> r [Event]
drinkPotion m i = do
    e <- case potionType i of
             Just Healing -> do
                 healed <- roll (1 `d` 8)
                 return [Healed m healed]
             Just Life -> do
                 healed <- roll (1 `d` 8)
                 return [GainedLife m healed]
             Just Acid -> do
                 dmg <- roll (1 `d` 6)
                 let diedE = if isDead (m { hp = hp m - dmg }) then [Died m] else []
                 return ([DrankAcid m, Damaged m m dmg] ++ diedE)
             Just Strength     -> return [GainedStrength m 1]
             Just Invisibility -> return [Vanished m]
             Just Confusion    -> return [Confused m]
             Just Darkness     -> return [Blinded m]
             otherwise -> return []
    return ([Drank m i] ++ e)

-- TODO change to be independent of player/mob
readScroll :: MonadRandom r => DLevel -> Mob -> Item -> r [Event]
readScroll lvl m i = do
    e <- case scrollType i of
             Just Fire -> do
                 dmg <- roll (6 `d` 6)
                 let p    = player lvl
                     ms   = L.filter (\m -> withinFov lvl p (at m)) (mobs lvl)
                     dmgE = map (\m -> Damaged p m dmg) ms
                 return (CastFire m dmg:dmgE)
             Just Lightning -> do
                 dmg <- roll (6 `d` 6)
                 let p    = player lvl
                     ms   = L.filter (\m -> withinFov lvl p (at m)) (mobs lvl)
                     dmgE = map (\m -> Damaged p m dmg) ms
                 return (CastLightning m dmg:dmgE)
             Just Teleport     -> do
                p <- randomPassable lvl
                return $ maybeToList (Teleported m <$> p)
             Just Mapping      -> return [Mapped lvl]
             Just Telepathy    -> return [GainedTelepathy m]
             otherwise -> return []
    return ([Read m i] ++ e)


