module RL.Action where

import RL.Event
import RL.Game
import RL.Random

import Data.Maybe (fromMaybe)
import qualified Data.List as L

attack :: MonadRandom r => Mob -> Mob -> r [Event]
attack attacker target = do
    let weap = weaponProperties =<< wielding (equipment attacker)
    -- attack roll
    atk <- roll (1 `d` 20)
    if atk + (fromMaybe 0 (bonus <$> weap)) >= thac0 attacker - mobAC target then do
        -- hit!
        dmg <- (+ strength attacker) <$> roll (maybe (baseDmg attacker) dmgd weap)
        let e = Damaged attacker target dmg
        if isDead (target { hp = hp target - dmg }) then
            return [e, Died target]
        else
            return [e]
    else
        return [Missed attacker target]

applyItem :: MonadRandom r => DLevel -> Mob -> Item -> r [Event]
applyItem lvl m i =
    if isEquippable i then return [Equipped m i]
    else if isDrinkable i then drinkPotion m i
    else if isReadable i then readScroll lvl m i
    else return []

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
                 return [DrankAcid m, Damaged m m dmg]
             Just Strength     -> return [GainedStrength m 1]
             Just Invisibility -> return [Vanished m]
             Just Confusion    -> return [Confused m]
             Just Darkness     -> return [Blinded m]
             otherwise -> return []
    return ([Drank m i] ++ e)

readScroll :: MonadRandom r => DLevel -> Mob -> Item -> r [Event]
readScroll lvl m i = do
    e <- case scrollType i of
             Just Fire -> do
                 dmg <- roll (6 `d` 6)
                 let p    = player lvl
                     ms   = L.filter (\m -> canSee lvl p (at m)) (mobs lvl)
                     dmgE = map (\m -> Damaged p m dmg) ms
                 return (CastFire m dmg:dmgE)
             Just Lightning -> do
                 dmg <- roll (6 `d` 6)
                 let p    = player lvl
                     ms   = L.filter (\m -> canSee lvl p (at m)) (mobs lvl)
                     dmgE = map (\m -> Damaged p m dmg) ms
                 return (CastLightning m dmg:dmgE)
             Just Teleport     -> do
                 p <- randomPassable lvl
                 return [Teleported m p]
             Just Mapping      -> return [Mapped lvl]
             Just Telepathy    -> return [GainedTelepathy lvl]
             otherwise -> return []
    return ([Read m i] ++ e)


-- generates random passable point
randomPassable :: MonadRandom m => DLevel -> m Point
randomPassable lvl = do
    p <- randomPoint (mapWidth lvl) (mapHeight lvl)
    let t = findTileAt p lvl
    if maybe False isPassable t then return p
    else randomPassable lvl

