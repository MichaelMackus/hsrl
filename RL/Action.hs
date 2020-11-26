module RL.Action where

import RL.Event
import RL.Mob
import RL.Random

import Data.Maybe (fromMaybe)

attack :: MonadRandom r => Mob -> Mob -> r [Event]
attack attacker target = do
    let weap = weaponProperties =<< wielding (equipment attacker)
    -- attack roll
    atk <- roll (1 `d` 20)
    if atk + (fromMaybe 0 (bonus <$> weap)) >= thac0 attacker - mobAC target then do
        -- hit!
        dmg <- roll (maybe (baseDmg attacker) dmgd weap)
        let e = Damaged attacker target dmg
        if isDead (target { hp = hp target - dmg }) then
            return [e, Died target]
        else
            return [e]
    else
        return [Missed attacker target]
