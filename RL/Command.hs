module RL.Command where

import RL.Game
import RL.State
import RL.Random

import Control.Monad (when)

-- commands, which produce events
class Command c where
    dispatch :: c -> Game ()

data AttackCommand = AttackMob Mob | AttackPlayer Player | Attack Mob Mob

instance Command AttackCommand where
    dispatch (AttackMob    m) = getPlayer >>= (\p -> dispatch (Attack p m))
    dispatch (AttackPlayer m) = getPlayer >>= (\p -> dispatch (Attack m p))

    -- any mob attacking another mob
    dispatch (Attack attacker target) = when (not (isDead target)) $ do
            dmg     <- roll (dmgd attacker)
            target' <- hurtMob target dmg
            when (isDead target') (send (Died target'))
            send (Attacked attacker target' dmg)
        where
            -- hurt    mob    dmg
            hurtMob :: Mob -> Int -> Game Mob
            hurtMob target dmg = do
                    p  <- getPlayer
                    if target == p then do
                        let p' = hurtMob dmg p
                        setPlayer p'
                        return p'
                    else do
                        ms <- getMobs
                        setMobs (map (hurtMob dmg) ms)
                        return target'
                where
                    hurtMob  dmg m = if m == target then target' else m
                    target' = target { hp = (hp target) - dmg }

