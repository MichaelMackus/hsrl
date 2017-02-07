module RL.Client.Time (end) where

-- represents ticks in time
--
-- does things like spawning mobs, cleanup dead mobs, heal wounds, etc.

import RL.Client
import RL.Game
import RL.State

import Control.Monad (replicateM_)
import Data.Maybe (isNothing, fromJust)

data Time = Ticks Int Env

-- end of 1 turn
end startEnv = Ticks 1 startEnv

instance Client Time where
    tick (Ticks i start) = replicateM_ i $ do
        -- cleanup dead mobs
        ms <- aliveMobs <$> getMobs
        setMobs ms

        -- TODO use Events instead of Messages to detect changes in state

        -- heal wounds if not attacked
        ms' <- allMobs <$> getLevel
        let healed = map (healDamaged (allMobs (level start)) (floor (1 / fromIntegral i))) ms'
        setMobs healed

        -- TODO spawn new mobs

healDamaged :: [Mob] -> Int -> Mob -> Mob
healDamaged prev amt m
        | hp m + amt > mhp m = m                 -- can't heal more than max
        | isNothing (findMob (mobId m) prev) = m -- new mob
        | otherwise = ifNDamaged heal m (fromJust (findMob (mobId m) prev))
    where
        ifNDamaged f m m' = if hp m == hp m' then f m else m
        heal m = m { hp = hp m + amt }
