module RL.Client.Time (end) where

-- represents ticks in time
--
-- does things like spawning mobs, cleanup dead mobs, heal wounds, etc.

import RL.Client
import RL.Game
import RL.State
import RL.Random
import RL.Generator
import RL.Generator.Mobs

import Control.Monad (replicateM_, when)
import Data.Maybe (isNothing, fromJust)

data Time = Ticks Int Env

-- end of 1 turn
-- TODO pass this mobGenerator?
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

        -- spawn new mobs
        let maxMobs = 5              -- TODO make this configurable
            conf = GenConfig 80 15 1 -- TODO make this configurable
        r <- roll (1 `d` 10)         -- 10% chance to spawn new mob
        when (length healed < maxMobs && r == 1) $ do
            max <- getRandomR (length healed + 1, maxMobs)
            g   <- getSplit
            lvl <- getLevel
            let s = mkGenState lvl g
                (spawned, _) = runGenerator (mobGenerator maxMobs) conf s
            setMobs spawned

healDamaged :: [Mob] -> Int -> Mob -> Mob
healDamaged prev amt m
        | hp m + amt > mhp m = m                 -- can't heal more than max
        | isNothing (findMob (mobId m) prev) = m -- new mob
        | otherwise = ifNDamaged heal m (fromJust (findMob (mobId m) prev))
    where
        ifNDamaged f m m' = if hp m == hp m' then f m else m
        heal m = m { hp = hp m + amt }
