module RL.Client.Time (end) where

-- represents ticks in time
--
-- does things like spawning mobs, cleanup dead mobs, heal wounds, etc.

import RL.Client
import RL.Event
import RL.Game
import RL.State
import RL.Random
import RL.Generator
import RL.Generator.Mobs

import Control.Monad (replicateM_, when)

data Time = Ticks Int

-- end of 1 turn
end = Ticks 1

instance Client Time where
    tick (Ticks i) = replicateM_ i $ do
        -- cleanup dead mobs
        ms <- aliveMobs <$> getMobs
        setMobs ms

        -- heal wounds if not attacked
        -- FIXME not properly doing anything
        ms' <- allMobs <$> getLevel
        evs <- getEvents
        let healed = map (healDamaged evs (floor (1 / fromIntegral i))) ms'
        setMobs healed

        -- spawn new mobs
        let maxMobs   = 10   -- TODO make this configurable
            maxMTries = 5    -- TODO make this configurable
        r <- roll (1 `d` 10) -- 10% chance to spawn new mob
        when (length healed < maxMobs && r == 1) $ do
            g   <- getSplit
            lvl <- getLevel
            let s = mkGenState lvl g
                -- TODO save config somewhere..
                (spawned, _) = runGenerator mobGenerator (MobConfig (length healed + 1) maxMTries (2,0)) s
            setMobs spawned

        -- mark as end of turn
        send EndOfTurn

-- heal damaged if mob not damaged 5 turns ago
healDamaged :: [Event] -> Int -> Mob -> Mob
healDamaged events amt m
    | hp m + amt > mhp m = m                     -- can't heal more than max
    | length (filter isEndOfTurn events) < 5 = m -- hasn't been 5 turns
    | otherwise =
        let events' = getEventsNTurns 5 events
        in  if not (isAttacked m events') then
                m { hp = hp m + amt }
            else
                m
