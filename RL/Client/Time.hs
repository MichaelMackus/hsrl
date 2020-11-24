module RL.Client.Time (nextTurn) where

-- represents ticks in time
--
-- does things like spawning mobs, cleanup dead mobs, heal wounds, etc.

import RL.Client
import RL.Client.Input
import RL.Client.AI
import RL.Event
import RL.Game
import RL.State
import RL.Random
import RL.Generator
import RL.Generator.Mobs

import Control.Monad (replicateM_, when)
import Data.Maybe (fromJust)

data Time = Ticks Int Action

nextTurn = Ticks 1 -- end of 1 turn

instance Client Time where
    -- ticks have passed (end of turn)
    tick (Ticks i a) = replicateM_ i . withEnv $ \env ->
        case menu env of
            NoMenu -> do
                tick (UserInput a) -- user movement
                tick AI            -- AI

                -- cleanup dead mobs
                ms <- aliveMobs <$> getMobs
                setMobs ms

                -- heal wounds if not attacked
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

                -- check what is on the current tile
                lvl <- getLevel
                let t = findTileAt (at (player lvl)) lvl 
                when (isDownStair (fromJust t)) $ send (StairsSeen Down)
                when (isUpStair   (fromJust t)) $ send (StairsSeen Up)
                -- check if there are items here
                let is = findItemsAt (at (player lvl)) lvl
                when (length is > 0) $ send (ItemsSeen (show (head is)) (length is))

                -- mark as end of turn
                send EndOfTurn
            otherwise -> tick (MenuInput (menu env) a)

-- heal damaged if mob not damaged 5 turns ago
healDamaged :: [Event] -> Int -> Mob -> Mob
healDamaged events amt m
    | hp m + amt > mhp m = m                     -- can't heal more than max
    | length (filter isEndOfTurn events) < 5 = m -- hasn't been 5 turns
    | otherwise =
        let events' = getEventsAfterTurns 5 events
        in  if not (isAttacked m events') then
                m { hp = hp m + amt }
            else
                m
