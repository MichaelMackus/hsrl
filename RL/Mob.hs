module RL.Mob (module RL.Mob, module RL.Types) where

import RL.Types

-- player/mobs
type HP = Int

-- this data structure is for a mobile creature
data Mob = Mob {
    mobId  :: Int,
    symbol :: Char,
    at     :: Point,
    hp     :: HP,
    dmgd   :: Dice
}

type Player  = Mob
-- data Event a = Won | Lost | Playing a

-- attack :: Mob -> Dice -> Game Event
-- attack m d = do

-- player :: Game Player
-- moveTo :: Point -> Game Event

-- dungeon :: Game s Dungeon
-- dungeon = runStateT

instance Eq Mob where
    m == m' = mobId m == mobId m'

-- configure default player
mkPlayer :: HP -> Dice -> Point -> Player
mkPlayer hp d at = Mob {
    mobId  = 0,
    symbol = '@',
    hp = hp,
    dmgd = d,
    at = at
}

-- helper functions for mob management

filterMobs :: [Mob] -> [Mob]
filterMobs = filter $ (> 0) . hp

-- move a mob by offset
moveMob :: Point -> Mob -> Mob
moveMob off m = m { at = addOffset off (at m) }

-- move a mob to exact point
moveMobTo :: Point -> Mob -> Mob
moveMobTo xy m = m { at = xy }

-- this adds a (+x, +y) offset to (x, y) map coordinate
addOffset :: Point -> Point -> Point
addOffset (offx, offy) (x, y) = (offx + x, offy + y)

-- initializes mob IDs
withMobIds :: [Mob] -> [Mob]
withMobIds = map withMobId . enumerateId
    where
        withMobId   (id, m) = if mobId m == 0 then m { mobId = id } else m
        enumerateId         = zip [0..]
