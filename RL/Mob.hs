module RL.Mob (Mob(..), Player(..), Point(..), filterMobs, moveMob, moveMobTo, addOffset ) where

import RL.Dice

-- player/mobs
type HP = Int

-- this data structure is for a mobile creature
data Mob = Mob {
    symbol :: Char,
    at     :: Point,
    hp     :: HP,
    dmgd   :: Dice
}

-- todo use mob identifier
instance Eq Mob where
    m == m' = symbol m == symbol m'

type Point = (Int, Int) -- represents an x, y coordinate on the map
type Player = Mob       -- our player is just an alias for mob

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
