module RL.Mob (Mob(..), Player(..), Point(..), filterMobs, moveMob, moveMobTo, addOffset ) where

-- player/mobs

type HP = Int

-- this data structure is for a mobile creature
data Mob = Mob {
    symbol :: Char,
    at     :: Point,
    hp     :: HP
}

type Player = Mob

filterMobs :: [Mob] -> [Mob]
filterMobs = filter $ (> 0) . hp

-- points on map used for mob & tile locations

type Point = (Int, Int)                 -- represents an x, y coordinate on the map

-- move a mob
moveMob :: Point -> Mob -> Mob
moveMob off m = Mob { symbol = symbol m, at = addOffset off (at m), hp = hp m }

moveMobTo :: Point -> Mob -> Mob
moveMobTo xy m = Mob { symbol = symbol m, at = xy, hp = hp m }

-- this adds a (+x, +y) offset to (x, y) map coordinate
addOffset :: Point -> Point -> Point
addOffset (offx, offy) (x, y) = (offx + x, offy + y)
