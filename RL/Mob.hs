module RL.Mob (module RL.Mob, module RL.Types) where

import RL.Types
import RL.Util (enumerate1)

-- player/mobs
type HP     = Int
type Radius = Double

-- this data structure is for a mobile creature
data Mob = Mob {
    mobId   :: Int,
    mobName :: String,
    symbol  :: Char,
    at      :: Point,
    hp      :: HP,
    dmgd    :: Dice,
    fov     :: Radius
}

instance Eq Mob where
    m == m' = mobId m == mobId m'

type Player = Mob

-- configure default player
mkPlayer :: HP -> Dice -> Point -> Radius -> Player
mkPlayer hp d at fov = Mob {
    mobId  = 0,
    mobName = "Player", -- TODO configurable name
    symbol = '@',
    hp = hp,
    dmgd = d,
    at = at,
    fov = fov
}

-- helper to prevent uninitialized fields
mob :: Mob
mob = Mob {
    mobId = -1,
    mobName = "",
    symbol = 'z',
    at = (-1,-1),
    hp = 0,
    dmgd = 1 `d` 2,
    fov = 5
}

-- helper functions for mob management

isDead :: Mob -> Bool
isDead m = hp m <= 0

aliveMobs :: [Mob] -> [Mob]
aliveMobs = filter (not . isDead)

deadMobs :: [Mob] -> [Mob]
deadMobs = filter isDead

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
withMobIds = map withMobId . enumerate1
    where
        withMobId   (id, m) = if mobId m == (-1) then m { mobId = id } else m
