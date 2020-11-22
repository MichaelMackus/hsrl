module RL.Mob (module RL.Mob, module RL.Types, module RL.Item) where

import RL.Item
import RL.Types
import RL.Util (enumerate1)

import Data.List (find)
import Data.Maybe (listToMaybe, catMaybes)

-- player/mobs
type HP     = Int
type Radius = Double

-- this data structure is for a mobile creature
data Mob = Mob {
    mobId     :: Int,
    mobName   :: String,
    symbol    :: Char,
    at        :: Point,
    hp        :: HP,
    mhp       :: HP,
    baseDmg   :: Dice,
    baseAC    :: AC,   -- default to 10 in AD&D
    thac0     :: Int,  -- traditional D&D combat rules
    fov       :: Radius,
    hearing   :: Radius,
    flags     :: [MobFlag],
    inventory :: [Item],
    equipment :: [Item],
    mobPath   :: [Point] -- current path for AI pathfinding
}

data MobFlag = Sleeping deriving Eq

instance Eq Mob where
    m == m' = mobId m == mobId m'

type Player = Mob

canAttack :: Mob -> Bool
canAttack = canMove

canMove :: Mob -> Bool
canMove m = not (isDead m) && length (filter isSleeping (flags m)) == 0

isSleeping :: MobFlag -> Bool
isSleeping (Sleeping) = True
isSleeping otherwise  = False

getWielding :: Mob -> Maybe Item
getWielding m =
    let weapons = filter isWeapon (equipment m)
    in  listToMaybe weapons

getEquipped :: Mob -> [Item]
getEquipped m = filter isArmor (equipment m)

-- does a simple foldr over the equipped armor, subtracting each of its defense
-- from the default AC of the Mob (default to 10 in AD&D)
mobAC :: Mob -> AC
mobAC m = foldr (\i ac -> ac - defense i) (baseAC m) . catMaybes . map armorProperties . getEquipped $ m

isWeapon :: Item -> Bool
isWeapon (Item _ (Weapon _)) = True
isWeapon otherwise           = False

isArmor :: Item -> Bool
isArmor (Item _ (Armor _)) = True
isArmor otherwise          = False

-- configure default player
mkPlayer :: HP -> Point -> Radius -> Player
mkPlayer hp at fov = mob {
    mobId  = 0,
    mobName = "Player", -- TODO configurable name
    symbol = '@',
    hp = hp,
    mhp = hp,
    at = at,
    fov = fov,
    equipment = [mace, leather]
}

isPlayer :: Mob -> Bool
isPlayer m = mobId m == 0

-- helper to prevent uninitialized fields
mob :: Mob
mob = Mob {
    mobId = -1,
    mobName = "",
    symbol = 'z',
    at = (-1,-1),
    hp = 0,
    mhp = 0,
    baseDmg = 1 `d` 3,
    baseAC = 10,
    thac0 = 20,
    fov = 5,
    hearing = 10.0,
    flags = [],
    inventory = [],
    equipment = [],
    mobPath = []
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
        -- prevent re-indexing player
        withMobId   (id, m) = if mobId m /= 0 then m { mobId = id } else m

findMob :: Int -> [Mob] -> Maybe Mob
findMob n = find ((n==) . mobId)
