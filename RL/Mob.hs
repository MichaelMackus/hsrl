module RL.Mob (module RL.Mob, module RL.Types, module RL.Item) where

import RL.Item
import RL.Types
import RL.Util (enumerate1)

import Data.List (find)
import Data.Maybe (catMaybes, maybeToList)

-- player/mobs
type HP     = Int
type Radius = Double

-- this data structure is for a mobile creature
data Mob = Mob {
    mobId          :: Int,
    mobName        :: String,
    symbol         :: Char,
    at             :: Point,
    hp             :: HP,
    mhp            :: HP,
    baseDmg        :: Dice,
    baseAC         :: AC,   -- default to 10 in AD&D
    thac0          :: Int,  -- traditional D&D combat rules
    strength       :: Int,  -- bonus to damage
    fov            :: Radius,
    hearing        :: Radius,
    flags          :: [MobFlag],
    inventory      :: [Item],
    equipment      :: MobEquipment,
    destination    :: Maybe Point, -- destination point
    isTelepathicOn :: [Int] -- dungeon levels mob is telepathic (can see all mobs) on
}
data MobFlag = Sleeping | Invisible | BlindedF | ConfusedF deriving Eq

data MobEquipment = MobEquipment {
    wielding :: Maybe Item,
    wearing :: [Item]
}
equipmentToList eq = maybeToList (wielding eq) ++ wearing eq

instance Eq Mob where
    m == m' = mobId m == mobId m'
instance Show Mob where
    show = mobName

type Player = Mob

canAttack :: Mob -> Bool
canAttack = canMove

canMove :: Mob -> Bool
canMove m = not (isDead m) && length (filter (== Sleeping) (flags m)) == 0

isSleeping :: Mob -> Bool
isSleeping m = Sleeping `elem` flags m

isBlinded :: Mob -> Bool
isBlinded m = BlindedF `elem` flags m

isConfused :: Mob -> Bool
isConfused m = ConfusedF `elem` flags m

isVisible :: Mob -> Bool
isVisible m = not (Invisible `elem` flags m)

-- does a simple foldr over the equipped armor, subtracting each of its defense
-- from the default AC of the Mob (default to 10 in AD&D)
mobAC :: Mob -> AC
mobAC m = foldr (\i ac -> ac - defense i) (baseAC m) . catMaybes . map armorProperties . wearing . equipment $ m

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
    equipment = MobEquipment (findItemByName "Mace" weapons) (maybeToList $ findItemByName "Leather Armor" armors)
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
    thac0 = 19,
    strength = 0,
    fov = 5,
    hearing = 10.0,
    flags = [],
    inventory = [],
    equipment = MobEquipment Nothing [],
    destination = Nothing,
    isTelepathicOn = []
}

-- helper functions for mob management

isDead :: Mob -> Bool
isDead m = hp m <= 0

aliveMobs :: [Mob] -> [Mob]
aliveMobs = filter (not . isDead)

deadMobs :: [Mob] -> [Mob]
deadMobs = filter isDead

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

-- moves mob, resetting the destination if we have reached it
moveMob :: Point -> Mob -> Mob
moveMob p m = let dest = if destination m == Just p then Nothing else destination m
              in  m { at = p, destination = dest }
