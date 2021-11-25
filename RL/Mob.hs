module RL.Mob (module RL.Mob, module RL.Types, module RL.Item) where

import RL.Item
import RL.Types
import RL.Util (enumerate1)

import Data.List (find, sort)
import Data.Maybe (catMaybes, maybeToList, isJust, fromJust)

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
    identified     :: [ItemType]
}
data MobFlag = Sleeping | Invisible | BlindedF | ConfusedF | TelepathicF | Undead | Resting deriving (Eq, Show)

data MobEquipment = MobEquipment {
    wielding :: Maybe Item,
    wearing :: Maybe Item,
    launcher :: Maybe Item,
    shield :: Maybe Item
}
equipmentToList eq = catMaybes [wielding eq, launcher eq, wearing eq, shield eq]

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

isTelepathic :: Mob -> Bool
isTelepathic m = TelepathicF `elem` flags m

isVisible :: Mob -> Bool
isVisible m = not (Invisible `elem` flags m)

isUndead :: Mob -> Bool
isUndead m = Undead `elem` flags m

isResting :: Mob -> Bool
isResting m = Resting `elem` flags m

-- does a simple foldr over the equipped armor, subtracting each of its defense
-- from the default AC of the Mob (default to 10 in AD&D)
mobAC :: Mob -> AC
mobAC m = foldr (\i ac -> ac - defense i) (baseAC m) . catMaybes . map armorProperties $ catMaybes [wearing (equipment m), shield (equipment m)]

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
    equipment = MobEquipment (findItemByName "Mace" weapons) (findItemByName "Leather Armor" armors) Nothing Nothing
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
    identified = [],
    equipment = MobEquipment Nothing Nothing Nothing Nothing
    -- TODO DR & weaknesses
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

insertMob :: [Mob] -> Mob -> [Mob]
insertMob ms m = m { mobId = maxId + 1 } : ms
    where maxId = if not (null ms) then last (sort (map mobId ms)) else 0

findMob :: Int -> [Mob] -> Maybe Mob
findMob n = find ((n==) . mobId)

-- -- moves mob, resetting the destination if we have reached it
-- moveMob :: Point -> Mob -> Mob
-- moveMob p m = let dest = if destination m == Just p then Nothing else destination m
--               in  m { at = p, destination = dest }
moveMob :: Point -> Mob -> Mob
moveMob p m = m { at = p }

isShielded :: Mob -> Bool
isShielded m = isJust (shield (equipment m))

handsFull :: Mob -> Bool
handsFull m = (maybe False isTwoHanded . wielding $ equipment m) || (maybe False isTwoHanded . launcher $ equipment m)

isSneaky :: Mob -> Bool
isSneaky m = let f = (=="Leather Armor") . itemDescription
             in  maybe True f $ wearing (equipment m)

