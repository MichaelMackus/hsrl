module RL.Event where

import RL.Dungeon
import RL.Util (takeWhiles, dropWhiles)

import Data.Maybe (isNothing, isJust)
import qualified Data.List as L

data Event = EventMessage Message | GameUpdate GameEvent deriving Eq

-- Informational messages displayed to user
data Message = ItemsSeen [Item] | StairsSeen VerticalDirection | InMelee | MenuChange Menu | Readied Item | Hostiles deriving Eq
data Menu = Inventory | NoMenu | ProjectileMenu | TargetMenu deriving (Eq, Show)
type Day = Int

-- Represents events that change the game state
data GameEvent = Damaged Mob Mob Int | Missed Mob Mob | Crit Mob Mob | Died Mob | Moved Mob Point
    | ThrownProjectile Mob Item Point | FiredProjectile Mob Item Item Point
    | Drank Mob Item | Healed Mob Int | GainedLife Mob Int | DrankAcid Mob | GainedStrength Mob Int | SpedUp Mob Int | Slowed Mob Int
    | GainedMobFlag Mob MobFlag | RemovedMobFlag Mob MobFlag
    | Read Mob Item | CastFire Mob Int | CastLightning Mob Int | Teleported Mob Point
    | StairsTaken VerticalDirection DLevel
    | Waken Mob | Slept Mob | MobSpawned Mob
    | FeatureInteracted Point Feature | BandageApplied Mob
    | Rested Player Day
    | ItemSpawned Point Item | ItemPickedUp Mob Item | Equipped Mob Item | EquipmentRemoved Mob Item
    | EndOfTurn | NewGame | QuitGame | Escaped | Saved deriving (Eq, Show)

getEventsAfterTurns :: Int -> [Event] -> [Event]
getEventsAfterTurns n = takeWhiles ((< n) . length . filter isEndOfTurn)

getEventsBeforeTurns :: Int -> [Event] -> [Event]
getEventsBeforeTurns n = dropWhiles ((< n) . length . filter isEndOfTurn)

getEventsThisTurn :: [Event] -> [Event]
getEventsThisTurn = L.takeWhile (not . isEndOfTurn)

occurredThisTurn :: (Event -> Bool) -> [Event] -> Bool
occurredThisTurn f es = let es' = L.takeWhile (not . f) es
                        in  isJust (L.find f es) && isNothing (L.find isEndOfTurn es')

turnsSince :: (Event -> Bool) -> [Event] -> Int
turnsSince f = length . L.filter isEndOfTurn . takeWhile (not . f)

isEndOfTurn :: Event -> Bool
isEndOfTurn (GameUpdate EndOfTurn) = True
isEndOfTurn otherwise              = False

isAttacked :: Event -> Bool
isAttacked (GameUpdate (Damaged _ _ _)) = True
isAttacked (GameUpdate (Missed  _ _  )) = True
isAttacked otherwise                    = False

-- check if mob recently moved to this tile
recentlyMoved :: Mob -> [Event] -> Bool
recentlyMoved m es = let f (GameUpdate (Moved m' _)) = m == m'
                         f otherwise                 = False
                     in  occurredThisTurn f es

-- check if recently picked up on this tile
recentlyPicked :: Mob -> [Event] -> Bool
recentlyPicked m es = let f (GameUpdate (ItemPickedUp m' _)) = m == m'
                          f otherwise                        = False
                      in  occurredThisTurn f es

-- check if this is a new game since player last moved
recentGame :: [Event] -> Bool
recentGame es = let f (GameUpdate NewGame) = True
                    f otherwise            = False
                in  occurredThisTurn f es
