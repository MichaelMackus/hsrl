module RL.Event where

import RL.Map
import RL.Util (takeWhiles, dropWhiles)

import Data.Maybe (isNothing, isJust)
import qualified Data.List as L

data Event = EventMessage Message | GameUpdate GameEvent deriving (Eq, Show)

-- Informational messages displayed to user
data Message = ItemsSeen [Item] | StairsSeen VerticalDirection | InMelee | MenuChange Menu | Readied Item 
    | PlayerRested | PlayerInDanger
    | AttackOfOpportunity Mob Mob | PlayerRetreated Mob deriving (Eq, Show)
data Menu = Inventory | NoMenu | ProjectileMenu | TargetMenu | DropMenu deriving (Eq, Show)

-- Represents events that change the game state
data GameEvent = Damaged Mob Mob Int | Missed Mob Mob | Crit Mob Mob | Died Mob | Moved Mob Point
    | ThrownProjectile Mob Item Point | FiredProjectile Mob Item Item Point
    | Drank Mob Item | Healed Mob Int | GainedLife Mob Int | DrankAcid Mob | GainedStrength Mob Int | SpedUp Mob Int | Slowed Mob Int
    | GainedMobFlag Mob MobFlag | RemovedMobFlag Mob MobFlag
    | Read Mob Item | CastFire Mob Int | CastLightning Mob Int | Teleported Mob Point
    | StairsTaken VerticalDirection DLevel
    | Waken Mob | Slept Mob | MobSpawned Mob
    | FeatureInteracted Point Feature | BandageApplied Mob
    | ItemSpawned Point Item | ItemPickedUp Mob Item | ItemDropped Mob Item | Equipped Mob Item | EquipmentRemoved Mob Item
    | GainedLevel Mob Int
    | EndOfTurn | NewGame | QuitGame | Escaped | Saved deriving (Eq, Show)

getEventsAfterTurns :: Int -> [Event] -> [Event]
getEventsAfterTurns n = takeWhiles ((< n) . length . filter isEndOfTurn)

getEventsBeforeTurns :: Int -> [Event] -> [Event]
getEventsBeforeTurns n = drop 1 . dropWhiles ((< n) . length . filter isEndOfTurn)

getEventsThisTurn :: [Event] -> [Event]
getEventsThisTurn = L.takeWhile (not . isEndOfTurn)

filterEventsThisTurn :: (Event -> Bool) -> [Event] -> [Event]
filterEventsThisTurn f = L.filter f . getEventsThisTurn

occurredThisTurn :: (Event -> Bool) -> [Event] -> Bool
occurredThisTurn f = not . null . filterEventsThisTurn f

occurredLastTurn :: (Event -> Bool) -> [Event] -> Bool
occurredLastTurn f = not . null . filterEventsThisTurn f . getEventsBeforeTurns 1

turnsSince :: (Event -> Bool) -> [Event] -> Int
turnsSince f = length . L.filter isEndOfTurn . takeWhile (not . f)

isEndOfTurn :: Event -> Bool
isEndOfTurn (GameUpdate EndOfTurn) = True
isEndOfTurn otherwise              = False

isAttacked :: Event -> Bool
isAttacked (GameUpdate (Damaged _ _ _)) = True
isAttacked (GameUpdate (Missed  _ _  )) = True
isAttacked otherwise                    = False

isMoved :: Event -> Bool
isMoved (GameUpdate (Moved _ _)) = True
isMoved otherwise                = False

tookStairs :: Event -> Bool
tookStairs (GameUpdate (StairsTaken _ _)) = True
tookStairs otherwise                      = False

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

-- check if mob died this turn
mobDied :: [Event] -> Mob -> Bool
mobDied es m = let f (GameUpdate (Died m')) = m == m'
                   f otherwise            = False
               in  occurredThisTurn f es
