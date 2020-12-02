module RL.Event where

import RL.Map
import RL.Util (takeWhiles, dropWhiles)
import qualified Data.List as L

-- Represents Game events

data Event = Attacked Mob Mob | Damaged Mob Mob Int | Missed Mob Mob | Crit Mob Mob | Died Mob | Moved Mob Point
    | Drank Mob Item | Healed Mob Int | GainedLife Mob Int | DrankAcid Mob | GainedStrength Mob Int | SpedUp Mob Int | Slowed Mob Int | Vanished Mob | Appeared Mob | Confused Mob | Sobered Mob | Blinded Mob | Unblinded Mob
    | Read Mob Item | CastFire Mob Int | CastLightning Mob Int | Teleported Mob Point | Mapped DLevel | GainedTelepathy Mob
    | DestinationSet Mob Point | DestinationAbrupted Mob Point
    | StairsTaken VerticalDirection DLevel | StairsSeen VerticalDirection
    | Waken Mob | Slept Mob | MobSeen Mob Mob | MobHeard Mob Mob | MobSpawned Mob
    | ItemsSeen [Item] | ItemPickedUp Mob Item | Equipped Mob Item | EquipmentRemoved Mob Item | EndOfTurn | NewGame
    | MenuChange Menu | QuitGame | Escaped deriving (Eq, Show)

data Menu = Inventory | Equipment | DrinkMenu | ReadMenu | NoMenu deriving (Eq, Show)

getEventsAfterTurns :: Int -> [Event] -> [Event]
getEventsAfterTurns n = takeWhiles ((< n) . length . filter isEndOfTurn)

getEventsBeforeTurns :: Int -> [Event] -> [Event]
getEventsBeforeTurns n = dropWhiles ((< n) . length . filter isEndOfTurn)

turnsSince :: (Event -> Bool) -> [Event] -> Int
turnsSince f = length . L.filter isEndOfTurn . takeWhile (not . f)

isEndOfTurn :: Event -> Bool
isEndOfTurn EndOfTurn = True
isEndOfTurn otherwise = False

isAttacked :: Mob -> [Event] -> Bool
isAttacked x = (> 0) . length . filter isAttacked'
    where
        isAttacked' (Attacked _ x') = x == x'
        isAttacked' otherwise       = False
