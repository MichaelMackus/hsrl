module RL.Event where

import RL.Map
import RL.Util (takeWhiles, dropWhiles)
import qualified Data.List as L

-- Represents Game events

data Event = Damaged Mob Mob Int | Missed Mob Mob | Crit Mob Mob | Died Mob | Moved Mob Point | StartedResting Mob | StoppedResting Mob | FailedRest Mob
    | MissileInterrupted Mob | ReadiedProjectile Mob Item | ThrownProjectile Mob Item Point | FiredProjectile Mob Item Item Point | TargetChanged Mob Point
    | Drank Mob Item | Healed Mob Int | GainedLife Mob Int | DrankAcid Mob | GainedStrength Mob Int | SpedUp Mob Int | Slowed Mob Int | Vanished Mob | Appeared Mob | Confused Mob | Sobered Mob | Blinded Mob | Unblinded Mob
    | Read Mob Item | CastFire Mob Int | CastLightning Mob Int | Teleported Mob Point | Mapped DLevel | GainedTelepathy Mob
    | DestinationSet Mob Point | DestinationAbrupted Mob Point
    | StairsTaken VerticalDirection DLevel | StairsSeen VerticalDirection
    | Waken Mob | Slept Mob | MobSeen Mob Mob | MobHeard Mob Mob | MobSpawned Mob
    | FeatureInteracted Point Feature | BandageApplied Mob
    | ItemSpawned Point Item | ItemsSeen [Item] | ItemPickedUp Mob Item | Equipped Mob Item | EquipmentRemoved Mob Item | EndOfTurn | NewGame
    | MenuChange Menu | QuitGame | Escaped | Saved deriving (Eq, Show)

-- TODO separate event types
-- -- event that updates game state
-- data UpdateEvent = Damaged Mob Mob Int | Missed Mob Mob | Crit Mob Mob | Died Mob | Moved Mob Point | StairsTaken VerticalDirection DLevel
--     | ReadiedProjectile Mob Item | ThrownProjectile Mob Item Point | FiredProjectile Mob Item Item Point | TargetChanged Mob Point
--     | Healed Mob Int | GainedMaxHP Mob Int | GainedStrength Mob Int | GainedFlag MobFlag | LostFlag MobFlag
--     -- Equipment
--     | ItemPickedUp Mob Item | ItemIdentified Mob Item | Equipped Mob Item | EquipmentRemoved Mob Item
--     | Drank Mob Item | Read Mob Item 
--     -- AI & automation
--     | DestinationSet Mob Point | DestinationAbrupted Mob Point | MobSeen Mob Mob | MobHeard Mob Mob | MobSpawned Mob
--     -- misc. game state
--     | TilesSeen [Point] DLevel | MenuChange Menu | QuitGame | EndOfTurn | NewGame deriving (Eq, Show)
-- event that only results in a message shown to player
-- data MessageEvent = StairsSeen VerticalDirection | ItemsSeen [Item] | Drank Mob Item | Read Mob Item 

data Menu = Inventory | NoMenu | ProjectileMenu | TargetMenu deriving (Eq, Show)

getEventsAfterTurns :: Int -> [Event] -> [Event]
getEventsAfterTurns n = takeWhiles ((< n) . length . filter isEndOfTurn)

getEventsBeforeTurns :: Int -> [Event] -> [Event]
getEventsBeforeTurns n = dropWhiles ((< n) . length . filter isEndOfTurn)

getEventsThisTurn :: [Event] -> [Event]
getEventsThisTurn = L.takeWhile (not . isEndOfTurn)

turnsSince :: (Event -> Bool) -> [Event] -> Int
turnsSince f = length . L.filter isEndOfTurn . takeWhile (not . f)

isEndOfTurn :: Event -> Bool
isEndOfTurn EndOfTurn = True
isEndOfTurn otherwise = False

isAttacked :: Event -> Bool
isAttacked (Damaged _ _ _) = True
isAttacked (Missed  _ _  ) = True
isAttacked otherwise       = False
