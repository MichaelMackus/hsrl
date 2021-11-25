module RL.Event where

import RL.Map
import RL.Util (takeWhiles, dropWhiles)
import qualified Data.List as L

-- Represents Game events

data Event = Damaged Mob Mob Int | Missed Mob Mob | Crit Mob Mob | Died Mob | Moved Mob Point
    | MissileInterrupted Mob | ReadiedProjectile Mob Item | ThrownProjectile Mob Item Point | FiredProjectile Mob Item Item Point | TargetChanged Mob Point
    | Drank Mob Item | Healed Mob Int | GainedLife Mob Int | DrankAcid Mob | GainedStrength Mob Int | SpedUp Mob Int | Slowed Mob Int | Vanished Mob | Appeared Mob | Confused Mob | Sobered Mob | Blinded Mob | Unblinded Mob
    | Read Mob Item | CastFire Mob Int | CastLightning Mob Int | Teleported Mob Point | Mapped DLevel | GainedTelepathy Mob
    -- TODO new monad for automation - the automation events don't need to be an event
    | DestinationSet Mob Point | DestinationAbrupted Mob Point
    | StairsTaken VerticalDirection DLevel | StairsSeen VerticalDirection
    | Waken Mob | Slept Mob | MobSpawned Mob
    -- TODO seen/heard events should be based on nearby movement and/or sound, unnecessary
    | MobSeen Mob Mob | MobHeard Mob Mob
    | FeatureInteracted Point Feature | BandageApplied Mob
    -- TODO item seen doesn't need event
    | ItemSpawned Point Item | ItemsSeen [Item] | ItemPickedUp Mob Item | Equipped Mob Item | EquipmentRemoved Mob Item | EndOfTurn | NewGame
    -- TODO new monad for menu change - doesn't need event(s)
    | MenuChange Menu | QuitGame | Escaped | Saved deriving (Eq, Show)

-- TODO separate event types ?
-- data Message = EventMessage Event | ItemsSeen [Item] | StairsSeen VerticalDirection

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
