module RL.Event where

import RL.Map
import RL.Util (takeWhiles, dropWhiles)
import qualified Data.List as L

-- Represents Game events

data Event = Attacked Mob Mob | Damaged Mob Mob Int | Missed Mob Mob | Crit Mob Mob | Died Mob | Moved Mob Point
    | Drank Mob Item | Healed Mob Int | GainedLife Mob Int | DrankAcid Mob | GainedStrength Mob Int | SpedUp Mob Int | Slowed Mob Int | Vanished Mob | Appeared Mob | Confused Mob | Sobered Mob | Blinded Mob | Unblinded Mob
    | Read Mob Item | CastFire Mob Int | CastLightning Mob Int | Teleported Mob Point | Mapped DLevel | GainedTelepathy DLevel
    | DestinationSet Mob Point | DestinationAbrupted Mob Point
    | StairsTaken VerticalDirection DLevel | StairsSeen VerticalDirection
    | Waken Mob | Slept Mob | MobSeen Mob Mob | MobHeard Mob Mob | MobSpawned Mob
    | ItemsSeen [Item] | ItemPickedUp Mob Item | Equipped Mob Item | EndOfTurn | NewGame
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

toMessage :: Event -> Maybe String
toMessage (NewGame) = Just $ "You delve underground, searching for your ancestors' sword."
toMessage (Escaped) = Just $ "There is no escape. You must avenge your ancestors!"
toMessage (Crit attacker target)
    | isPlayer attacker = Just $ "CRITICAL HIT!"
toMessage (Damaged attacker target dmg)
    | isPlayer attacker && isPlayer target = Just $ "You hurt yourself for " ++ show dmg ++ " damage! Be more careful!"
    | isPlayer attacker = Just $ "You hit the " ++ mobName target ++ " for " ++ show dmg ++ " damage"
    | isPlayer target = Just $ "You were hit by the " ++ mobName attacker ++ " for " ++ show dmg
    | otherwise = Just $ "The " ++ mobName attacker ++ " hit the " ++ mobName target ++ " for " ++ show dmg
toMessage (Missed attacker target)
    | isPlayer attacker = Just $ "You missed the " ++ mobName target
    | isPlayer target = Just $ "The " ++ mobName attacker ++ " missed"
    | otherwise = Just $ "The " ++ mobName attacker ++ " missed the " ++ mobName target
toMessage (Died m)
    | isPlayer m = Just $ "You died!"
    | otherwise  = Just $ "You killed the " ++ mobName m
toMessage (StairsTaken Up _) = Just $ "You've gone up stairs."
toMessage (StairsTaken Down _) = Just $ "You've gone down stairs."
toMessage (Waken m) = Just $ "The " ++ mobName m ++ " has waken up."
toMessage (Slept m) = Just $ "The " ++ mobName m ++ " has fell asleep."
toMessage (StairsSeen Up) = Just $ "You see stairs going up."
toMessage (StairsSeen Down) = Just $ "You see stairs going down."
toMessage (ItemsSeen items) = let suffix = if length items > 1 then "There are " ++ show (length items - 1) ++ " more items here." else ""
                              in  Just $ "You see a " ++ show (head items) ++ ". " ++ suffix
toMessage (ItemPickedUp m item) | isPlayer m = Just $ "You have picked up a " ++ show item ++ "."
toMessage (Equipped m item) | isPlayer m = Just $ "You have equipped up a " ++ show item ++ "."
-- toMessage (MenuChange Equipment) = Just $ "Pick an item to equip."
-- toMessage (MenuChange DrinkMenu) = Just $ "Pick a potion to quaff."
-- toMessage (MenuChange ReadMenu)  = Just $ "Pick a scroll to read."
toMessage (MenuChange NoMenu) = Nothing
toMessage (MenuChange otherwise) = Just $ "Pick an item to use or equip. Press space to cancel."
toMessage (Drank           m p) | isPlayer m = Just $ "You drank the " ++ show p ++ "."
toMessage (Healed          m n) | isPlayer m = Just $ "You were healed of " ++ show n ++ " points of damage."
toMessage (GainedLife      m n) | isPlayer m = Just $ "Praise the sun! You feel youthful."
toMessage (GainedStrength  m n) | isPlayer m = Just $ "You feel empowered!"
toMessage (DrankAcid       m  ) | isPlayer m = Just $ "It BURNS!"
toMessage (Vanished        m  ) | isPlayer m = Just $ "You can no longer see yourself!"
toMessage (Confused        m  ) | isPlayer m = Just $ "You feel drunk."
toMessage (Blinded         m  ) | isPlayer m = Just $ "You can no longer see your surroundings!"
toMessage (Read            m s) | isPlayer m = Just $ "You read the " ++ show s ++ "."
toMessage (CastFire        m n) | isPlayer m = Just $ "Roaring flames erupt all around you!"
toMessage (CastLightning   m n) | isPlayer m = Just $ "KABOOM! Lightning strikes everything around you."
toMessage (Teleported      m p) | isPlayer m = Just $ "You feel disoriented."
toMessage (Mapped          lvl)              = Just $ "You suddenly understand the layout of the current level."
toMessage (GainedTelepathy lvl)              = Just $ "You sense nearby danger."
toMessage otherwise = Nothing
