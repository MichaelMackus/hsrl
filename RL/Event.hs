module RL.Event where

import RL.Map
import RL.Util (takeWhiles, dropWhiles)

-- Represents Game events

data Event = Attacked Mob Mob | Damaged Mob Mob Int | Missed Mob Mob | Died Mob | Moved Mob Point
    | StairsTaken VerticalDirection DLevel | StairsSeen VerticalDirection
    | Waken Mob | Slept Mob | MobSeen Mob Mob | MobHeard Mob Mob | MobSpawned Mob
    | ItemsSeen [Item] | ItemPickedUp Mob Item | Equipped Mob Item | EndOfTurn | NewGame
    | MenuChange Menu | QuitGame deriving (Eq, Show)

data Menu = Inventory | Equipment | NoMenu deriving (Eq, Show)

getEventsAfterTurns :: Int -> [Event] -> [Event]
getEventsAfterTurns n = takeWhiles ((< n) . length . filter isEndOfTurn)

getEventsBeforeTurns :: Int -> [Event] -> [Event]
getEventsBeforeTurns n = dropWhiles ((< n) . length . filter isEndOfTurn)

isEndOfTurn :: Event -> Bool
isEndOfTurn EndOfTurn = True
isEndOfTurn otherwise = False

isAttacked :: Mob -> [Event] -> Bool
isAttacked x = (> 0) . length . filter isAttacked'
    where
        isAttacked' (Attacked _ x') = x == x'
        isAttacked' otherwise       = False

toMessage :: Event -> Maybe String
toMessage (Damaged attacker target dmg)
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
toMessage (MenuChange Equipment) = Just $ "Pick an item to equip."
toMessage otherwise = Nothing
