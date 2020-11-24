module RL.Event where

import RL.Mob
import RL.Util (takeWhiles, dropWhiles)

-- Represents Game events

data Event = Attacked Mob Mob Int | Missed Mob Mob | Died Mob | EndOfTurn | Moved Mob Point | StairsTaken VerticalDirection | Waken Mob | Slept Mob
    | MenuChange Menu | StairsSeen VerticalDirection | ItemsSeen String Int | ItemPickedUp String deriving (Eq, Show)

data Menu = Inventory | Equip | NoMenu deriving (Eq, Show)

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
        isAttacked' (Attacked _ x' _) = x == x'
        isAttacked' otherwise         = False

toMessage :: Event -> Maybe String
toMessage (Attacked attacker target dmg)
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
toMessage (StairsTaken Up) = Just $ "You've gone up stairs."
toMessage (StairsTaken Down) = Just $ "You've gone down stairs."
toMessage (Waken m) = Just $ "The " ++ mobName m ++ " has waken up."
toMessage (Slept m) = Just $ "The " ++ mobName m ++ " has fell asleep."
toMessage (StairsSeen Up) = Just $ "You see stairs going up."
toMessage (StairsSeen Down) = Just $ "You see stairs going down."
toMessage (ItemsSeen item n) = let suffix = if n > 1 then "There are " ++ show (n - 1) ++ " more items here." else ""
                               in  Just $ "You see a " ++ item ++ ". " ++ suffix
toMessage (ItemPickedUp item) = Just $ "You have picked up a " ++ item ++ "."
toMessage (MenuChange Equip) = Just $ "Pick an item to equip."
toMessage otherwise = Nothing

