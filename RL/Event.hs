module RL.Event where

import RL.Mob
import RL.Util (takeWhiles)

-- Represents Game events

data Event = Attacked Mob Mob Int | Died Mob | EndOfTurn | Moved Mob Point deriving Eq

getEventsNTurns :: Int -> [Event] -> [Event]
getEventsNTurns n = takeWhiles ((< n) . length . filter isEndOfTurn)

isEndOfTurn :: Event -> Bool
isEndOfTurn EndOfTurn = True
isEndOfTurn otherwise = False

isAttacked :: Mob -> [Event] -> Bool
isAttacked x = (> 0) . length . filter isAttacked'
    where
        isAttacked' (Attacked _ x' _) = x == x'
        isAttacked' otherwise         = False
