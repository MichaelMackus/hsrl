module RL.Dice (Dice(..), d) where

import System.Random

data Dice  = D Int Sides
type Sides = Int

-- construct dice from number of dice & sides, used like: 2 `d` 4 or 1 `d` 20
d :: Int -> Sides -> Dice
d n ns = D n ns
