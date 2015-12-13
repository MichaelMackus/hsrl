module RL.Dice (Dice(..), d) where

-- helper module to construct a Dice object
--
-- the Dice then gets rolled in RL.Random (roll) sometime in the future

import System.Random

data Dice  = D Int Sides
type Sides = Int

-- construct dice from number of dice & sides, used like: 2 `d` 4 or 1 `d` 20
d :: Int -> Sides -> Dice
d n ns = D n ns
