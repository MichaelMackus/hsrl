module RL.Random (Dice, d, roll, randomBlankPoint) where

import RL.Dice
import RL.Game
import RL.Map
import RL.State

import Control.Monad
import System.Random

-- main roll function
--
-- generates random int and increments seed
roll :: Dice -> GameState Int
roll (D n ns) = do
        (r, g') <- fmap doRoll getSeed
        setSeed g'
        return r
    where
        doRoll = randomR (minInt, maxInt)
        minInt = n
        maxInt = ns * n

-- generates random map point
randomBlankPoint :: GameState Point
randomBlankPoint =  do
        p <- randomPoint
        t <- getTileAt p
        if isPassable t then
            return p
        else
            randomBlankPoint
    where
        randomPoint = liftM2 (,) (roll randomCol) (roll randomRow)
        randomCol   = 1 `d` maxColumns
        randomRow   = 1 `d` maxRows

