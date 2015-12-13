module RL.IO (
    module RL.IO,
    module RL.State
) where

-- Helper module which contains useful functions within the GameState.
--
-- Also exports getters/setters in RL.State

import RL.Dice
import RL.Game
import RL.State

import Control.Monad          (liftM2)
import Control.Monad.IO.Class (liftIO)
import System.Random

-- basic IO helper
io :: IO a -> GameState a
io = liftIO

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
    cols <- maxColumn
    rows <- maxRow
    p    <- randomPoint cols rows
    t    <- getTileAt p
    if isPassable t then
        return p
    else
        randomBlankPoint

-- generates random point
-- between     maxX   maxY
randomPoint :: Int -> Int -> GameState Point
randomPoint x y = liftM2 (,) (roll $ 1 `d` x) (roll $ 1 `d` y)
