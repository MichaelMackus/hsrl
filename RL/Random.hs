module RL.Random (d, randomBlankPoint) where

import RL.Game
import RL.IO
import RL.Map
import RL.Mob
import RL.State

import Control.Monad.State
import Data.Time.Clock.POSIX
import System.Random

-- random number IO

-- main random function, used like: 2 `d` 4 or 1 `d` 20
d :: Int -> Int -> GameState Int
d n ns = do
        g <- getSeed
        let (r, g') = randomInt g
        setSeed g'                -- advance seed
        return r
    where
        randomInt g = randomR (minInt, maxInt) g
        minInt      = n
        maxInt      = ns * n

-- helper functions

randomPoint :: GameState Point
randomPoint = liftM2 (,) (1 `d` maxColumns) (1 `d` maxRows)

randomBlankPoint :: GameState Point
randomBlankPoint =  do
    p <- randomPoint
    t <- getTileAt p
    if isPassable t then
        return p
    else
        randomBlankPoint
