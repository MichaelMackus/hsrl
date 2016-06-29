module RL.Generator.Paths (Path(..), paths) where

import RL.Generator
import RL.Generator.Cells
import RL.Types

import Control.Applicative
import Control.Monad (forM, when)
import Data.List (sortBy)

data Path = P Point Point deriving Show

-- generate list of paths between cells
paths :: [Cell] -> Generator Path [Path]
paths cs = generate maxTries $ do
        ps <- concat <$> forM cs (`generatePath` cs)
        setGData ps

        done <- allCellsReachable cs
        when done markGDone

        return ps
    where maxTries = 5

allCellsReachable :: [Cell] -> Generator Path Bool
allCellsReachable cs = return True

-- generate a path between cells
generatePath :: Cell -> [Cell] -> Generator Path [Path]
generatePath c cs = do
    let target = findNeighbor c cs

    -- TODO generate Path at right angle
    return [P (cpoint c) (cpoint target)]

-- find closest cell
findNeighbor :: Cell -> [Cell] -> Cell
findNeighbor c cs
    | null cs   = error "findNeighbor expects non-empty list"
    | otherwise = head $ sortBy sortF cs
        where sortF c1 c2 = compare (distanceBetween c c1) (distanceBetween c c2)

-- measure distance between cells
distanceBetween :: Cell -> Cell -> Int
distanceBetween c1 c2 = distance (cpoint c1) (cpoint c2)

-- distance between points
distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = xs^2 + ys^2
    where xs = abs $ x1 - x2
          ys = abs $ y1 - y2
