module RL.Generator.Paths (Path(..), paths, getTileAt) where

import RL.Generator
import RL.Generator.Cells (cells, Cell, cpoint)
import RL.Types

import Control.Applicative
import Control.Monad (forM, when)
import Data.List (sortBy, deleteBy)
import Data.Maybe (listToMaybe)

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
    | otherwise = head . sortBy sortF . deleteBy (equating cpoint) c $ cs
        where sortF           c1 c2 = compare (distanceBetween c c1) (distanceBetween c c2)
              distanceBetween c1 c2 = distance (cpoint c1) (cpoint c2)
              equating            f = \a b -> f a == f b

getTileAt :: Point -> [Path] -> Maybe Tile
getTileAt p ps = do
        p <- path
        Just '#'
    where path     = listToMaybe $ filter pAt ps
          pAt path = intersects p path

type Slope = Double

-- tests if point intersects path
intersects :: Point -> Path -> Bool
intersects p (P p1 p2) = b == intercept p m
    where m = slope p1 p2
          b = intercept p1 m

-- get y-intercept
intercept :: Point -> Slope -> Double
intercept (x, y) m = fromIntegral y - (m * fromIntegral x)

-- slope of two points
slope :: Point -> Point -> Slope
slope (x1, y1) (x2, y2) = fromIntegral ys / fromIntegral xs
    where xs = abs $ x1 - x2
          ys = abs $ y1 - y2

-- distance between points
distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = xs + ys
    where xs = abs $ x1 - x2
          ys = abs $ y1 - y2
