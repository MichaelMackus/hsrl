module RL.Generator.Paths (Path(..), paths, getTileAt) where

import RL.Generator
import RL.Generator.Cells (Cell(..), cpoint)
import RL.Map hiding (Path)
import qualified RL.Generator.Cells as C

import Control.Monad (forM)
import Data.List (sortBy, deleteBy, nub)
import Data.Maybe (listToMaybe)

data Path = P Point Point deriving Eq

-- generate list of paths between cells
paths :: [Cell] -> Generator c [Path] [Path]
paths cs = do
    ps  <- getGData

    let reachable   = reachableCells cs ps
        unreachable = unreachableCells cs ps

    if (null unreachable) then do
        markGDone
        return ps
    else do
        ps' <- nub . (ps ++) . concat <$> forM reachable (`generatePath` unreachable)
        setGData ps'
        resetCounter -- keep generating until all paths are reachable
        return ps'

reachableCells :: [Cell] -> [Path] -> [Cell]
reachableCells [] _ = []
reachableCells (c:[]) _ = [c]
reachableCells (c:_ ) [] = [c]
reachableCells (c:cs) ps = (c:filter isReachable cs)
    where isReachable c' = not . null . filter (\p -> within (cpoint c') p && intersects (cpoint c') p) $ ps

unreachableCells :: [Cell] -> [Path] -> [Cell]
unreachableCells cs ps =
    let reachable = reachableCells cs ps
    in  filter (`notElem` reachable) cs

-- generate a path between cells
generatePath :: Cell -> [Cell] -> Generator c [Path] [Path]
generatePath c [] = return []
generatePath c cs =
    return . maybe [] id $ do
        target <- findNeighbor c cs
        let path = P (cpoint c) (cpoint target)
        return (makeRightAngle path)


-- find closest cell
findNeighbor :: Cell -> [Cell] -> Maybe Cell
findNeighbor c cs
    | null cs   = Nothing
    | otherwise = Just . head . sortBy sortF . deleteBy (equating cpoint) c $ cs
        where sortF           c1 c2 = compare (distanceBetween c c1) (distanceBetween c c2)
              distanceBetween c1 c2 = distance (cpoint c1) (cpoint c2)
              equating            f = \a b -> f a == f b

getPTileAt :: Point -> [Path] -> Maybe Tile
getPTileAt p ps = maybe Nothing (const (Just Cavern)) path
    where path     = listToMaybe $ filter pAt ps
          pAt path = intersects p path && within p path

-- combines C.getTileAt and P.getTileAt
getTileAt :: Point -> [Cell] -> [Path] -> Maybe Tile
getTileAt p cs ps = maybe (getPTileAt p ps) Just $ C.getTileAt p cs

-- transform a path into a right angle, to prevent diagonals
makeRightAngle :: Path -> [Path]
makeRightAngle (P (x1,y1) (x2,y2)) =
    -- make longer path first
    if (abs $ x1 - y2) > (abs $ y1 - y2) then
        [ P (x1,y1) (x2,y1), P (x2,y1) (x2,y2) ]
    else
        [ P (x1,y1) (x1,y2), P (x1,y2) (x2,y2) ]

-- tests if point is within path boundaries
within :: Point -> Path -> Bool
within (x,y) (P (px,py) (px',py')) = (x >= px && x <= px' && y >= py && y <= py') || (x <= px && x >= px' && y <= py && y >= py')

-- tests if point intersects path
intersects :: Point -> Path -> Bool
intersects p (P p1 p2) = b == intercept p m
    where m = slope p1 p2
          b = intercept p1 m

