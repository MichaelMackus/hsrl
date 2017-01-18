module RL.Generator.Paths (Path(..), paths, getTileAt) where

import RL.Generator
import RL.Generator.Cells (Cell, cpoint)
import RL.Types

import Control.Monad (forM, when, filterM, mapM)
import Control.Monad.State (State, runState, get)
import Data.List (sortBy, deleteBy, filter)
import Data.Maybe (listToMaybe)

data Path = P Point Point deriving (Show, Eq)

start :: Path -> Point
start (P p _) = p

end :: Path -> Point
end (P _ p) = p

-- generate list of paths between cells
paths :: [Cell] -> Generator Path [Path]
paths cs = do
    ps  <- getGData
    ps' <- (\ps' -> ps ++ concat ps') <$> forM (unreachableCells cs ps) (`generatePath` cs)

    setGData ps'

    when (allCellsReachable cs ps') markGDone

    return ps'

allCellsReachable :: [Cell] -> [Path] -> Bool
allCellsReachable [] _ = False
allCellsReachable (c:[]) [] = True
allCellsReachable _ [] = False
allCellsReachable (c:cs) ps = length (c:cs) == length (reachableCells (c:cs) ps)

reachableCells :: [Cell] -> [Path] -> [Cell]
reachableCells [] _ = []
reachableCells (c:[]) [] = [c]
reachableCells _ [] = []
reachableCells (c:cs) ps = (c:filter isReachable cs)
    where isReachable c' = not (null (pathBetween c'))
          pathBetween c' = findPath ps (c:cs) (cpoint c) (cpoint c')

type PathFinder = State ([Path], [Cell])

findPath :: [Path] -> [Cell] -> Point -> Point -> [Path]
findPath ps cs p1 p2 = fst (runState (pathFinder p1 p2) (ps, cs))

pathFinder :: Point -> Point -> PathFinder [Path]
pathFinder p1 p2 = do
    p1s <- findPathsAt p1
    p2s <- findPathsAt p2

    -- TODO filter p1s' by isPathTo
    p1s' <- filterM (\p -> start p `isPathTo` p2) . filter (\p -> start p == p1) $ p1s
    -- TODO filterM
    let p2s' = filter (\p -> end p == p1) p2s
        connecting = []

    -- TODO list of Monad
    -- connecting <- findPathsBetween <$> start <*> end

    return (p1s' ++ connecting ++ p2s')

findPathsBetween :: Path -> Path -> PathFinder [Path]
findPathsBetween p1 p2 =
    if end p1 == start p2 then
        return []
    else do
        -- TODO findPathsAt and find path to p2 from results
        return []

-- TODO doesn't work if paths intersect
-- TODO walk through cells
-- FIXME endless loop
isPathTo :: Point -> Point -> PathFinder Bool
isPathTo p1 p2 =
    if p1 == p2 then
        return True
    else do
        (ps, cs) <- get

        -- Step 1. Find paths (starting) at p1
        starts <- findPathsAt p1

        -- Step 2. Find paths (ending) at p2
        ends   <- findPathsAt p2

        -- Step 3. Check `isPathTo` for each point in path
        -- TODO endless loop here
        starts' <- or <$> mapM (\p -> (start p `isPathTo` p2) <||> (end p `isPathTo` p2)) starts
        ends'   <- or <$> mapM (\p -> (p1 `isPathTo` start p) <||> (p2 `isPathTo` end p)) ends

        return (starts' && ends')
    where
        findPathsStartingAt p = return . filter (not . (== p1) . end) =<< findPathsAt p
        a <||> b = a >>= \a' -> if a' then return a' else b
        -- f p = (start p `isPathTo` p2) <||> (end p `isPathTo` p2)
        -- g p = (p1 `isPathTo` start p) <||> (p2 `isPathTo` end p)

-- find paths through Cells
findPathsAt :: Point -> PathFinder [Path]
findPathsAt p = do
    (ps, cs) <- get

    let ps' = filter (\(P p1 p2) -> p1 == p || p2 == p) ps
        -- cs' = map toPath . filter (\c -> cpoint c == p) cs -- TODO find accounting for dimensions

    -- TODO convert cells to paths

    return ps'

findConnectedPaths :: Point -> [Path] -> [Path]
findConnectedPaths p ps = filter connectedPaths ps
    where connectedPaths (P p1 p2) = p == p1 || p == p2

unreachableCells :: [Cell] -> [Path] -> [Cell]
unreachableCells cs ps = let reachable = reachableCells cs ps
                         in filter (`notIn` reachable) cs
    where notIn c cs' = cpoint c `notElem` (map cpoint cs')

-- -- generate a path between cells
-- generatePath :: Cell -> [Cell] -> Generator Path [Path]
-- generatePath c cs = do
--     let target = findNeighbor c cs
--     return (makeRightAngle $ P (cpoint c) (cpoint target))

-- generate a path between cells
generatePath :: Cell -> [Cell] -> Generator Path [Path]
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

getTileAt :: Point -> [Path] -> Maybe Tile
getTileAt p ps = do
        p <- path
        Just '#'
    where path     = listToMaybe $ filter pAt ps
          pAt path = intersects p path && within p path

-- transform a path into a right angle, to prevent diagonals
makeRightAngle :: Path -> [Path]
makeRightAngle (P (x1,y1) (x2,y2)) =
    -- make longer path first
    if (abs $ x1 - y2) > (abs $ y1 - y2) then
        [ P (x1,y1) (x2,y1), P (x2,y1) (x2,y2) ]
    else
        [ P (x1,y1) (x1,y2), P (x1,y2) (x2,y2) ]

type Slope = Double

-- tests if point is within path boundaries
within :: Point -> Path -> Bool
within (x,y) (P (px,py) (px',py')) = x >= px && x <= px' && y >= py && y <= py'

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
