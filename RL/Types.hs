module RL.Types (module RL.Types, module RL.Dice) where

import RL.Dice

import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as Set

-- the dungeon Map is just a map of Points -> Tiles
data Dungeon = Dungeon (Map Point Tile)
type Tile    = Char
type Point   = (Int, Int)

-- the dungeon is printable
instance Show Dungeon where
    show = unlines . toTiles


-- Map constructor
mkDungeon :: [[Tile]] -> Dungeon
mkDungeon = Dungeon . M.fromList . enumerateMap

-- blank map
blankMap :: Int -> Int -> [[Tile]]
blankMap w h = replicate (h - 1) $ " " ++ floor ++ " "
    where floor = replicate (w - 1) ' '

-- Quick Tile generator
blankBox :: Dimension -> [[Tile]]
blankBox (w,h) = [top] ++ space ++ [bot]
    where top   = replicate w '-'
          bot   = top
          space = replicate (h - 2) $ "|" ++ floor ++ "|"
          floor = replicate (w - 2) ' '

-- This allows for easy iteration of the map
--
--         given:          produces
--
--        location   tile    tile
iterMap :: (Point -> Tile -> Tile) -> Dungeon -> Dungeon
iterMap f (Dungeon d) = Dungeon (M.mapWithKey f d)

dneighbors :: Dungeon -> Point -> [Point]
dneighbors d p = mapDungeon f d
    where
        f p' t = if p `touching` p' && isPassable t then Just p' else Nothing
        touching (p1x, p1y) (p2x, p2y) = (p1x == p2x && p1y + 1 == p2y) || 
                                         (p1x + 1 == p2x && p1y == p2y) ||
                                         (p1x == p2x && p1y - 1 == p2y) ||
                                         (p1x - 1 == p2x && p1y == p2y)

dfinder :: Dungeon -> Point -> Set Point
dfinder d p = Set.fromList (dneighbors d p)

mapDungeon :: (Point -> Tile -> Maybe r) -> Dungeon -> [r]
mapDungeon f (Dungeon d) = catMaybes . map snd . M.toList $ M.mapWithKey f d

isPassable :: Tile -> Bool
isPassable ' ' = False
isPassable otherwise = True

-- dungeon cell box (w x h)
type Dimension = (Width, Height)
type Width     = Int
type Height    = Int

-- used like: 2 `x` 4
x :: Int -> Int -> Dimension
x w h = (w, h)

-- helper function for map construction
enumerateMap :: [[Tile]] -> [(Point, Tile)]
enumerateMap = concat . map toPoints . enumerate . map enumerate
    where toPoints (y, ts)   = map (toPoint y) ts
          toPoint   y (x, t) = ((x, y), t)
          enumerate          = zip [0..] :: [b] -> [(Int, b)]

-- helper function for map deconstruction
toTiles :: Dungeon -> [[Tile]]
toTiles (Dungeon d) = justTiles . mtiles . sortYs $ M.toList d
    where sortYs    ts = L.sortBy compareTs ts
          mtiles    ts = L.groupBy (\t t' -> (compare (pointY t) (pointY t')) == EQ) ts
          justTiles ts = map (map snd) ts
          pointY    t  = snd (fst t)
          compareTs    = (\((x, y), _) ((x', y'), _) -> compare y y' `mappend` compare x x')

-- Point helpers
type Slope = Double

-- get y-intercept
intercept :: Point -> Slope -> Double
intercept (x, y) m = fromIntegral y - (m * fromIntegral x)

-- slope of two points
slope :: Point -> Point -> Slope
slope (x1, y1) (x2, y2) = fromIntegral ys / fromIntegral xs
    where xs = abs $ x1 - x2
          ys = abs $ y1 - y2

-- distance between points
distance :: (Floating a, Ord a) => Point -> Point -> a
distance (x1, y1) (x2, y2) = sqrt (xs^2 + ys^2)
    where xs = abs (fromIntegral x1 - fromIntegral x2)
          ys = abs (fromIntegral y1 - fromIntegral y2)
