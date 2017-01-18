module RL.Types (module RL.Types, module RL.Dice) where

import RL.Dice

import qualified Data.List as L
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map as M

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

dneighbors :: Dungeon -> (Point, Tile) -> [(Point, Tile)]
dneighbors d (p, t) = mapDungeon (\p' t' -> if p == p' && t == t' then Just (p', t') else Nothing) d
    where
        mapDungeon :: (Point -> Tile -> Maybe r) -> Dungeon -> [r]
        mapDungeon f (Dungeon d) = catMaybes . map snd . M.toList $ M.mapWithKey f d

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
