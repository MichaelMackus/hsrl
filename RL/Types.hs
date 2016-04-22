module RL.Types where

import Data.Map (Map)
import qualified Data.Map as M

-- the dungeon Map is just a map of Points -> Tiles
type Dungeon = Map Point Tile
type Tile    = Char
type Point   = (Int, Int)

-- Map constructor
mkMap :: [[Tile]] -> Dungeon
mkMap = M.fromList . enumerateMap

-- Quick Map generator
-- generateMap :: Dimension -> Dimension -> Dungeon
-- generateMap = iterMap fillSpace . mkMap blankDungeon
--     where fillSpace p = id

-- a dungeon Cell is a grid of things
type Dimension = Int

-- Quick Cell generator
blankDungeon :: Dimension -> Dimension -> [[Tile]]
blankDungeon w h = [top] ++ space ++ [bot]
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
iterMap = M.mapWithKey

-- helper function for map construction
enumerateMap :: [[Tile]] -> [(Point, Tile)]
enumerateMap = map toPoint . enumerate . concat . map enumerate
    where toPoint (y, (x, t)) =  ((x, y), t)
          enumerate           = zip [0..] :: [b] -> [(Int, b)]
