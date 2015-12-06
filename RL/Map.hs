module RL.Map ( Tile(..), Map(..), tile, enumerateMap, maxRows, maxColumns, isPassable, fromTile ) where

import RL.Mob

-- map

data Tile = Wall | Floor
type Map = [[Tile]]
type EnumerableMap = [(Point, Tile)] -- represents a map with coordinates (enumerable)

-- TODO calculate
maxRows    = 44 :: Int
maxColumns = 8  :: Int

tile :: Char -> Tile
tile '|' = Wall
tile otherwise = Floor

fromTile :: Tile -> Char
fromTile Wall  = '|'
fromTile otherwise = '.'

isPassable :: Maybe Tile -> Bool
isPassable (Just Floor) = True
isPassable otherwise    = False


-- helper function
enumerateMap :: Map -> EnumerableMap
enumerateMap = concat . map enumerateRow . enumerate
    where enumerateRow  (y, r) = map (\(x, t) -> ((x, y), t)) $ enumerate r
          enumerate            = zip [0..]
