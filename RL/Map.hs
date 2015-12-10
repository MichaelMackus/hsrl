module RL.Map ( Tile(..), Map(..), Level(..), EnumeratedMap, tile, enumerateMap, maxRows, maxColumns, isPassable, fromTile ) where

import RL.Mob

-- map

data Tile = Wall | Floor
type Map = [[Tile]]

data Level = Level {
    tiles :: EnumeratedMap,
    lmobs :: [Mob] -- todo remove from game
}

type EnumeratedMap = [(Point, Tile)]

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
enumerateMap :: Map -> EnumeratedMap
enumerateMap = concat . map enumerateRow . enumerate
    where enumerateRow  (y, r) = map (\(x, t) -> ((x, y), t)) $ enumerate r
          enumerate            = zip [0..]
