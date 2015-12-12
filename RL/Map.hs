module RL.Map ( Level(..), Tile(..), Map(..), Point(..), MapIterator, tile, iterateMap, maxRows, maxColumns, isPassable, fromTile ) where

import RL.Mob

-- represents a level in the dungeon
data Level = Level {
    tiles :: Map,
    player :: Player,
    mobs :: [Mob]
    -- todo items
}

data Tile = TWall | Wall | Floor
type Map = [[Tile]]

type MapIterator = [(Point, Tile)]

-- TODO calculate
maxRows    = 44 :: Int
maxColumns = 8  :: Int

tile :: Char -> Tile
tile '|' = Wall
tile '-' = TWall
tile otherwise = Floor

fromTile :: Tile -> Char
fromTile Wall  = '|'
fromTile TWall = '-'
fromTile otherwise = '.'

isPassable :: Maybe Tile -> Bool
isPassable (Just Floor) = True
isPassable otherwise    = False


-- helper function
iterateMap :: Map -> MapIterator
iterateMap = concat . map enumerateRow . enumerate
    where enumerateRow  (y, r) = map (\(x, t) -> ((x, y), t)) $ enumerate r
          enumerate            = zip [0..]
