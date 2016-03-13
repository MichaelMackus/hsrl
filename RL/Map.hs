module RL.Map (
    Level(..),
    Tile(..),
    Tiles,
    TilesIterator,
    tile,
    iterateTiles,
    isPassable,
    fromTile,

    module RL.Mob
) where

import RL.Mob
import RL.Util

-- represents a level in the dungeon
data Level = Level {
    tiles :: Tiles,
    player :: Player,
    mobs :: [Mob]
    -- todo items
}

data Tile = TWall | Wall | Floor | Rock deriving (Show, Eq)
type Tiles = [[Tile]]

type TilesIterator = [(Point, Tile)]

tile :: Char -> Tile
tile '|'       = Wall
tile '-'       = TWall
tile '.'       = Floor
tile otherwise = Rock

fromTile :: Tile -> Char
fromTile Wall      = '|'
fromTile TWall     = '-'
fromTile Floor     = '.'
fromTile otherwise = ' '

isPassable :: Maybe Tile -> Bool
isPassable (Just Floor) = True
isPassable otherwise    = False


-- helper function
iterateTiles :: Tiles -> TilesIterator
iterateTiles = enumerate2

-- toTiles :: TilesIterator -> Tiles
-- toTiles it = k
