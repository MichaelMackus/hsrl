module RL.Map (module RL.Map, module RL.Mob, module RL.Types) where

import RL.Mob
import RL.Types

import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as Set

-- the dungeon Map is just a map of Points -> Tiles
data Dungeon = DTree DLevel Dungeon Dungeon | DTip DLevel | DNotGenerated
data DLevel  = DLevel {
    tiles :: Map Point Tile,
    player :: Player,
    items :: [()],
    mobs :: [Mob]
}

type Tile    = Char

-- the dungeon is printable
instance Show DLevel where
    show = unlines . toTiles

-- Map constructor
mkLevel :: [[Tile]] -> DLevel
mkLevel ts = DLevel (M.fromList (enumerateMap ts)) p [] []
    where p = Mob 0 '@' (0,0) 0 (1 `d` 4)

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
iterMap :: (Point -> Tile -> Tile) -> DLevel -> DLevel
iterMap f lvl = lvl { tiles = M.mapWithKey f (tiles lvl) }

-- TODO findInLevel and return container around Tile & contents
findTile :: Point -> DLevel -> Maybe Tile
findTile p lvl = M.lookup p (tiles lvl)

dneighbors :: DLevel -> Point -> [Point]
dneighbors d p = mapDLevel f d
    where
        f p' t = if p `touching` p' && isPassable t then Just p' else Nothing
        touching (p1x, p1y) (p2x, p2y) = (p1x == p2x && p1y + 1 == p2y) || 
                                         (p1x + 1 == p2x && p1y == p2y) ||
                                         (p1x == p2x && p1y - 1 == p2y) ||
                                         (p1x - 1 == p2x && p1y == p2y)

dfinder :: DLevel -> Point -> Set Point
dfinder d p = Set.fromList (dneighbors d p)

mapDLevel :: (Point -> Tile -> Maybe r) -> DLevel -> [r]
mapDLevel f lvl = catMaybes . map snd . M.toList $ M.mapWithKey f (tiles lvl)

isPassable :: Tile -> Bool
isPassable ' ' = False
isPassable otherwise = True

-- helper function for map construction
enumerateMap :: [[Tile]] -> [(Point, Tile)]
enumerateMap = concat . map toPoints . enumerate . map enumerate
    where toPoints (y, ts)   = map (toPoint y) ts
          toPoint   y (x, t) = ((x, y), t)
          enumerate          = zip [0..] :: [b] -> [(Int, b)]

-- helper function for map deconstruction
toTiles :: DLevel -> [[Tile]]
toTiles lvl = justTiles . mtiles . sortYs $ M.toList (tiles lvl)
    where sortYs    ts = L.sortBy compareTs ts
          mtiles    ts = L.groupBy (\t t' -> (compare (pointY t) (pointY t')) == EQ) ts
          justTiles ts = map (map snd) ts
          pointY    t  = snd (fst t)
          compareTs    = (\((x, y), _) ((x', y'), _) -> compare y y' `mappend` compare x x')

-- TODO remove below
--
-- -- represents a level in the dungeon
-- data Level = Level {
--     tiles :: Tiles,
--     player :: Player,
--     mobs :: [Mob]
--     -- todo items
-- }

-- data Tile = TWall | Wall | Floor | Rock deriving (Show, Eq)
-- type Tiles = [[Tile]]

-- type TilesIterator = [(Point, Tile)]

-- tile :: Char -> Tile
-- tile '|'       = Wall
-- tile '-'       = TWall
-- tile '.'       = Floor
-- tile otherwise = Rock

-- fromTile :: Tile -> Char
-- fromTile Wall      = '|'
-- fromTile TWall     = '-'
-- fromTile Floor     = '.'
-- fromTile otherwise = ' '

-- isPassable :: Maybe Tile -> Bool
-- isPassable (Just Floor) = True
-- isPassable otherwise    = False


-- -- helper function
-- iterateTiles :: Tiles -> TilesIterator
-- iterateTiles = enumerate2

-- -- toTiles :: TilesIterator -> Tiles
-- -- toTiles it = k
