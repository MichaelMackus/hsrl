module RL.Map (module RL.Map, module RL.Mob, module RL.Types) where

import RL.Item
import RL.Mob
import RL.Types
import RL.Util (enumerate2d, unenumerate2d)

import Data.Map (Map)
import Data.Maybe (catMaybes, isNothing, isJust)
import Data.Set (Set)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as Set

-- represents a tree of dungeon levels (TODO currently only 1 dimension)
data Dungeon = DungeonLevel DLevel Dungeon | DTip DLevel

-- insert level at DLevel's depth
insertLevel :: DLevel -> Dungeon -> Dungeon
insertLevel lvl (DTip prev)
    | depth prev == depth lvl = DTip lvl
    | otherwise = DungeonLevel prev (DTip lvl)
insertLevel lvl (DungeonLevel start rest)
    | depth start == depth lvl = DungeonLevel lvl rest
    | otherwise = DungeonLevel start (insertLevel lvl rest)

-- return level at depth
atDepth :: Int -> Dungeon -> Maybe DLevel
atDepth d (DTip lvl) = if depth lvl == d then Just lvl else Nothing
atDepth d (DungeonLevel start rest) =
    if depth start == d then Just start
    else atDepth d rest

-- the dungeon Map is just a map of Points -> Tiles
data DLevel  = DLevel {
    depth :: Int,
    tiles :: Map Point Tile,
    player :: Player, -- TODO move to env?
    seen  :: [Point], -- seen tiles
    items :: [(Point, Item)],
    mobs :: [Mob]
}

allMobs :: DLevel -> [Mob]
allMobs lvl = (player lvl:mobs lvl)

instance Eq DLevel where
    d == d' = depth d == depth d' && mobs d == mobs d' && tiles d == tiles d'

data Tile = Floor | Cavern | Rock | StairUp (Maybe DLevel) | StairDown DLevel

instance Eq Tile where
    Floor         == Floor         = True
    Cavern        == Cavern        = True
    (StairUp _)   == (StairUp _)   = True
    (StairDown _) == (StairDown _) = True
    _ == _ = False

fromTile :: Tile -> Char
fromTile Floor     = '.'
fromTile Cavern    = '.'
fromTile (StairUp _) = '<'
fromTile (StairDown _) = '>'
fromTile Rock = '#'

getStairLvl :: Tile -> Maybe DLevel
getStairLvl (StairUp   lvl) = lvl
getStairLvl (StairDown lvl) = Just lvl
getStairLvl otherwise = Nothing

isStair :: Tile -> Bool
isStair t = isDownStair t || isUpStair t

isDownStair :: Tile -> Bool
isDownStair (StairDown _) = True
isDownStair otherwise = False

isUpStair :: Tile -> Bool
isUpStair (StairUp _) = True
isUpStair otherwise = False

-- the dungeon is printable
instance Show DLevel where
    show = unlines . map (map fromTile) . toTiles

-- Map constructor
mkLevel :: Int -> [[Tile]] -> DLevel
mkLevel depth ts = DLevel depth (M.fromList (enumerateMap ts)) p [] [] []
    where p = mob

-- blank map
blankMap :: Int -> Int -> [[Tile]]
blankMap w h = map (map (const Rock)) . replicate (h - 1) $ " " ++ floor ++ " "
    where floor = replicate (w - 1) ' '

-- Quick Tile generator
blankBox :: Dimension -> [[Tile]]
blankBox (w,h) = map (map (const Rock)) ([top] ++ space ++ [bot])
    where top   = replicate w ' '
          bot   = top
          space = replicate (h - 2) $ " " ++ floor ++ " "
          floor = replicate (w - 2) ' '

-- This allows for easy iteration of the map
--
--         given:          produces
--
--        location   tile    tile
iterMap :: (Point -> Tile -> Tile) -> DLevel -> DLevel
iterMap f lvl = lvl { tiles = M.mapWithKey f (tiles lvl) }

findTileAt :: Point -> DLevel -> Maybe Tile
findTileAt p lvl = M.lookup p (tiles lvl)

findTile :: ((Point, Tile) -> Bool) -> DLevel -> Maybe (Point, Tile)
findTile f lvl = L.find f (M.toList (tiles lvl))

findItemsAt :: Point -> DLevel -> [Item]
findItemsAt p lvl = map snd (filter f (items lvl))
    where f (p', _) = p == p'

replaceItemsAt :: Point -> DLevel -> [Item] -> DLevel
replaceItemsAt p lvl is =
    let dropped = filter (\(p',_) -> p /= p') (items lvl)
    in  lvl { items = dropped ++ (zip (repeat p) is) }

findMobAt :: Point -> DLevel -> Maybe Mob
findMobAt p lvl = if at (player lvl) == p then Just (player lvl)
                  else L.find ((== p) . at) (mobs lvl)

findTileOrMob :: Point -> DLevel -> Either Tile Mob
findTileOrMob p lvl =
    let t = M.lookup p (tiles lvl)
        m = findMobAt p lvl
        err = error ("Error during finding " ++ show p)
    in  maybe (maybe err Left t) Right m

dneighbors :: DLevel -> Point -> [Point]
dneighbors d p = neighbors d p f
    where
        f (p', Nothing) = False
        f (p', Just t)  = if p `touching` p' && isPassable t then True else False

-- generic neighbors function
neighbors :: DLevel -> Point -> ((Point, Maybe Tile) -> Bool) -> [Point]
neighbors d p@(x, y) f
    | isNothing (M.lookup p (tiles d)) = []
    | otherwise =
        let ts = tiles d
            ps = [ ((x + 1), y),
                   ((x - 1), y),
                   (x, (y + 1)),
                   (x, (y - 1)) ]
            ts' = map (`M.lookup` ts) ps
        in  map fst (filter f (zip ps ts'))


touching (p1x, p1y) (p2x, p2y) = (p1x == p2x && p1y + 1 == p2y) ||
                                 (p1x + 1 == p2x && p1y == p2y) ||
                                 (p1x == p2x && p1y - 1 == p2y) ||
                                 (p1x - 1 == p2x && p1y == p2y)

dfinder :: DLevel -> Point -> Set Point
dfinder d p = Set.fromList (dneighbors d p)

-- this passes through non passables
dfinder' :: DLevel -> Point -> Set Point
dfinder' d p = Set.fromList (neighbors d p f)
    where
        f (_ , Nothing) = False
        f (p', Just t)  = if p' `touching` p then True else False

mapDLevel :: (Point -> Tile -> Maybe r) -> DLevel -> [r]
mapDLevel f lvl = catMaybes . map snd . M.toList $ M.mapWithKey f (tiles lvl)

isPassable :: Tile -> Bool
isPassable Rock = False
isPassable otherwise = True

-- helper function for map construction
enumerateMap :: [[Tile]] -> [(Point, Tile)]
enumerateMap = enumerate2d

-- helper function for map deconstruction
toTiles :: DLevel -> [[Tile]]
toTiles lvl = unenumerate2d (M.toList (tiles lvl))

-- draw straight line from point to point to test seen
isObstructed :: DLevel -> Point -> Point -> Bool
isObstructed lvl p p' =
    let l = filter (\p'' -> p'' /= p && p'' /= p') (line p p')
        f p = maybe False isPassable (findTileAt p lvl)
    in  not (length (filter f l) == length l)

-- can mob see a point on the map?
canSee :: DLevel -> Mob -> Point -> Bool
canSee lvl m p =
    if distance (at m) p > fov m then
        False
    else
        not (isObstructed lvl (at m) p)

-- can mob see a point on the map with telepathy?
canSeeTelepathic :: DLevel -> Mob -> Point -> Bool
canSeeTelepathic lvl m p =
    -- test for telepathic sight
    let m'           = findMobAt p lvl
        isTelepathic = isJust m' && depth lvl `elem` isTelepathicOn m
    in  canSee lvl m p || isTelepathic

-- can mob see a point on the map if blind?
canSeeBlind :: DLevel -> Mob -> Point -> Bool
canSeeBlind lvl m p =
    -- test for telepathic sight
    let isBlind = BlindedF `elem` flags m
    in  if isBlind then distance (at m) p < 2
        else canSee lvl m p


mapWidth :: DLevel -> Int
mapWidth lvl = length (toTiles lvl !! 0)

mapHeight :: DLevel -> Int
mapHeight lvl = length (toTiles lvl)
