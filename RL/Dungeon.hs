module RL.Dungeon (module RL.Dungeon, module RL.Mob, module RL.Types) where

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
-- TODO need to separate this out to concrete object that can store
-- TODO dungeon data independent of the dungeon level
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
    depth :: Depth,
    tiles :: Map Point Tile,
    player :: Player, -- TODO move to env?
    items :: [(Point, Item)],
    features :: [(Point, Feature)],
    mobs :: [Mob]
}

data Feature = Fountain Int | Chest [Item] | Altar | Campfire deriving (Eq, Show)

allMobs :: DLevel -> [Mob]
allMobs lvl = (player lvl:mobs lvl)

instance Eq DLevel where
    d == d' = depth d == depth d'

data Tile = Floor | Cavern | Rock | StairUp (Maybe DLevel) | StairDown DLevel

instance Eq Tile where
    Floor         == Floor         = True
    Cavern        == Cavern        = True
    (StairUp _)   == (StairUp _)   = True
    (StairDown _) == (StairDown _) = True
    Rock          == Rock          = True
    _ == _ = False

fromTile :: Tile -> Char
fromTile Floor     = '.'
fromTile Cavern    = '.'
fromTile (StairUp _) = '<'
fromTile (StairDown _) = '>'
fromTile Rock = '#'

fromFeature :: Feature -> Char
fromFeature Altar = '_'
fromFeature Campfire = '*'
fromFeature (Fountain _) = '{'
fromFeature (Chest _) = '='

getStairLvl :: Tile -> Maybe DLevel
getStairLvl (StairUp   lvl) = lvl
getStairLvl (StairDown lvl) = Just lvl
getStairLvl otherwise = Nothing

getStairDir :: Tile -> Maybe VerticalDirection
getStairDir (StairUp   _) = Just Up
getStairDir (StairDown _) = Just Down
getStairDir otherwise     = Nothing

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
mkLevel depth ts = DLevel depth (M.fromList (enumerate2d ts)) p [] [] []
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

findFeatureAt :: Point -> DLevel -> Maybe Feature
findFeatureAt p lvl = L.lookup p (features lvl)

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

-- passable dungeon neighbors for pathfinding
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
            ps = [ (x + 1, y),
                   (x - 1, y),
                   (x, y + 1),
                   (x, y - 1),
                   (x - 1, y - 1),
                   (x + 1, y + 1),
                   (x - 1, y + 1),
                   (x + 1, y - 1) ]
            ts' = map (`M.lookup` ts) ps
        in  map fst (filter f (zip ps ts'))


touching :: Point -> Point -> Bool
touching (p1x, p1y) (p2x, p2y) = (p1x == p2x && p1y + 1 == p2y) ||
                                 (p1x + 1 == p2x && p1y == p2y) ||
                                 (p1x == p2x && p1y - 1 == p2y) ||
                                 (p1x - 1 == p2x && p1y == p2y) ||
                                 (p1x - 1 == p2x && p1y - 1 == p2y) ||
                                 (p1x - 1 == p2x && p1y + 1 == p2y) ||
                                 (p1x + 1 == p2x && p1y - 1 == p2y) ||
                                 (p1x + 1 == p2x && p1y + 1 == p2y)

-- returns passable neighbors around point
-- considers stairs (mobs?) as impassable, unless end
dfinder :: DLevel -> Point -> Point -> Set Point
dfinder d end p = Set.fromList (filter f (dneighbors d p))
    where f p' = end == p' || isRunnable d p'

-- this passes through non passables
dfinder' :: DLevel -> Point -> Set Point
dfinder' d p = Set.fromList (neighbors d p f)
    where
        f (_ , Nothing) = False
        f (p', Just t)  = if p' `touching` p then True else False

mapDLevel :: (Point -> Tile -> Maybe r) -> DLevel -> [r]
mapDLevel f lvl = catMaybes . map snd . M.toList $ M.mapWithKey f (tiles lvl)

-- can mob run through point as part of automation?
isRunnable :: DLevel -> Point -> Bool
isRunnable lvl p = let t = findTileAt p lvl
                       f = findFeatureAt p lvl
                   in  maybe False (not . isStair) t && isNothing f

isPassable :: Tile -> Bool
isPassable Rock = False
isPassable otherwise = True

-- helper function for map construction
enumerateMap :: DLevel -> [(Point, Tile)]
enumerateMap = enumerate2d . toTiles

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
    if isBlinded m then distance (at m) p < 2
    else withinFov lvl m p && not (isSleeping m)

-- can mob see a mob on the map?
canSeeMob :: DLevel -> Mob -> Mob -> Bool
canSeeMob lvl m target =
    if isVisible target then canSee lvl m (at target)
    else canSee lvl m (at target) && distance (at m) (at target) < 2

withinFov :: DLevel -> Mob -> Point -> Bool
withinFov lvl m p = distance (at m) p <= fov m && not (isObstructed lvl (at m) p)


-- can mob see a point on the map with telepathy?
canSense :: DLevel -> Mob -> Point -> Bool
canSense lvl m p = isTelepathic m && isJust (findMobAt p lvl)

mapWidth :: DLevel -> Int
mapWidth lvl = length (toTiles lvl !! 0)

mapHeight :: DLevel -> Int
mapHeight lvl = length (toTiles lvl)

-- is a mob in melee?
inMelee :: DLevel -> Mob -> Bool
inMelee lvl m | isPlayer m = not . null $ L.filter (touching (at m) . at) (mobs lvl)
inMelee lvl m | otherwise  = touching (at m) (at (player lvl))
