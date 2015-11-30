import UI.HSCurses.Curses
import Control.Monad.State

-- helper functions

enumerate :: (Enum a, Num a) => [b] -> [(a, b)]
enumerate = zip [0..]

-- unenumerate :: (Enum a, Num a) => [(a, b)] -> [b]
-- unenumerate l = snd $ unzip (enumerate l)

-- main game functions

io :: IO a -> GameState a
io = liftIO

setup :: IO ()
setup = do
    initCurses
    cursSet CursorInvisible
    echo False

defaultGame = Game {
    gameMap = toMap [
        "--------------------------------------------",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "--------------------------------------------" ],
    gamePlayer = Mob { symbol = '@', loc = (5, 20) }
}

toMap :: [String] -> Map
toMap = map (map toTile)

fromMap :: Map -> [String]
fromMap = map (map fromTile)

-- mapat :: (Tile a) => (a -> a) -> Point -> [[a]]

-- represents a map w/ the player obj
-- withPlayer :: Player -> Map -> Map
-- withPlayer p m = map unenumerate . map lineWithPlayer p $ enumerate m
--     where lineWithPlayer p (y, l)         = map (tileWithPlayer y) $ enumerate l
--           tileWithPlayer y (x, t@(tt, m)) = if (loc p) == (x, y) then (withMob p t) else t

-- withMob :: Mob -> Tile -> Tile
-- withMob m t = T (tileType t) (withMob' $ mob t)
--     where withMob' Nothing = m
--           withMob' Just tm = tm

toTile :: Char -> Tile
-- toTile '@' = T Floor defaultPlayer
toTile t = T { tileType = (toTileType t), mob = Nothing }

toTileType :: Char -> TileType
toTileType '-'       = WallH
toTileType '|'       = WallV
toTileType otherwise = Floor

fromTile :: Tile -> Char
fromTile (T _ (Just m)) = symbol m
fromTile (T t Nothing)  = fromTileType t

fromTileType :: TileType -> Char
fromTileType  WallH     = '-'
fromTileType  WallV     = '|'
fromTileType  otherwise = '.'

data TileType = WallV | WallH | Floor
data Tile = T {
    tileType :: TileType,
    mob :: Maybe Mob
}
type Map = [[Tile]]

type Point = (Int, Int)

data Mob = Mob {
    symbol :: Char,
    loc :: Point
}
type Player = Mob

data Game = Game {
    gameMap :: Map,
    gamePlayer :: Mob
}

type GameState = StateT Game IO

-- State helper functions

getMap :: GameState Map
getMap = do
    g <- get
    return (gameMap g)

getPlayer :: GameState Player
getPlayer = do
    g <- get
    return (gamePlayer g)

-- Map modification functions

drawMap :: GameState ()
drawMap = do p <- getPlayer
             m <- getMap
             io $ drawMap' m >> refresh
    where 
        drawMap'        = mapM_ drawLine . enumerate . fromMap
        drawLine (y, l) = mvWAddStr stdScr y 0 l

movePlayer :: Point -> GameState ()
movePlayer (x, y) = return ()
        
-- getTile :: Point -> GameState ()
-- getTile (x, y) = do

gameLoop :: GameState ()
gameLoop = do
        drawMap
        continue <- doAction
        if continue then gameLoop else return ()

data Action = Quit | Move Char | None

getAction :: GameState Action
getAction = io $ do
        c <- getCh
        return $ getAction' c
    where 
        getAction' (KeyChar 'q') = Quit
        getAction' (KeyChar  c ) = Move c
        otherwise                = None

doAction :: GameState Bool -- Continue Loop ?
doAction = getAction >>= doAction'
    where
        doAction' Quit      = return False
        doAction' (Move c)  = do
            case c of  
                'j' -> movePlayer (0, 1)
                'k' -> movePlayer (0, -1)
                'h' -> movePlayer (-1, 0)
                'l' -> movePlayer (1, 0)
            return True
        doAction' otherwise = return True

cleanupGame :: IO ()
cleanupGame = endWin

main = do
    setup
    runStateT gameLoop defaultGame
    cleanupGame
