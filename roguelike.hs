import UI.HSCurses.Curses

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import Data.Time.Clock.POSIX
import System.Random

-- global game

data Game = Game {
    level :: Map,
    player :: Player,
    mobs :: [Mob],
    seed :: Maybe StdGen,
    messages :: [Message]
}

type Message = String

defaultGame :: Game
defaultGame = Game {
    level = defaultMap,
    player = defaultPlayer,
    mobs = [],
    messages = [],
    seed = Nothing
}

type GameState = StateT Game IO

io :: IO a -> GameState a
io = liftIO

debug :: Show a => a -> GameState ()
debug str = io $ debug' str' >> return ()
    where debug' = mvWAddStr stdScr 20 0
          str'   = "debug: " ++ show str

-- map

data Tile = Wall | Floor
type Map = [[Tile]]

defaultMap :: Map
defaultMap = toMap [
        "--------------------------------------------",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "--------------------------------------------" ]
    where toMap = map toRow
          toRow = map tile

maxRows = (length defaultMap) - 1
maxColumns = (length $ defaultMap !! 0) - 1

type EnumerableMap = [(Point, Tile)]

enumerateMap :: Map -> EnumerableMap
enumerateMap = concat . map enumerateRow . enumerate
    where enumerateRow  (y, r) = map (\(x, t) -> ((x, y), t)) $ enumerate r
          enumerate            = zip [0..]

tile :: Char -> Tile
tile '|' = Wall
tile otherwise = Floor

fromTile :: Tile -> Char
fromTile Wall  = '|'
fromTile otherwise = '.'

isPassable :: Maybe Tile -> Bool
isPassable (Just Floor) = True
isPassable otherwise    = False

type Point = (Int, Int)

addOffset :: Point -> Point -> Point
addOffset (offx, offy) (x, y) = (offx + x, offy + y)

-- drawing functions

-- master draw function: draws player, mobs, and tiles
draw :: GameState ()
draw = do 
    getMap >>= drawMap
    getMobsWithPlayer >>= drawMobs
    getMessages >>= drawMessages
    getSeed >>= debug
    io refresh

drawMap :: Map -> GameState ()
drawMap m = io $ mapM_ drawTile (enumerateMap m)
    where drawTile (p, t) = drawTileAt p t

drawMobs :: [Mob] -> GameState ()
drawMobs = io . mapM_ drawMob
    where drawMob m = drawCharAt (at m) (symbol m)

drawTileAt :: Point -> Tile -> IO ()
drawTileAt p = drawCharAt p . fromTile

drawCharAt :: Point -> Char -> IO ()
drawCharAt (x, y) = mvAddCh' y x
    where mvAddCh' y x c = mvWAddStr stdScr y x [c]

sendMessages :: [Message] -> GameState [Message]
sendMessages msgs = (++ msgs) <$> getMessages

clearMessages :: GameState ()
clearMessages = do
    p    <- getPlayer
    m    <- getMap
    ms   <- getMobs
    g    <- getSeed
    put $ Game { level = m, player = p, mobs = ms, seed = Just g, messages = [] }

drawMessages :: [Message] -> GameState ()
drawMessages = mapM_ drawMessage . enumerate
    where 
        drawMessage  (i,    msg) = drawMessage' (0, 15 + i) msg
        drawMessage' (x, y) msg  = io $ mvWAddStr stdScr y x msg
        enumerate                = zip [0..]

-- player / mobs

type HP = Int
data Mob = Mob {
    symbol :: Char,
    at     :: Point,
    hp     :: HP,
    dmgd   :: GameState Int
}

type Player = Mob

defaultMob = Mob {
    hp     = 10,
    dmgd   = 1 `d` 4,
    symbol = undefined,
    at     = (maxColumns, maxRows)
}

defaultPlayer = defaultMob { symbol = '@' }
orc           = defaultMob { symbol = 'o', hp = 7 }
kobold        = defaultMob { symbol = 'k', hp = 4 }
goblin        = defaultMob { symbol = 'g', hp = 2 }

moveDir :: Dir -> GameState ()
moveDir NW    = movePlayer (-1, -1)
moveDir North = movePlayer ( 0, -1)
moveDir NE    = movePlayer ( 1, -1)
moveDir East  = movePlayer ( 1,  0)
moveDir SE    = movePlayer ( 1,  1)
moveDir South = movePlayer ( 0,  1)
moveDir SW    = movePlayer (-1,  1)
moveDir West  = movePlayer (-1,  0)

moveMob :: Point -> Mob -> Mob
moveMob off m = Mob { symbol = symbol m, at = addOffset off (at m), hp = hp m, dmgd = dmgd m }

moveMobTo :: Mob -> Point -> Mob
moveMobTo m xy = Mob { symbol = symbol m, at = xy, hp = hp m, dmgd = dmgd m }

filterMobs :: [Mob] -> [Mob]
filterMobs = filter $ (> 0) . hp

-- state manipulation

getMap :: GameState Map
getMap = level <$> get

getPlayer :: GameState Player
getPlayer = player <$> get

getMobs :: GameState [Mob]
getMobs = mobs <$> get

getSeed :: GameState StdGen
getSeed = do g <- get 
             let s = seed g
             maybe fallback return s
             -- if isNothing s then
             --     seedError
             --     return fallbac
             -- else 
             --     return s
    where fallback  = io seedError >> return fallback'
          fallback' = mkStdGen 0
          seedError = error "Invalid seed"

getMobsWithPlayer :: GameState [Mob]
getMobsWithPlayer = do g <- get
                       return (player g : mobs g)

getMobAt :: Point -> GameState (Maybe Mob)
getMobAt p = (getMobAt' . mobs) <$> get
    where getMobAt' = listToMaybe . getMobsAt
          getMobsAt = filter ((p ==) . at)

getTileAt :: Point -> GameState (Maybe Tile)
getTileAt p = do g <- get
                 return $ filterMap (enumerateMap $ level g)
    where filterMap          = listToMaybe . map snd . filter filterTile
          filterTile (p', t) = p == p'

setPlayer :: Player -> GameState ()
setPlayer p = do 
    m    <- getMap
    ms   <- getMobs
    g    <- getSeed
    msgs <- getMessages
    put $ Game { level = m, player = p, mobs = ms, seed = Just g, messages = msgs }

movePlayer :: Point -> GameState ()
movePlayer (0, 0) = return () -- todo advance clock
movePlayer off = do p <- getPlayer
                    targetM <- getMobAt  $ newloc p
                    targetT <- getTileAt $ newloc p
                    maybe (moveToTile p targetT) attack targetM
    where moveToTile  p t   = when (isPassable t) $ setPlayer (movePlayer p)
          movePlayer        = moveMob off
          newloc            = addOffset off . at

attack :: Mob -> GameState ()
attack target = do 
        m    <- getMap
        p    <- getPlayer
        ms   <- getMobs
        dmg  <- dmgd p
        g    <- getSeed
        msgs <- sendMessages ["dmg: " ++ show dmg, "hp: " ++ show (hp p)]
        put $ Game { level = m, player = p, mobs = filterMobs $ map (hurtMob dmg) ms, seed = Just g, messages = msgs }
    where 
        hurtMob  dmg m = if matchMob m then hurtMob' dmg m else m
        hurtMob' dmg m = Mob { symbol = symbol target, at = at target, hp = (hp target) - dmg, dmgd = dmgd target }
        matchMob     m = symbol target == symbol m

setSeed :: StdGen -> GameState ()
setSeed g = do 
    m    <- getMap
    ms   <- getMobs
    p    <- getPlayer
    msgs <- getMessages
    put $ Game { level = m, player = p, mobs = ms, seed = Just g, messages = msgs }

getMessages :: GameState [Message]
getMessages = messages <$> get

-- random number IO

defaultSeed :: GameState StdGen
defaultSeed = io $ mkStdGen <$> roundTime
    where roundTime = round `fmap` getPOSIXTime

randomPoint :: GameState Point
randomPoint = liftM2 (,) (1 `d` maxColumns) (1 `d` maxRows)

randomBlankPoint :: GameState Point
randomBlankPoint =  do
    p <- randomPoint
    t <- getTileAt p
    if isPassable t then
        return p
    else
        randomBlankPoint

d :: Int -> Int -> GameState Int
d n ns = do g <- getSeed
            let (r, g') = randomInt g
            setSeed g'
            return r
    where randomInt g = randomR (minInt, maxInt) g
          minInt      = n
          maxInt      = ns * n

-- general IO

runGame :: Game -> GameState () -> IO ()
runGame g gs = runGameState >>= endGame
    where runGameState = runStateT (setupGame >> gs) g

setupGame :: GameState ()
setupGame = do 
        io setupCurses          -- setup basic tty
        defaultSeed >>= setSeed -- seed the RNG
        m   <- getMap
        ms  <- setupMobs         -- setup random mobs
        p   <- setupPlayer       -- setup player start
        g   <- getSeed
        msg <- getMessages
        put $ Game { level = m, player = p, mobs = ms, seed = Just g, messages = msg }
    where setupCurses = do 
            initCurses
            cursSet CursorInvisible
            echo False

setupMobs :: GameState [Mob]
setupMobs = mapM mapMob defaultMobs
    where
        mapMob         m = randomBlankPoint >>= moveMobTo' m
        moveMobTo'   m p = return $ moveMobTo m p
        defaultMobs      = [kobold, orc, goblin]

setupPlayer :: GameState Player
setupPlayer = do 
    p <- getPlayer
    start <- randomBlankPoint
    return Mob { symbol = symbol p, at = start, hp = hp p, dmgd = dmgd p }

gameLoop :: GameState ()
gameLoop = draw >> doAction

data Action = Quit | Move Dir | None
data Dir    = North | East | South | West | NE | NW | SE | SW

doAction :: GameState ()
doAction = do clearMessages  -- clear previous action messages
              a <- getAction
              playing <- isPlaying a
              when playing $ do
                    case a of Move d    -> moveDir d
                              otherwise -> return ()
                    gameLoop
    where getAction                = io $ getAction' <$> getCh
          getAction' (KeyChar 'q') = Quit
          getAction' (KeyChar 'k') = Move North
          getAction' (KeyChar 'j') = Move South
          getAction' (KeyChar 'h') = Move West
          getAction' (KeyChar 'l') = Move East
          getAction' (KeyChar 'u') = Move NE
          getAction' (KeyChar 'y') = Move NW
          getAction' (KeyChar 'b') = Move SW
          getAction' (KeyChar 'n') = Move SE
          getAction' otherwise     = None

endGame :: (a, Game) -> IO ()
endGame gs = endWin

-- change playing status on action/state
isPlaying :: Action -> GameState Bool
isPlaying Quit      = return False
isPlaying otherwise = not <$> winCondition
    where winCondition = null <$> getMobs

main = runGame defaultGame gameLoop
