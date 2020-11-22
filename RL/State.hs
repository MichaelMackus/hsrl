module RL.State (module RL.State, get, gets) where

-- state manipulation
--
-- helper getter/setter function for state manipulation
--
-- TODO look into lenses

import RL.Game

import Control.Monad.State
import Data.Maybe
import Data.List (find)

getLevel :: Game DLevel
getLevel = gets level

-- this is used internally to update the current dungeon level
setLevel :: DLevel -> Game ()
setLevel lvl = do
    game <- get
    put $ game { level = lvl }

getDungeon :: Game Dungeon
getDungeon = gets dungeon

setDungeon :: Dungeon -> Game ()
setDungeon dng = do
    game <- get
    put (game { dungeon = dng })

-- use this to change to a different dungeon level
changeLevel :: DLevel -> Game ()
changeLevel lvl = do
        game <- get
        let dng     = dungeon game
            lvl'    = atDepth (depth lvl) dng
            isStair = if depth lvl > depth (level game) then isUpStair else isDownStair

        when (depth lvl /= depth (level game)) $ do
            setLevel (maybe lvl (placeOnStair isStair) lvl')
            setDungeon . insertLevel (level game) . (`insertLevel` dng) =<< getLevel
    where
        placeOnStair isStair lvl' =
            let t         = findTile (isStair . snd) lvl'
                f  (p, _) = lvl' { player = (player lvl') { at = p } }
            in  maybe lvl' f t

getEvents :: Game [Event]
getEvents = gets events

getPlayer :: Game Player
getPlayer = gets $ player . level

setPlayer :: Player -> Game ()
setPlayer p = do
    game <- get
    lvl  <- getLevel
    setLevel $ lvl { player = p }

getMobs :: Game [Mob]
getMobs = fmap mobs getLevel

getMob :: Int -> Game (Maybe Mob)
getMob id = do
    ms <- getMobs
    return (find ((== id) . mobId) ms)

setMobs :: [Mob] -> Game ()
setMobs ms = do
    lvl <- getLevel
    setLevel $ lvl { mobs = filter (not . isPlayer) ms, player = maybe (player lvl) id (find isPlayer ms) }

setMob :: Mob -> Game ()
setMob m = do
    ms <- getMobs
    let ms' = map (\m' -> if m == m' then m else m') ms
    setMobs ms'

modifyMob :: Int -> (Mob -> Mob) -> Game ()
modifyMob id f = do
    m <- getMob id
    when (isJust m) $ setMob (f (fromJust m))

isGameWon :: Game Bool
isGameWon = do
    lvl <- getLevel
    ms  <- aliveMobs <$> getMobs
    let down = findTile (isDownStair . snd) lvl

    return ((null ms) && isNothing down)

-- getSeed :: Game StdGen
-- getSeed = do
--         s <- gets seed
--         maybe fallback return s  -- todo error/exception
--     where
--         fallback  = return $ mkStdGen 0

-- setSeed :: StdGen -> Game ()
-- setSeed g = do
--     game <- get
--     put $ game { seed = Just g }

getMobsWithPlayer :: Game [Mob]
getMobsWithPlayer = do
    lvl <- getLevel
    return (player lvl : mobs lvl)

getMobAt :: Point -> Game (Maybe Mob)
getMobAt p = fmap getMobAt' getMobs
    where getMobAt' = listToMaybe . getMobsAt
          getMobsAt = filter ((p ==) . at)

getTileAt :: Point -> Game (Maybe Tile)
getTileAt p = findTileAt p <$> getLevel

-- max map rows
maxRow :: Game Int
maxRow = do
    ts <- toTiles <$> getLevel
    return (length ts)

-- max map columns
maxColumn :: Game Int
maxColumn = do
    ts <- toTiles <$> getLevel
    when (length ts < 1) (error "Unable to find max column - tiles empty!")
    return (length $ ts !! 0)

send :: Event -> Game ()
send e = do
    env <- get
    put (env { events = (e:events env) })

-- convenience function to perform Game () action
whenEnv :: (Env -> Bool) -> Game () -> Game ()
whenEnv f k = do
    env <- get
    when (f env) k
