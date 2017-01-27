module RL.State where

-- state manipulation
--
-- helper getter/setter function for state manipulation
--
-- TODO look into lenses

import RL.Game

import Control.Monad.State
import Data.Maybe

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
-- TODO set player to up/down stair
changeLevel :: DLevel -> Game ()
changeLevel lvl = do
    game <- get
    let dng  = dungeon game
        lvl' = atDepth (depth lvl) dng

    when (lvl /= level game) $ do
        setLevel (maybe lvl id lvl')
        when (isNothing lvl') $ setDungeon (insertLevel lvl dng)

-- generateLevel :: Int -> Game ()
-- generateLevel depth = do
--     conf <- ask
--     g    <- getSplit
--     let (lvl, s) = runGenerator (levelGenerator depth) conf (initState g)
--     setLevel (lvl)

getMessages :: Game [Message]
getMessages = gets messages

getPlayer :: Game Player
getPlayer = gets $ player . level

setPlayer :: Player -> Game ()
setPlayer p = do
    game <- get
    lvl  <- getLevel
    setLevel $ lvl { player = p }

getMobs :: Game [Mob]
getMobs = fmap mobs getLevel

setMobs :: [Mob] -> Game ()
setMobs ms = do
    lvl <- getLevel
    setLevel $ lvl { mobs = filterMobs ms }

isGameWon :: Game Bool
isGameWon = do
    ms <- filterMobs <$> getMobs
    return $ null ms

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
    return (length $ ts !! 0)

sendMessage :: Message -> Game ()
sendMessage msg = do
    game <- get
    put $ game { messages = msg : (messages game) }
