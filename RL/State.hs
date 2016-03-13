module RL.State where

-- state manipulation
--
-- helper getter/setter function for state manipulation
--
-- TODO look into lenses

import RL.Game
import RL.Util

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import System.Random

getLevel :: GameState Level
getLevel = gets level

setLevel :: Level -> GameState ()
setLevel lvl = do
    game <- get
    put $ game { level = lvl }

getMessages :: GameState [Message]
getMessages = gets messages

getTiles :: GameState Tiles
getTiles = gets $ tiles . level

getPlayer :: GameState Player
getPlayer = gets $ player . level

setPlayer :: Player -> GameState ()
setPlayer p = do
    game <- get
    lvl  <- getLevel
    setLevel $ lvl { player = p }

getMobs :: GameState [Mob]
getMobs = fmap mobs getLevel

setMobs :: [Mob] -> GameState ()
setMobs ms = do
    lvl <- getLevel
    setLevel $ lvl { mobs = filterMobs ms }

isGameWon :: GameState Bool
isGameWon = do
    ms <- filterMobs <$> getMobs
    return $ null ms

getSeed :: GameState StdGen
getSeed = do
        s <- gets seed
        maybe fallback return s  -- todo error/exception
    where
        fallback  = return $ mkStdGen 0

setSeed :: StdGen -> GameState ()
setSeed g = do
    game <- get
    put $ game { seed = Just g }

getMobsWithPlayer :: GameState [Mob]
getMobsWithPlayer = do
    lvl <- getLevel
    return (player lvl : mobs lvl)

getMobAt :: Point -> GameState (Maybe Mob)
getMobAt p = fmap getMobAt' getMobs
    where getMobAt' = listToMaybe . getMobsAt
          getMobsAt = filter ((p ==) . at)

getTileAt :: Point -> GameState (Maybe Tile)
getTileAt p = do
        map <- getTiles
        return $ filterTiles (enumerate2 map)
    where filterTiles         = listToMaybe . map itToTile . filter filterTile
          filterTile  (p', t) = p == p'
          itToTile    (_,  t) = t

-- max map rows
maxRow :: GameState Int
maxRow = do
    m <- getTiles
    return (length m)

-- max map columns
maxColumn :: GameState Int
maxColumn = do
    m <- getTiles
    return (length $ m !! 0)

sendMessage :: Message -> GameState ()
sendMessage msg = do
    game <- get
    put $ game { messages = msg : (messages game) }
