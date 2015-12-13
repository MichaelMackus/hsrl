module RL.State (
    getLevel,
    getMessages,
    getMap,
    getMobs,
    setMobs,
    getMobsWithPlayer,
    sendMessage,
    getPlayer,
    setPlayer,
    getMobAt,
    getTileAt,
    getSeed,
    setSeed,
) where

import RL.Game
import RL.Map
import RL.Mob

import Control.Monad.State
import Data.Maybe
import System.Random

-- state manipulation
--
-- helper getter/setter function for state manipulation
--
-- TODO look into lenses

getLevel :: GameState Level
getLevel = gets level

setLevel :: Level -> GameState ()
setLevel lvl = do
    game <- get
    put $ game { level = lvl }

getMessages :: GameState [Message]
getMessages = gets messages

getMap :: GameState Map
getMap = gets $ tiles . level

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
        map <- getMap
        return $ filterMap (iterateMap map)
    where filterMap          = listToMaybe . map snd . filter filterTile
          filterTile (p', t) = p == p'

sendMessage :: Message -> GameState ()
sendMessage msg = do
    game <- get
    put $ game { messages = msg : (messages game) }
