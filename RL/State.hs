module RL.State ( GameState, getMap, getMobs, getMobsWithPlayer, getMessages, getPlayer, getSeed, getMobAt, getTileAt, setSeed, setPlayer ) where

import RL.Game
import RL.IO
import RL.Map
import RL.Mob

-- core
import Control.Applicative
import Control.Monad.State
import Data.Maybe
import System.Random

-- state manipulation
--
-- helper getter/setter function for state manipulation
--
-- TODO look into lenses

-- main state monad - all functions with state act within this monad
type GameState = StateT Game IO

getMap :: GameState Map
getMap = level <$> get

getPlayer :: GameState Player
getPlayer = player <$> get

setPlayer :: Player -> GameState ()
setPlayer p = do
    m    <- getMap
    ms   <- getMobs
    g    <- getSeed
    msgs <- getMessages
    put $ Game { level = m, player = p, mobs = ms, seed = Just g, messages = msgs }

getMobs :: GameState [Mob]
getMobs = mobs <$> get

getSeed :: GameState StdGen
getSeed = do
         s <- seed <$> get
         maybe fallback return s  -- todo error/exception
    where
        fallback  = io seedError >> return fallback'
        fallback' = mkStdGen 0
        seedError = error "Invalid seed"

setSeed :: StdGen -> GameState ()
setSeed g = do
    m    <- getMap
    ms   <- getMobs
    p    <- getPlayer
    msgs <- getMessages
    put $ Game { level = m, player = p, mobs = ms, seed = Just g, messages = msgs }

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

getMessages :: GameState [Message]
getMessages = messages <$> get
