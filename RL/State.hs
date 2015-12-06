module RL.State ( GameState, getMap, getMobs, getMobsWithPlayer, getMessages, getPlayer, getSeed, getMobAt, getTileAt, setSeed, setPlayer ) where

import RL.Game
import RL.Map
import RL.Mob

-- core
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
getMap = fmap level get

getPlayer :: GameState Player
getPlayer = fmap player get

setPlayer :: Player -> GameState ()
setPlayer p = do
    game <- get
    put $ game { player = p }

getMobs :: GameState [Mob]
getMobs = fmap mobs get

getSeed :: GameState StdGen
getSeed = do
        s <- fmap seed get
        maybe fallback return s  -- todo error/exception
    where
        fallback  = return $ mkStdGen 0

setSeed :: StdGen -> GameState ()
setSeed g = do
    game <- get
    put $ game { seed = Just g }

getMobsWithPlayer :: GameState [Mob]
getMobsWithPlayer = do
    game <- get
    return (player game : mobs game)

getMobAt :: Point -> GameState (Maybe Mob)
getMobAt p = fmap (getMobAt' . mobs) get
    where getMobAt' = listToMaybe . getMobsAt
          getMobsAt = filter ((p ==) . at)

getTileAt :: Point -> GameState (Maybe Tile)
getTileAt p = do
        game <- get
        return $ filterMap (enumerateMap $ level game)
    where filterMap          = listToMaybe . map snd . filter filterTile
          filterTile (p', t) = p == p'

getMessages :: GameState [Message]
getMessages = fmap messages get
