module RL.Game (Game(..), Client(..), Message, GameState) where

import RL.Mob
import RL.Map

import Control.Monad.State
import Control.Monad.Reader
import Graphics.Vty
import System.Random

-- global game state
type GameState = StateT Game Display

type Display = ReaderT Vty IO

-- global game
data Game = Game {
    level    :: Level,       -- current dungeon layout/mobs
    clients  :: [Client],    -- player and AI client(s)
    messages :: [Message],   -- things that happened so far
    seed     :: Maybe StdGen -- randomization
}

-- represents AI or player client
data Client = AIClient | UIClient

-- simple string event
type Message = String
