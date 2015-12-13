module RL.Game (
    GameState,
    Game(..),
    Message,
    module RL.Map,  -- export map
    module RL.Mob   -- export mobs
) where

-- represents the global Game and GameState types
--
-- GameState is a basic State transformer over the global Game object, and the
-- Renderer reader (which renders to the display).
--
-- The Game contains a Level (which represents the dungeon layout & mobs),
-- messages (things that have happened), and random seed (for RNG).

import RL.Mob
import RL.Map
import RL.Renderer

import Control.Monad.State
import Control.Monad.Reader
import System.Random

-- global game state
type GameState = StateT Game Renderer

-- global game
data Game = Game {
    level    :: Level,       -- current dungeon layout/mobs
    messages :: [Message],   -- things that happened so far
    seed     :: Maybe StdGen -- randomization
}

-- simple string event
type Message = String
