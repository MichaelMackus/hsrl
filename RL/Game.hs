module RL.Game ( Game(..), Message, Mob, Player, GameState ) where

import RL.Mob
import RL.Map
import RL.Renderer

import Control.Monad.State
import System.Random

-- global game structure
data Game = Game {
    -- todo  Level
    level :: Map,
    player :: Player,
    mobs :: [Mob],
    seed :: Maybe StdGen,
    messages :: [Message]
}

type Message = String

-- main state monad - all functions with state act within this monad
type GameState = StateT Game Renderer
