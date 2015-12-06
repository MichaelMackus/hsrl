module RL.Game ( Game(..), GameState, Message, Mob, Player ) where

import RL.Map
import RL.Mob

import System.Random
import Control.Monad.State

-- global game

data Game = Game {
    level :: Map,
    player :: Player,
    mobs :: [Mob],
    seed :: Maybe StdGen,
    messages :: [Message]
}

type GameState = StateT Game IO

type Message = String

