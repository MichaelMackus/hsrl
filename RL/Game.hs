module RL.Game ( Game(..), Message, Mob, Player ) where

import RL.Map
import RL.Mob

import System.Random

-- global game

data Game = Game {
    level :: Map,
    player :: Player,
    mobs :: [Mob],
    seed :: Maybe StdGen,
    messages :: [Message]
}

type Message = String

