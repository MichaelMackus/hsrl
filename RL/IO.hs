module RL.IO (io) where

import RL.Game
import RL.State

import Control.Monad.IO.Class (liftIO)

-- IO

-- basic IO helper
io :: IO a -> GameState a
io = liftIO
