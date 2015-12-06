module RL.IO ( io, debug ) where

import RL.Game
import RL.State

import Control.Monad.IO.Class (liftIO)
import UI.HSCurses.Curses

-- IO

-- basic IO helper
io :: IO a -> GameState a
io = liftIO

-- KISS
debug :: Show a => a -> GameState ()
debug str = io $ debug' str' >> return ()
    where debug' = mvWAddStr stdScr 20 0
          str'   = "debug: " ++ show str
