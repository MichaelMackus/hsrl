module RL.Client.AI ( AI(..), module RL.Client ) where

-- basic AI
--
-- TODO do something

import RL.Game
import RL.Client

-- AI
data AI = AI

instance Client AI where
    tick _ ai = do
        return ai

