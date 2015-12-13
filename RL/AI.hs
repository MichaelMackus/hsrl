module RL.AI (AI(..)) where

import RL.Game

-- AI
data AI = AI

instance Client AI where
    tick ai = do
        return ai

