module RL.Client (Client(..)) where

-- Exposes Client class.
--
-- Clients do something to the Game state, such as:
--
--  taking user input and altering the state
--  randomly generated AI

import RL.Game

-- represents a client that does something to the state
-- (see: RL.Input and RL.AI)
class Client c where
    -- 1 turn tick, modifies   state & Client
    tick :: Client c => Action -> c -> Game c
