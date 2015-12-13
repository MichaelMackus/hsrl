module RL.Setup.Dungeon where

-- Basic random dungeon generator

import RL.Dice
import RL.Game
import RL.IO

setupDungeon :: GameState ()
setupDungeon = return ()

data Cell = Cell Int Int Tiles

cell :: Int -> Int -> Cell
