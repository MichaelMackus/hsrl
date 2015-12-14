module RL.Setup.Dungeon where

-- Basic random dungeon generator
--
-- TODO encapsulate in own state machine, only needs Map/Level

import RL.Dice
import RL.Game
import RL.IO

setupDungeon :: GameState ()
setupDungeon = return ()

-- generate cell
cell :: Cell -> GameState DCell
cell (Cell d(w, h)) = do
    start <- randomBlankPoint
    -- todo build cell around start
cell  otherwise     = return

genTile :: Point -> GameState Tile

-- possible dungeon cells (w x h)
cells :: [Cell]
cells = [ 9 `x` 9,
          3 `x` 3,
          4 `x` 4,
          3 `x` 9,
          9 `x` 3 ]

--          non-generated  generated dungeon tiles
data Cell = Cell Dimension | DCell Point Tiles

type Dimension = (Width, Height)
type Width     = Int
type Height    = Int

x :: Int -> Int -> Cell
x w h = Cell w h

-- generates random map point
randomBlankPoint :: GameState Point
randomBlankPoint =  do
    cols <- maxColumn
    rows <- maxRow
    p    <- randomPoint cols rows
    t    <- getTileAt p
    if isPassable t then
        return p
    else
        randomBlankPoint

