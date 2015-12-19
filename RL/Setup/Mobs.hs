module RL.Setup.Mobs where

-- todo move to mob generator
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

