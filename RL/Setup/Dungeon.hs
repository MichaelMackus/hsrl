module RL.Setup.Dungeon where

-- Basic random dungeon generator
--
-- TODO encapsulate in own state machine, only needs Map/Level and Random

import RL.Dice
import RL.Game
import RL.IO

import Control.Applicative
import Control.Monad.State

type DungeonState = State Dungeon
type Dungeon      = ([Cell], [Passage])

-- setup the randomized dungeon
setupDungeon :: DungeonState ()
setupDungeon = return ()

-- represents a box of tiles
data Cell = C Point Tiles deriving (Show)

--       max    random cells
-- cells :: Int -> GameState [Cell]
-- cells max = take max <$> cells' []
--     where
--         cells' cs = do
--             c <- openCell
--             repeat openCell
--             return (cs ++ [c])

-- generate blank cell
openCell :: [Cell] -> GameState Cell
openCell cs = do
        c <- cell

        let touchingCells = filter (not . isTouching c) cs
        if null touchingCells then
            return c
        else
            openCell cs
    where
        isTouching c c2 = cpoint c2 == cpoint c

-- generate random dungeon cell
cell :: GameState Cell
cell = do
        dim   <- getDim
        start <- randomCellPoint dim
        genCell start dim
    where
        getDim = (dims !!) <$> (roll $ 1 `d` (length dims - 1))
        dims   = [ 3 `x` 3,
                   4 `x` 4,
                   3 `x` 6,
                   6 `x` 3,
                   6 `x` 6 ]

-- generate random path
-- path :: Cell -> Cell -> GameState Passage
-- path (C (x1, y1) t1s) (C (x2, y2) t2s) = do
    -- edge points

-- represents a straight path (between cells)
data Passage = Passage Point Dir Tiles
data Dir     = North | East | South | West

-- generates random map point for particular dimensions
randomCellPoint :: Dimension -> GameState Point
randomCellPoint (w, h) = do
        rows <- maxRow
        cols <- maxColumn
        p    <- randomPoint rows cols
        
        --          pad for walls
        let maxP = (rows - 1, cols - 1)

        if willFit p maxP then
            return p
        else
            randomCellPoint (w, h)

    where
        willFit (x, y) (maxX, maxY) = (x > 1 && y > 1) &&
                                      ((x + w) < maxX) &&
                                      ((y + h) < maxY)


-- just a blank dungeon cell
genCell :: Point -> Dimension -> GameState Cell
genCell p (w, h) = return (C p buildCell)
    where buildCell = replicate h (replicate w Floor)

-- dungeon cell box (w x h)
type Dimension = (Width, Height)
type Width     = Int
type Height    = Int

-- used like: 2 `x` 4
x :: Int -> Int -> Dimension
x w h = (w, h)

--      cell to dim
cdim :: Cell -> Dimension
cdim (C _ c) = (length $ c !! 0, length c)

--      cell to point
cpoint :: Cell -> Point
cpoint (C p _) = p

