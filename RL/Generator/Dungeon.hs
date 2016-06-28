module RL.Generator.Dungeon where

-- Basic random dungeon generator.
--
-- Encapsulated in own state machine, only needs Config and Random

import RL.Dice
import RL.Random
import RL.Types
import RL.Generator

import Control.Applicative
import Control.Monad (ap, liftM)
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (listToMaybe)

-- delegates to runGenerator dgenerator
generateDungeon :: GenConfig -> StdGen -> Dungeon
generateDungeon c g = let ((a, g'), s) = runGenerator dgenerator c g (mkGenState []) in a

-- Quick Map generator
dgenerator :: Generator s Dungeon
dgenerator = do
        conf <- ask
        g    <- getSplit
        let cs = runGenerator_ cells conf g (mkGenState [])
        return (toDungeon conf cs)
    where
        toDungeon conf cs = iterMap fillDng blankDng
            where blankDng     = mkDungeon $ blankMap (dwidth conf) (dheight conf)
                  fillDng  p t = maybe t id $ getTileAt p cs

data Cell = C Point [[Tile]] deriving (Show)

-- generate a list of dungeon cells
cells :: Generator Cell [Cell]
cells = generate maxTries $ do
        c     <- cell
        inDng <- inDungeon c
        cs    <- getGData
        let touchingCells = filter (isIntersectingPad 1 c) cs
        when (inDng && null touchingCells) $ appendGData c
        getGData
    where maxTries = 5

-- generate random dungeon cell
cell :: Generator Cell Cell
cell = do
        dim   <- getDim
        start <- randomCellPoint dim
        return $ genCell start dim
    where
        getDim         = (dims !!) <$> (roll $ 1 `d` (length dims - 1))
        dims           = [ 3 `x` 3,
                           4 `x` 4,
                           3 `x` 6,
                           6 `x` 3,
                           6 `x` 6 ]

-- generates random map point for particular dimensions
randomCellPoint :: Dimension -> Generator s Point
randomCellPoint (w, h) = do
    c <- ask
    let cols = dwidth c
        rows = dheight c

    randomPoint (cols - 1) (rows - 1)

-- get a tile at specific point
getTileAt :: Point -> [Cell] -> Maybe Tile
getTileAt (x, y) cs = do
        c <- cell
        Just '*'
        -- Just (tileIn c)
    where cell  = listToMaybe $ filter cAt cs
          cAt c = let (cx, cy) = cpoint c
                      (cw, ch) = (cx + cwidth c, cy + cheight c)
                  in  (x >= cx && x < cw) && (y >= cy && y < ch)


-- just a blank dungeon cell
genCell :: Point -> Dimension -> Cell
genCell p (w, h) = C p buildCell
    where buildCell = blankMap w h


-- tests if a cell intersects another cell (collision detection) with padding
isIntersectingPad :: Int -> Cell -> Cell -> Bool
isIntersectingPad p c c2 = isIntersecting (pad c) (pad c2)
    where pad (C (x, y) ts) = (C (x - p, y - p) $ padTs ts)
          padTs             = map padTs' . padTs'
          padTs'        []  = []
          padTs'        ts  = (head ts):ts ++ [last ts]

-- tests if a cell intersects another cell (collision detection)
isIntersecting :: Cell -> Cell -> Bool
isIntersecting c c2 = (((leftX c >= leftX c2 && leftX c <= rightX c2)
                            || (rightX c >= leftX c2 && rightX c <= rightX c2))
                            || ((leftX c2 >= leftX c && leftX c2 <= rightX c)
                            || (rightX c2 >= leftX c && rightX c2 <= rightX c)))
                        && (((topY c >= topY c2 && topY c <= botY c2)
                            || (botY c >= topY c2 && botY c <= botY c2))
                            || (topY c2 >= topY c && topY c2 <= botY c)
                            || (botY c2 >= topY c && botY c2 <= botY c))


-- tests if a cell is within dungeon boundaries
inDungeon :: Cell -> Generator Cell Bool
inDungeon c = do
    conf <- ask

    let mx = dwidth conf - 1
        my = dheight conf - 1

    return (leftX c >= 0 && rightX c <= mx && topY c >= 0 && botY c <= my)

-- cell points
leftX  c = fst $ cpoint c
rightX c = cwidth c + leftX c
topY   c = snd $ cpoint c
botY   c = cheight c + topY c
cpoint  (C p _ ) = p
cwidth  (C _ ts) = length $ head ts
cheight (C _ ts) = length ts
ctiles  (C _ ts) = ts
