module RL.Generator.Cells (Cell(..), cells, getTileAt, cpoint, cwidth, cheight, ctiles) where

import RL.Generator
import RL.Map
import RL.Random

import Control.Monad (when)
import Control.Monad.Reader (ask)
import Data.Maybe (listToMaybe)

data Cell = C Point [[Tile]]

instance Eq Cell where
    -- TODO OR tiles intersect
    c == c' = cpoint c == cpoint c'

-- generate a list of dungeon cells
cells :: Generator [Cell] [Cell]
cells = do
        c     <- cell
        inDng <- inDungeon c
        cs    <- getGData

        -- append cell if not touching any other cells
        let touchingCells = filter (isIntersectingPad 1 c) cs
        when (inDng && null touchingCells) $ appendGData c

        -- mark generation as done if we hit maxCells
        cs' <- getGData
        when (length cs' >= maxCells) markGDone

        return cs'
    where
        maxCells = 10 -- TODO this should be based on a formula of the dungeon dimensions

-- generate random dungeon cell
cell :: Generator [Cell] Cell
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

-- just a blank dungeon cell
genCell :: Point -> Dimension -> Cell
genCell p (w, h) = C p buildCell
    where buildCell = blankMap w h

-- get a tile at specific point
getTileAt :: Point -> [Cell] -> Maybe Tile
getTileAt (x, y) cs = maybe Nothing (const (Just Floor)) cell
    where cell  = listToMaybe $ filter cAt cs
          cAt c = let (cx, cy) = cpoint c
                      (cw, ch) = (cx + cwidth c, cy + cheight c)
                  in  (x >= cx && x < cw) && (y >= cy && y < ch)

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
inDungeon :: Cell -> Generator [Cell] Bool
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
