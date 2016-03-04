module RL.Setup.Dungeon where

-- Basic random dungeon generator.
--
-- Encapsulated in own state machine, only needs Config and Random

import RL.Dice
import RL.Game
import RL.IO
import RL.Map

import Data.Maybe
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Random

type DState     = StateT DLayout DGenerator
type DGenerator = RandT StdGen (Reader DConfig)

type DLayout = ([Cell], [Passage])
data DConfig = DConfig {
    dwidth   :: Int,
    dheight  :: Int,
    maxCells :: Int
}

-- generate a randomized dungeon
generateIODungeon :: DConfig -> IO (TilesIterator, DLayout)
generateIODungeon c = newStdGen >>= (\g -> return $ generateDungeon g c)

-- generate a randomized dungeon
generateDungeon :: StdGen -> DConfig -> (TilesIterator, DLayout)
generateDungeon = runReader . evalRandT generator
    where
        generator :: DGenerator (TilesIterator, DLayout)
        generator = runStateT generateTiles initialL
        initialL  = ([], [])

-- setup the randomized dungeon
generateTiles :: DState TilesIterator
generateTiles = do
    generateCells =<< asks maxCells
    cs' <- gets fst
    toTiles cs'


-- represents a box of tiles
data Cell = C Point Tiles deriving (Show)

--       max    random cells
generateCells :: Int -> DState ()
generateCells max = replicateM_ max openCell

-- generate blank cell
openCell :: DState ()
openCell = do
        (cs, pass) <- get
        c          <- cell

        let touchingCells = filter (isTouching c) cs
        if null touchingCells then
            put (c : cs, pass)
        else
            openCell
    where
        isTouching c c2 = cpoint c2 == cpoint c

toTiles :: [Cell] -> DState TilesIterator
toTiles cs = do
        c <- ask
        -- TODO un-iterate
        return (map getTile $ blankTiles c)
    where
        getTile i@(p, t) = maybe i ((,) p) $ tileAt p cs
        blankTiles     c = iterateTiles $ take (dheight c) (repeat . take (dwidth c) $ repeat Rock)
        -- uniterate     ts = snd . unzip

tileAt :: Point -> [Cell] -> Maybe Tile
tileAt p = listToMaybe . mapMaybe maybeTileIn
    where
        maybeTileIn :: Cell -> Maybe Tile
        maybeTileIn = lookup p . iterateCTiles
        iterateCTiles :: Cell -> TilesIterator
        iterateCTiles  c          = map (addPoint c) $ iterateTiles (ctiles c)
        addPoint       c (p', t)  = (addPoint' (cpoint c) p', t)
        addPoint' (x, y) (x', y') = (x + x', y + y')


-- generate random dungeon cell
-- TODO convert rand function
cell :: DState Cell
cell = do
        dim   <- getDim
        start <- randomCellPoint dim
        return $ genCell start dim
    where
        getDim         = (dims !!) <$> (droll $ 1 `d` (length dims - 1))
        dims           = [ 3 `x` 3,
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
randomCellPoint :: Dimension -> DState Point
randomCellPoint (w, h) = do
        c <- ask

        let cols = dwidth c
            rows = dheight c

        p <- randomDPoint rows cols

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
genCell :: Point -> Dimension -> Cell
genCell p (w, h) = C p buildCell
    where buildCell = replicate h (replicate w Floor)

-- helper function to roll within DState
--
-- TODO remove and refactor roll function with MonadRandom
droll :: Dice -> DState Int
droll (D n ns) = lift $ getRandomR (n, ns * n)

-- generates random point
-- between     maxX   maxY
--
-- TODO remove and refactor roll function with MonadRandom
randomDPoint :: Int -> Int -> DState Point
randomDPoint x y = liftM2 (,) (droll $ 1 `d` x) (droll $ 1 `d` y)

-- dungeon cell box (w x h)
type Dimension = (Width, Height)
type Width     = Int
type Height    = Int

-- used like: 2 `x` 4
x :: Int -> Int -> Dimension
x w h = (w, h)

--      cell to dim
ctiles :: Cell -> Tiles
ctiles (C _ ts) = ts

--      cell to dim
cdim :: Cell -> Dimension
cdim (C _ c) = (length $ c !! 0, length c)

--      cell to point
cpoint :: Cell -> Point
cpoint (C p _) = p

