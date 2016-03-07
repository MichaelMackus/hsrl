module RL.Setup.Dungeon where

-- Basic random dungeon generator.
--
-- Encapsulated in own state machine, only needs Config and Random

import RL.Dice
import RL.IO
import RL.Map
import RL.Util

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

-- generate dungeon, via IO seeded RNG
generateIO :: DConfig -> IO Tiles
generateIO = evalRandIO . liftRand . generateDungeon

-- generate a randomized dungeon
--
-- Format is same as randomR, so you can do:
--
--  `liftRand generateDungeon config`
--
-- From any `Rand StdGen a` context.
generateDungeon :: DConfig -> StdGen -> (Tiles, StdGen)
generateDungeon c g = runReader (runRandT generator g) c

-- dungeon generator
generator :: DGenerator Tiles
generator = evalStateT generateTiles initialL
    where initialL  = ([], [])

-- setup the randomized dungeon
generateTiles :: DState Tiles
generateTiles = do
    max <- asks maxCells
    replicateM_ max openCell
    getDTiles


-- represents a box of tiles
data Cell = C Point Tiles deriving (Show)

-- generate blank cell
openCell :: DState Cell
openCell = do
        (cs, pass) <- get
        c          <- cell

        let touchingCells = filter (isTouching c) cs
        if null touchingCells then do
            put (c : cs, pass)
            return c
        else
            openCell
    where
        isTouching c c2 = cpoint c2 == cpoint c

getDTiles :: DState Tiles
getDTiles = do
    conf <- ask
    cs   <- gets fst

    return $ toTiles conf cs

toTiles :: DConfig -> [Cell] -> Tiles
toTiles conf cs = unenumerate2 . map updateTile . enumerate2 $ blankTiles
    where
        updateTile (p, t) = (p, maybe t id $ tileAt p cs)
        blankTiles        = take (dwidth conf) (repeat . take (dwidth conf) $ repeat Rock)


tileAt :: Point -> [Cell] -> Maybe Tile
tileAt p = listToMaybe . mapMaybe maybeTileIn
    where
        maybeTileIn :: Cell -> Maybe Tile
        maybeTileIn = lookup p . iterateCTiles
        iterateCTiles :: Cell -> TilesIterator
        iterateCTiles  c          = map (addPoint c) $ enumerate2 (ctiles c)
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

