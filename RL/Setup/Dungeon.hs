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

type Dungeon    = StateT DState DGenerator
type DGenerator = RandT StdGen (Reader DConfig)

data DState  = DState {
    dcells :: [Cell],
    dcount :: Int
}

data DConfig = DConfig {
    dwidth   :: Int,
    dheight  :: Int,
    maxCells :: Int
}

-- generate dungeon, via IO seeded RNG
generateIO :: DConfig -> IO (Tiles, DState)
generateIO = evalRandIO . liftRand . generateDungeon

-- generate a randomized dungeon
--
-- Format is same as randomR, so you can do:
--
--  `liftRand generateDungeon config`
--
-- From any `Rand StdGen a` context.
generateDungeon :: DConfig -> StdGen -> ((Tiles, DState), StdGen)
generateDungeon c g = runReader (runRandT generator g) c

-- dungeon generator
generator :: DGenerator (Tiles, DState)
generator = runStateT generateTiles initialDng
    where initialDng = DState { dcells = [], dcount = 0 }

-- setup the randomized dungeon
generateTiles :: Dungeon Tiles
generateTiles = do
    max <- asks maxCells
    replicateM_ max openCell
    getDTiles


-- represents a box of tiles
data Cell = C Point Tiles deriving (Show)

-- generate blank cell
openCell :: Dungeon ()
openCell = do
        dng   <- get
        c     <- cell
        inDng <- inDungeon c

        let touchingCells = filter (isIntersecting c) $ dcells dng
        if inDng && null touchingCells then
            put $ dng { dcells = c : (dcells dng), dcount = 0 }
        else
            -- increment counter, and only continue when < 3 consecutive tries
            let dng'     = dng { dcount = dcount dng + 1 }
                continue = dcount dng' < 3
            in  when continue $ put dng' >> openCell

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
    where

-- tests if a cell is within dungeon boundaries
inDungeon :: Cell -> Dungeon Bool
inDungeon c = do
    conf <- ask

    let mx = dwidth conf - 1
        my = dheight conf - 1

    return (leftX c >= 0 && rightX c <= mx && topY c >= 0 && botY c <= my)


getDTiles :: Dungeon Tiles
getDTiles = do
    conf <- ask
    cs   <- gets dcells

    return $ toTiles conf cs

toTiles :: DConfig -> [Cell] -> Tiles
toTiles conf cs = unenumerate2 . map updateTile $ enumerate2 blankTiles
    where
        updateTile (p, t) = (p, maybe t id $ tileAt p cs)
        blankTiles        = replicate (dheight conf - 1) (replicate (dwidth conf - 1) Rock)


tileAt :: Point -> [Cell] -> Maybe Tile
tileAt p = listToMaybe . mapMaybe maybeTileIn
    where
        maybeTileIn :: Cell -> Maybe Tile
        maybeTileIn = lookup p . iterateCTiles
        iterateCTiles :: Cell -> TilesIterator
        iterateCTiles  c          = map (addPoint c) $ enumerate2 (ctiles c)
        addPoint       c (p, t)   = (addCPoint c p, t)


addCPoint :: Cell -> Point -> Point
addCPoint c (x, y) = (cx + x, cy + y)
    where cx = fst $ cpoint c
          cy = snd $ cpoint c


-- generate random dungeon cell
-- TODO convert rand function
cell :: Dungeon Cell
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
randomCellPoint :: Dimension -> Dungeon Point
randomCellPoint (w, h) = do
    c <- ask

    let cols = dwidth c
        rows = dheight c

    randomDPoint (cols - 1) (rows - 1)


-- just a blank dungeon cell
genCell :: Point -> Dimension -> Cell
genCell p (w, h) = C p buildCell
    where buildCell = replicate h (replicate w Floor)

-- helper function to roll within Dungeon
--
-- TODO remove and refactor roll function with MonadRandom
droll :: Dice -> Dungeon Int
droll (D n ns) = lift $ getRandomR (n, ns * n)

-- generates random point
-- between     maxX   maxY
--
-- TODO remove and refactor roll function with MonadRandom
randomDPoint :: Int -> Int -> Dungeon Point
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

--      cell to width
cwidth :: Cell -> Int
cwidth = fst . cdim

--      cell to height
cheight :: Cell -> Int
cheight = snd . cdim

--      cell to point
cpoint :: Cell -> Point
cpoint (C p _) = p

-- cell points
leftX  c = fst $ cpoint c
rightX c = cwidth c + leftX c
topY   c = snd $ cpoint c
botY   c = cheight c + topY c
