module RL.DungeonGenerator (generateDungeon) where

-- Basic random dungeon generator.
--
-- Encapsulated in own state machine, only needs Config and Random

import RL.Dice
import RL.Random
import RL.Types

import Control.Applicative
import Control.Monad (ap, liftM)
import Control.Monad.Reader
import Control.Monad.State

data Generator s a = Generator ((GenConfig, GenState s) -> (a, GenState s))
data GenState  s = GenState {
    rng    :: StdGen, -- RNG
    gdata  :: [s],    -- container for data
    gcount :: Int     -- increment of generator (easy failover to prevent endless loop)
}
data GenConfig = GenConfig {
    dwidth  :: Int,
    dheight :: Int
}

-- delegates to runGenerator dgenerator
generateDungeon :: GenConfig -> StdGen -> (Dungeon, StdGen)
generateDungeon c g = let (r, s) = runGenerator dgenerator c initState in (r, rng s)
    where initState = GenState g [] 0

-- Quick Map generator
dgenerator :: Generator s Dungeon
dgenerator = do
    c <- ask
    let blankDng = mkDungeon $ blankMap (dwidth c, dheight c)
    return blankDng

type CellGenerator = Generator Cell
data Cell = C Point [[Tile]] deriving (Show)

cgenerator :: CellGenerator [Cell]
cgenerator = do
    c       <- cell
    inDng   <- inDungeon c
    s       <- get
    let touchingCells = filter (isIntersecting c) cs
        (cs, i)       = (gdata s, gcount s)

    if inDng && null touchingCells then
        (put $ s { gdata = (c:cs), gcount = 0 }) >> return (c:cs)
    else do
        -- increment counter, and only continue when < 3 consecutive tries, TODO this can be abstracted into monad
        put $ s { gdata = cs, gcount = i + 1 }
        if (i + 1 < 3) then cgenerator else return cs

-- generate random dungeon cell
cell :: CellGenerator Cell
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
genCell p dim = C p buildCell
    where buildCell = blankMap dim

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
inDungeon :: Cell -> CellGenerator Bool
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
cwidth  (C _ ts) = head $ map length ts
cheight (C _ ts) = length ts

runGenerator :: Generator s a -> GenConfig -> GenState s -> (a, GenState s)
runGenerator (Generator gen) c s = gen (c, s)

instance Monad (Generator s) where
    gen >>= f = Generator $ \(c, s) ->
        let (r, s) = runGenerator gen c s
        in runGenerator (f r) c s

    return = pure

instance MonadState (GenState s) (Generator s) where
    get    = Generator $ \(c, s) -> (s,  s)
    put s' = Generator $ \(c, s) -> ((), s')

instance MonadReader GenConfig (Generator s) where
    ask       = Generator $ \(c, s) -> (c, s)
    reader  f = Generator $ \(c, s) -> (f c, s)
    local f m = Generator $ \(c, s) ->
        let c' = f c
        in runGenerator m c' s

instance MonadRandom (Generator s) where
    getRandom     = withRng random
    getRandoms    = withRng $ \g -> (randoms g, g)
    getRandomR    = withRng . randomR
    getRandomRs r = withRng $ \g -> (randomRs r g, g)

withRng :: (StdGen -> (a, StdGen)) -> Generator s a
withRng f = Generator $ \(c, s) ->
    let (r, g) = f $ rng s
    in  (r, s { rng = g })

instance Functor (Generator s) where
    fmap = liftM
instance Applicative (Generator s) where
    (<*>)  = ap
    pure x = Generator $ \(c, rng) -> (x, rng)
