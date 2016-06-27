module RL.Generator (Generator, GenConfig(..), GenState(..), generate, runGenerator, runGenerator_, ioGenerator, mkGenState, getGCount, incGCount, getGData, appendGData) where

-- Basic random dungeon generator.
--
-- Encapsulated in own state machine, only needs Config and Random

import RL.Dice
import RL.Random
import RL.Types

import Control.Applicative
import Control.Monad (ap, liftM)
import Control.Monad.Cont
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (listToMaybe)

import Debug.Trace

data Generator s a = Generator (ReaderT GenConfig (Roller (State (GenState s))) a) | ContGenerator (ContT a (Generator s) a)

-- configuration to generate dwidth x dheight dungeon with specified sparsity (inverse density)
data GenConfig = GenConfig {
    dwidth   :: Int,
    dheight  :: Int,
    sparsity :: Int,
    gmax     :: Int
}

-- container for generator state
data GenState s = GenState {
    gdata  :: [s],
    gcount :: Int
}

-- constructor for function of Reader, StdGen, State
mkGenerator :: ((GenConfig, StdGen, GenState s) -> ((a, StdGen), GenState s)) -> Generator s a
mkGenerator f = Generator . ReaderT $ \r -> (mkRoller $ \g -> (state $ \s -> f (r, g, s)))

-- constructor for initial gen state
mkGenState :: [s] -> GenState s
mkGenState s = GenState { gdata = s, gcount = 0 }

runGenerator :: Generator s a -> GenConfig -> StdGen -> GenState s -> ((a, StdGen), GenState s)
runGenerator (Generator gen) c g = runState (runRoller genRoller g)
    where genRoller = runReaderT gen c
runGenerator gen@(ContGenerator cont) c g = runGenerator (runContT cont return) c g

runGenerator_ :: Generator s a -> GenConfig -> StdGen -> GenState s -> a
runGenerator_ gen c g s = fst . fst $ runGenerator gen c g s

ioGenerator :: Generator s a -> GenConfig -> IO (a, GenState s)
ioGenerator g c = newStdGen >>= ioGenerator'
    where ioGenerator' rng = let ((r, _), s) = runGenerator g c rng (mkGenState [])
                             in return (r, s)

-- Wrap a generator in a ContGenerator.
--
-- This attempts to endlessly generate a result, until either:
--
-- 1) gcount >= gmax
-- 2) isGenDone == True
--
-- Then, it returns the latest result.
generate :: Generator s a -> Generator s a
generate gen = ContGenerator (ContT continue)
    where continue next = do
            r <- gen
            i <- getGCount
            c <- ask
            done <- isGenDone

            -- have we hit the gcount limit, or are done generating?
            if i >= (gmax c) || done then
                next r
            else do
                incGCount
                continue next

isGenDone :: Generator s Bool
isGenDone = do
        c <- ask
        i <- length <$> getGData
        return (i >= sparsity c)
        -- TODO figure out better formula
        -- let n = f (fromIntegral $ dwidth c) (fromIntegral $ dheight c) (fromIntegral $ sparsity c)
        -- return (i + 1 >= n)
    where
        f = (\w h s -> floor $ (w * h) / s)

-- get generator data from state
getGData :: Generator s [s]
getGData = mkGenerator $ \(c, g, s) -> ((gdata s, g), s)

-- append data to state, also resetting the generator count
appendGData :: s -> Generator s ()
appendGData x = mkGenerator $ \(c, g, s) -> (((), g), appended s)
    where appended s = s { gdata = (x:gdata s), gcount = 0 }

-- get generator count
getGCount :: Generator s Int
getGCount = mkGenerator $ \(c, g, s) -> ((gcount s, g), s)

-- increment generator count
incGCount :: Generator s ()
incGCount = mkGenerator $ \(c, g, s) -> (((), g), inc s)
    where inc s = s { gcount = (gcount s + 1) }

instance Monad (Generator s) where
    gen >>= f = mkGenerator $ \(c, g, s) ->
        let ((r, g'), s') = runGenerator gen c g s
        in runGenerator (f r) c g' s'

    return = pure

instance MonadReader GenConfig (Generator s) where
    ask       = mkGenerator $ \(c, g, s) -> ((c, g), s)
    reader  f = mkGenerator $ \(c, g, s) -> ((f c, g), s)
    local f m = mkGenerator $ \(c, g, s) ->
        let c' = f c
        in runGenerator m c' g s

instance MonadRandom (Generator s) where
    getRandom     = Generator . ReaderT $ \c -> getRandom
    getRandoms    = Generator . ReaderT $ \c -> getRandoms
    getRandomR  r = Generator . ReaderT $ \c -> getRandomR r
    getRandomRs r = Generator . ReaderT $ \c -> getRandomRs r

instance MonadSplit StdGen (Generator s) where
    getSplit = Generator . ReaderT $ \c -> getSplit

instance Functor (Generator s) where
    fmap = liftM
instance Applicative (Generator s) where
    (<*>)  = ap
    pure x = mkGenerator $ \(c, rng, s) -> ((x, rng), s)
