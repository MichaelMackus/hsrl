module RL.Generator (Generator, GenConfig(..), GenState, generate, runGenerator, runGenerator_, ioGenerator, mkGenState, getGData, appendGData) where

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

-- Generator monad which generates a list of objects of type s based on GenConfig.
-- ContGenerator represents a Generator wrapped in a Cont monad - see "generate".
data Generator s a = Generator (ReaderT GenConfig (Roller (State (GenState s))) a) | ContGenerator (ContT a (Generator s) a)

-- Configuration to generate something within dwidth x dheight dungeon
-- gmax is the max length of the state (when generation is considered done)
data GenConfig = GenConfig {
    dwidth   :: Int,
    dheight  :: Int,
    gmax     :: Int
}

-- Generator state
type GenState s = [s]

-- run a generator, returning the full modified RNG & state
runGenerator :: Generator s a -> GenConfig -> StdGen -> GenState s -> ((a, StdGen), GenState s)
runGenerator (Generator gen) c g = runState (runRoller genRoller g)
    where genRoller = runReaderT gen c
runGenerator gen@(ContGenerator cont) c g = runGenerator (runContT cont return) c g

-- run a generator, returning only the result
runGenerator_ :: Generator s a -> GenConfig -> StdGen -> GenState s -> a
runGenerator_ gen c g s = fst . fst $ runGenerator gen c g s

-- run a generator through IO
ioGenerator :: Generator s a -> GenConfig -> IO (a, GenState s)
ioGenerator g c = newStdGen >>= ioGenerator'
    where ioGenerator' rng = let ((r, _), s) = runGenerator g c rng (mkGenState [])
                             in return (r, s)

-- Wrap a generator in a ContGenerator.
--
-- This attempts to endlessly generate a result, until either:
--
-- 1) i >= maxTries
-- 2) length getGData >= gmax
--
-- Then, it returns the latest result.
generate :: Int -> Generator s a -> Generator s a
generate maxTries gen = ContGenerator (ContT $ continue 0)
    where
        continue i next = do
            prevLen <- length <$> getGData
            r       <- gen
            c       <- ask
            d       <- getGData
            curLen  <- length <$> getGData

            -- have we hit the max limit, or are done generating?
            if i >= maxTries || curLen >= gmax c then
                next r
            else
                -- reset i if we've appended data
                if curLen > prevLen then
                    continue 0 next
                else
                    continue (i + 1) next

-- constructor for function of Reader, StdGen, State
mkGenerator :: ((GenConfig, StdGen, GenState s) -> ((a, StdGen), GenState s)) -> Generator s a
mkGenerator f = Generator . ReaderT $ \r -> (mkRoller $ \g -> (state $ \s -> f (r, g, s)))

-- constructor for initial gen state
mkGenState :: [s] -> GenState s
mkGenState s = []

-- get generator data from state
getGData :: Generator s [s]
getGData = mkGenerator $ \(c, g, s) -> ((s, g), s)

-- append data to state, also resetting the generator count
appendGData :: s -> Generator s ()
appendGData x = mkGenerator $ \(c, g, s) -> (((), g), appended s)
    where appended s = (x:s)

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
