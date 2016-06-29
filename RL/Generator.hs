module RL.Generator (Generator, GenConfig(..), GenState(..), generate, runGenerator, runGenerator_, ioGenerator, mkGenState, getGData, appendGData, setGData, isGDone, markGDone) where

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
newtype Generator s a = Generator {
    runGenerator :: (GenConfig -> StdGen -> GenState s -> (a, StdGen, GenState s))
}

-- Configuration to generate something within dwidth x dheight dungeon
data GenConfig = GenConfig {
    dwidth  :: Int,
    dheight :: Int
}

-- Generator state
data GenState s = GenState {
    gdata :: [s],
    gdone :: Bool
}

-- run a generator, returning only the result
runGenerator_ :: Generator s a -> GenConfig -> StdGen -> GenState s -> a
runGenerator_ gen c g s = let (r, _, _) = runGenerator gen c g s
                          in r

-- run a generator through IO
ioGenerator :: Generator s a -> GenConfig -> IO (a, GenState s)
ioGenerator g c = newStdGen >>= ioGenerator'
    where ioGenerator' rng = let (r, _, s) = runGenerator g c rng (mkGenState [])
                             in return (r, s)

-- Wrap a generator in a ContGenerator.
--
-- This attempts to endlessly generate a result, until either:
--
-- 1) i >= maxTries. The "i" counter is incremented on each contination (after
-- the wrapped generator has done 1 pass). If the state has changed, the
-- counter is reset to 0.
--
-- 2) isGDone returns True
--
-- Then, it returns the latest result.
generate :: Int -> Generator s a -> Generator s a
generate maxTries gen = runContT (ContT (continue 0)) return
    where
        continue i next = do
            prevLen <- length <$> getGData
            r       <- gen
            c       <- ask
            d       <- getGData
            curLen  <- length <$> getGData
            done    <- isGDone

            -- have we hit the max limit, or are done generating?
            if i >= maxTries || done then
                next r
            else
                -- reset i if we've appended data
                if curLen /= prevLen then
                    continue 0 next
                else
                    continue (i + 1) next

-- constructor for initial gen state
mkGenState :: [s] -> GenState s
mkGenState s = GenState [] False

-- is generation done?
isGDone :: Generator s Bool
isGDone = Generator $ \c g s -> (gdone s, g, s)

-- mark generation as done
markGDone :: Generator s ()
markGDone = Generator $ \c g s -> ((), g, done s)
    where done s = s { gdone = True }

-- get generator data from state
getGData :: Generator s [s]
getGData = Generator $ \c g s -> (gdata s, g, s)

-- append data to state
appendGData :: s -> Generator s ()
appendGData x = Generator $ \c g s -> ((), g, appended s)
    where appended s = s { gdata = (x:gdata s) }

-- set data to state
setGData :: [s] -> Generator s ()
setGData gdata = Generator $ \c g s -> ((), g, s { gdata = gdata })

instance Monad (Generator s) where
    gen >>= f = Generator $ \c g s ->
        let (r, g', s') = runGenerator gen c g s
        in runGenerator (f r) c g' s'

    return = pure

instance MonadReader GenConfig (Generator s) where
    ask    = readGen ask
    reader = readGen . reader
    local f m = Generator $ \c g s ->
        let c' = f c
        in runGenerator m c' g s

-- helper for MonadReader
readGen :: ReaderT GenConfig (Generator s) a -> Generator s a
readGen r = do
        c <- getConf
        runReaderT r c
    where getConf = Generator $ \c g s -> (c, g, s)

instance MonadSplit StdGen (Generator s) where
    getSplit = do
        g' <- rollGen getSplit
        Generator $ \c g s -> (g, g', s)

instance MonadRandom (Generator s) where
    getRandom     = rollGen getRandom
    getRandoms    = rollGen getRandoms
    getRandomR  r = rollGen (getRandomR r)
    getRandomRs r = rollGen (getRandomRs r)

-- helper for MonadRandom instance
rollGen :: Roller (Generator s) a -> Generator s a
rollGen rand = do
        g       <- getGen
        (r, g') <- runRoller rand g
        Generator $ \c g s -> (r, g', s)
    where getGen = Generator $ \c g s -> (g, g, s)

instance Functor (Generator s) where
    fmap = liftM
instance Applicative (Generator s) where
    (<*>)  = ap
    pure x = Generator $ \c rng s -> (x, rng, s)
