module RL.Generator (Generator, GenConfig(..), GenState(..), runGenerator, mkGenState, initState, getGData, appendGData, setGData, isGDone, markGDone) where

import RL.Random

import Control.Monad (ap, liftM)
import Control.Monad.Cont
import Control.Monad.Random
import Control.Monad.Reader

-- Generator monad which generates a list of objects of type s based on GenConfig.
-- ContGenerator represents a Generator wrapped in a Cont monad - see "generate".
newtype Generator s a = Generator {
    unwrapGenerator :: (GenConfig -> GenState s -> (a, GenState s))
}

-- Configuration to generate something within dwidth x dheight dungeon
data GenConfig = GenConfig {
    dwidth   :: Int,
    dheight  :: Int,
    maxTries :: Int -- max amount of times to wrap in Cont monad (markGDone aborts the continuation)
}

-- Generator state
data GenState s = GenState {
    gdata :: s,
    gdone :: Bool,
    gen   :: StdGen,
    i     :: Int
}

runGenerator :: Generator s a -> GenConfig -> GenState s -> (a, GenState s)
runGenerator gen conf s =
    let gen' = generate (maxTries conf) gen
    in  unwrapGenerator gen' conf s

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
generate maxTries gen = runContT (ContT continue) return
    where
        continue next = do
            -- get generated data (counter reset here if data modified)
            r       <- gen
            i       <- getCounter
            done    <- isGDone

            -- increment gen counter for next iteration
            incCounter

            -- have we hit the max limit, or are done generating?
            if i >= maxTries || done then
                next r
            else
                continue next

-- constructor for initial gen state
mkGenState :: s -> StdGen -> GenState s
mkGenState s g = GenState s False g 0

initState :: StdGen -> GenState [s]
initState g = GenState [] False g 0

-- is generation done?
isGDone :: Generator s Bool
isGDone = Generator $ \c s -> (gdone s, s)

-- mark generation as done
markGDone :: Generator s ()
markGDone = Generator $ \c s -> ((), done s)
    where done s = s { gdone = True }

-- get generator data from state
getGData :: Generator s s
getGData = Generator $ \c s -> (gdata s, s)

-- append data to state
appendGData :: s -> Generator [s] ()
appendGData x = Generator $ \c s -> ((), appended s)
    where appended s = s { gdata = (x:gdata s), i = 0 }

-- set data to state
setGData :: s -> Generator s ()
setGData gdata = Generator $ \c s -> ((), s { gdata = gdata, i = 0 })

-- increment try counter
incCounter :: Generator s ()
incCounter = Generator $ \c s -> ((), s { i = i s + 1 })

-- get try counter
getCounter :: Generator s Int
getCounter = Generator $ \c s -> (i s, s)

instance Monad (Generator s) where
    gen >>= f = Generator $ \c s ->
        let (r, s') = unwrapGenerator gen c s
        in unwrapGenerator (f r) c s'

    return = pure

instance MonadReader GenConfig (Generator s) where
    ask    = readGen ask
    reader = readGen . reader
    local f m = Generator $ \c s ->
        let c' = f c
        in unwrapGenerator m c' s

-- helper for MonadReader
readGen :: ReaderT GenConfig (Generator s) a -> Generator s a
readGen r = do
        c <- getConf
        runReaderT r c
    where getConf = Generator $ \c s -> (c, s)

instance MonadSplit StdGen (Generator s) where
    getSplit = do
        g' <- rollGen getSplit
        Generator $ \c s -> (gen s, s { gen = g' })

instance MonadRandom (Generator s) where
    getRandom     = rollGen getRandom
    getRandoms    = rollGen getRandoms
    getRandomR  r = rollGen (getRandomR r)
    getRandomRs r = rollGen (getRandomRs r)

-- helper for MonadRandom instance
rollGen :: Roller (Generator s) a -> Generator s a
rollGen rand = do
        s       <- get
        (r, g') <- runRoller rand (gen s)
        Generator $ \c s -> (r, s { gen = g' })
    where get = Generator $ \c s -> (s, s)

instance Functor (Generator s) where
    fmap = liftM
instance Applicative (Generator s) where
    (<*>)  = ap
    pure x = Generator $ \c s -> (x, s)
