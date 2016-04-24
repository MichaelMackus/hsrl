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

data Generator a = Generator ((GenConfig, GenState) -> (a, GenState))
data GenState    = GenState {
    rng    :: StdGen,
    gcount :: Int
}
data GenConfig = GenConfig {
    dwidth  :: Int,
    dheight :: Int
}

-- delegates to runGenerator dgenerator
generateDungeon :: GenConfig -> StdGen -> (Dungeon, StdGen)
generateDungeon c g = let (r, s) = runGenerator dgenerator c initState in (r, rng s)
    where initState = GenState g 0

-- Quick Map generator
dgenerator :: Generator Dungeon
dgenerator = do
    c <- ask
    let blankDng = mkDungeon $ blankMap (dwidth c) (dheight c)
        

    return blankDng

runGenerator :: Generator a -> GenConfig -> GenState -> (a, GenState)
runGenerator (Generator gen) c s = gen (c, s)

instance Monad Generator where
    gen >>= f = Generator $ \(c, s) ->
        let (r, s) = runGenerator gen c s
        in runGenerator (f r) c s

    return = pure

instance MonadReader GenConfig Generator where
    ask       = Generator $ \(c, s) -> (c, s)
    reader  f = Generator $ \(c, s) -> (f c, s)
    local f m = Generator $ \(c, s) ->
        let c' = f c
        in runGenerator m c' s

instance MonadRandom Generator where
    getRandom     = withRng random
    getRandoms    = withRng $ \g -> (randoms g, g)
    getRandomR    = withRng . randomR
    getRandomRs r = withRng $ \g -> (randomRs r g, g)

withRng :: (StdGen -> (a, StdGen)) -> Generator a
withRng f = Generator $ \(c, s) ->
    let (r, g) = f $ rng s
    in  (r, s { rng = g })

instance Functor Generator where
    fmap = liftM
instance Applicative Generator where
    (<*>)  = ap
    pure x = Generator $ \(c, rng) -> (x, rng)
