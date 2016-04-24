module RL.Game (Game, Env(..), runGame, runIO, withEnv, mkEnv, withRng, iterDungeon) where

import RL.Random
import RL.Types

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.State
import System.Random

data Game a = GameState (Env -> (a, Env))
data Env    = Env {
    dungeon :: Dungeon,
    rng     :: StdGen
}

runGame :: Game a -> Env -> (a, Env)
runGame (GameState pr) e = pr e

runIO :: Game a -> Env -> IO (a, Env)
runIO g e = return (runGame g e)

mkEnv :: Env
mkEnv = Env (generateDungeon 10 10) $ mkStdGen 0


-- get/setters
withEnv :: (Env -> Game a) -> Game a
withEnv f = f =<< get

-- modify RNG helper function
withRng :: (StdGen -> (a, StdGen)) -> Game a
withRng f = withEnv $ \e -> do
    g <- gets rng
    let (r, g') = f g
    put $ e { rng = g' }
    return r

iterDungeon :: (Point -> Tile -> Tile) -> Game Dungeon
iterDungeon f = pure . iterMap f . dungeon =<< get

-- plumbing

instance Monad Game where
    g >>= k = GameState $ \e ->
        let (r, e')  = runGame g e
        in  runGame (k r) e'
    return = pure

instance MonadState Env Game where
    get   = GameState $ \e -> (e,  e)
    put e = GameState $ \_ -> ((), e)

instance MonadRandom Game where
    getRandom     = withRng random
    getRandoms    = withRng $ \g -> (randoms g, g)
    getRandomR    = withRng . randomR
    getRandomRs r = withRng $ \g -> (randomRs r g, g)

instance Functor Game where
    fmap = liftM

instance Applicative Game where
    (<*>)  = ap
    pure x = GameState $ \e -> (x, e)
