module RL.Game (Game, Env(..), Event(..), runGame, withEnv, withRng, iterLevel, module RL.Map) where

import RL.Event
import RL.Map

import Control.Monad (liftM)
import Control.Monad.Random
import Control.Monad.State

data Game a = GameState (Env -> (a, Env))
data Env    = Env {
    dungeon  :: Dungeon,
    level    :: DLevel,
    rng      :: StdGen,
    events   :: [Event]
}

runGame :: Game a -> Env -> (a, Env)
runGame (GameState pr) e = pr e

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

iterLevel :: (Point -> Tile -> Tile) -> Game DLevel
iterLevel f = pure . iterMap f . level =<< get

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

instance MonadSplit StdGen Game where
    getSplit      = withRng $ \g -> split g

instance Functor Game where
    fmap = liftM

instance Applicative Game where
    (<*>)  = ap
    pure x = GameState $ \e -> (x, e)
