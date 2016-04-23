module RL.Game (Game, Env(..), runGame, withEnv, mkEnv, withRng, iterDungeon) where

import RL.Types

import Control.Applicative
import Control.Monad.State
import System.Random

data Game a = Game (Env -> (a, Env))

data Env = Env {
    dungeon :: Dungeon,
    rng     :: StdGen
}

runGame :: Game a -> Env -> (a, Env)
runGame (Game pr) e = pr e

-- get/setters
withEnv :: (Env -> Game a) -> Game a
withEnv f = f =<< getEnv

withRng :: (StdGen -> (a, StdGen)) -> Game a
withRng f = withEnv $ \e -> do
    g       <- rng <$> getEnv
    let (r, g') = f g
    setEnv $ e { rng = g' }
    return r

iterDungeon :: (Point -> Tile -> Tile) -> Game Dungeon
iterDungeon f = pure . iterMap f . dungeon =<< getEnv

getEnv :: Game Env
getEnv = Game $ \e -> (e, e)

setEnv :: Env -> Game ()
setEnv e = Game $ \e' -> ((), e)

-- plumbing

mkEnv :: Env
mkEnv = Env (generateDungeon 10 10) $ mkStdGen 0

instance Monad Game where
    g >>= k = Game $ \e ->
        let (r, e')  = runGame g e
        in  runGame (k r) e'
    return  x       = Game $ \e -> (x, e)

instance Functor Game where
    fmap f g = Game $ \e ->
        let (r, e') = runGame g e
        in  (f r, e')

instance Applicative Game where
    gf <*> g = Game $ \e ->
        let (f, e') = runGame gf e
        in  runGame (fmap f g) e'
    pure x = return x
