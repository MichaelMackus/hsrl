module RL.Game (Game, Env(..), runGame, withEnv) where

import Control.Applicative
import RL.Types
import System.Random

newtype Game s a = Game { runGame :: s -> (a, s) }

-- data Result = Won | Lost | Playing
data Env = Env {
    dungeon :: Dungeon,
    rng     :: StdGen
}

withEnv :: (s -> Game s a) -> Game s a
withEnv f = getEnv >>= f

modifyEnv :: (s -> (a, s)) -> Game s a
modifyEnv = Game

getEnv :: Game s s
getEnv = Game $ \s -> (s, s)


-- attack :: Mob -> Game ()
-- player :: Game Player
-- moveTo :: Point -> Game ()

instance Monad (Game s) where
    g >>= f  = modifyEnv $ \e ->
        let (r, e') = runGame g e
        in runGame (f r) e'

    return a = modifyEnv $ \e -> (a, e)

-- instance Applicative (Game s) where
--     pure  = return
--     (<*>) = ap

-- instance Applicative Game where
    

-- basic MonadRand instance for Dungeon Generation
-- instance MonadRandom Dungeon where
-- instance MonadRandom Game where
--     getRandom :: m a

