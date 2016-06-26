{-# LANGUAGE DefaultSignatures, DeriveFunctor #-}

module RL.Random where

import RL.Types

import Control.Applicative
import Control.Monad          (liftM2, ap)
import Control.Monad.Random   (MonadRandom(..), MonadSplit(..))
import Control.Monad.State    (StateT(..))
import System.Random

roll :: MonadRandom m => Dice -> m Int
roll (D n ns) = getRandomR (minInt, maxInt)
    where minInt = n
          maxInt = ns * n

-- generates random point
-- between     maxX   maxY
randomPoint :: MonadRandom m => Int -> Int -> m Point
randomPoint x y = liftM2 (,) (roll $ 1 `d` x) (roll $ 1 `d` y)

newtype Roller m a = Roller { getRand :: StateT StdGen m a } deriving Functor

instance Monad m => Monad (Roller m) where
    return x = mkRoller $ \s -> return (x, s)
    r >>= f  = mkRoller $ \s -> do
        (x, s') <- runRoller r s
        runRoller (f x) s'

instance (Applicative m, Monad m) => Applicative (Roller m) where
    pure x = mkRoller $ \g -> return (x, g)
    (<*>)  = ap

instance Monad m => MonadRandom (Roller m) where
    getRandom     = mkRoller $ \g -> return (random g)
    getRandoms    = mkRoller $ \g -> return (randoms g, g)
    getRandomR  r = mkRoller $ \g -> return (randomR r g)
    getRandomRs r = mkRoller $ \g -> return (randomRs r g, g)

instance Monad m => MonadSplit StdGen (Roller m) where
    getSplit = mkRoller $ \g -> return (g, snd $ next g)

mkRoller :: Monad m => (StdGen -> m (a, StdGen)) -> Roller m a
mkRoller = Roller . StateT

runRoller :: Monad m => Roller m a -> StdGen -> m (a, StdGen)
runRoller = runStateT . getRand
