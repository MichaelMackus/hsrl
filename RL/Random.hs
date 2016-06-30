{-# LANGUAGE DefaultSignatures, DeriveFunctor #-}

module RL.Random (roll, randomPoint, Roller(..)) where

import RL.Types

import Control.Applicative
import Control.Monad          (liftM2, ap)
import Control.Monad.Random   (MonadRandom(..), MonadSplit(..))
import System.Random

newtype Roller m a = Roller { runRoller :: StdGen -> m (a, StdGen) } deriving Functor

roll :: MonadRandom m => Dice -> m Int
roll (D n ns) = getRandomR (minInt, maxInt)
    where minInt = n
          maxInt = ns * n

-- generates random point
-- between     maxX   maxY
randomPoint :: MonadRandom m => Int -> Int -> m Point
randomPoint x y = liftM2 (,) (roll $ 1 `d` x) (roll $ 1 `d` y)

instance Monad m => Monad (Roller m) where
    return x = Roller $ \s -> return (x, s)
    r >>= f  = Roller $ \s -> do
        (x, s') <- runRoller r s
        runRoller (f x) s'

instance (Applicative m, Monad m) => Applicative (Roller m) where
    pure x = Roller $ \g -> return (x, g)
    (<*>)  = ap

instance Monad m => MonadRandom (Roller m) where
    getRandom     = Roller $ \g -> return (random g)
    getRandoms    = Roller $ \g -> return (randoms g, g)
    getRandomR  r = Roller $ \g -> return (randomR r g)
    getRandomRs r = Roller $ \g -> return (randomRs r g, g)

instance Monad m => MonadSplit StdGen (Roller m) where
    getSplit = Roller $ \g -> return (g, snd $ next g)
