{-# LANGUAGE DefaultSignatures, DeriveFunctor #-}

module RL.Random (roll, pick, pickRarity, randomPoint, Roller(..), module System.Random, module Control.Monad.Random) where

import RL.Types

import Control.Monad          (liftM2, ap)
import Control.Monad.Random
import System.Random

newtype Roller m a = Roller { runRoller :: StdGen -> m (a, StdGen) } deriving Functor

roll :: MonadRandom m => Dice -> m Int
roll (D n ns) = getRandomR (minInt, maxInt)
    where minInt = n
          maxInt = ns * n

-- pick randomly from a list
pick :: MonadRandom m => [a] -> m (Maybe a)
pick [] = return Nothing
pick xs = getRandomR (0, length xs - 1) >>= return . Just . (xs !!)

-- pick randomly from a list using a rarity function
pickRarity :: MonadRandom m => (a -> Rational) -> [a] -> m (Maybe a)
pickRarity f l = do
        res <- roll (1 `d` 100)
        pick (filter (\x -> res <= percentage (f x)) l)
    where
        percentage :: Rational -> Int
        percentage r = floor (fromRational r * 100)

-- generates random point
-- between     maxX   maxY
randomPoint :: MonadRandom m => Int -> Int -> m Point
randomPoint x y = liftM2 (,) (roll $ 1 `d` x) (roll $ 1 `d` y)

-- generates random point
-- randomPointBetween :: MonadRandom m => Int -> Int -> m Point
-- randomPointBetween x y x' y' = liftM2 (,) (roll $ x `d` x') (roll $ y `d` y')

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
