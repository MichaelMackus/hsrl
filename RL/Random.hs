{-# LANGUAGE DefaultSignatures, DeriveFunctor #-}

module RL.Random (roll, pick, shuffle, pickRarity, randomChance, randomPoint, randomTile, randomPassable, randomDir, Roller(..), module System.Random, module Control.Monad.Random) where

import RL.Types
import RL.Map

import Control.Monad.Random
import Control.Monad.ST
import Data.Array.ST
import Data.Ratio
import GHC.Arr
import System.Random
import qualified Data.List as L


newtype Roller m a = Roller { runRoller :: StdGen -> m (a, StdGen) } deriving Functor

roll :: MonadRandom m => Dice -> m Int
roll (D n ns) = getRandomR (minInt, maxInt)
    where minInt = n
          maxInt = ns * n

-- pick randomly from a list
pick :: MonadRandom m => [a] -> m (Maybe a)
pick [] = return Nothing
pick xs = Just <$> uniform xs

-- pick randomly from a list using a rarity function, always returning a result for a list
pickRarity :: MonadRandom m => (a -> Rational) -> [a] -> m (Maybe a)
pickRarity f l =
    if null (L.filter (\a -> f a >= 1%100) l) then return Nothing
    else do
        r <- pick =<< filterM (randomChance . f) l
        case r of
            Just _    -> return r
            otherwise -> pickRarity f l

randomChance :: MonadRandom m => Rational -> m Bool
randomChance freq = (<= percentage freq) <$> roll (1 `d` 100)

percentage :: Rational -> Int
percentage r = floor (fromRational r * 100)

-- generates random point
-- between     maxX   maxY
randomPoint :: MonadRandom m => Int -> Int -> m Point
randomPoint x y = liftM2 (,) (roll $ 1 `d` x) (roll $ 1 `d` y)

-- generates random tile matching criteria
randomTile :: MonadRandom m => (Point -> Tile -> Bool) -> DLevel -> m (Maybe Point)
randomTile f lvl = do
    let ts = filter (uncurry f) $ enumerateMap lvl
    t <- pick ts
    case t of
        Just (p, t) -> return (Just p)
        otherwise   -> return Nothing
-- generates random passable point
randomPassable :: MonadRandom m => DLevel -> m (Maybe Point)
randomPassable = randomTile (const isPassable)

randomDir :: MonadRandom m => m Dir
randomDir = toEnum <$> roll (1 `d` 7)

-- generates random point
-- randomPointBetween :: MonadRandom m => Int -> Int -> m Point
-- randomPointBetween x y x' y' = liftM2 (,) (roll $ x `d` x') (roll $ y `d` y')

shuffle :: MonadRandom m => [a] -> m [a]
shuffle xs = do
    let l = length xs
    rands <- forM [0..(l-2)] $ \i -> getRandomR (i, l-1)
    let ar = runSTArray $ do
        ar <- thawSTArray $ listArray (0, l-1) xs
        forM_ (zip [0..] rands) $ \(i, j) -> do
            vi <- readSTArray ar i
            vj <- readSTArray ar j
            writeSTArray ar j vi
            writeSTArray ar i vj
        return ar
    return (elems ar)

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
