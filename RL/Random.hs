module RL.Random (MonadRandom(..), module RL.Random, module System.Random) where

import RL.Types

import Control.Monad          (liftM2)
import Control.Monad.Random   (MonadRandom(..))
import Control.Monad.Random.Class (getRandomR)
import Control.Monad.IO.Class (liftIO)
import System.Random

-- main roll function
--
-- generates random int and increments seed
roll :: MonadRandom m => Dice -> m Int
roll (D n ns) = getRandomR (minInt, maxInt)
    where minInt = n
          maxInt = ns * n

-- generates random point
-- between     maxX   maxY
randomPoint :: MonadRandom m => Int -> Int -> m Point
randomPoint x y = liftM2 (,) (roll $ 1 `d` x) (roll $ 1 `d` y)

