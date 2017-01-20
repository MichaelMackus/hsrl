module RL.Generator.Dungeon (generateLevel, ioGenerateLevel, module RL.Generator) where

import RL.Map
import RL.Generator
import RL.Generator.Cells (cells)
import RL.Generator.Paths (paths, getTileAt)

import Control.Monad.Random (getSplit)
import Control.Monad.Reader (ask)
import System.Random (StdGen)

-- Basic random dungeon generator.
--
-- Encapsulated in own state machine, only needs Config and Random
generateLevel :: GenConfig -> StdGen -> DLevel
generateLevel c g = let (a, g', s) = runGenerator dgenerator c g in a

-- helper to wrap generateLevel in IO
ioGenerateLevel :: GenConfig -> IO DLevel
ioGenerateLevel c = fst <$> ioGenerator dgenerator c

-- Quick Map generator
-- dgenerator :: (Monad m, MonadReader GenConfig m, MonadSplit StdGen m) => m DLevel
dgenerator :: Generator s DLevel
dgenerator = do
        conf <- ask
        g    <- getSplit
        let (cs, g', _) = runGenerator cells conf g
            (ps, _ , _) = runGenerator (paths cs) conf g'

        -- ensure we only generate the dungeon once, TODO check dungeon dimensions
        markGDone

        return (toLevel conf cs ps)

toLevel conf cs ps = iterMap fillDng blankDng
    where blankDng     = mkLevel $ blankMap (dwidth conf) (dheight conf)
          fillDng  p t = maybe t id $ getTileAt p cs ps
