module RL.Generator.Dungeon (generateDungeon, ioGenerateDungeon) where

import RL.Types
import RL.Generator
import RL.Generator.Cells (cells)
import RL.Generator.Paths (paths, getTileAt)

import Control.Monad.Random (getSplit)
import Control.Monad.Reader (ask)
import System.Random (StdGen)

-- Basic random dungeon generator.
--
-- Encapsulated in own state machine, only needs Config and Random
generateDungeon :: GenConfig -> StdGen -> Dungeon
generateDungeon c g = let (a, g', s) = runGenerator dgenerator c g in a

-- helper to wrap generateDungeon in IO
ioGenerateDungeon :: GenConfig -> IO Dungeon
ioGenerateDungeon c = fst <$> ioGenerator dgenerator c

-- Quick Map generator
-- dgenerator :: (Monad m, MonadReader GenConfig m, MonadSplit StdGen m) => m Dungeon
dgenerator :: Generator s Dungeon
dgenerator = do
        conf <- ask
        g    <- getSplit
        let (cs, g', _) = runGenerator cells conf g
            (ps, _ , _) = runGenerator (paths cs) conf g'

        -- ensure we only generate the dungeon once, TODO check dungeon dimensions
        markGDone

        return (toDungeon conf cs ps)

toDungeon conf cs ps = iterMap fillDng blankDng
    where blankDng     = mkDungeon $ blankMap (dwidth conf) (dheight conf)
          fillDng  p t = maybe t id $ getTileAt p cs ps
