module RL.Generator.Dungeon (generateDungeon, ioGenerateDungeon) where

import RL.Dice
import RL.Random
import RL.Types
import RL.Generator
import RL.Generator.Cells
import RL.Generator.Paths

import Control.Applicative
import Control.Monad.Random (getSplit)
import Control.Monad.Reader (ask)
import System.Random (StdGen)

-- Basic random dungeon generator.
--
-- Encapsulated in own state machine, only needs Config and Random
generateDungeon :: GenConfig -> StdGen -> Dungeon
generateDungeon c g = let (a, g', s) = runGenerator dgenerator c g (mkGenState []) in a

-- helper to wrap generateDungeon in IO
ioGenerateDungeon :: GenConfig -> IO Dungeon
ioGenerateDungeon c = fst <$> ioGenerator dgenerator c

-- Quick Map generator
-- dgenerator :: (Monad m, MonadReader GenConfig m, MonadSplit StdGen m) => m Dungeon
dgenerator :: Generator s Dungeon
dgenerator = do
        conf <- ask
        g    <- getSplit
        let cs = runGenerator_ cells conf g (mkGenState [])
        return (toDungeon conf cs)
    where
        toDungeon conf cs = iterMap fillDng blankDng
            where blankDng     = mkDungeon $ blankMap (dwidth conf) (dheight conf)
                  fillDng  p t = maybe t id $ getTileAt p cs

