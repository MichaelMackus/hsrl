module RL.Generator.Dungeon (generateDungeon, ioGenerateDungeon) where

import RL.Types
import RL.Generator
import RL.Generator.Cells (cells, Cell)
import RL.Generator.Paths (paths, Path)
import qualified RL.Generator.Cells as C
import qualified RL.Generator.Paths as P

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
        return (toDungeon conf cs ps)
    where
        toDungeon conf cs ps = iterMap fillDng blankDng
            where blankDng     = mkDungeon $ blankMap (dwidth conf) (dheight conf)
                  fillDng  p t = maybe t id $ getTileAt p cs ps

-- combines C.getTileAt and P.getTileAt
getTileAt :: Point -> [Cell] -> [Path] -> Maybe Tile
getTileAt p cs ps = maybe (P.getTileAt p ps) Just $ C.getTileAt p cs
