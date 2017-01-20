module RL.Generator.DLevel (levelGenerator, module RL.Generator) where

import RL.Map
import RL.Generator
import RL.Generator.Cells (cells, cwidth, cheight, cpoint)
import RL.Generator.Paths (paths, getTileAt)

import Control.Monad.Random (getSplit)
import Control.Monad.Reader (ask)

-- Quick Map generator
-- dgenerator :: (Monad m, MonadReader GenConfig m, MonadSplit StdGen m) => m DLevel
levelGenerator :: Generator s DLevel
levelGenerator = do
        conf <- ask
        g    <- getSplit
        let (cs, g', _) = runGenerator cells conf g
            (ps, _ , _) = runGenerator (paths cs) conf g'

        -- ensure we only generate the dungeon once, TODO check dungeon dimensions
        markGDone

        -- TODO place player randomly around dungeon
        let (cx, cy) = cpoint (cs !! 0)
            player = Mob {
                mobId  = 0,
                symbol = '@',
                at     = (cx + floor (fromIntegral (cwidth (cs !! 0)) / 2), cy + floor (fromIntegral (cheight (cs !! 0)) / 2)),
                hp     = 10,
                dmgd   = 1 `d` 4
            }
            lvl = (toLevel conf cs ps) { player = player }

        return lvl

toLevel conf cs ps = iterMap fillDng blankDng
    where blankDng     = mkLevel $ blankMap (dwidth conf) (dheight conf)
          fillDng  p t = maybe t id $ getTileAt p cs ps
