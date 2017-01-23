module RL.Generator.Dungeon (generateLevel, module RL.Generator) where

import RL.Map
import RL.Generator
import RL.Generator.Cells (cells, cwidth, cheight, cpoint)
import RL.Generator.Paths (paths, getTileAt)
import RL.Random (StdGen)

-- Quick Map generator
-- dgenerator :: (Monad m, MonadReader GenConfig m, MonadSplit StdGen m) => m DLevel
generateLevel :: GenConfig -> StdGen -> (DLevel, StdGen)
generateLevel conf g =
        let s = initState g
            (cs, s' ) = runGenerator cells conf (initState (gen s))
            (ps, s'') = runGenerator (paths cs) conf (initState (gen s'))
            (cx, cy) = cpoint (cs !! 0) -- TODO place player randomly around dungeon
            player   = Mob {
                mobId  = 0,
                symbol = '@',
                at     = (cx + floor (fromIntegral (cwidth (cs !! 0)) / 2), cy + floor (fromIntegral (cheight (cs !! 0)) / 2)),
                hp     = 10,
                dmgd   = 1 `d` 4
            }
            lvl = (toLevel conf cs ps) { player = player }
        in (lvl, gen s'')

toLevel conf cs ps = iterMap fillDng blankDng
    where blankDng     = mkLevel $ blankMap (dwidth conf) (dheight conf)
          fillDng  p t = maybe t id $ getTileAt p cs ps
