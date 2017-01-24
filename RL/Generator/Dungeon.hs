module RL.Generator.Dungeon (generateLevel, module RL.Generator) where

import RL.Map
import RL.Generator
import RL.Generator.Cells (cells)
import RL.Generator.Paths (paths, getTileAt)
import RL.Generator.Mobs (playerGenerator, mobGenerator)
import RL.Random (StdGen, split)

-- Quick Map generator
-- dgenerator :: (Monad m, MonadReader GenConfig m, MonadSplit StdGen m) => m DLevel
generateLevel :: GenConfig -> StdGen -> (DLevel, StdGen)
generateLevel conf g =
        let s = initState g
            (cs, s' ) = runGenerator cells conf (initState (gen s))
            (ps, s'') = runGenerator (paths cs) conf (initState (gen s'))
            (player, s''') = runGenerator (playerGenerator 10 (1 `d` 4)) conf (mkGenState cs (gen s''))
            (mobs, _) = runGenerator (mobGenerator 5) conf (mkGenState lvl (gen s'''))
            lvl = maybe (error "No player generated") (toLevel conf cs ps) player
        in  (lvl { mobs = mobs }, snd (split (gen s''')))

toLevel conf cs ps player = lvl { player = player }
    where lvl = iterMap fillDng blankDng
          blankDng     = mkLevel $ blankMap (dwidth conf) (dheight conf)
          fillDng  p t = maybe t id $ getTileAt p cs ps
