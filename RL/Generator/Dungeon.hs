module RL.Generator.Dungeon (generateLevel, module RL.Generator) where

import RL.Map
import RL.Generator
import RL.Generator.Cells (cells, cmid, cpoint)
import RL.Generator.Paths (paths, getTileAt)
import RL.Generator.Mobs (playerGenerator, mobGenerator)
import RL.Random (StdGen)
import RL.Util (comparing)

import Control.Monad.Random (getSplit)
import Control.Monad.Reader (ask)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.List as L

-- Quick Map generator
-- dgenerator :: (Monad m, MonadReader GenConfig m, MonadSplit StdGen m) => m DLevel
generateLevel :: GenConfig -> StdGen -> (DLevel, StdGen)
generateLevel conf g =
    let (r, s) = runGenerator levelGenerator conf (initState g)
    in  (r, gen s)

levelGenerator :: Generator s DLevel
levelGenerator = do
        conf   <- ask
        cs     <- runGenerator' cells conf initState
        ps     <- runGenerator' (paths cs) conf initState
        player <- fromMaybe (error errPlayer) <$> runGenerator' (playerGenerator 10 (1 `d` 4)) conf (mkGenState cs)

        let lvl = toLevel conf cs ps player
        mobs <- runGenerator' (mobGenerator 5) conf (mkGenState lvl)
        g    <- getSplit

        -- generate up/down stairs
        let lvl'    = iterMap f lvl
            f p t   = if p == at player && not (isStair t) then (StairUp prevLvl)
                      else if Just p == lastP then (StairDown nextLvl)
                           else t
            lastP   = cmid <$> listToMaybe (reverse (L.sortBy (comparing' (distance (at player))) cs))
            nextLvl = fst (generateLevel conf g)
            prevLvl = fst (generateLevel conf g)
            -- prevLvl = undefined TODO set to Maybe argument

        return (lvl' { mobs = withMobIds mobs })
    where
        runGenerator' :: Generator s a -> GenConfig -> (StdGen -> GenState s) -> Generator t a
        runGenerator' gen conf f = do
            g  <- getSplit
            let r = runGenerator gen conf (f g)

            return (fst r)

        comparing' f = \a b -> comparing f (cpoint a) (cpoint b)

        errPlayer = "No player generated"
        errTile   = "Player is not standing on a tile!"

toLevel conf cs ps player = lvl { player = player }
    where lvl = iterMap fillDng (blankDng conf)
          fillDng  p t = maybe t id $ getTileAt p cs ps

blankDng conf = mkLevel $ blankMap (dwidth conf) (dheight conf)
