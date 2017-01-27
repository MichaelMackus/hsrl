module RL.Generator.Dungeon (generateLevel, levelGenerator, module RL.Generator) where

import RL.Map
import RL.Generator
import RL.Generator.Cells (cells, cmid, cpoint)
import RL.Generator.Paths (paths, getTileAt)
import RL.Generator.Mobs (playerGenerator, mobGenerator)
import RL.Random (StdGen)
import RL.Util (comparing)

import Control.Monad.Random (getSplit)
import Control.Monad.Reader (ask)
import Data.Maybe (fromMaybe, listToMaybe, isJust, fromJust)
import qualified Data.List as L

-- Quick Map generator
-- dgenerator :: (Monad m, MonadReader GenConfig m, MonadSplit StdGen m) => m DLevel
generateLevel :: Maybe DLevel -> GenConfig -> StdGen -> (DLevel, StdGen)
generateLevel prev conf g =
    let (r, s) = runGenerator (levelGenerator prev) conf (initState g)
    in  (r, gen s)

levelGenerator :: Maybe DLevel -> Generator s DLevel
levelGenerator prev = do
        conf   <- ask
        cs     <- runGenerator' cells conf initState
        ps     <- runGenerator' (paths cs) conf initState
        player <- fromMaybe (error errPlayer) <$> runGenerator' (playerGenerator 10 (1 `d` 4)) conf (mkGenState cs)

        let d   = maybe 1 ((+1) . depth) prev
            lvl = toLevel conf d cs ps player

        mobs <- runGenerator' (mobGenerator 5) conf (mkGenState lvl)
        g    <- getSplit

        -- generate up/down stairs
        let lvl'    = iterMap f lvl
            f p t   = if p == at player && not (isStair t) && isJust prev then (StairUp (fromJust prev))
                      else if Just p == lastP then (StairDown nextLvl)
                           else t
            lastP   = cmid <$> listToMaybe (reverse (L.sortBy (comparing' (distance (at player))) cs))
            nextLvl = fst (generateLevel (Just lvl') conf g)

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

toLevel conf depth cs ps player = lvl { player = player }
    where lvl = iterMap fillDng (blankDng depth conf)
          fillDng  p t = maybe t id $ getTileAt p cs ps

blankDng depth conf = mkLevel depth $ blankMap (dwidth conf) (dheight conf)
