module RL.Generator.Dungeon (DungeonConfig(..), PlayerConfig(..), levelGenerator, module RL.Generator) where

import RL.Map
import RL.Generator
import RL.Generator.Cells (cells, cmid, cpoint, CellConfig(CellConfig))
import RL.Generator.Paths (paths, getTileAt)
import RL.Generator.Mobs (playerGenerator, mobGenerator, MobConfig(..), PlayerConfig(..))
import RL.Random (StdGen)
import RL.Util (comparing)

import Control.Monad.Random (getSplit)
import Control.Monad.Reader (ask)
import Data.Maybe (fromMaybe, listToMaybe, isJust, fromJust)
import qualified Data.List as L

data DungeonConfig = DungeonConfig {
    dwidth   :: Int,
    dheight  :: Int,
    maxTries :: Int,
    prevLevel :: Maybe DLevel,
    maxDepth :: Int,
    mobConfig :: MobConfig,
    playerConfig :: PlayerConfig
}

instance GenConfig DungeonConfig where
    generating conf = (< maxTries conf) <$> getCounter

levelGenerator :: Generator DungeonConfig s DLevel
levelGenerator = do
        conf   <- ask
        cs     <- runGenerator' cells (mkCellConf conf) initState
        ps     <- runGenerator' (paths cs) conf initState
        player <- fromMaybe (error errPlayer) <$> runGenerator' playerGenerator (playerConfig conf) (mkGenState cs)

        let prev = prevLevel conf
            d    = maybe 1 ((+1) . depth) prev
            lvl  = toLevel conf d cs ps player

        mobs <- runGenerator' mobGenerator (mobConfig conf) (mkGenState lvl)
        g    <- getSplit

        -- generate up/down stairs
        let lvl'    = iterMap f lvl
            f p t   = if p == at player && not (isStair t) && isJust prev then (StairUp (fromJust prev))
                      else if Just p == lastP && d + 1 <= maxDepth conf then (StairDown nextLvl)
                           else t
            lastP   = cmid <$> listToMaybe (reverse (L.sortBy (comparing' (distance (at player))) cs))
            nextLvl = fst (runGenerator levelGenerator (conf { prevLevel = Just lvl' }) (initState g))

        return (lvl' { mobs = withMobIds mobs })
    where
        runGenerator' :: GenConfig c => Generator c s a -> c -> (StdGen -> GenState s) -> Generator DungeonConfig t a
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

mkCellConf conf = CellConfig (dwidth conf) (dheight conf) (maxTries conf)
