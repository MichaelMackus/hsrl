module RL.Generator.Dungeon (DungeonConfig(..), PlayerConfig(..), ItemConfig(..), levelGenerator, randomItemAppearances, module RL.Generator) where

import RL.Map
import RL.Generator
import RL.Generator.Cells (cells, cmid, cpoint, CellConfig(CellConfig))
import RL.Generator.Paths (paths, getTileAt)
import RL.Generator.Mobs (playerGenerator, mobGenerator, MobConfig(..), PlayerConfig(..))
import RL.Generator.Items
import RL.Generator.Features
import RL.Pathfinder
import RL.Random (StdGen, pick)
import RL.Util (comparing)

import Control.Monad.Random (getSplit)
import Control.Monad.Reader (ask)
import Data.Maybe (fromMaybe, listToMaybe, isJust, fromJust, isNothing)
import qualified Data.List as L

data DungeonConfig = DungeonConfig {
    dwidth   :: Int,
    dheight  :: Int,
    maxTries :: Int,
    prevLevel :: Maybe DLevel,
    maxDepth :: Int,
    mobConfig :: MobConfig,
    itemConfig :: ItemConfig,
    playerConfig :: PlayerConfig,
    featureConfig :: FeatureConfig
}

instance GenConfig DungeonConfig where
    generating conf = (< maxTries conf) <$> getCounter

levelGenerator :: Generator DungeonConfig s DLevel
levelGenerator = do
        conf   <- ask
        cs     <- runGenerator' cells (mkCellConf conf) initState
        ps     <- runGenerator' (paths cs) conf initState -- TODO ensure all reachable!
        player <- fromMaybe (error errPlayer) <$> runGenerator' playerGenerator (playerConfig conf) (mkGenState cs)

        let prev = prevLevel conf
            d    = maybe 1 ((+1) . depth) prev
            lvl  = toLevel conf d cs ps player

        -- generate features
        fs   <- runGenerator' featuresGenerator (featureConfig conf) (mkGenState (lvl, cs))
        let lvl' = lvl { features = L.filter (\(p,f) -> p /= at player) fs }
        g    <- getSplit

        -- generate up/down stairs
        lastP <- pick (L.filter (\p -> isNothing (L.lookup p (features lvl')) && p /= at player) (map cmid cs))
        let lvl''   = iterMap f lvl'
            f p t   = if p == at player && not (isStair t) then (StairUp prev)
                      else if Just p == lastP && d + 1 <= maxDepth conf then (StairDown nextLvl)
                           else t
            nextLvl = fst (runGenerator levelGenerator (conf { prevLevel = Just lvl'' }) (initState g))

        -- ensure we can reach the end
        if isJust lastP && isJust (findPath (dfinder lvl'' (fromJust lastP)) distance (fromJust lastP) (at player)) then do
            items <- runGenerator' itemsGenerator (itemConfig conf) (mkGenState lvl'')
            mobs  <- runGenerator' mobGenerator (mobConfig conf) (mkGenState lvl'')
            return (lvl'' { mobs = mobs, items = items })
        else
            levelGenerator
    where
        runGenerator' :: GenConfig c => Generator c s a -> c -> (StdGen -> GenState s) -> Generator DungeonConfig t a
        runGenerator' gen conf f = do
            g  <- getSplit
            let r = runGenerator gen conf (f g)

            return (fst r)

        errPlayer = "No player generated"

toLevel conf depth cs ps player = lvl { player = player }
    where lvl = iterMap fillDng (blankDng depth conf)
          fillDng  p t = maybe t id $ getTileAt p cs ps

blankDng depth conf = mkLevel depth $ blankMap (dwidth conf) (dheight conf)

mkCellConf conf = CellConfig (dwidth conf) (dheight conf) (maxTries conf)
