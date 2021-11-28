module RL.Generator.Features (FeatureConfig(..), featuresGenerator) where

import RL.Generator
import RL.Generator.Cells (Cell(..), cmid)
import RL.Generator.Paths (Path(..))
import RL.Generator.Items (generateChestItems, ItemConfig(..))
import RL.Item
import RL.Map (DLevel(..), Point(..), Feature(..), Difficulty)
import RL.Random

import Control.Monad
import Control.Monad.Reader
import Data.Map (Map)
import Data.Maybe (maybeToList)

data FeatureConfig = FeatureConfig {
    maxFeatures :: Int,
    cellFeatureChance :: Rational, -- chance for a cell feature (Chest, Altar, or Fountain)
    fItemAppearances :: Map ItemType String
}

instance GenConfig FeatureConfig where
    generating conf = (< maxFeatures conf) <$> getCounter

-- TODO don't generate on player
featuresGenerator :: Generator FeatureConfig (DLevel, [Cell]) [(Point, Feature)]
featuresGenerator = getGData >>= \(lvl, cs) ->
    forMConcat cs $ \c -> do
        conf <- ask
        r    <- randomChance (cellFeatureChance conf)
        if r then do
            f <- maybeToList <$> pickFeature
            return $ zip (repeat (cmid c)) f
        else
            return []

-- pick an item at random for the given depth
pickFeature :: Generator FeatureConfig (DLevel, [Cell]) (Maybe Feature)
pickFeature = do
    lvl <- fst <$> getGData
    chestContents <- fillChest =<< asks fItemAppearances
    pickRarity (featureRarity (depth lvl)) [Chest chestContents, Fountain 2, Altar]
    where
        fillChest app = do
            lvl <- fst <$> getGData
            app <- asks fItemAppearances
            g   <- getSplit
            let chestConf = ItemConfig { minItems = 3,
                                         maxItems = 10,
                                         itemGenChance = 1 % 6,
                                         itemAppearances = app }
            return $ evalGenerator (generateChestItems (depth lvl)) chestConf (mkGenState [] g)

featureRarity :: Difficulty -> Feature -> Rational
featureRarity d (Chest _) = 1 % 2
featureRarity d (Fountain _) = 1 % 3
featureRarity d (Altar) = 1 % 5

forMConcat :: Monad m => [a] -> (a -> m [b]) -> m [b]
forMConcat l = fmap concat . forM l
