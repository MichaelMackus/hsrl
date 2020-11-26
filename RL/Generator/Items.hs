{-# LANGUAGE TupleSections #-}

module RL.Generator.Items (itemsGenerator, ItemConfig(..)) where

-- generate random items in dungeon

import RL.Generator
import RL.Item
import RL.Map
import RL.Random

import Control.Monad.Reader (ask)
import Data.Maybe (isJust, catMaybes, fromJust)

data ItemConfig = ItemConfig {
    maxItems :: Int
}

instance GenConfig ItemConfig where
    generating conf = return True

itemsGenerator :: Generator ItemConfig DLevel [(Point, Item)]
itemsGenerator = do
    conf <- ask
    lvl  <- getGData
    items' <- (++ items lvl) . catMaybes <$> replicateM (maxItems conf - length (items lvl)) generateItem
    when (length items' >= maxItems conf) markGDone
    setGData (lvl { items = items' })
    return items'

generateItem :: Generator ItemConfig DLevel (Maybe (Point, Item))
generateItem = do
    lvl <- getGData
    let ts = toTiles lvl
        dheight = length ts
        dwidth  = if length ts > 0 then length (ts !! 0) else 0

    p <- randomPoint dwidth dheight
    let tile = maybe Nothing (\t -> if not (isStair t) then Just t else Nothing) (findTileAt p lvl)
    typ <- pickItemType
    if isJust tile && isPassable (fromJust tile) && isJust typ then do
        i <- pickItem (fromJust typ)
        return ((p, ) <$> i)
    else
        return Nothing

pickItemType :: Generator c s (Maybe ItemType)
pickItemType = pickRarity typeRarity itemTypes
pickItem :: ItemType -> Generator c s (Maybe Item)
pickItem typ =
    case typ of
        (Weapon _) -> pickRarity itemRarity weapons
        (Armor  _) -> pickRarity itemRarity armors
        (Potion  ) -> pickRarity itemRarity potions
        (Tool    ) -> pickRarity itemRarity tools

