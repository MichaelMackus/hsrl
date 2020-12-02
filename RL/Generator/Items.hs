{-# LANGUAGE TupleSections #-}

module RL.Generator.Items (ItemConfig(..), itemsGenerator, randomItemAppearances) where

-- generate random items in dungeon

import RL.Generator
import RL.Item
import RL.Map
import RL.Random

import Control.Monad.Reader (ask)
import Data.Map (Map)
import Data.Ratio
import Data.Maybe (isJust, catMaybes, fromJust)
import qualified Data.List as L
import qualified Data.Map as M

data ItemConfig = ItemConfig {
    maxItems :: Int,
    minItems :: Int,
    itemGenChance :: Rational,
    itemAppearances :: Map ItemType String
}

instance GenConfig ItemConfig where
    generating conf = (< maxItems conf) <$> getCounter

itemsGenerator :: Generator ItemConfig DLevel [(Point, Item)]
itemsGenerator = do
    conf <- ask
    lvl  <- getGData
    items' <- maybe (items lvl) (:items lvl) <$> generateItem
    setGData (lvl { items = items' })
    return items'

randomItemAppearances :: MonadRandom m => m (Map ItemType String)
randomItemAppearances = do
    let f is = zip (map itemType is) . map itemDescription
    potApps <- M.fromList . f potions <$> shuffle potions
    scrApps <- M.fromList . f scrolls <$> shuffle scrolls
    return (M.union potApps scrApps)

generateItem :: Generator ItemConfig DLevel (Maybe (Point, Item))
generateItem = do
    lvl  <- getGData
    conf <- ask
    r    <- randomChance (itemGenChance conf)
    if length (items lvl) < minItems conf || r then do
        let tileF _ t = not (isStair t) && isPassable t
        p <- randomTile tileF lvl
        i <- fmap (updateAppearance (itemAppearances conf)) <$> pickItem (depth lvl)
        return ((,) <$> p <*> i)
    else
        return Nothing

updateAppearance :: Map ItemType String -> Item -> Item
updateAppearance apps i =
    let app = M.lookup (itemType i) apps
        f s = i { itemDescription = s }
    in  maybe i f app

-- pick an item at random for the given depth
pickItem :: MonadRandom m => Difficulty -> m (Maybe Item)
pickItem d = do
    typ <- pickItemType d
    case typ of
        Just (Weapon _) -> pickRarity (itemRarity d) weapons
        Just (Armor  _) -> pickRarity (itemRarity d) armors
        Just (Potion _) -> pickRarity (itemRarity d) potions
        Just (Scroll _) -> pickRarity (itemRarity d) scrolls
        Just (Tool    ) -> pickRarity (itemRarity d) tools
        otherwise       -> return Nothing

pickItemType :: MonadRandom m => Difficulty -> m (Maybe ItemType)
pickItemType d = pickRarity (typeRarity d) itemTypes

-- rarity for item types at depth
typeRarity :: Difficulty -> ItemType -> Rational
typeRarity d t
    | d == 1 = case t of 
                   (Weapon _) -> 1 % 5
                   (Armor _)  -> 1 % 7
                   (Potion _) -> 1 % 10
                   (Scroll _) -> 1 % 20
                   (Tool)     -> 0 % 10
    | d <= 3 = case t of 
                   (Weapon _) -> 1 % 5
                   (Armor _)  -> 1 % 7
                   (Potion _) -> 1 % 5
                   (Scroll _) -> 1 % 10
                   (Tool)     -> 0 % 7
    | otherwise = case t of 
                   (Weapon _) -> 1 % 5
                   (Armor _)  -> 1 % 7
                   (Potion _) -> 1 % 3
                   (Scroll _) -> 1 % 4
                   (Tool)     -> 0 % 5

-- item rarity at depth
itemRarity :: Difficulty -> Item -> Rational
itemRarity d (Item "Mace"             _) = (1 % 10)
itemRarity d (Item "Dagger"           _) = (4 % 10)
itemRarity d (Item "Quarterstaff"     _) = (2 % 10)
itemRarity d (Item "Sword"            _) = (1 % 10)
itemRarity d (Item "Two-Handed Sword" _) = (1 % 20)
itemRarity d (Item "Leather Armor"    _) = (4 % 10)
itemRarity d (Item "Chain Mail"       _) = (2 % 10)
itemRarity d (Item "Plate Mail"       _) = (1 % 10)
itemRarity d (Item "Full Plate"       _) = (1 % 50)
itemRarity d (Item "Small Shield"     _) = (2 % 10)
itemRarity d (Item "Tower Shield"     _) = (1 % 20)
itemRarity d (Item _         (Potion t)) = potionRarities t
itemRarity d (Item _         (Scroll t)) = scrollRarities t
itemRarity d otherwise = (0 % 10)

potionRarities :: PotionType -> Rational
potionRarities Healing = (1 % 5)
potionRarities Acid = (1 % 5)
potionRarities Darkness = (1 % 5)
potionRarities _ = (1 % 10)

scrollRarities :: ScrollType -> Rational
scrollRarities _ = (1 % 10)
