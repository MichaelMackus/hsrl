{-# LANGUAGE TupleSections #-}

module RL.Generator.Items (ItemConfig(..), itemsGenerator, generateChestItems, randomItemAppearances) where

-- generate random items in dungeon

import RL.Generator
import RL.Item
import RL.Map
import RL.Random

import Control.Monad.Reader (ask)
import Data.Map (Map)
import Data.Ratio
import Data.Maybe (isJust, catMaybes, fromJust, maybeToList, isNothing)
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
    items' <- maybe (items lvl) (:items lvl) <$> generateFloorItem
    setGData (lvl { items = items' })
    return items'

-- generate items in a chest
-- TODO minItems
generateChestItems :: Difficulty -> Generator ItemConfig [Item] [Item]
generateChestItems d = do
    conf <- ask
    is   <- getGData
    i    <- generateItem (typeRarity d) (itemRarity d)
    let is' = maybeToList i ++ is
    setGData is'
    return is'

-- generate an item on the floor
generateFloorItem :: Generator ItemConfig DLevel (Maybe (Point, Item))
generateFloorItem = do
    lvl <- getGData
    let tileF p t = not (isStair t) && isPassable t && isNothing (L.lookup p (features lvl))
    i   <- generateItem (typeRarity (depth lvl)) (itemRarity (depth lvl)) -- TODO minItems
    p   <- randomTile tileF lvl
    return ((,) <$> p <*> i)

-- generate an item using specified rarity functions
generateItem :: (ItemType -> Rational) -> (Item -> Rational) -> Generator ItemConfig a (Maybe Item)
generateItem f g = do
    conf <- ask
    r    <- randomChance (itemGenChance conf)
    if r then do
        fmap (updateAppearance (itemAppearances conf)) <$> randomItem f g
    else
        return Nothing


randomItemAppearances :: MonadRandom m => m (Map ItemType String)
randomItemAppearances = do
    let f is = zip (map itemType is) . map itemDescription
    potApps <- M.fromList . f potions <$> shuffle potions
    scrApps <- M.fromList . f scrolls <$> shuffle scrolls
    return (M.union potApps scrApps)

randomItem :: MonadRandom m => (ItemType -> Rational) -> (Item -> Rational) -> m (Maybe Item)
randomItem f g = do
    t <- pickRarity f itemTypes
    case t of
        Just (Weapon _) -> pickRarity g weapons
        Just (Armor  _) -> pickRarity g armors
        Just (Potion _) -> pickRarity g potions
        Just (Scroll _) -> pickRarity g scrolls
        otherwise       -> return Nothing

updateAppearance :: Map ItemType String -> Item -> Item
updateAppearance apps i =
    let app = M.lookup (itemType i) apps
        f s = i { itemDescription = s }
    in  maybe i f app

-- rarity for item types at depth
typeRarity :: Difficulty -> ItemType -> Rational
typeRarity d t
    | d == 1 = case t of 
                   (Weapon _) -> 1 % 5
                   (Armor _)  -> 1 % 7
                   (Potion _) -> 1 % 10
                   (Scroll _) -> 1 % 20
                   (Bandage)  -> 0 % 10
    | d <= 3 = case t of 
                   (Weapon _) -> 1 % 5
                   (Armor _)  -> 1 % 7
                   (Potion _) -> 1 % 5
                   (Scroll _) -> 1 % 10
                   (Bandage)  -> 0 % 10
    | otherwise = case t of 
                   (Weapon _) -> 1 % 5
                   (Armor _)  -> 1 % 7
                   (Potion _) -> 1 % 3
                   (Scroll _) -> 1 % 4
                   (Bandage)  -> 0 % 10

-- item rarity at depth
itemRarity :: Difficulty -> Item -> Rational
itemRarity d (Item "Mace"             _) = (1 % 10)
itemRarity d (Item "Dagger"           _) = (4 % 10)
itemRarity d (Item "Quarterstaff"     _) = (2 % 10)
itemRarity d (Item "Sword"            _) = (1 % 10)
itemRarity d (Item "Two-Handed Sword" _) = (1 % 20)
itemRarity d (Item "Bow"              _) = (1 % 10)
itemRarity d (Item "Arrow"            _) = (2 % 10)
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
