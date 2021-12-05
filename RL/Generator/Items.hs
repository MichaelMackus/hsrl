{-# LANGUAGE TupleSections #-}

module RL.Generator.Items (ItemConfig(..), generateChestItems, randomItemAppearances) where

-- generate random items in dungeon

import RL.Generator
import RL.Item
import RL.Map
import RL.Random
import RL.Util

import Control.Monad.Reader (ask)
import Data.Map (Map)
import Data.Ratio
import Data.Maybe (isJust, catMaybes, fromJust, maybeToList, isNothing, fromMaybe)
import qualified Data.List as L
import qualified Data.Map as M

data ItemConfig = ItemConfig {
    itemAppearances :: Map ItemType String
}

maxRecursion = 10

instance GenConfig ItemConfig where
    generating conf = (< maxRecursion) <$> getCounter

-- generate items in a chest
-- TODO not generating gold in chest when generating magic item
generateChestItems :: Difficulty -> Generator ItemConfig [Item] [Item]
generateChestItems d = do
    conf <- ask
    is   <- getGData
    markGDone
    generateItems =<< goldValue d

goldValue :: MonadRandom m => Difficulty -> m Int
goldValue d | d < 5     = return . fromMaybe 100 =<< pick [250,  500,  1000]
goldValue d | d < 10    = return . fromMaybe 100 =<< pick [1000, 2000, 3000]
goldValue d | otherwise = return . fromMaybe 100 =<< pick [2000, 3000, 5000]

-- generate an item using specified rarity functions
generateItems :: Int -> Generator ItemConfig a [Item]
generateItems value = do
    conf   <- ask
    majorN <- numMagicItems (majorMagicChances  value)
    medN   <- numMagicItems (mediumMagicChances (value - majorN * 5000))
    minorN <- numMagicItems (minorMagicChances  (value - majorN * 5000 - medN * 1000))
    majorI <- replicateMs majorN majorMagicItem
    medI   <- replicateMs medN   mediumMagicItem
    minorI <- replicateMs minorN minorMagicItem

    let gpVal = value - (majorN * 5000) - (medN * 1000) - (minorN * 100)
        is    = map (updateAppearance (itemAppearances conf)) (majorI ++ medI ++ minorI)

    if gpVal > 0 then return $ (gold gpVal):is
    else              return is

majorMagicChances :: Int -> Int
majorMagicChances value | value < 5000 = 0
majorMagicChances value | otherwise    = 1 + majorMagicChances (value - 5000)
mediumMagicChances :: Int -> Int
mediumMagicChances value | value < 1000 = 0
mediumMagicChances value | otherwise    = 1 + mediumMagicChances (value - 1000)
minorMagicChances :: Int -> Int
minorMagicChances value | value < 100 = 0
minorMagicChances value | otherwise   = 1 + minorMagicChances (value - 100)

majorMagicItem :: MonadRandom m => m [Item]
majorMagicItem = roll (1 `d` 6) >>= \r -> case r of
    r | r == 1 -> replicateM 6 generatePotion
      | r <= 3 -> (:[]) <$> generateScroll    (1 `d` 6  `plus` 12)
      | r <= 5 -> generateEquipment (1 `d` 6  `plus` 12)
      | r == 6 -> generateMisc      (1 `d` 20 `plus` 40)

mediumMagicItem :: MonadRandom m => m [Item]
mediumMagicItem = roll (1 `d` 6) >>= \r -> case r of
    r | r == 1 -> replicateM 3 generatePotion
      | r <= 3 -> (:[]) <$> generateScroll (1 `d` 6  `plus` 6)
      | r <= 5 -> generateEquipment (1 `d` 6  `plus` 6)
      | r == 6 -> generateMisc      (1 `d` 20 `plus` 20)

minorMagicItem :: MonadRandom m => m [Item]
minorMagicItem = roll (1 `d` 6) >>= \r -> case r of
    r | r == 1 -> (:[]) <$> generatePotion
      | r <= 3 -> (:[]) <$> generateScroll (1 `d` 6)
      | r <= 5 -> generateEquipment (1 `d` 6)
      | r == 6 -> generateMisc      (1 `d` 20)

numMagicItems :: MonadRandom m => Int -> m Int
numMagicItems 0   = return 0
numMagicItems num = do
    r <- roll $ 1 `d` 10
    if r == 1 then (+1) <$> numMagicItems (num - 1)
    else           numMagicItems (num - 1)

randomItemAppearances :: MonadRandom m => m (Map ItemType String)
randomItemAppearances = do
    let f is = zip (map itemType is) . map itemDescription
    potApps <- M.fromList . f potions <$> shuffle potions
    scrApps <- M.fromList . f scrolls <$> shuffle scrolls
    return (M.union potApps scrApps)

updateAppearance :: Map ItemType String -> Item -> Item
updateAppearance apps i =
    let app = M.lookup (itemType i) apps
        f s = i { itemDescription = s }
    in  maybe i f app

generatePotion :: MonadRandom m => m Item
generatePotion = do
    r <- roll (1 `d` 100)
    let t = case r of
                r | r <= 20   -> Strength
                  | r <= 30   -> Invisibility
                  | r <= 40   -> Acid
                  | r <= 50   -> Confusion
                  | r <= 70   -> Life
                  | otherwise -> Healing
    return $ potion "Clear" t

generateScroll :: MonadRandom m => Dice -> m Item
generateScroll _ = do
    r <- roll (1 `d` 100)
    let t = case r of
                r | r <= 10   -> Fire
                  | r <= 20   -> Lightning
                  | r <= 50   -> Teleport
                  | r <= 70   -> Telepathy
                  | otherwise -> Mapping
    return $ scroll "Random" t

generateEquipment :: MonadRandom m => Dice -> m [Item]
generateEquipment _ = do
    i <- fromMaybe dagger <$> pickRarity equipRarity (weapons ++ armors)
    if i == arrow then return (replicate 20 i)
    else               return [i]

-- TODO misc. magic item
generateMisc :: MonadRandom m => Dice -> m [Item]
generateMisc _ = do
    i <- fromMaybe dagger <$> pickRarity equipRarity (weapons ++ armors)
    if i == arrow then return (replicate 20 i)
    else               return [i]

-- item rarity at depth
equipRarity :: Item -> Rational
equipRarity (Item "Mace"             _) = (1 % 10)
equipRarity (Item "Dagger"           _) = (4 % 10)
equipRarity (Item "Quarterstaff"     _) = (2 % 10)
equipRarity (Item "Sword"            _) = (1 % 10)
equipRarity (Item "Two-Handed Sword" _) = (1 % 20)
equipRarity (Item "Bow"              _) = (1 % 10)
equipRarity (Item "Arrow"            _) = (2 % 10)
equipRarity (Item "Leather Armor"    _) = (4 % 10)
equipRarity (Item "Chain Mail"       _) = (2 % 10)
equipRarity (Item "Plate Mail"       _) = (1 % 10)
equipRarity (Item "Full Plate"       _) = (1 % 50)
equipRarity (Item "Small Shield"     _) = (2 % 10)
equipRarity (Item "Tower Shield"     _) = (1 % 20)
equipRarity otherwise = (0 % 10)
