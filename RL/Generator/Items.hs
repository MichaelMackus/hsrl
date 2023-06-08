{-# LANGUAGE TupleSections, FlexibleInstances #-}

module RL.Generator.Items (ItemConfig(..), generateChestItems, randomItemAppearances) where

-- generate random items in dungeon

import RL.Generator
import RL.Item
import RL.Dungeon
import RL.Random
import RL.Util

import Control.Monad.Reader (ask)
import Control.Monad.State (gets)
import Data.Map (Map)
import Data.Ratio
import Data.Maybe (isJust, catMaybes, fromJust, maybeToList, isNothing, fromMaybe)
import qualified Data.List as L
import qualified Data.Map as M

data ItemConfig = ItemConfig {
    itemAppearances :: Map ItemType String
}

instance GenConfig ItemConfig s where
    -- generating conf = (< maxItems conf) <$> getCounter
    generating conf = (< maxRecursion) <$> getCounter

maxRecursion = 10

-- generate items in a chest
generateChestItems :: Difficulty -> Generator ItemConfig [Item] [Item]
generateChestItems d = do
    conf <- ask
    is   <- getGData
    markGDone
    generateItems d

goldValue :: MonadRandom m => Difficulty -> m Int
goldValue diff | diff < 5     = (* 100 ) <$> roll (1 `d` 10)
goldValue diff | diff < 10    = (* 1000) <$> roll (1 `d` 6)
goldValue diff | otherwise    = (* 1000) <$> roll (1 `d` 12)

data TreasureTable = TreasureTableItem Rational Dice Item | TreasureTableMagicItem Rational (Either Int Dice) -- TODO magic items + scroll/pots

itemsFromTreasureTables :: MonadRandom m => [TreasureTable] -> m [Item]
itemsFromTreasureTables is = reduceGold . concat <$> mapM f is
    where f :: MonadRandom m => TreasureTable -> m [Item]
          f (TreasureTableItem r d i) = do
                res <- randomChance r
                n   <- roll d
                if res then return (replicate n i)
                else return []
          f (TreasureTableMagicItem r (Right d)) = do
                res <- randomChance r
                n   <- roll d
                if res then concat <$> replicateM n magicItem
                else return []
          f (TreasureTableMagicItem r (Left n)) = do
                res <- randomChance r
                if res then concat <$> replicateM n magicItem
                else return []

treasureB =  [TreasureTableItem      (50 % 100) (1 `d` 8) (gold 10),   -- cp
              TreasureTableItem      (25 % 100) (1 `d` 6) (gold 100),  -- sp
              TreasureTableItem      (25 % 100) (1 `d` 4) (gold 500),  -- ep
              TreasureTableItem      (25 % 100) (1 `d` 3) (gold 1000), -- gp
              TreasureTableItem      (25 % 100) (1 `d` 6) (gold 500),  -- TODO gems
              TreasureTableItem      (25 % 100) (1 `d` 6) (gold 1000), -- TODO jewelry
              TreasureTableMagicItem (10 % 100) (Left 1) ]

treasureF =  [TreasureTableItem      (10 % 100) (2 `d` 10) (gold 100),   -- sp
              TreasureTableItem      (20 % 100) (1 `d` 8 ) (gold 500),   -- ep
              TreasureTableItem      (45 % 100) (1 `d` 12) (gold 1000),  -- gp
              TreasureTableItem      (30 % 100) (1 `d` 3 ) (gold 10000), -- pp
              TreasureTableItem      (20 % 100) (2 `d` 12) (gold 500),   -- TODO gems
              TreasureTableItem      (10 % 100) (1 `d` 10) (gold 1000),  -- TODO jewelry
              TreasureTableMagicItem (25 % 100) (Left 4) ] -- TODO guaranteed scroll

treasureG =  [TreasureTableItem      (50 % 100) (1 `d` 4 ) (gold 10000), -- gp
              TreasureTableItem      (50 % 100) (1 `d` 6 ) (gold 1000),  -- pp
              TreasureTableItem      (25 % 100) (3 `d` 6)  (gold 500),   -- TODO gems
              TreasureTableItem      (25 % 100) (1 `d` 10) (gold 1000),  -- TODO jewelry
              TreasureTableMagicItem (35 % 100) (Left 5) ] -- TODO guaranteed scroll

-- generate an item using specified rarity functions
generateItems :: Difficulty -> Generator ItemConfig a [Item]
generateItems d = itemsFromTreasureTables $ if d < 5 then treasureB
                                            else if d < 10 then treasureF
                                            else treasureG

majorMagicChances :: Int -> Int
majorMagicChances value | value < 5000 = 0
majorMagicChances value | otherwise    = 1 + majorMagicChances (value - 5000)
mediumMagicChances :: Int -> Int
mediumMagicChances value | value < 1000 = 0
mediumMagicChances value | otherwise    = 1 + mediumMagicChances (value - 1000)
minorMagicChances :: Int -> Int
minorMagicChances value | value < 100 = 0
minorMagicChances value | otherwise   = 1 + minorMagicChances (value - 100)

-- TODO item tables
magicItem :: MonadRandom m => m [Item]
magicItem = minorMagicItem

majorMagicItem :: MonadRandom m => m [Item]
majorMagicItem = roll (1 `d` 6) >>= \r -> case r of
    r | r == 1 -> replicateM 6 generatePotion
      | r <= 3 -> (:[]) <$> generateScroll    (1 `d` 6  `plus` 12)
      | r <= 5 -> generateMagicEquipment (1 `d` 6  `plus` 12)
      | r == 6 -> generateMisc      (1 `d` 20 `plus` 40)

mediumMagicItem :: MonadRandom m => m [Item]
mediumMagicItem = roll (1 `d` 6) >>= \r -> case r of
    r | r == 1 -> replicateM 3 generatePotion
      | r <= 3 -> (:[]) <$> generateScroll (1 `d` 6  `plus` 6)
      | r <= 5 -> generateMagicEquipment (1 `d` 6  `plus` 6)
      | r == 6 -> generateMisc      (1 `d` 20 `plus` 20)

minorMagicItem :: MonadRandom m => m [Item]
minorMagicItem = roll (1 `d` 6) >>= \r -> case r of
    r | r == 1 -> (:[]) <$> generatePotion
      | r <= 3 -> (:[]) <$> generateScroll (1 `d` 6)
      | r <= 5 -> generateMagicEquipment (1 `d` 6)
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

-- TODO cursed item, healing, etc.
generateMagicEquipment :: MonadRandom m => Dice -> m [Item]
generateMagicEquipment d = do
        equips <- generateEquipment
        mapM gen equips
    where gen i@(Item n (Weapon props)) = let effects = if n == "Sword" || n == "Two-Handed Sword" then swordEffects else weaponEffects
                                          in  return . maybe i (\effs -> Item n (Weapon (props { weaponEffects = effs } ))) =<< pick effects
          gen i@(Item n (Armor  props)) = return . maybe i (\effs -> Item n (Armor (props { armorEffects = effs } ))) =<< pick armorEffects
          swordEffects                  = [[ItemTelepathy, ItemBonus 1], [ItemFire, ItemBonus 1], [ItemBonus 1], [ItemBonus 2], [ItemBonus 3]]
          weaponEffects                 = [[ItemBonus 1], [ItemBonus 2]]
          armorEffects                  = [[ItemBonus 1], [ItemBonus 2], [ItemBonus 3]]

generateEquipment :: MonadRandom m => m [Item]
generateEquipment = do
    i <- fromMaybe dagger <$> pickRarity equipRarity (weapons ++ armors)
    if i == arrow then return (replicate 20 i)
    else               return [i]

-- TODO misc. magic item
generateMisc :: MonadRandom m => Dice -> m [Item]
generateMisc = generateMagicEquipment
-- generateMisc _ = do
--     i <- fromMaybe dagger <$> pickRarity equipRarity (weapons ++ armors)
--     if i == arrow then return (replicate 20 i)
--     else               return [i]

-- item rarity at depth
equipRarity :: Item -> Rational
equipRarity (Item "Mace"             _) = (1 % 10)
equipRarity (Item "Dagger"           _) = (4 % 10)
equipRarity (Item "Hand Axe"         _) = (2 % 10)
equipRarity (Item "Quarterstaff"     _) = (2 % 10)
equipRarity (Item "Sword"            _) = (1 % 10)
equipRarity (Item "Two-Handed Sword" _) = (1 % 20)
equipRarity (Item "Bow"              _) = (1 % 10)
equipRarity (Item "Arrow"            _) = (2 % 10)
equipRarity (Item "Leather Armor"    _) = (4 % 10)
equipRarity (Item "Chain Mail"       _) = (2 % 10)
equipRarity (Item "Plate Mail"       _) = (1 % 10)
equipRarity (Item "Full Plate"       _) = (0 % 10)
equipRarity (Item "Small Shield"     _) = (2 % 10)
equipRarity (Item "Tower Shield"     _) = (1 % 20)
equipRarity otherwise = (0 % 10)
