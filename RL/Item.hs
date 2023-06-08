module RL.Item where

import RL.Types
import RL.Util (groupBy')

import Data.Maybe (isJust, fromJust)
import qualified Data.List as L

data Item = Item {
    itemDescription :: ItemName,
    itemType :: ItemType
} deriving Eq

-- TODO gems
type ItemName = String
type RandomName = String
type ItemRarity = Rational
data ItemType = Gold Int | Weapon WeaponProperties | Armor ArmorProperties | Potion PotionType | Scroll ScrollType deriving (Eq, Ord)
data PotionType = Healing | Life | Acid | Strength | Invisibility | Confusion | Darkness deriving (Show, Eq, Ord)
data ScrollType = Fire | Lightning | Teleport | Mapping | Telepathy deriving (Show, Eq, Ord)
data MagicItemEffects = ItemTelepathy | ItemFire | ItemBonus Int | ItemCurse | ItemHealing | ItemMobBonus String Int deriving (Show, Eq, Ord)

data WeaponProperties = WeaponProperties {
    dmgd :: Dice,
    bonus :: Int, -- positive OR negative bonus to attack & damage rolls
    twoHanded :: Bool,
    projectileType :: Maybe ProjectileType,
    launcherType :: Maybe ProjectileType,
    weaponEffects :: [MagicItemEffects]
} deriving (Eq, Ord)

data ProjectileType = Thrown | Arrow | Bullet deriving (Eq, Ord)

data ArmorProperties = ArmorProperties {
    defense :: Int, -- this is opposite of traditional AD&D - this number is subtracted by 10 for the *true* AC of a mob
    slot    :: ArmorSlot,
    armorEffects :: [MagicItemEffects]
} deriving (Eq, Ord)
data ArmorSlot = Body | Hand deriving (Eq, Ord)

instance Show Item where
    show (Item n (Potion _)) = n ++ " Potion"
    show (Item n (Scroll _)) = "Scroll labeled \"" ++ n ++ "\""
    show (Item n (Gold   x)) = show x ++ " Gold"
    show (Item n _) = n
instance Show ItemType where
    show (Weapon _) = "weapon"
    show (Armor _) = "armor"
    show (Potion t) = "potion"
    show (Scroll t) = "scroll"

-- show true name if in identified list
showIdentified :: [ItemType] -> Item -> String
showIdentified identified i@(Item n (Weapon props)) = showItemWithEffects (weaponEffects props) i
showIdentified identified i@(Item n (Armor  props)) = showItemWithEffects (armorEffects props) i
showIdentified identified i                         = if itemType i `elem` identified then itemTrueName i else show i

showItemWithEffects props (Item n _) = foldr f n props
    where f ItemTelepathy      n = n ++ " of telepathy"
          f ItemFire           n = n ++ " of fire"
          f ItemHealing        n = n ++ " of healing"
          f (ItemBonus x)      n = "+" ++ show x ++ " " ++ n
          f (ItemMobBonus m x) n = "+" ++ show x ++ " to " ++ m ++ "s " ++ n
          f ItemCurse          n = "cursed " ++ n

gold :: Int -> Item
gold = Item "Gold" . Gold

-- reduces multiple gold in list to single item
reduceGold :: [Item] -> [Item]
reduceGold is = let gp = foldr f 0 is
                    f (Item _ (Gold n)) accum = n + accum
                    f otherwise         accum = accum
                in  (gold gp):(L.filter (not . isGold) is)

isGold :: Item -> Bool
isGold (Item _ (Gold _)) = True
isGold otherwise         = False

goldAmount :: [Item] -> Int
goldAmount = foldr f 0
    where f (Item _ (Gold a)) b = a + b
          f otherwise         b = b

-- get true item name (after identified)
itemTrueName :: Item -> String
itemTrueName i = show (i { itemDescription = (typeTrueName (itemType i)) })
    where typeTrueName (Weapon _) = itemDescription i
          typeTrueName (Armor  _) = itemDescription i
          typeTrueName (Potion t) = show t
          typeTrueName (Scroll t) = show t
          typeTrueName (Gold   _) = itemDescription i

weaponProperties :: Item -> Maybe WeaponProperties
weaponProperties (Item _ (Weapon prop)) = Just prop
weaponProperties otherwise              = Nothing

-- in squares
itemRange :: Item -> Maybe Double
itemRange i = projectileRange <$> (projectileType =<< weaponProperties i)

-- in squares
projectileRange :: ProjectileType -> Double
projectileRange Thrown = 2.5
projectileRange Arrow  = 10
projectileRange Bullet = 5

-- TODO mob bonus
weaponBonus :: WeaponProperties -> Int
weaponBonus prop = foldr f (bonus prop) (weaponEffects prop)
    where f (ItemBonus n) accum = n + accum
          f otherwise     accum = accum

-- TODO mob bonus
weaponDamage :: WeaponProperties -> Dice
weaponDamage prop = foldr f (dmgd prop) (weaponEffects prop)
    where f (ItemBonus n) accum = accum `plus` n
          f ItemFire      accum = accum `plus` 4 -- TODO fire should do additional d8 damage
          f otherwise     accum = accum

armorDefense :: ArmorProperties -> Int
armorDefense prop = defense prop + foldr f 0 (armorEffects prop)
    where f (ItemBonus n) accum = n + accum
          f otherwise     accum = accum

groupItems :: [Item] -> [(Int, Item)]
groupItems = map f . groupBy' g
    where f l    = (length l, head l)
          g i i' = i == i' && isStackable i

ungroupItems :: [(Int, Item)] -> [Item]
ungroupItems = concat . map f
    where f (n, i) = replicate n i

inventoryLetters :: [Char]
inventoryLetters = map toEnum ([fromEnum 'a'..fromEnum 'z'] ++ [fromEnum 'A'..fromEnum 'Z'])

fromInventoryLetter :: Char -> [Item] -> Maybe Item
fromInventoryLetter ch is = snd . snd <$> (L.find f . zip inventoryLetters . groupItems . L.filter (not . isGold) $ is)
    where f (ch', (_, i)) = ch == ch'

armorProperties :: Item -> Maybe ArmorProperties
armorProperties (Item _ (Armor prop)) = Just prop
armorProperties otherwise             = Nothing

armorSlot :: Item -> Maybe ArmorSlot
armorSlot i = slot <$> armorProperties i

findItemByName :: String -> [Item] -> Maybe Item
findItemByName n = L.find f
    where f i = itemDescription i == n

isStackable :: Item -> Bool
isStackable i = isProjectile i || isReadable i || isDrinkable i

isProjectile :: Item -> Bool
isProjectile = maybe False (isJust . projectileType) . weaponProperties

isLauncher :: Item -> Bool
isLauncher = maybe False (isJust . launcherType) . weaponProperties

launchesProjectile :: Item -> Item -> Bool
launchesProjectile l p = let prop = fromJust . weaponProperties
                             ltyp = launcherType (prop l)
                             ptyp = projectileType (prop p)
                         in  isLauncher l && isProjectile p && ltyp == ptyp

isTwoHanded :: Item -> Bool
isTwoHanded = maybe False twoHanded . weaponProperties

isWeapon :: Item -> Bool
isWeapon (Item _ (Weapon _)) = True
isWeapon otherwise           = False

isArmor :: Item -> Bool
isArmor (Item _ (Armor _)) = True
isArmor otherwise          = False

isHeavyArmor :: Item -> Bool
isHeavyArmor (Item "Plate Mail" _) = True
isHeavyArmor (Item "Full Plate" _) = True
isHeavyArmor (Item "Chain Mail" _) = True
isHeavyArmor otherwise             = False

isEquippable :: Item -> Bool
isEquippable i = isWeapon i || isArmor i

isDrinkable :: Item -> Bool
isDrinkable = isJust . potionType

isReadable :: Item -> Bool
isReadable = isJust . scrollType

isFragile :: Item -> Bool
isFragile i = let prop = fromJust (weaponProperties i)
              in  isProjectile i && maybe False (==Arrow) (projectileType prop)

scrollType :: Item -> Maybe ScrollType
scrollType (Item _ (Scroll t)) = Just t
scrollType otherwise = Nothing

potionType :: Item -> Maybe PotionType
potionType (Item _ (Potion t)) = Just t
potionType otherwise = Nothing

isShield :: Item -> Bool
isShield i = maybe False ((== Hand) . slot) (armorProperties i)

itemSymbol :: Item -> Char
itemSymbol   (Item _ (Weapon _)) = ')'
itemSymbol i@(Item _ (Armor  _)) = if isShield i then '0' else ']'
itemSymbol   (Item _ (Potion _)) = '!'
itemSymbol   (Item _ (Scroll _)) = '?'
itemSymbol   (Item _ (Gold  _))  = '$'

dagger  = weapon "Dagger" (WeaponProperties (1 `d` 4) 0 False (Just Thrown) Nothing [])
handAxe = weapon "Hand Axe" (WeaponProperties (1 `d` 6) 0 False (Just Thrown) Nothing [])
bow     = weapon "Bow" (WeaponProperties (1 `d` 6) 0 True Nothing (Just Arrow) [])
arrow   = weapon "Arrow" (WeaponProperties (1 `d` 3) 0 True (Just Arrow) Nothing [])
weapons = [ weapon "Mace" (WeaponProperties (1 `d` 6) 0 False Nothing Nothing []),
            dagger,
            handAxe,
            weapon "Quarterstaff" (WeaponProperties (1 `d` 8) 0 True Nothing Nothing []),
            weapon "Sword" (WeaponProperties (1 `d` 8) 0 False Nothing Nothing []),
            weapon "Two-Handed Sword" (WeaponProperties (1 `d` 10) 0 True Nothing Nothing []),
            bow,
            arrow,
            weapon "Sling" (WeaponProperties (1 `d` 4) 0 True Nothing (Just Bullet) []),
            weapon "Rock" (WeaponProperties (1 `d` 3) 0 True (Just Bullet) Nothing []),
            -- TODO generate on last level
            weapon "Ornate Sword" (WeaponProperties (1 `d` 10) 3 False Nothing Nothing []) ]

armors = [ armor "Leather Armor" (ArmorProperties 2 Body[]),
           armor "Chain Mail" (ArmorProperties 4 Body []),
           armor "Plate Mail" (ArmorProperties 6 Body []),
           armor "Full Plate" (ArmorProperties 9 Body []),
           armor "Small Shield" (ArmorProperties 1 Hand []),
           armor "Tower Shield" (ArmorProperties 2 Hand []) ]

potions = [ potion "Blue" Healing,
            potion "Yellow" Life,
            potion "Black" Acid,
            potion "Red" Strength,
            potion "White" Invisibility,
            potion "Green" Confusion,
            potion "Orange" Darkness ]

scrolls = [ scroll "READ ME" Fire,
            scroll "HEHE" Lightning,
            scroll "ABRACADABRA" Teleport,
            scroll "TRY THIS" Mapping,
            scroll "LOREM IPSUM" Telepathy ]

tools = []

-- helpers to generate items

weapon :: ItemName -> WeaponProperties -> Item
weapon n prop = Item n (Weapon prop)

armor :: ItemName -> ArmorProperties -> Item
armor n prop = Item n (Armor prop)

potion :: RandomName -> PotionType -> Item
potion n t = Item n (Potion t)

scroll :: RandomName -> ScrollType -> Item
scroll n t = Item n (Scroll t)
