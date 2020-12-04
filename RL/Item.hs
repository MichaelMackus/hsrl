module RL.Item where

import RL.Types

import Data.Maybe (isJust, fromJust)
import qualified Data.List as L

data Item = Item {
    itemDescription :: ItemName,
    itemType :: ItemType
} deriving Eq

type ItemName = String
type RandomName = String
type ItemRarity = Rational
data ItemType = Weapon WeaponProperties | Armor ArmorProperties | Potion PotionType | Scroll ScrollType | Tool deriving (Eq, Ord)
data PotionType = Healing | Life | Acid | Strength | Invisibility | Confusion | Darkness deriving (Show, Eq, Ord)
data ScrollType = Fire | Lightning | Teleport | Mapping | Telepathy deriving (Show, Eq, Ord)

instance Show Item where
    show (Item n (Potion _ )) = n ++ " Potion"
    show (Item n (Scroll _ )) = "Scroll labeled \"" ++ n ++ "\""
    show (Item n _) = n
instance Show ItemType where
    show (Weapon _) = "weapon"
    show (Armor _) = "armor"
    show (Potion t) = "potion"
    show (Scroll t) = "scroll"
    show (Tool) = "tool"

-- show true name if in identified list
showIdentified :: [ItemType] -> Item -> String
showIdentified identified i = if itemType i `elem` identified then itemTrueName i else show i

-- get true item name (after identified)
itemTrueName :: Item -> String
itemTrueName i = show (i { itemDescription = (typeTrueName (itemType i)) })
    where typeTrueName (Weapon _) = itemDescription i
          typeTrueName (Armor  _) = itemDescription i
          typeTrueName (Potion t) = show t
          typeTrueName (Scroll t) = show t
          typeTrueName (Tool    ) = itemDescription i

data WeaponProperties = WeaponProperties {
    dmgd :: Dice,
    bonus :: Int, -- positive OR negative bonus to attack & damage rolls
    twoHanded :: Bool,
    critRange :: Int,
    projectileType :: Maybe ProjectileType,
    launcherType :: Maybe ProjectileType
} deriving (Eq, Ord)

data ProjectileType = Thrown | Arrow | Bullet deriving (Eq, Ord)

weaponProperties :: Item -> Maybe WeaponProperties
weaponProperties (Item _ (Weapon prop)) = Just prop
weaponProperties otherwise              = Nothing

data ArmorProperties = ArmorProperties {
    defense :: Int, -- this is opposite of traditional AD&D - this number is subtracted by 10 for the *true* AC of a mob
    slot    :: ArmorSlot
} deriving (Eq, Ord)
data ArmorSlot = Chest | Hand deriving (Eq, Ord)

inventoryLetters :: [Char]
inventoryLetters = map toEnum ([fromEnum 'a'..fromEnum 'z'] ++ [fromEnum 'A'..fromEnum 'Z'])

fromInventoryLetter :: Char -> [Item] -> Maybe Item
fromInventoryLetter ch is = L.lookup ch (zip inventoryLetters is)

armorProperties :: Item -> Maybe ArmorProperties
armorProperties (Item _ (Armor prop)) = Just prop
armorProperties otherwise             = Nothing

armorSlot :: Item -> Maybe ArmorSlot
armorSlot i = slot <$> armorProperties i

findItemByName :: String -> [Item] -> Maybe Item
findItemByName n = L.find f
    where f i = itemDescription i == n

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

itemTypes = [ Weapon undefined, Armor undefined, Potion undefined, Scroll undefined, Tool ]

isShield :: Item -> Bool
isShield i = maybe False ((== Hand) . slot) (armorProperties i)

itemSymbol :: Item -> Char
itemSymbol   (Item _ (Weapon _)) = ')'
itemSymbol i@(Item _ (Armor  _)) = if isShield i then '0' else ']'
itemSymbol   (Item _ (Potion _)) = '!'
itemSymbol   (Item _ (Scroll _)) = '?'
itemSymbol   (Item _ (Tool)) = '/'

dagger = weapon "Dagger" (WeaponProperties (1 `d` 4) 0 False 19 (Just Thrown) Nothing)
bow    = weapon "Bow" (WeaponProperties (1 `d` 6) 0 True 20 Nothing (Just Arrow))
arrow  = weapon "Arrow" (WeaponProperties (1 `d` 3) 0 True 20 (Just Arrow) Nothing)
weapons = [ weapon "Mace" (WeaponProperties (1 `d` 6) 0 False 20 Nothing Nothing),
            dagger,
            weapon "Quarterstaff" (WeaponProperties (1 `d` 8) 0 True 20 Nothing Nothing),
            weapon "Sword" (WeaponProperties (1 `d` 8) 0 False 20 Nothing Nothing),
            weapon "Two-Handed Sword" (WeaponProperties (1 `d` 10) 0 True 20 Nothing Nothing),
            bow,
            arrow,
            weapon "Sling" (WeaponProperties (1 `d` 4) 0 True 20 Nothing (Just Bullet)),
            weapon "Rock" (WeaponProperties (1 `d` 3) 0 True 20 (Just Bullet) Nothing),
            -- TODO generate on last level
            weapon "Ornate Sword" (WeaponProperties (1 `d` 10) 3 False 19 Nothing Nothing) ]

armors = [ armor "Leather Armor" (ArmorProperties 2 Chest),
           armor "Chain Mail" (ArmorProperties 6 Chest),
           armor "Plate Mail" (ArmorProperties 8 Chest),
           armor "Full Plate" (ArmorProperties 10 Chest),
           armor "Small Shield" (ArmorProperties 1 Hand),
           armor "Tower Shield" (ArmorProperties 2 Hand) ]

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
