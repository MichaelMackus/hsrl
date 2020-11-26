module RL.Item where

import RL.Types

import Data.Maybe (isJust)
import Data.Ratio
import qualified Data.List as L

data Item = Item {
    itemDescription :: ItemName,
    itemType :: ItemType
} deriving Eq

type ItemName = String
type RandomName = String
type ItemRarity = Rational
data ItemType = Weapon WeaponProperties | Armor ArmorProperties | Potion PotionType | Scroll ScrollType | Tool deriving Eq
data PotionType = Healing | Life | Acid | Strength | Invisibility | Confusion | Darkness deriving (Show, Eq)
data ScrollType = Fire | Lightning | Teleport | Mapping | Telepathy deriving (Show, Eq)

instance Show Item where
    show (Item n (Potion _ )) = n ++ " Potion"
    show (Item n (Scroll _ )) = "Scroll labeled \"" ++ n ++ "\""
    show (Item n _) = n
instance Show ItemType where
    show (Weapon _) = "weapon"
    show (Armor _) = "armor"
    show (Potion _) = "potion"
    show (Scroll _) = "scroll"
    show (Tool) = "tool"

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
    twoHanded :: Bool
} deriving Eq

weaponProperties :: Item -> Maybe WeaponProperties
weaponProperties (Item _ (Weapon prop)) = Just prop
weaponProperties otherwise              = Nothing

data ArmorProperties = ArmorProperties {
    defense :: Int, -- this is opposite of traditional AD&D - this number is subtracted by 10 for the *true* AC of a mob
    slot    :: ArmorSlot
} deriving Eq
data ArmorSlot = Head | Chest | Legs | Feet | Hands deriving Eq

inventoryLetters :: [Char]
inventoryLetters = map toEnum ([fromEnum 'a'..fromEnum 'z'] ++ [fromEnum 'A'..fromEnum 'Z'])

fromInventoryLetter :: Char -> [Item] -> Maybe Item
fromInventoryLetter ch is = L.lookup ch (zip inventoryLetters is)

armorProperties :: Item -> Maybe ArmorProperties
armorProperties (Item _ (Armor prop)) = Just prop
armorProperties otherwise             = Nothing

findItemByName :: String -> [Item] -> Maybe Item
findItemByName n = L.find f
    where f i = itemDescription i == n

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

scrollType :: Item -> Maybe ScrollType
scrollType (Item _ (Scroll t)) = Just t
scrollType otherwise = Nothing

potionType :: Item -> Maybe PotionType
potionType (Item _ (Potion t)) = Just t
potionType otherwise = Nothing

itemTypes = [ Weapon undefined, Armor undefined, Potion undefined, Scroll undefined, Tool ]

itemSymbol :: Item -> Char
itemSymbol (Item _ (Weapon _)) = ')'
itemSymbol (Item _ (Armor _)) = ']'
itemSymbol (Item _ (Potion _)) = '!'
itemSymbol (Item _ (Scroll _)) = '?'
itemSymbol (Item _ (Tool)) = '/'

weapons = [ weapon "Mace" (WeaponProperties (1 `d` 6) 0 False),
            weapon "Dagger" (WeaponProperties (1 `d` 4) 0 False),
            weapon "Quarterstaff" (WeaponProperties (1 `d` 8) 0 True),
            weapon "Sword" (WeaponProperties (1 `d` 8) 0 False),
            weapon "Two-Handed Sword" (WeaponProperties (1 `d` 10) 0 True) ]

armors = [ armor "Leather Armor" (ArmorProperties 2 Chest),
           armor "Chain Mail" (ArmorProperties 6 Chest),
           armor "Plate Mail" (ArmorProperties 8 Chest),
           armor "Full Plate" (ArmorProperties 10 Chest) ]

potions = [ potion "Blue" Healing,
            potion "Yellow" Life,
            potion "Black" Acid,
            potion "Red" Strength,
            potion "White" Invisibility,
            potion "Grey" Confusion,
            potion "Silver" Darkness ]

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
