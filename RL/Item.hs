module RL.Item where

import RL.Types

import Data.Ratio

data Item = Item ItemName ItemType ItemRarity

type ItemName = String
type ItemRarity = Rational
data ItemType = Weapon WeaponProperties | Armor ArmorProperties | Potion | Tool

instance Show Item where
    show (Item n _ _) = n
instance Show ItemType where
    show (Weapon _) = "weapon"
    show (Armor _) = "armor"
    show (Potion) = "potion"
    show (Tool) = "tool"
    show otherwise = "undefined item type"

data WeaponProperties = WeaponProperties {
    dmgd :: Dice,
    bonus :: Int, -- positive OR negative bonus to attack & damage rolls
    twoHanded :: Bool
}

weaponProperties :: Item -> Maybe WeaponProperties
weaponProperties (Item _ (Weapon prop) _) = Just prop
weaponProperties otherwise              = Nothing

data ArmorProperties = ArmorProperties {
    defense :: Int, -- this is opposite of traditional AD&D - this number is subtracted by 10 for the *true* AC of a mob
    slot    :: ArmorSlot
}
data ArmorSlot = Head | Chest | Legs | Feet | Hands

armorProperties :: Item -> Maybe ArmorProperties
armorProperties (Item _ (Armor prop) _) = Just prop
armorProperties otherwise             = Nothing

itemType :: Item -> Item -> Bool
itemType (Item _ (Weapon _) _) (Item _ (Weapon _) _) = True
itemType (Item _ (Armor _) _) (Item _ (Armor _) _) = True
itemType (Item _ Potion _) (Item _ Potion _) = True
itemType (Item _ Tool _) (Item _ Tool _) = True
itemType _ _ = False

isWeapon :: Item -> Bool
isWeapon (Item _ (Weapon _) _) = True
isWeapon otherwise           = False

isArmor :: Item -> Bool
isArmor (Item _ (Armor _) _) = True
isArmor otherwise          = False

itemTypes = [ Weapon undefined, Armor undefined, Potion, Tool ]

itemSymbol :: Item -> Char
itemSymbol (Item _ (Weapon _) _) = ')'
itemSymbol (Item _ (Armor _) _) = ']'
itemSymbol (Item _ (Potion) _) = '!'
itemSymbol (Item _ (Tool) _) = '/'
itemSymbol otherwise = '~'

-- rarity for item types
typeRarity :: ItemType -> Rational
typeRarity (Weapon _) = 1 % 5
typeRarity (Armor _) = 1 % 7
typeRarity (Potion) = 1 % 10
typeRarity (Tool) = 1 % 10

itemRarity :: Item -> Rational
itemRarity (Item _ _ r) = r

weapons = [mace, dagger, quarterstaff, sword, twoHandedSword]
armors = [leather, chainMail, plateMail, fullPlate]
potions = []
tools = []

-- some simple items
mace = Item "Mace" (Weapon $ WeaponProperties (1 `d` 6) 0 False) (1 % 10)
dagger = Item "Dagger" (Weapon $ WeaponProperties (1 `d` 6) 0 False) (4 % 10)
quarterstaff = Item "Quarterstaff" (Weapon $ WeaponProperties (1 `d` 8) 0 True) (2 % 10)
sword = Item "Sword" (Weapon $ WeaponProperties (1 `d` 8) 0 False) (1 % 10)
twoHandedSword = Item "Two-Handed Sword" (Weapon $ WeaponProperties (1 `d` 10) 0 True) (1 % 20)
leather = Item "Leather" (Armor $ ArmorProperties 2 Chest) (4 % 10)
chainMail = Item "Chain Mail" (Armor $ ArmorProperties 6 Chest) (2 % 10)
plateMail = Item "Plate Mail" (Armor $ ArmorProperties 8 Chest) (1 % 10)
fullPlate = Item "Plate Mail" (Armor $ ArmorProperties 10 Chest) (1 % 50)
