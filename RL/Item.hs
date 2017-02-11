module RL.Item where

import RL.Types

data Item = Item ItemName ItemType

type ItemName = String
data ItemType = Weapon WeaponProperties | Armor ArmorProperties | Potion | Tool

instance Show Item where
    show (Item n _) = n

data WeaponProperties = WeaponProperties {
    dmgd :: Dice,
    bonus :: Int -- positive OR negative bonus to attack & damage rolls
}

weaponProperties :: Item -> Maybe WeaponProperties
weaponProperties (Item _ (Weapon prop)) = Just prop
weaponProperties otherwise              = Nothing

data ArmorProperties = ArmorProperties {
    defense :: Int, -- this is opposite of traditional AD&D - this number is subtracted by 10 for the *true* AC of a mob
    slot    :: ArmorSlot
}
data ArmorSlot = Head | Chest | Legs | Feet | Hands

armorProperties :: Item -> Maybe ArmorProperties
armorProperties (Item _ (Armor prop)) = Just prop
armorProperties otherwise             = Nothing

itemType :: Item -> Item -> Bool
itemType (Item _ (Weapon _)) (Item _ (Weapon _)) = True
itemType (Item _ (Armor _)) (Item _ (Armor _)) = True
itemType (Item _ Potion) (Item _ Potion) = True
itemType (Item _ Tool) (Item _ Tool) = True
itemType _ _ = False
