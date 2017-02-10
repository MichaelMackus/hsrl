module RL.Item where

import RL.Types

data Item = Item ItemName ItemType

type ItemName = String
data ItemType = Weapon WeaponProperties | Armor ArmorProperties | Potion | Tool

data WeaponProperties = WeaponProperties {
    dmgd :: Dice
}

data ArmorProperties = ArmorProperties {
    defense :: AC,
    slot    :: ArmorSlot
}
data ArmorSlot = Head | Chest | Legs | Feet | Hands
