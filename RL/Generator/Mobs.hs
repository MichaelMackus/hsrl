{-# LANGUAGE FlexibleInstances #-}

module RL.Generator.Mobs (playerGenerator, mobGenerator, PlayerConfig(..), MobConfig(..)) where

-- TODO make deeper dungeon much more difficult

import RL.Game hiding (updateFlags)
import RL.Generator
import RL.Generator.Cells (Cell, cmid)
import RL.Random

import Control.Monad.State
import Data.Maybe (catMaybes, isNothing)
import Data.Ratio
import qualified Data.List as L

data PlayerConfig = PlayerConfig {
    playerHp :: Int,
    playerLevel :: Int,
    playerFov :: Radius,
    playerItems :: [Item]
}

instance GenConfig PlayerConfig s where
    generating conf = return True

-- TODO need item config for generating treasure
data MobConfig = MobConfig {
    maxMobs :: Int,
    minMobs :: Int,
    mobGenChance :: Rational,
    mobSleepingChance :: Rational,
    difficultyRange :: (Int, Int) -- min - fst, max + snd
}

instance GenConfig MobConfig DLevel where
    generating conf = do
        ms <- gets mobs
        return (length ms < maxMobs conf)

playerGenerator :: Generator PlayerConfig [Cell] (Maybe Player)
playerGenerator = do
    (PlayerConfig hp plvl fov inv) <- ask
    cs <- getGData
    if not (null cs) then do
        -- TODO place player randomly around dungeon
        let p  = cmid (cs !! 0)
            pl = (mkPlayer p fov) { inventory = inv, identified = map itemType inv }
        markGDone
        Just <$> updatePlayerLevel pl
    else
        return Nothing

-- level up player to configured exp level
updatePlayerLevel :: Player -> Generator PlayerConfig [Cell] Player
updatePlayerLevel p = do
    (PlayerConfig hp plvl fov inv) <- ask
    if plvl > 1 then
        let neededLevels = max 0 (plvl - 1)
            hp' = floor (fromIntegral hp / 2.0  *  fromIntegral neededLevels) + hp
        in  return $ p { mlvl = plvl, hp = hp', mhp = hp', xp = expForLevel plvl, fov = fov, inventory = inv }
    else return $ p { hp = hp, mhp = hp, fov = fov, inventory = inv }

-- configure default player
mkPlayer :: Point -> Radius -> Player
mkPlayer at fov = mob {
    mobId  = 0,
    mobName = "Player", -- TODO configurable name
    symbol = '@',
    at = at,
    fov = fov,
    equipment = MobEquipment (findItemByName "Mace" weapons) (findItemByName "Leather Armor" armors) (findItemByName "Bow" weapons) Nothing,
    savingThrow = 14
}

mobGenerator :: Generator MobConfig DLevel [Mob]
mobGenerator = do
    conf <- ask
    lvl <- getGData
    let diff = depth lvl
    mobs' <- maybe (mobs lvl) (insertMob (mobs lvl))  <$> generateMob diff
    setGData (lvl { mobs = mobs' })
    return mobs'

generateMob :: Difficulty -> Generator MobConfig DLevel (Maybe Mob)
generateMob diff = do
    -- generate mob based on random difficulty - this way we have more variation of mobs
    -- TODO don't generate in player los
    lvl   <- getGData
    conf  <- ask
    diff' <- max 1 <$> getRandomR (diff - fst (difficultyRange conf), diff + snd (difficultyRange conf))
    r     <- randomChance (mobGenChance conf)
    if length (mobs lvl) < minMobs conf || r then do
        let tileF p t = not (isStair t) && isNothing (L.lookup p (features lvl)) && isPassable t && isNothing (findMobAt p lvl) && not (canSee lvl (player lvl) p)
        p <- randomTile tileF lvl
        -- TODO give mob (identified) items
        m <- pickRarity (mobRarity diff) dngMobs
        traverse updateFlags ((\p m -> m { at = p }) <$> p <*> m)
    else
        return Nothing

-- random chance to be sleeping or wandering
-- TODO don't ever put undead to sleep
updateFlags :: Mob -> Generator MobConfig DLevel Mob
updateFlags m = do
    chance <- asks mobSleepingChance
    lvl    <- getGData
    r      <- randomChance chance
    if not (isUndead m) && r then return $ m { flags = Sleeping:flags m }
    else return m

dngMobs = [ mob {
                mobName = "Kobold",
                symbol = 'k',
                hp = 2,
                mhp = 2,
                baseDmg = 1 `d` 4,
                baseAC  = 6,
                speed = 20
            },
            mob {
                mobName = "Goblin",
                symbol = 'g',
                hp = 3,
                mhp = 3,
                baseDmg = 1 `d` 6,
                baseAC  = 6,
                speed = 20
            },
            mob {
                mobName = "Grid Bug",
                symbol = 'x',
                hp = 2,
                mhp = 2,
                baseDmg = 1 `d` 2,
                thac0   = 22,
                baseAC  = 9,
                speed = 20
            },
            mob {
                mobName = "Rat",
                symbol = 'r',
                hp = 2,
                mhp = 2,
                baseDmg = 1 `d` 2,
                thac0   = 22,
                baseAC  = 9,
                speed = 20
            },
            mob {
               mobName = "Orc",
               symbol = 'o',
               hp = 4,
               mhp = 4,
               baseDmg = 1 `d` 6,
               baseAC  = 6,
               speed = 40
            },
            mob {
               mobName = "Skeleton",
               symbol = 'S',
               hp = 4,
               mhp = 4,
               baseDmg = 1 `d` 6,
               baseAC  = 7,
               flags = [Undead],
               speed = 20
            },
            mob {
               mobName = "Zombie",
               symbol = 'Z',
               hp = 9,
               mhp = 9,
               baseDmg = 1 `d` 8,
               thac0   = 18,
               baseAC  = 8,
               flags = [Undead],
               speed = 20
            },
            -- mob {
            --    mobName = "Troglodyte",
            --    symbol = 't',
            --    hp = 9,
            --    mhp = 9,
            --    baseDmg = 2 `d` 4 `plus` 1, -- TODO multiple attacks
            --    thac0   = 18,
            --    baseAC  = 5,
            --    speed = 40
            -- },
            mob {
               mobName = "Ogre",
               symbol = 'O',
               hp = 19,
               mhp = 19,
               baseDmg = 1 `d` 10,
               thac0   = 15,
               baseAC  = 5,
               speed = 30
            },
            mob {
               mobName = "Bugbear",
               symbol = 'B',
               hp = 14,
               mhp = 14,
               baseDmg = 2 `d` 4,
               thac0   = 16,
               baseAC  = 5,
               speed = 30
            },
            mob {
               mobName = "Black Dragon",
               symbol = 'D',
               hp = 31,
               mhp = 31,
               baseDmg = 2 `d` 10,
               thac0   = 13,
               baseAC  = 2,
               speed = 80
            }
          ]

mobRarity :: Difficulty -> Mob -> Rational
mobRarity d m
    -- | d <= 3 = case mobName m of
    --                 "Kobold"   -> (1 % 7)
    --                 "Grid Bug" -> (1 % 3)
    --                 "Rat"      -> (1 % 5)
    --                 otherwise  -> (0 % 10)
    | d == 1 = case mobName m of
                    "Grid Bug" -> (1 % 3)
                    "Kobold"   -> (1 % 5)
                    "Goblin"   -> (1 % 7)
                    "Rat"      -> (1 % 5)
                    otherwise  -> (0 % 10)
    | d == 2 = case mobName m of
                    "Kobold"   -> (1 % 5)
                    "Rat"      -> (1 % 5)
                    "Goblin"   -> (1 % 7)
                    "Skeleton" -> (1 % 7)
                    "Orc"      -> (1 % 10)
                    otherwise  -> (0 % 10)
    | d <  5 = case mobName m of
                    "Kobold"   -> (1 % 5)
                    "Goblin"   -> (1 % 5)
                    "Skeleton" -> (1 % 7)
                    "Orc"      -> (1 % 10)
                    "Zombie"   -> (1 % 15)
                    "Bugbear"  -> (1 % 30)
                    "Ogre"     -> (1 % 30)
                    otherwise  -> (0 % 10)
    | d < 10 = case mobName m of
                    "Kobold"   -> (1 % 5)
                    "Goblin"   -> (1 % 5)
                    "Orc"      -> (1 % 7)
                    "Skeleton" -> (1 % 5)
                    "Zombie"   -> (1 % 10)
                    "Bugbear"  -> (1 % 20)
                    "Ogre"     -> (1 % 20)
                    otherwise  -> (0 % 10)
    | d < 15 = case mobName m of
                    "Kobold"   -> (1 % 5)
                    "Goblin"   -> (1 % 5)
                    "Orc"      -> (1 % 10)
                    "Skeleton" -> (1 % 10)
                    "Zombie"   -> (1 % 10)
                    "Bugbear"  -> (1 % 15)
                    "Ogre"     -> (1 % 15)
                    otherwise  -> (0 % 10)
    | d >= 15 = case mobName m of
                    "Orc"          -> (1 % 5)
                    "Skeleton"     -> (1 % 5)
                    "Zombie"       -> (1 % 7)
                    "Ogre"         -> (1 % 10)
                    "Bugbear"      -> (1 % 10)
                    "Black Dragon" -> (1 % 20)
                    otherwise      -> (0 % 10)
