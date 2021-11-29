module RL.Generator.Mobs (playerGenerator, mobGenerator, PlayerConfig(..), MobConfig(..)) where

import RL.Game hiding (updateFlags)
import RL.Generator
import RL.Generator.Cells (Cell, cmid)
import RL.Random

import Data.Maybe (catMaybes, isNothing)
import Data.Ratio
import qualified Data.List as L

data PlayerConfig = PlayerConfig {
    playerHp :: Int,
    playerFov :: Radius,
    playerItems :: [Item]
}

instance GenConfig PlayerConfig where
    generating conf = return True

-- TODO need item config for generating treasure
data MobConfig = MobConfig {
    maxMobs :: Int,
    minMobs :: Int,
    mobGenChance :: Rational,
    mobSleepingChance :: Rational,
    difficultyRange :: (Int, Int) -- min - fst, max + snd
}

instance GenConfig MobConfig where
    generating conf = (< maxMobs conf) <$> getCounter

playerGenerator :: Generator PlayerConfig [Cell] (Maybe Player)
playerGenerator = do
    (PlayerConfig hp fov inv) <- ask
    cs <- getGData
    if not (null cs) then
        -- TODO place player randomly around dungeon
        let p  = cmid (cs !! 0)
            pl = (mkPlayer hp p fov) { inventory = inv, identified = map itemType inv }
        in  markGDone >> return (Just pl)
    else
        return Nothing

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
        traverse updateFlags (moveMob <$> p <*> m)
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
                baseAC  = 6
            },
            mob {
                mobName = "Goblin",
                symbol = 'g',
                hp = 3,
                mhp = 3,
                baseDmg = 1 `d` 6,
                baseAC  = 6
            },
            mob {
                mobName = "Grid Bug",
                symbol = 'x',
                hp = 2,
                mhp = 2,
                baseDmg = 1 `d` 2,
                thac0   = 22,
                baseAC  = 9
            },
            mob {
                mobName = "Rat",
                symbol = 'r',
                hp = 2,
                mhp = 2,
                baseDmg = 1 `d` 2,
                thac0   = 22,
                baseAC  = 9
            },
            mob {
               -- TODO fast
               mobName = "Orc",
               symbol = 'o',
               hp = 4,
               mhp = 4,
               baseDmg = 1 `d` 6,
               baseAC  = 6
            },
            mob {
               mobName = "Skeleton",
               symbol = 'S',
               hp = 4,
               mhp = 4,
               baseDmg = 1 `d` 6,
               baseAC  = 7,
               flags = [Undead]
            },
            mob {
               mobName = "Zombie",
               symbol = 'Z',
               hp = 9,
               mhp = 9,
               baseDmg = 1 `d` 8,
               thac0   = 18,
               baseAC  = 8,
               flags = [Undead]
            },
            mob {
               mobName = "Ogre",
               symbol = 'O',
               hp = 19,
               mhp = 19,
               baseDmg = 1 `d` 10,
               thac0   = 15,
               baseAC  = 5
            },
            mob {
               mobName = "Black Dragon",
               symbol = 'D',
               hp = 31,
               mhp = 31,
               baseDmg = 2 `d` 10,
               thac0   = 13,
               baseAC  = 2
            }
          ]

mobRarity :: Difficulty -> Mob -> Rational
mobRarity d m
    | d <= 3 = case mobName m of
                    "Kobold"   -> (1 % 10)
                    "Grid Bug" -> (1 % 3)
                    "Rat"      -> (1 % 5)
                    otherwise  -> (0 % 10)
    | d <= 5 = case mobName m of
                    "Kobold"   -> (1 % 5)
                    "Goblin"   -> (1 % 10)
                    otherwise  -> mobRarity 1 m
    | d < 10 = case mobName m of
                    "Goblin"   -> (1 % 5)
                    "Orc"      -> (1 % 10)
                    "Skeleton" -> (1 % 15)
                    otherwise  -> mobRarity 5 m
    | d >= 10 = case mobName m of
                    "Orc"          -> (1 % 5)
                    "Skeleton"     -> (1 % 7)
                    "Zombie"       -> (1 % 10)
                    "Ogre"         -> (1 % 15)
                    "Black Dragon" -> (1 % 20)
                    otherwise      -> mobRarity 9 m
