module RL.Generator.Mobs (playerGenerator, mobGenerator, PlayerConfig(..), MobConfig(..)) where

import RL.Game
import RL.Generator
import RL.Generator.Cells (Cell, cmid)
import RL.Random

import Data.Maybe (catMaybes)
import Data.Ratio

data PlayerConfig = PlayerConfig {
    playerHp :: Int,
    playerFov :: Radius
}

instance GenConfig PlayerConfig where
    generating conf = return True

data MobConfig = MobConfig {
    maxMobs :: Int,
    maxMTries :: Int,
    difficultyRange :: (Int, Int) -- min - fst, max + snd
}

instance GenConfig MobConfig where
    generating conf = (< maxMTries conf) <$> getCounter

playerGenerator :: Generator PlayerConfig [Cell] (Maybe Player)
playerGenerator = do
    (PlayerConfig hp fov) <- ask
    cs <- getGData
    if not (null cs) then
        -- TODO place player randomly around dungeon
        let p = cmid (cs !! 0)
        in  markGDone >> return (Just (mkPlayer hp p fov))
    else
        return Nothing

mobGenerator :: Generator MobConfig DLevel [Mob]
mobGenerator = do
    conf <- ask
    lvl <- getGData
    let diff = depth lvl
    mobs' <- withMobIds . (++ (mobs lvl)) . catMaybes <$> replicateM (maxMobs conf - length (mobs lvl)) (generateMob diff)
    when (length mobs' == maxMobs conf) markGDone
    setGData (lvl { mobs = mobs' })
    return mobs'

generateMob :: Difficulty -> Generator MobConfig DLevel (Maybe Mob)
generateMob diff = do
    lvl  <- getGData
    let ts = toTiles lvl
        dheight = length ts
        dwidth  = if length ts > 0 then length (ts !! 0) else 0
    p    <- randomPoint dwidth dheight

    -- generate mob based on random difficulty - this way we have more variation of mobs
    conf  <- ask
    diff' <- max 1 <$> getRandomR (diff - fst (difficultyRange conf), diff + snd (difficultyRange conf))
    m     <- pickRarity (mobRarity diff) dngMobs
    if maybe False isPassable (findTileAt p lvl) then
        return (moveMob p . updateFlags lvl <$> m)
    else
        return Nothing

updateFlags :: DLevel -> Mob -> Mob
updateFlags lvl m = let maxDist = 10
                        pDist = distance (at m) (at (player lvl))
                        flags = if pDist >= maxDist then [Sleeping] else []
                    in  m { flags = flags }

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
               baseAC  = 7
            },
            mob {
               mobName = "Zombie",
               symbol = 'Z',
               hp = 9,
               mhp = 9,
               baseDmg = 1 `d` 8,
               thac0   = 18,
               baseAC  = 8
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
    | d == 1 = case mobName m of
                    "Kobold"   -> (1 % 5)
                    "Goblin"   -> (1 % 10)
                    "Grid Bug" -> (1 % 5)
                    otherwise  -> (0 % 10)
    | d == 2 = case mobName m of
                    "Orc"      -> (1 % 5)
                    otherwise  -> mobRarity 1 m
    | d < 5  = case mobName m of
                    "Skeleton" -> (1 % 7)
                    "Zombie"   -> (1 % 10)
                    otherwise  -> mobRarity 2 m
    | d >= 5 = case mobName m of
                    "Black Dragon" -> (1 % 20)
                    "Ogre"         -> (1 % 15)
                    otherwise      -> mobRarity 3 m
