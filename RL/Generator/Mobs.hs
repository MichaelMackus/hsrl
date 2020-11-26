module RL.Generator.Mobs (playerGenerator, mobGenerator, PlayerConfig(..), MobConfig(..)) where

import RL.Game
import RL.Generator
import RL.Generator.Cells (Cell, cmid)
import RL.Random
import RL.Util (lookupMax)

import Data.Maybe (catMaybes)

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

type Difficulty = Int

generateMob :: Difficulty -> Generator MobConfig DLevel (Maybe Mob)
generateMob diff = do
        lvl  <- getGData
        let ts = toTiles lvl
            dheight = length ts
            dwidth  = if length ts > 0 then length (ts !! 0) else 0
        p    <- randomPoint dwidth dheight

        -- choose a random difficulty - this way we have more variation of mobs
        conf  <- ask
        diff' <- max 1 <$> getRandomR (diff - fst (difficultyRange conf), diff + snd (difficultyRange conf))

        let t  = maybe Nothing (\t -> if isPassable t then Just t else Nothing) (findTileAt p lvl)
            ms = maybe [] (const . maybe [] id $ lookupMax diff' mobs) t

        if length ms > 0 then do
            m <- pick ms
            return $ do
                m <- m

                let maxDist = 10
                    pDist = distance (at m) (at (player lvl))
                    flags = if pDist >= maxDist then [Sleeping] else []

                return (m { at = p, flags = flags })
        else
            return Nothing
    where
        mobs = [ (1, [ mob {
                      mobName = "Kobold",
                      symbol = 'k',
                      hp = 5,
                      mhp = 5,
                      baseDmg = 1 `d` 4
                  },
                  mob {
                      mobName = "Goblin",
                      symbol = 'g',
                      hp = 4,
                      mhp = 4,
                      baseDmg = 1 `d` 3,
                      baseAC  = 8
                  },
                  mob {
                      mobName = "Grid Bug",
                      symbol = 'x',
                      hp = 3,
                      mhp = 3,
                      baseDmg = 1 `d` 4,
                      thac0   = 22
                  }
               ]), (2, [ mob {
                      mobName = "Orc",
                      symbol = 'o',
                      hp = 6,
                      mhp = 6,
                      baseDmg = 1 `d` 6,
                      thac0   = 18,
                      baseAC  = 8
                   }
               ]) ]
