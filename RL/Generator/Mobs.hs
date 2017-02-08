module RL.Generator.Mobs (playerGenerator, mobGenerator, PlayerConfig(..), MobConfig(..)) where

import RL.Game
import RL.Generator
import RL.Generator.Cells (Cell, cmid)
import RL.Random
import RL.Util (lookupMax)

import Control.Monad (replicateM, when)
import Control.Monad.Reader (ask)
import Data.Maybe (catMaybes)

data PlayerConfig = PlayerConfig {
    playerHp :: Int,
    playerDmg :: Dice,
    playerFov :: Radius
}

instance GenConfig PlayerConfig where
    generating conf = return True

data MobConfig = MobConfig {
    maxMobs :: Int,
    maxTries :: Int
}

instance GenConfig MobConfig where
    generating conf = (< maxTries conf) <$> getCounter

playerGenerator :: Generator PlayerConfig [Cell] (Maybe Player)
playerGenerator = do
    (PlayerConfig hp dmg fov) <- ask
    cs <- getGData
    if not (null cs) then
        -- TODO place player randomly around dungeon
        let p = cmid (cs !! 0)
        in  markGDone >> return (Just (mkPlayer hp dmg p fov))
    else
        return Nothing

mobGenerator :: Generator MobConfig DLevel [Mob]
mobGenerator = do
    conf <- ask
    lvl <- getGData
    let diff = depth lvl
    mobs' <- (++ (mobs lvl)) . catMaybes <$> replicateM (maxMobs conf - length (mobs lvl)) (generateMob diff)
    when (length mobs' == maxMobs conf) markGDone
    when (length mobs' /= length (mobs lvl)) (setGData (lvl { mobs = mobs' }))
    return mobs'

type Difficulty = Int

generateMob :: Difficulty -> Generator MobConfig DLevel (Maybe Mob)
generateMob diff = do
        lvl  <- getGData
        let ts = toTiles lvl
            dheight = length ts
            dwidth  = if length ts > 0 then length (ts !! 0) else 0
        p    <- randomPoint dwidth dheight

        let t  = maybe Nothing (\t -> if isPassable t then Just t else Nothing) (findTileAt p lvl)
            -- TODO randomly choose smaller difficulties
            ms = maybe [] (const . maybe [] id $ lookupMax diff mobs) t

        if length ms > 0 then do
            m <- pick ms
            return $ fmap (\m -> m { at = p }) m
        else
            return Nothing
    where
        mobs = [ (1, [ mob {
                      mobName = "Kobold",
                      symbol = 'k',
                      hp = 5,
                      mhp = 5,
                      dmgd = 1 `d` 4
                  },
                  mob {
                      mobName = "Goblin",
                      symbol = 'g',
                      hp = 4,
                      mhp = 4,
                      dmgd = 1 `d` 3
                  },
                  mob {
                      mobName = "Grid Bug",
                      symbol = 'x',
                      hp = 3,
                      mhp = 3,
                      dmgd = 1 `d` 2
                  }
               ]), (2, [ mob {
                      mobName = "Orc",
                      symbol = 'o',
                      hp = 6,
                      mhp = 6,
                      dmgd = 1 `d` 6
                   }
               ]) ]
