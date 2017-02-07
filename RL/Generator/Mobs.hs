module RL.Generator.Mobs (playerGenerator, mobGenerator) where

import RL.Game
import RL.Generator
import RL.Generator.Cells (Cell, cmid)
import RL.Random
import RL.Util (lookupMax)

import Control.Monad (replicateM, when)
import Control.Monad.Reader (ask)
import Data.Maybe (catMaybes)

playerGenerator :: HP -> Dice -> Radius -> Generator [Cell] (Maybe Player)
playerGenerator hp dmg fov = do
    cs <- getGData
    if not (null cs) then
        -- TODO place player randomly around dungeon
        let p = cmid (cs !! 0)
        in  markGDone >> return (Just (mkPlayer hp dmg p fov))
    else
        return Nothing

mobGenerator :: Int -> Generator DLevel [Mob]
mobGenerator maxMobs = do
    lvl   <- getGData
    let diff = depth lvl
    mobs' <- (++ (mobs lvl)) . catMaybes <$> replicateM (maxMobs - length (mobs lvl)) (generateMob diff)
    when (length mobs' == maxMobs) markGDone
    when (length mobs' /= length (mobs lvl)) (setGData (lvl { mobs = mobs' }))
    return mobs'

type Difficulty = Int

generateMob :: Difficulty -> Generator DLevel (Maybe Mob)
generateMob diff = do
        conf <- ask
        lvl  <- getGData
        p    <- randomPoint (dwidth conf) (dheight conf)

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
