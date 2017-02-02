module RL.Generator.Mobs (playerGenerator, mobGenerator) where

import RL.Game
import RL.Generator
import RL.Generator.Cells (Cell, cmid)
import RL.Random

import Control.Monad (replicateM, when)
import Control.Monad.Reader (ask)
import Data.Maybe (catMaybes)

-- TODO initial state

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
    mobs' <- (++ (mobs lvl)) . catMaybes <$> replicateM (maxMobs - length (mobs lvl)) mob
    when (length mobs' == maxMobs) markGDone
    when (length mobs' /= length (mobs lvl)) (setGData (lvl { mobs = mobs' }))
    return mobs'

mob :: Generator DLevel (Maybe Mob)
mob = do
        conf <- ask
        lvl  <- getGData
        p    <- randomPoint (dwidth conf) (dheight conf)

        let t = maybe Nothing (\t -> if isPassable t then Just t else Nothing) (findTileAt p lvl)
            m = maybe Nothing (const (Just (kobold 0 p))) t

        return m
    where
        kobold id p = Mob {
            mobId = id,
            symbol = 'k',
            hp = 5,
            dmgd = 1 `d` 4,
            at = p,
            fov = 5
        }
