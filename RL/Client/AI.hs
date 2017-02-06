{-# LANGUAGE TupleSections #-}

module RL.Client.AI ( AI(..), module RL.Client ) where

-- basic AI

import RL.Game
import RL.Client
import RL.Pathfinder
import RL.State

import Control.Monad (forM, forM_, when)
import Data.Maybe (isJust, fromJust)

-- AI
data AI = AI

instance Client AI where
    tick ai = do
            player   <- getPlayer
            ms       <- aliveMobs <$> getMobs
            smelling <- forM ms (`canSmell` player)
            seeing   <- forM ms (`canSee` player)

            let ms' = zipWith3 (,,) ms smelling seeing

            setMobs =<< forM ms' moveCloser

            -- send message for dead mobs
            dead <- deadMobs <$> getMobs
            forM_ dead (\t -> when (isDead t) (sendMessage $ "The " ++ mobName t ++ " died"))

            -- cleanup dead mobs
            setMobs =<< aliveMobs <$> getMobs
        where
            moveCloser :: (Mob, Bool, Bool) -> Game Mob
            moveCloser (m, smelling, seeing) = do
                p   <- getPlayer
                lvl <- getLevel

                let f  g  = findPath (g lvl) distance (at p) (at m)
                    -- first attempt to find walkable path
                    path  = maybe (f dfinder') Just (f dfinder)
                    next  = fromJust path !! 1
                    t     = findTileAt next lvl
                    isValidPath = isJust path && length (fromJust path) > 1 &&
                                  isJust t && isPassable (fromJust t)

                if (smelling || seeing) && isValidPath then
                    if next == at p && not (isDead p) then
                        attackPlayer m
                    else
                        return (moveMobTo next m)
                else
                    return m

attackPlayer attacker = do
    p         <- getPlayer
    (dmg, p') <- attack p attacker

    sendMessage ("You were hit! " ++ (show dmg) ++ " damage")
    when (isDead p') (sendMessage ("You died!"))

    return attacker

canSee :: Mob -> Mob -> Game Bool
canSee m1 m2 = do
    lvl <- getLevel
    if distance (at m1) (at m2) <= fov m1 then
        return (isJust $ findPath (dfinder lvl) distance (at m2) (at m1))
    else
        return False

-- TODO use configurable nose (i.e. dogs/wolfs smell better than other mobs)
canSmell :: Mob -> Mob -> Game Bool
canSmell m1 m2 = return (distance (at m1) (at m2) <= range)
    where range = 10
