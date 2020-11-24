{-# LANGUAGE TupleSections #-}

module RL.Client.AI ( AI(..), module RL.Client ) where

-- basic AI

import RL.Game
import RL.Command
import RL.Client
import RL.Pathfinder
import RL.State
import RL.Random

import Control.Monad (forM, forM_, when)
import Data.Maybe (isJust, fromJust, fromMaybe)

-- AI
data AI = AI

instance Client AI where
    tick ai = whenEnv isTicking $ do
            player   <- getPlayer
            ms       <- aliveMobs <$> getMobs
            forM_ ms automate
            -- cleanup dead mobs
            setMobs =<< aliveMobs <$> getMobs

automate :: Mob -> Game ()
automate m = do
        p    <- getPlayer
        lvl  <- getLevel
        let seen    = seenPath lvl m p
            heard   = heardPath lvl m p
            curPath = mobPath m
        -- if mob sees player, go directly there
        if isJust seen then
            moveCloser m p (fromJust seen)
        -- if mob has seen/heard player, follow that (previous) path
        else if not (null curPath) then
            moveCloser m p curPath
        -- end of path and can hear player, follow that path
        else if isJust heard then
            moveCloser m p (fromJust heard)
        else do
            -- wander randomly
            let neighbors = dneighbors lvl (at m)
            p   <- pick neighbors
            maybe (return ()) (dispatch . MoveMob m) p
    where
        moveCloser m p path = do
            lvl <- getLevel
            when (Sleeping `elem` flags m) $ dispatch (Wake m)

            let next  = path !! 1
                t     = findTileAt next lvl
                isValidPath = length path > 1 && isJust t && isPassable (fromJust t)

            m' <- fromJust <$> getMob (mobId m)
            when isValidPath $ do
                if next == at p && not (isDead p) then do
                    dispatch (AttackPlayer m')
                else do
                    dispatch (MoveMob m' next)
                    -- update path
                    let p' = if length (tail path) > 1 then tail path else []
                    modifyMob (mobId m') (\m -> m { mobPath = p' })

seenPath :: DLevel -> Mob -> Mob -> Maybe [Point]
seenPath lvl m1 m2 =
    if canSee lvl m1 (at m2) then
        findPath (dfinder lvl) distance (at m2) (at m1)
    else
        Nothing

heardPath :: DLevel -> Mob -> Mob -> Maybe [Point]
heardPath lvl m1 m2 =
    if distance (at m1) (at m2) <= hearing m1 then
        findPath (dfinder lvl) distance (at m2) (at m1)
    else
        Nothing

-- TODO use configurable nose (i.e. dogs/wolfs smell better than other mobs)
-- TODO perhaps can look into something simple like a smell trail (nethack's way)
-- canSmell :: Mob -> Mob -> Game Bool
-- canSmell m1 m2 = return (distance (at m1) (at m2) <= range)
    -- where range = 10
