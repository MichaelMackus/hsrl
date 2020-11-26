module RL.AI where

import RL.Action
import RL.Event
import RL.Game
import RL.Pathfinder
import RL.Random

import Data.Maybe (isJust, isNothing, fromJust)

-- TODO mobs seem to get confused when running into other mobs
automate :: Mob -> GameEnv [Event]
automate m = do
        env <- ask
        let lvl     = level env
            p       = player lvl
            seen    = seenPath lvl m p
            heard   = heardPath lvl m p
            wakeE   = if (Sleeping `elem` flags m) then [Waken m] else []
        mobE <- mobTurn m p seen heard
        if isJust seen then
            return $ wakeE ++ [MobSeen m p] ++ mobE
        else if isJust heard then
            return $ wakeE ++ [MobHeard m p] ++ mobE
        else
            return $ wakeE ++ mobE
    where
        mobTurn m p seen heard = do
            lvl <- asks level
            let curPath = mobPath lvl m
            -- if mob sees player, go directly there
            if isJust seen then
                moveCloser m p (fromJust seen)
            -- if mob has seen/heard player, follow that (previous) path
            else if isJust curPath then
                moveCloser m p (fromJust curPath)
            -- end of path and can hear player, follow that path
            else if isJust heard then
                moveCloser m p (fromJust heard)
            else do
                -- wander randomly
                let neighbors = dneighbors lvl (at m)
                p <- pick neighbors
                case p of
                    Just p  -> if isNothing (findMobAt p lvl) then return [Moved m p]
                               else return []
                    Nothing -> return []
        moveCloser :: Mob -> Mob -> [Point] -> GameEnv [Event]
        moveCloser m p path = do
            lvl <- asks level
            let next        = path !! 1
                t           = findTileAt next lvl
                isValidPath = length path > 1 && isJust t && isPassable (fromJust t)
                m'          = fromJust (findMob (mobId m) (mobs lvl))
            if isValidPath then
                if next == at p && not (isDead p) then
                    attack m' p
                else if isNothing (findMobAt next lvl) then
                    return [Moved m' next]
                else return []
            else return []

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


mobPath :: DLevel -> Mob -> Maybe [Point]
mobPath lvl m = let seenDist  = distance (at m) <$> lastSeen m
                    heardDist = distance (at m) <$> lastHeard m
                    curP      = if isNothing (lastSeen m) then lastHeard m
                                else if isNothing (lastHeard m) then lastSeen m
                                else if seenDist > heardDist then lastHeard m
                                else lastSeen m
                in  (\p -> findPath (dfinder lvl) distance p (at m)) =<< curP