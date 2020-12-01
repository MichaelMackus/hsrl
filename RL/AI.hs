module RL.AI where

import RL.Action
import RL.Event
import RL.Game
import RL.Pathfinder
import RL.Random

import Data.Maybe (isJust, isNothing, fromJust)
import Data.Set (Set)
import qualified Data.List as L
import qualified Data.Set as Set

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
        else if not (Sleeping `elem` flags m) then
            return mobE
        else
            return []
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
                let neighbors = aiNeighbors lvl (at m)
                p <- pick neighbors
                case p of
                    Just p  -> return [Moved m p]
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
    if isVisible m2 && canSee lvl m1 (at m2) then
        findAIPath lvl distance (at m2) (at m1)
    else if not (isVisible m2) && distance (at m1) (at m2) < 2 then
        findAIPath lvl distance (at m2) (at m1)
    else
        Nothing

heardPath :: DLevel -> Mob -> Mob -> Maybe [Point]
heardPath lvl m1 m2 =
    -- TODO should be able to still somewhat track if invisible
    if isVisible m2 && distance (at m1) (at m2) <= hearing m1 then
        findAIPath lvl distance (at m2) (at m1)
    else
        Nothing


mobPath :: DLevel -> Mob -> Maybe [Point]
mobPath lvl m = (\p -> findAIPath lvl distance p (at m)) =<< destination m

-- find AI path, first trying to find optimal path around mobs
-- fallback is naive dfinder to allow mobs to bunch up
findAIPath lvl h end s = let optimal = findPath (aiFinder lvl) h end s
                         in  if isNothing optimal then findPath (dfinder lvl) h end s else optimal

aiFinder :: DLevel -> Point -> Set Point
aiFinder d p = Set.fromList (aiNeighbors d p)

aiNeighbors d p = L.filter f (dneighbors d p)
    where f p = let m' = findMobAt p d
                in  isNothing m' || isPlayer (fromJust m')

