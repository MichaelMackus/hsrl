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
        env   <- ask
        lvl   <- asks level
        heard <- canHear (events env) m (player lvl)
        let seen  = canSeeMob lvl m (player lvl)
            wakeE = if (heard && isSleeping m) then [Waken m] else []
            path  = findOptimalPath lvl distance (at (player lvl)) (at m)
        moveE <- if (seen || heard) && isJust path then
                    moveCloser m (player lvl) (fromJust path)
                 else if isJust (curMobPath lvl m) then
                    moveCloser m (player lvl) (fromJust (curMobPath lvl m))
                 else if not (isSleeping m) then
                    wander m
                 else return []
        if seen then
            return $ wakeE ++ [MobSeen m (player lvl)] ++ moveE
        else if heard then
            return $ wakeE ++ [MobHeard m (player lvl)] ++ moveE
        else
            return $ wakeE ++ moveE
    where
        --- wander randomly
        wander :: Mob -> GameEnv [Event]
        wander m = do
                lvl <- asks level
                p   <- pick (aiNeighbors lvl (at m))
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
                    attack m' (wielding (equipment m')) p
                else if isNothing (findMobAt next lvl) then
                    return [Moved m' next]
                else return []
            else return []

-- there's only a chance the mob can hear the player, depending on if
-- player is sneaky or if mob is sleeping
canHear :: MonadRandom m => [Event] -> Mob -> Mob -> m Bool
canHear es m1 m2 =
    -- wake up 5 in 6 times if combat is heard
    let chance = if hearsCombat es m1 then (5 % 6)
                 else (if isSneaky m2 then (1 % 6) else (2 % 6)) * (if isSleeping m1 then (1 % 10) else 1)
    in  if distance (at m1) (at m2) <= hearing m1 then randomChance chance
        else return False

hearsCombat :: [Event] -> Mob -> Bool
hearsCombat es m =
    let f (Damaged atk tgt _) = g atk tgt
        f (Missed  atk tgt)   = g atk tgt
        f otherwise           = False
        -- check if mob can hear attacker or target... uses sight distance so further away mobs stay sleeping
        g atk tgt             = distance (at atk) (at m) <= fov m || distance (at tgt) (at m) <= hearing m
    in  not . null $ L.filter f es

curMobPath :: DLevel -> Mob -> Maybe [Point]
curMobPath lvl m = (\p -> findOptimalPath lvl distance p (at m)) =<< destination m

-- find AI path, first trying to find optimal path around mobs
-- fallback is naive dfinder to allow mobs to bunch up
findOptimalPath lvl h end s = let optimal = findPath (aiFinder lvl) h end s
                              in  if isNothing optimal then findPath (dfinder lvl) h end s else optimal

aiFinder :: DLevel -> Point -> Set Point
aiFinder d p = Set.fromList (aiNeighbors d p)

aiNeighbors d p = L.filter f (dneighbors d p)
    where f p = let m' = findMobAt p d
                in  isNothing m' || isPlayer (fromJust m')

