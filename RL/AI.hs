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
        let lvl     = level env
            p       = player lvl
            seenP   = seenPath lvl m p
            heardP  = heardPath lvl m p
            wakeE   = if (Sleeping `elem` flags m) then [Waken m] else []
        heardApproach <- canHear env m p True
        heardNearby   <- canHear env m p False
        mobE          <- mobTurn m p seenP heardP
        if heardApproach && isJust seenP then
            return $ wakeE ++ [MobSeen m p] ++ mobE
        else if heardNearby && isJust heardP then
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
            -- TODO FIXME
            -- else if isJust heard then
            --     moveCloser m p (fromJust heard)
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

-- there's only a chance the mob can hear the player
-- TODO more chance if awake?
canHear :: MonadRandom m => Env -> Mob -> Mob -> Bool -> m Bool
canHear env m1 m2 isCombat =
    -- wake up 5 in 6 times if combat is heard
    let es     = getEventsThisTurn (events env)
        chance = if hearsCombat es m1 then (5 % 6)
                 -- 1 or 2 in 6 chance every round *or* dungeon turn if not in combat
                 else (if isSneaky m2 then (1 % 6) else (2 % 6)) *  (if not isCombat then (1 % 10) else 1)
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

heardPath :: DLevel -> Mob -> Mob -> Maybe [Point]
heardPath lvl m1 m2 =
    if distance (at m1) (at m2) <= hearing m1 then
        findPath (dfinder lvl) distance (at m2) (at m1)
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

