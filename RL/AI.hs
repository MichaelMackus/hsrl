{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts #-}

module RL.AI (runAI, automate, AIAction, AIState(..), defaultAIState) where

import RL.Action
import RL.Event
import RL.Game
import RL.Pathfinder
import RL.Random

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe)
import Data.Set (Set)
import Data.Tuple (swap)
import qualified Data.List as L
import qualified Data.Set as Set

-- TODO there should be an expiration for seen/heard (maybe based on
-- TODO monster intelligence) - then, we only choose optimal path with
-- TODO length less than expiration (so rats/etc. can't surround players
-- TODO as easily)

-- TODO heard checks should only be done only on specific turn intervals to make sneaking more optimal
-- TODO same with seen checks? (So player can sneek past visible mobs?)

newtype AIAction a = AIAction { aiAction :: StateT AIState (WriterT [Event] (ReaderT Env (Rand StdGen))) a }
    deriving (Monad, Applicative, Functor, MonadReader Env, MonadState AIState, MonadWriter [Event], MonadRandom)

instance GameAction AIAction where
    getEnv = ask
    insertEvents = tell

data AIState = AIState { destination :: Maybe Point, curMobId :: Id }

defaultAIState = AIState Nothing

runAI :: AIAction a -> Env -> AIState -> StdGen -> ([Event], AIState)
runAI k env s g = let k' = execStateT (aiAction k) s
                  in  swap $ evalRand (runReaderT (runWriterT k') env) g

automate :: AIAction ()
automate = getMob >>= \m -> do
    env   <- ask
    lvl   <- asks level
    heard <- canHear (events env) m (player lvl)
    let seen  = canSeeMob lvl m (player lvl)
        path  = findOptimalPath lvl distance (at (player lvl)) (at m)
    when (heard && isSleeping m) $ insertEvent (Waken m) -- TODO not waking mob

    when heard $ updateDestination (at (player lvl))
    when seen  $ updateDestination (at (player lvl))
    curPath <- curMobPath

    if (seen || heard) && isJust path then
       moveCloser (player lvl) (fromJust path)
    else if not (null curPath) then
       moveCloser (player lvl) curPath
    else if not (isSleeping m) then
       wander
    else return ()

--- wander randomly
wander :: AIAction ()
wander = getMob >>= \m -> do
    lvl <- asks level
    p   <- pick (aiNeighbors lvl (at m) (at m))
    when (isJust p) $ insertEvent (Moved m (fromJust p))

moveCloser :: Mob -> [Point] -> AIAction ()
moveCloser p path = getMob >>= \m -> do
    lvl <- asks level
    let next        = path !! 1
        t           = findTileAt next lvl
        isValidPath = length path > 1 && isJust t && isPassable (fromJust t)
        -- m'          = fromJust (findMob (mobId m) (mobs lvl)) -- TODO is this necessary?
    if isValidPath then
        if next == at p && not (isDead p) then do
            attack m (wielding (equipment m)) p
            clearDestination
        else if isNothing (findMobAt next lvl) then
            insertEvent $ Moved m next
        else
            clearDestination
    else
        clearDestination

updateDestination :: Point -> AIAction ()
updateDestination p = modify $ \s -> s { destination = Just p }
clearDestination :: AIAction ()
clearDestination = modify $ \s -> s { destination = Nothing }
getDestination :: AIAction (Maybe Point)
getDestination = gets destination
getMob :: AIAction Mob
getMob = do
    ms <- asks (mobs . level)
    i  <- gets curMobId
    let m = findMob i ms
    maybe (error "Unable to find mob for AI action!") return m

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

curMobPath :: AIAction [Point]
curMobPath = getMob >>= \m -> do
    lvl  <- asks level
    dest <- getDestination
    if (isJust dest) then return $ fromMaybe [] (findOptimalPath lvl distance (fromJust dest) (at m))
    else return []

-- find AI path, first trying to find optimal path around mobs
-- fallback is naive dfinder to allow mobs to bunch up
findOptimalPath lvl h end s = let optimal = findPath (aiFinder lvl end) h end s
                              in  if isNothing optimal then findPath (dfinder lvl end) h end s else optimal

aiFinder :: DLevel -> Point -> Point -> Set Point
aiFinder d end p = Set.fromList (aiNeighbors d end p)

aiNeighbors :: DLevel -> Point -> Point -> [Point]
aiNeighbors d end p = L.filter f (dneighbors d p)
    where f p = let m' = findMobAt p d
                    isntM m' = isNothing m' || isPlayer (fromJust m')
                in  isntM m' && (isRunnable d p || p == end)

