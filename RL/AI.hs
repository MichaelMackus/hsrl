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

-- TODO enable player retreat from monster - monsters (of equal/greater speed
-- to player) should have roughly a 50/50 chance of pursuing a fleeing PC based
-- on reaction roll... we can code this by allowing the PC a 50/50 chance to
-- escape the melee
--
-- See this thread for more info:
-- https://www.dragonsfoot.org/forums/viewtopic.php?f=15&t=85025&sid=77d787b84d53bc9082ff6994d39166bd&start=30

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

data AIState = AIState { destination :: Maybe Point, movementPoints :: Int, curMobId :: Id }

defaultAIState = AIState Nothing 0

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
    when (heard && isSleeping m) $ gameEvent (Waken m)

    when heard $ updateDestination (at (player lvl))
    when seen  $ updateDestination (at (player lvl))
    curPath <- curMobPath

    atk <- attackRetreating m
    if atk then do
        r <- roll $ 1 `d` 2
        if r == 1 then
            moveCloser (player lvl) (fromJust path)
        else
            seenMessage $ PlayerRetreated m
    else
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
    when (isJust p) $ tryMove (fromJust p)

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
            tryMove next
        else
            clearDestination
    else
        clearDestination

-- TODO allow multiple movements if able
tryMove :: Point -> AIAction ()
tryMove p = do
    pl <- getPlayer
    m  <- getMob
    mp <- (+ mobSpeed m) <$> gets movementPoints
    if mp >= mobSpeed pl then do
        gameEvent $ Moved m p
        modify $ \s -> s { movementPoints = max 0 (mp - mobSpeed pl) }
    else
        incMovePoints mp

-- increment mob movement points
incMovePoints :: Int -> AIAction ()
incMovePoints mp = modify $ \s -> s { movementPoints = mp }

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
