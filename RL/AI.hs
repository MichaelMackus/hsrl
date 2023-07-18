{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts #-}

module RL.AI (runAI, automate, AIAction, AIState(..), defaultAIState) where

import RL.Action
import RL.Event
import RL.Game
import RL.Pathfinder
import RL.Random
import RL.Util

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe, listToMaybe)
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

data AIState = AIState { path :: Maybe [Point], movementPoints :: Int, curMobId :: Id, fleeing :: Bool }

defaultAIState mid = AIState Nothing 0 mid False

runAI :: AIAction a -> Env -> AIState -> StdGen -> ([Event], AIState)
runAI k env s g = let k' = execStateT (aiAction k) s
                  in  swap $ evalRand (runReaderT (runWriterT k') env) g

automate :: AIAction ()
automate = do
    m       <- getMob
    env     <- ask
    lvl     <- asks level
    heard   <- canHear (events env) m (player lvl)
    s       <- get
    let seen  = canSeeMob lvl m (player lvl)
        path  = findOptimalPath lvl (at (player lvl)) (at m)

    -- check for morale
    when (seenMonsterDiedThisTurn env m) $ do
        r <- roll $ 2 `d` 6
        let isFleeing = True
        when isFleeing $ do
            modify $ \s -> s { fleeing = isFleeing, path = flee lvl m (player lvl) }
            insertMessage $ MobFlees m
    s       <- get
    curPath <- curMobPath
    atk     <- attackRetreating m
    if atk then do
        r <- roll $ 1 `d` 2
        if r == 1 then
            walkPathOrWander
        else
            seenMessage $ PlayerRetreated m
    else if isSleeping m then
        when heard $ gameEvent (Waken m)
    else do
        when (not (fleeing s)) $ do
            when heard $ updateDestination (at (player lvl))
            when seen  $ updateDestination (at (player lvl))

        walkPathOrWander

--- wander randomly
wander :: AIAction ()
wander = do
    clearDestination
    m   <- getMob
    lvl <- asks level
    p   <- pick (L.filter (isRunnable lvl) $ dneighbors lvl (at m))
    when (isJust p) $ tryMove (fromJust p) >> return ()


walkPathOrWander :: AIAction ()
walkPathOrWander = do
    m    <- getMob
    lvl  <- asks level
    path <- gets path
    case path of
        Just path -> do
           let next        = path !! 1
               t           = findTileAt next lvl
               p           = player lvl -- TODO don't tie to player
               isValidPath = length path > 1 && isJust t && isPassable (fromJust t)

           if isValidPath then do
               if next == at p && not (isDead p) then do
                   attack m (wielding (equipment m)) p
                   clearDestination
               else if isNothing (findMobAt next lvl) then do
                   whenM (tryMove next) $
                       modify $ \s -> s { path = Just (tail path) }
               else wander
            else wander
        Nothing -> wander

-- TODO allow multiple movements if able
tryMove :: Point -> AIAction Bool
tryMove p = do
    pl <- getPlayer
    m  <- getMob
    mp <- (+ mobSpeed m) <$> gets movementPoints
    if mp >= mobSpeed pl then do
        gameEvent $ Moved m p
        modify $ \s -> s { movementPoints = max 0 (mp - mobSpeed pl) }
        return True
    else do
        incMovePoints mp
        return False

-- increment mob movement points
incMovePoints :: Int -> AIAction ()
incMovePoints mp = modify $ \s -> s { movementPoints = mp }

updateDestination :: Point -> AIAction ()
updateDestination p = do
    env <- ask
    mob <- getMob
    let path = findOptimalPath (level env) p (at mob)
    modify $ \s -> s { path = path }
clearDestination :: AIAction ()
clearDestination = modify $ \s -> s { path = Nothing, fleeing = False }
getMob :: AIAction Mob
getMob = do
    ms <- asks (mobs . level)
    i  <- gets curMobId
    let m = findMob i ms
    maybe (error "Unable to find mob for AI action!") return m

curMobPath :: AIAction [Point]
curMobPath = getMob >>= \m -> do
    lvl  <- asks level
    fromMaybe [] <$> gets path

-- find AI path, first trying to find optimal path around mobs
-- fallback is naive dfinder to allow mobs to bunch up
-- TODO fallback path should allow mobs to bunch up
findOptimalPath lvl end s = let optimal = findPath (dfinder lvl end s) s end
                            in  if isNothing optimal then findPath (\p -> dneighbors lvl p) s end else optimal -- TODO need fallback

-- TODO prevent mobs from fleeing into player square, probably need new
-- TODO finder for this
-- TODO if it gets enclosed by another mob, must fight
-- TODO still getting issues with this sometimes when mob blocking
--
-- TODO swap spots with mobs when fleeing?
flee :: DLevel -> Mob -> Mob -> Maybe [Point]
flee lvl m m' = let mpoints   = L.nub . concat . map snd . findPathsFrom (dfinder lvl (at m) (at m)) $ at m -- passable points from mob
                    f p       = dfinder lvl (at m) from p
                    fleePaths = findPathsFrom f from
                    from      = at m'
                    dest      = pickGreatestPoint mpoints fleePaths
                in  case dest of
                        Just dest -> let path = findPath (dfinder lvl dest (at m)) (at m) dest
                                     in  path
                        Nothing   -> Nothing

pickGreatestPoint :: [Point] -> [(Int, [Point])] -> Maybe Point
pickGreatestPoint mpoints ps = listToMaybe =<< (listToMaybe . L.filter g . map (reverse . snd) . L.sortBy f $ ps)
    where f (w, _) (w', _) = compare w' w
          g (p:ps)         = p `elem` mpoints
          g []             = False
