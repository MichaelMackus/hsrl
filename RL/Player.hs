{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts #-}

module RL.Player (isAutomating, isTicking, handleInput, automatePlayer, runPlayerAction) where

import RL.Action
import RL.UI.Common (Key(..), KeyMod)
import RL.Event
import RL.Types
import RL.Game
import RL.Pathfinder
import RL.Random
import RL.Util

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Maybe (listToMaybe, maybeToList, fromJust, isJust, isNothing, fromMaybe)
import Data.Tuple (swap)
import qualified Data.List as L

newtype PlayerAction a = PlayerAction { playerAction :: StateT InputState (WriterT [Event] (ReaderT Env (Rand StdGen))) a }
    deriving (Monad, Applicative, Functor, MonadReader Env, MonadState InputState, MonadWriter [Event], MonadRandom)

instance GameAction PlayerAction where
    getEnv = ask
    insertEvents = tell

data InputState   = InputState { menu :: Maybe Menu,
                                 destination :: Maybe Point,
                                 readied :: Maybe Item,
                                 target :: Maybe Point }

runPlayerAction :: PlayerAction a -> Env -> InputState -> StdGen -> ([Event], InputState)
runPlayerAction k env s g = let k' = execStateT (playerAction k) s
                            in  swap $ evalRand (runReaderT (runWriterT k') env) g

-- checks if we are running to a destination
isAutomating :: InputState -> Bool
isAutomating env = isJust (destination env)

-- detects if we're ticking (i.e. AI and other things should be active)
isTicking :: InputState -> Bool
isTicking = isNothing . menu

-- handle input from user
handleInput :: Key -> [KeyMod] -> PlayerAction ()
handleInput k km = do
    m   <- gets menu
    env <- ask
    let lvl = level env
        p   = player lvl
    if isNothing m then do -- normal gameplay (not in menu)
        evs <- case k of
            (KeyChar 'k')     -> moveOrAttack North
            (KeyChar 'j')     -> moveOrAttack South
            (KeyChar 'h')     -> moveOrAttack West
            (KeyChar 'l')     -> moveOrAttack East
            (KeyChar 'u')     -> moveOrAttack NE
            (KeyChar 'y')     -> moveOrAttack NW
            (KeyChar 'b')     -> moveOrAttack SW
            (KeyChar 'n')     -> moveOrAttack SE
            (KeyChar '8')     -> moveOrAttack North
            (KeyChar '2')     -> moveOrAttack South
            (KeyChar '4')     -> moveOrAttack West
            (KeyChar '6')     -> moveOrAttack East
            (KeyChar '9')     -> moveOrAttack NE
            (KeyChar '7')     -> moveOrAttack NW
            (KeyChar '1')     -> moveOrAttack SW
            (KeyChar '3')     -> moveOrAttack SE
            KeyUp             -> moveOrAttack North
            KeyRight          -> moveOrAttack East
            KeyLeft           -> moveOrAttack West
            KeyDown           -> moveOrAttack South
            (KeyChar 'f')     -> return $ tryFire lvl p -- TODO skip to targetting menu if projectile/launcher readied
            (KeyChar 't')     -> return $ tryFire lvl p
            (KeyChar 'r')     -> return [MenuChange Inventory]
            (KeyChar '>')     -> maybeToList <$> (takeStairs Down)
            (KeyChar '<')     -> maybeToList <$> (takeStairs Up)
            (KeyChar 'i')     -> return [MenuChange Inventory]
            (KeyChar 'Q')     -> return [QuitGame]
            (KeyChar 'g')     -> return (maybeToList (pickup (level env)))
            (KeyChar ',')     -> return (maybeToList (pickup (level env)))
            (KeyChar 'w')     -> return [MenuChange Inventory]
            (KeyChar 'W')     -> return [MenuChange Inventory]
            (KeyChar 'e')     -> return [MenuChange Inventory]
            (KeyChar 'q')     -> return [MenuChange Inventory]
            (KeyChar 's')     -> return [Saved]
            (KeyMouseLeft to) -> (when (canSee lvl p to || to `elem` seen lvl) $ startRunning to) >> return []
            otherwise         -> return []
        insertEvents evs
    else return () -- TODO menu handling

-- automate player turn
automatePlayer :: PlayerAction ()
automatePlayer = do
    s   <- get
    env <- ask
    when (isJust (destination s)) $
        if canAutomate env then continueRunning
        else clearDestination

moveOrAttack :: (MonadRandom m, MonadReader Env m) => Dir -> m [Event]
moveOrAttack dir = do
    env <- ask
    let lvl = level env
        p   = player lvl
    moveOrAttackAt (addDir dir (at p))

moveOrAttackAt :: (MonadRandom m, MonadReader Env m) => Point -> m [Event]
moveOrAttackAt to = do
    env <- ask
    let lvl      = level env
        p        = player lvl
        moveE    = [Moved p to]
        stairE   = maybe [] (maybeToList . stairF) $ findTileAt to lvl
        stairF   = \t -> StairsTaken (fromJust (getStairDir t)) <$> getStairLvl t
    case (findMobAt to lvl, findTileAt to lvl, findFeatureAt to lvl) of
        (Just m, _, _) -> attack p (wielding (equipment p)) m
        (_, _, Just f) -> interactFeature to f
        (_, Just t, _) -> if isPassable t then return (moveE ++ stairE) else return []
        otherwise   -> return []

interactFeature :: (MonadRandom m, MonadReader Env m) => Point -> Feature -> m [Event]
interactFeature p f = do
    pl  <- asks (player . level)
    evs <- case f of
                (Fountain n) | n > 0 -> do
                   healed <- roll (2 `d` 8)
                   return [FeatureInteracted p (Fountain n), Healed pl healed]
                (Chest is) -> return (FeatureInteracted p (Chest is):map (ItemSpawned p) is)
                otherwise  -> return []
    return (evs ++ [DestinationAbrupted pl p])

tryFire :: DLevel -> Mob -> [Event]
tryFire lvl m =
    if inMelee lvl m then [MissileInterrupted m]
    else [MenuChange ProjectileMenu]

pickup :: DLevel -> Maybe Event
pickup lvl = 
    let is = findItemsAt (at (player lvl)) lvl
    in  ItemPickedUp (player lvl) <$> listToMaybe is

takeStairs :: MonadReader Env m => VerticalDirection -> m (Maybe Event)
takeStairs v = do
    lvl <- asks level
    let p    = player lvl
        t    = fromMaybe (error "Unable to find stairs tile") $ findTileAt (at p) lvl
        lvl' = getStairLvl t
    if ((v == Up && isUpStair t) || (v == Down && isDownStair t)) && isJust lvl' then
        return $ StairsTaken v <$> lvl'
    else if isNothing lvl' then
        return $ Just Escaped
    else
        return Nothing

setDestination :: Point -> PlayerAction ()
setDestination to = do
    s <- get
    put $ s { destination = Just to }

clearDestination :: PlayerAction ()
clearDestination = do
    s <- get
    put $ s { destination = Nothing }

startRunning :: Point -> PlayerAction ()
startRunning to = do
    env  <- ask
    dest <- gets destination
    let p     = player (level env)
        path  = findPath (dfinder (level env) to) distance to (at p)
    when (isJust path && length (fromJust path) > 1 && canAutomate env && isNothing dest) $ do
        setDestination to
        continueRunning

continueRunning :: PlayerAction ()
continueRunning = do
    env  <- ask
    dest <- gets destination
    when (isJust dest) $
        let to    = fromJust dest
            p     = player (level env)
            path  = findPath (dfinder (level env) to) distance to (at p)
        in  if isJust path && length (fromJust path) > 1 then do
                moveOrAttackAt (fromJust path !! 1)
                when (fromJust path !! 1 == to) clearDestination
            else clearDestination
