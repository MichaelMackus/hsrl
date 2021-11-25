{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts #-}

module RL.Player (InputState(..), isAutomating, isTicking, handleInput, automatePlayer, runPlayerAction) where

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
-- TODO simplify and move isConfused to automated player function
handleInput :: Key -> [KeyMod] -> PlayerAction ()
handleInput k km = do
    m   <- gets menu
    env <- ask
    let lvl = level env
        p   = player lvl
    if isConfused p then
        if k == (KeyChar 'Q') then insertEvent QuitGame
        else if k == (KeyChar 's') then insertEvent Saved
        else randomDir >>= moveOrAttack >>= insertEvents
    else if isNothing m then
        -- normal gameplay (not in menu or confused)
        case k of
            (KeyChar 'k')     -> insertEvents =<< moveOrAttack North
            (KeyChar 'j')     -> insertEvents =<< moveOrAttack South
            (KeyChar 'h')     -> insertEvents =<< moveOrAttack West
            (KeyChar 'l')     -> insertEvents =<< moveOrAttack East
            (KeyChar 'u')     -> insertEvents =<< moveOrAttack NE
            (KeyChar 'y')     -> insertEvents =<< moveOrAttack NW
            (KeyChar 'b')     -> insertEvents =<< moveOrAttack SW
            (KeyChar 'n')     -> insertEvents =<< moveOrAttack SE
            (KeyChar '8')     -> insertEvents =<< moveOrAttack North
            (KeyChar '2')     -> insertEvents =<< moveOrAttack South
            (KeyChar '4')     -> insertEvents =<< moveOrAttack West
            (KeyChar '6')     -> insertEvents =<< moveOrAttack East
            (KeyChar '9')     -> insertEvents =<< moveOrAttack NE
            (KeyChar '7')     -> insertEvents =<< moveOrAttack NW
            (KeyChar '1')     -> insertEvents =<< moveOrAttack SW
            (KeyChar '3')     -> insertEvents =<< moveOrAttack SE
            KeyUp             -> insertEvents =<< moveOrAttack North
            KeyRight          -> insertEvents =<< moveOrAttack East
            KeyLeft           -> insertEvents =<< moveOrAttack West
            KeyDown           -> insertEvents =<< moveOrAttack South
            (KeyChar 'f')     -> tryFire lvl p -- TODO skip to targetting menu if projectile/launcher readied
            (KeyChar 't')     -> tryFire lvl p
            (KeyChar 'r')     -> changeMenu Inventory
            (KeyChar '>')     -> insertEvents =<< maybeToList <$> (takeStairs Down)
            (KeyChar '<')     -> insertEvents =<< maybeToList <$> (takeStairs Up)
            (KeyChar 'i')     -> changeMenu Inventory
            (KeyChar 'Q')     -> insertEvent QuitGame
            (KeyChar 'g')     -> insertEvents (maybeToList (pickup (level env)))
            (KeyChar ',')     -> insertEvents (maybeToList (pickup (level env)))
            (KeyChar 'w')     -> changeMenu Inventory
            (KeyChar 'W')     -> changeMenu Inventory
            (KeyChar 'e')     -> changeMenu Inventory
            (KeyChar 'q')     -> changeMenu Inventory
            (KeyChar 's')     -> insertEvent Saved
            (KeyMouseLeft to) -> when (canSee lvl p to || to `elem` seen lvl) $ startRunning to
            otherwise         -> return ()
    else handleMenu k km (fromJust m)

handleMenu :: Key -> [KeyMod] -> Menu -> PlayerAction ()
handleMenu k km Inventory = do
    p   <- player . level <$> getEnv
    lvl <- level          <$> getEnv
    let ch = charFromKey k
        i  = (`fromInventoryLetter` (inventory p)) =<< ch
    e <- maybe (return []) (applyItem lvl p) i
    insertEvents e
    closeMenu
handleMenu k km ProjectileMenu = do
    p   <- player . level <$> getEnv
    lvl <- level          <$> getEnv
    let ch   = charFromKey k
        i    = (`fromInventoryLetter` (inventory p)) =<< ch
        targets = L.filter (canSee lvl p) . L.sortBy (comparing (distance (at p))) . map at $ mobs lvl
    if maybe False isProjectile i && length targets > 0 then do
        readyProjectile (fromJust i)
        changeMenu TargetMenu
        changeTarget (head targets)
    else
        closeMenu
handleMenu k km TargetMenu = do
    s   <- get
    p   <- player . level <$> getEnv
    lvl <- level          <$> getEnv
    let targets = L.filter (canSee lvl p) . L.sortBy (comparing (distance (at p))) . map at $ mobs lvl
    case k of
        (KeyChar    '\t') | length targets > 0 ->
            -- change to next target based on tab char
            let i   = fromMaybe 0 $ (\t -> L.findIndex (== t) targets) =<< target s
                tgt = if i + 1 >= length targets then head targets else targets !! (i + 1)
            in  changeTarget tgt
        (KeyEnter) | isJust (target s) && isJust (readied s) && isJust (findMobAt (fromJust $ target s) lvl) -> do
            evs <- fire lvl p (fromJust (readied s)) (fromJust (findMobAt (fromJust $ target s) lvl))
            insertEvents evs
            clearTarget
        (KeyMouseLeft to) | to `elem` targets && isJust (findMobAt to lvl) && isJust (readied s) -> do
            evs <- fire lvl p (fromJust (readied s)) (fromJust (findMobAt to lvl))
            insertEvents evs
            clearTarget
        otherwise -> closeMenu
handleMenu k km otherwise = return ()

charFromKey :: Key -> Maybe Char
charFromKey (KeyChar ch) = Just ch
charFromKey otherwise = Nothing

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
    case f of
        (Fountain n) | n > 0 -> do
           healed <- roll (2 `d` 8)
           return [FeatureInteracted p (Fountain n), Healed pl healed]
        (Chest is) -> return (FeatureInteracted p (Chest is):map (ItemSpawned p) is)
        otherwise  -> return []

tryFire :: DLevel -> Mob -> PlayerAction ()
tryFire lvl m =
    if inMelee lvl m then return () -- TODO message: [MissileInterrupted m]
    else changeMenu ProjectileMenu

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

readyProjectile :: Item -> PlayerAction ()
readyProjectile i = modify $ \s -> s { readied = Just i }

changeMenu :: Menu -> PlayerAction ()
changeMenu m = modify $ \s -> s { menu = Just m }

closeMenu :: PlayerAction ()
closeMenu = modify $ \s -> s { menu = Nothing }

changeTarget :: Point -> PlayerAction ()
changeTarget p = modify $ \s -> s { target = Just p }

clearTarget :: PlayerAction ()
clearTarget = modify $ \s -> s { target = Nothing }
