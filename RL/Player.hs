{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts #-}

module RL.Player (PlayerAction, runPlayerAction, execPlayerAction, evalPlayerAction, InputState(..), defaultInputState, Menu(..), startTurn, isTicking, readyForInput, handleInput, seenAtDepth) where

import RL.Action
import RL.UI.Common (Key(..), KeyMod)
import RL.Event
import RL.Types
import RL.Game
import RL.Pathfinder
import RL.Random
import RL.Util

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (listToMaybe, maybeToList, fromJust, isJust, isNothing, fromMaybe)
import Data.Tuple (swap)
import qualified Data.List as L
import qualified Data.Map  as M

newtype PlayerAction a = PlayerAction { playerAction :: PlayerT Identity a }
    deriving (Monad, Applicative, Functor, MonadReader Env, MonadState InputState, MonadWriter [Event], MonadRandom)

type PlayerT m a = StateT InputState (WriterT [Event] (ReaderT Env (RandT StdGen m))) a

instance GameAction PlayerAction where
    getEnv = ask
    insertEvents = tell

data InputState = InputState { menu :: Maybe Menu,
                               destination :: Maybe Point,
                               readied :: Maybe Item,
                               target :: Maybe Point,
                               seen :: [(Depth, [Point])] }

defaultInputState = InputState Nothing Nothing Nothing Nothing []

runPlayerAction :: PlayerAction a -> Env -> InputState -> StdGen -> (a, ([Event], InputState))
runPlayerAction k env s g = let k'             = runStateT (playerAction k) s
                                ((r, is), evs) =  evalRand (runReaderT (runWriterT k') env) g
                            in  (r, (evs, is))

execPlayerAction :: PlayerAction a -> Env -> InputState -> StdGen -> ([Event], InputState)
execPlayerAction k env s g = snd $ runPlayerAction k env s g

evalPlayerAction :: PlayerAction a -> Env -> InputState -> StdGen -> a
evalPlayerAction k env s g = fst $ runPlayerAction k env s g

-- start of player turn
startTurn :: PlayerAction ()
startTurn = do
    s   <- get
    env <- ask
    updateSeen
    if isJust (destination s) && canAutomate env then continueRunning
    else clearDestination

-- detects if we're ticking (i.e. AI and other things should be active)
isTicking :: InputState -> Bool
isTicking = isNothing . menu

-- returns true if we're ready for input from the keyboard (i.e. we're not running to a destination)
readyForInput :: InputState -> Bool
readyForInput = isNothing . destination

-- handle input from user
handleInput :: Key -> [KeyMod] -> PlayerAction ()
handleInput k km = do
    m   <- gets menu
    env <- ask
    let lvl = level env
        p   = player lvl
    if isConfused p then
        if k == (KeyChar 'Q') then gameEvent QuitGame
        else if k == (KeyChar 's') then gameEvent Saved
        else bump =<< randomDir
    else if isNothing m then
        -- normal gameplay (not in menu or confused)
        case k of
            (KeyChar 'k')     -> bump North
            (KeyChar 'j')     -> bump South
            (KeyChar 'h')     -> bump West
            (KeyChar 'l')     -> bump East
            (KeyChar 'u')     -> bump NE
            (KeyChar 'y')     -> bump NW
            (KeyChar 'b')     -> bump SW
            (KeyChar 'n')     -> bump SE
            (KeyChar '8')     -> bump North
            (KeyChar '2')     -> bump South
            (KeyChar '4')     -> bump West
            (KeyChar '6')     -> bump East
            (KeyChar '9')     -> bump NE
            (KeyChar '7')     -> bump NW
            (KeyChar '1')     -> bump SW
            (KeyChar '3')     -> bump SE
            KeyUp             -> bump North
            KeyRight          -> bump East
            KeyLeft           -> bump West
            KeyDown           -> bump South
            (KeyChar 'f')     -> tryFire lvl p
            (KeyChar 't')     -> tryFire lvl p
            (KeyChar 'r')     -> changeMenu Inventory
            (KeyChar '>')     -> takeStairs Down
            (KeyChar '<')     -> takeStairs Up
            (KeyChar 'i')     -> changeMenu Inventory
            (KeyChar 'Q')     -> gameEvent QuitGame
            (KeyChar 'g')     -> gameEvents (maybeToList (pickup (level env)))
            (KeyChar ',')     -> gameEvents (maybeToList (pickup (level env)))
            (KeyChar 'w')     -> changeMenu Inventory
            (KeyChar 'W')     -> changeMenu Inventory
            (KeyChar 'e')     -> changeMenu Inventory
            (KeyChar 'q')     -> changeMenu Inventory
            (KeyChar 's')     -> gameEvent Saved
            (KeyMouseLeft to) -> whenM (hasSeen to) $ startRunning to
            otherwise         -> return ()
    else handleMenu k km (fromJust m)

handleMenu :: Key -> [KeyMod] -> Menu -> PlayerAction ()
handleMenu k km Inventory = do
    p   <- getPlayer
    lvl <- level <$> getEnv
    let ch = charFromKey k
        i  = (`fromInventoryLetter` (inventory p)) =<< ch
    when (isJust i) $ applyItem lvl p (fromJust i)
    closeMenu
handleMenu k km ProjectileMenu = do
    p <- getPlayer
    let ch   = charFromKey k
        i    = (`fromInventoryLetter` (inventory p)) =<< ch
    targets <- getTargets
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
    rdy <- getReadied
    let fireF to = do
            fire lvl p (fromJust rdy) (fromJust (findMobAt to lvl))
            clearTarget
            closeMenu
    if isJust (target s) && isJust rdy then do
        targets <- getTargets
        case k of
            (KeyChar 'r') -> changeMenu ProjectileMenu
            (KeyChar '\t') | length targets > 0 ->
                -- change to next target based on tab char
                let i   = fromMaybe 0 $ (\t -> L.findIndex (== t) targets) =<< target s
                    tgt = if i + 1 >= length targets then head targets else targets !! (i + 1)
                in  changeTarget tgt
            (KeyChar 'f')     | isJust (findMobAt (fromJust $ target s) lvl)   -> fireF (fromJust $ target s)
            (KeyChar 't')     | isJust (findMobAt (fromJust $ target s) lvl)   -> fireF (fromJust $ target s)
            (KeyEnter)        | isJust (findMobAt (fromJust $ target s) lvl)   -> fireF (fromJust $ target s)
            (KeyMouseLeft to) | to `elem` targets && isJust (findMobAt to lvl) -> fireF to
            otherwise -> closeMenu
    else closeMenu
handleMenu k km otherwise = return ()

charFromKey :: Key -> Maybe Char
charFromKey (KeyChar ch) = Just ch
charFromKey otherwise = Nothing

getPlayerTile :: PlayerAction Tile
getPlayerTile = do
    lvl <- level <$> getEnv
    let t = findTileAt (at (player lvl)) lvl
    maybe (error "Invalid player tile!") return t

bump :: Dir -> PlayerAction ()
bump dir = do
    env <- ask
    let lvl = level env
        p   = player lvl
    bumpAt (addDir dir (at p))

bumpAt :: Point -> PlayerAction ()
bumpAt to = do
    env <- ask
    let lvl      = level env
        p        = player lvl
    case (findMobAt to lvl, findTileAt to lvl, findFeatureAt to lvl) of
        (Just m, _, _) -> attack p (wielding (equipment p)) m
        (_, _, Just f) -> interactFeature to f
        (_, Just t, _) -> let stairE   = maybe [] (maybeToList . stairF) $ findTileAt to lvl
                              stairF   = \t -> StairsTaken (fromJust (getStairDir t)) <$> getStairLvl t
                          in  when (isPassable t) $ gameEvents (Moved p to:stairE)
        otherwise      -> return ()

interactFeature :: Point -> Feature -> PlayerAction ()
interactFeature p f = do
    pl  <- asks (player . level)
    gameEvent $ FeatureInteracted p f
    case f of
        (Fountain n) | n > 0 -> do
           healed <- roll (2 `d` 8)
           gameEvent $ Healed pl healed
        (Chest is) -> gameEvents $ map (ItemSpawned p) is
        f  -> return ()

-- attempt to fire if readied weapon
tryFire :: DLevel -> Mob -> PlayerAction ()
tryFire lvl m =
    if inMelee lvl m then insertMessage InMelee
    else do
        rdy     <- getReadied
        targets <- getTargets
        if isJust rdy then
            if length targets > 0 then do
                changeMenu TargetMenu
                changeTarget (head targets)
            else return ()
        else changeMenu ProjectileMenu

pickup :: DLevel -> Maybe GameEvent
pickup lvl = 
    let is = findItemsAt (at (player lvl)) lvl
    in  ItemPickedUp (player lvl) <$> listToMaybe is

takeStairs :: VerticalDirection -> PlayerAction ()
takeStairs v = do
    lvl <- asks level
    let p    = player lvl
        t    = fromMaybe (error "Unable to find stairs tile") $ findTileAt (at p) lvl
        lvl' = getStairLvl t
    if ((v == Up && isUpStair t) || (v == Down && isDownStair t)) && isJust lvl' then
        when (isJust lvl') $ gameEvent $ StairsTaken v (fromJust lvl')
    else if isNothing lvl' then
        gameEvent Escaped
    else
        return ()

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
    when (isJust path && length (fromJust path) > 1) $ do
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
                bumpAt (fromJust path !! 1)
                when (fromJust path !! 1 == to) clearDestination
                when (not (canAutomate env))    clearDestination
            else clearDestination

readyProjectile :: Item -> PlayerAction ()
readyProjectile i = do
    modify $ \s -> s { readied = Just i }
    insertMessage (Readied i)

-- get readied item from pack
getReadied :: PlayerAction (Maybe Item)
getReadied = do
    p <- getPlayer
    r <- gets readied
    case r of
        Nothing -> return Nothing
        Just i  -> return $ L.find (== i) (inventory p)

changeMenu :: Menu -> PlayerAction ()
changeMenu m = do
    modify $ \s -> s { menu = Just m }
    insertMessage (MenuChange m)

closeMenu :: PlayerAction ()
closeMenu = modify $ \s -> s { menu = Nothing }

changeTarget :: Point -> PlayerAction ()
changeTarget p = modify $ \s -> s { target = Just p }

clearTarget :: PlayerAction ()
clearTarget = modify $ \s -> s { target = Nothing }

-- -- update newly seen tiles at end of turn
updateSeen :: PlayerAction ()
updateSeen = do
    lvl  <- asks level
    -- update seen tiles
    updateSeenDepth (depth lvl) =<< seenTiles
    -- add messages for currently seen tile
    t    <- getPlayerTile
    when (isDownStair t) $ seenMessage (StairsSeen Down)
    when (isUpStair t)   $ seenMessage (StairsSeen Up)
    let is = findItemsAt (at (player lvl)) lvl
    when (length is > 0) $ seenMessage (ItemsSeen is)

updateSeenDepth :: Depth -> [Point] -> PlayerAction ()
updateSeenDepth d ts = modify $ \s -> s { seen = (d, ts):filter f (seen s) }
    where f (d', _) = d /= d'

seenTiles :: PlayerAction [Point]
seenTiles = do
    lvl <- asks level
    ts  <- getSeen
    let p      = player lvl
        points = M.keys (tiles lvl)
    if mobMapped (depth lvl) p then return points
    else return $ ts ++ (filter (canSee lvl p) points)

getSeen :: PlayerAction [Point]
getSeen = asks level >>= \lvl -> seenAtDepth (depth lvl) <$> get

seenAtDepth :: Depth -> InputState -> [Point]
seenAtDepth d is = fromMaybe [] (L.lookup d (seen is))

hasSeen :: Point -> PlayerAction Bool
hasSeen p = do
    lvl <- asks level
    ps  <- getSeen
    let pl = player lvl
    return $ p `elem` ps || canSee lvl pl p

getTargets :: PlayerAction [Point]
getTargets = do
    env <- getEnv 
    p   <- getPlayer
    return (L.filter (canSee (level env) p) . L.sortBy (comparing (distance (at p))) . map at $ mobs (level env))
