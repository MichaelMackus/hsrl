{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts #-}

module RL.Player (PlayerAction, runPlayerAction, execPlayerAction, evalPlayerAction, InputState(..), defaultInputState, Menu(..), updateSeen, updateHeard, startTurn, isTicking, readyForInput, handleInput, seenAtDepth, heardAtDepth) where

-- TODO when automating, there's a delay waiting for movement points
-- TODO should have an impementation of retreat (monsters may attack the fleeing player at a bonus... note in OSE this depends on initiative roll, so 50/50 chance monster doesn't get AoO)
-- TODO monster morale should have an effect as well, this way player gets AoO too

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
import Data.Maybe (catMaybes, listToMaybe, maybeToList, fromJust, isJust, isNothing, fromMaybe)
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
                               path :: Maybe [Point],
                               readied :: Maybe Item,
                               target :: Maybe Point,
                               seen :: [(Depth, [Point])],  -- seen map tile
                               heard :: [(Depth, [Point])] -- heard mob
                             }

defaultInputState = InputState Nothing Nothing Nothing Nothing [] []

runPlayerAction :: PlayerAction a -> Env -> InputState -> StdGen -> (a, ([Event], InputState))
runPlayerAction k env s g = let k'             = runStateT (playerAction k) s
                                ((r, is), evs) =  evalRand (runReaderT (runWriterT k') env) g
                            in  (r, (evs, is))

execPlayerAction :: PlayerAction a -> Env -> InputState -> StdGen -> ([Event], InputState)
execPlayerAction k env s g = snd $ runPlayerAction k env s g

evalPlayerAction :: PlayerAction a -> Env -> InputState -> StdGen -> a
evalPlayerAction k env s g = fst $ runPlayerAction k env s g

startTurn :: PlayerAction ()
startTurn = do
    -- attempt to automate player turn if running
    s   <- get
    env <- ask
    if isJust (path s) && canAutomate env then continueRunning
    else clearDestination

-- detects if we're ticking (i.e. AI and other things should be active)
isTicking :: InputState -> Bool
isTicking = isNothing . menu

-- returns true if we're ready for input from the keyboard (i.e. we're not running to a destination)
readyForInput :: InputState -> Bool
readyForInput = isNothing . path

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
            (KeyChar '>')     -> goStairs Down
            (KeyChar '<')     -> goStairs Up
            (KeyChar '*')     -> goFeature Campfire
            (KeyChar 'i')     -> changeMenu Inventory
            (KeyChar 'Q')     -> gameEvent QuitGame
            (KeyChar 'g')     -> gameEvents (maybeToList (pickup (level env)))
            (KeyChar 'G')     -> gameEvents (pickupAll (level env))
            (KeyChar 'd')     -> changeMenu DropMenu
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
handleMenu k km DropMenu = do
    p   <- getPlayer
    lvl <- level <$> getEnv
    let ch = charFromKey k
        i  = (`fromInventoryLetter` (inventory p)) =<< ch
    when (isJust i) $ gameEvent (ItemDropped p (fromJust i))
    closeMenu
handleMenu k km ProjectileMenu = do
    p <- getPlayer
    let ch   = charFromKey k
        i    = (`fromInventoryLetter` (inventory p)) =<< ch
    if maybe False isProjectile i then do
        readyProjectile (fromJust i)
        targets <- getTargets
        if length targets > 0 then do
            changeMenu TargetMenu
            changeTarget (head targets)
        else do
            insertMessage NoTargetsInRange
            closeMenu
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
    env <- ask
    pl  <- asks (player . level)
    gameEvent $ FeatureInteracted p f
    case f of
        (Fountain n) | n > 0 -> do
           healed <- roll (2 `d` 8)
           gameEvent $ Healed pl healed
        (Chest is) -> gameEvents $ map (ItemSpawned p) is
        -- TODO level up at altars?
        -- Altar      ->
        --     if canRest env then gameEvent $ Healed pl (mhp pl - hp pl)
        --     else insertMessage Hostiles
        Campfire   -> do
            if canRest env then do
                gameEvents [Rested pl (depth (level env)) (currentDay (events env)), Healed pl (mhp pl - hp pl)]
                -- check for levelup
                when (needsLevelUp pl) $ do
                    gameEvent $ GainedLevel pl (mlvl pl + 1)
                    bonusHP <- roll $ 1 `d` 8
                    gameEvent $ GainedLife pl bonusHP
            else insertMessage PlayerInDanger
        f  -> return ()

-- attempt to fire if readied weapon
tryFire :: DLevel -> Mob -> PlayerAction ()
tryFire lvl m = do
    rdy     <- getReadied
    targets <- getTargets
    if inMelee lvl (player lvl) then insertMessage InMelee
    else if isJust rdy && length targets > 0 then do
            changeMenu TargetMenu
            changeTarget (head targets)
    else changeMenu ProjectileMenu

tryInventory :: PlayerAction ()
tryInventory = getEnv >>= \env ->
    if inMelee (level env) (player (level env)) then insertMessage InMelee
    else changeMenu Inventory

pickup :: DLevel -> Maybe GameEvent
pickup lvl = 
    let is = findItemsAt (at (player lvl)) lvl
    in  ItemPickedUp (player lvl) <$> listToMaybe is

pickupAll :: DLevel -> [GameEvent]
pickupAll lvl = 
    let is = findItemsAt (at (player lvl)) lvl
    in  map (ItemPickedUp (player lvl)) is

-- take stairs or goto up/down stair
goStairs :: VerticalDirection -> PlayerAction ()
goStairs v = do
    lvl <- asks level
    t   <- getPlayerTile
    if ((v == Up && isUpStair t) || (v == Down && isDownStair t)) && isJust (getStairLvl t) then takeStairs v
    else do
        ts <- getSeen
        let to = fst <$> findTile (\(_, t) -> v == Up && isUpStair t || v == Down && isDownStair t) lvl
        when (isJust to && fromJust to `elem` ts) $ startRunning (fromJust to)

-- take stairs or goto up/down stair
-- TODO sometimes not working to get to campfire after finding downstair
goFeature :: Feature -> PlayerAction ()
goFeature f = do
    lvl  <- asks level
    let pl                    = player lvl
        to                    = fst <$> (L.find findF . L.sortBy sortF $ features lvl)
        sortF (p1, _) (p2, _) = comparing (distance (at pl)) p1 p2
        findF (_, f')         = f == f'
    case to of
        Just to -> whenM (hasSeen to) $ startRunning to
        Nothing -> return ()

takeStairs :: VerticalDirection -> PlayerAction ()
takeStairs v = do
    t <- getPlayerTile
    let lvl = getStairLvl t
    if ((v == Up && isUpStair t) || (v == Down && isDownStair t)) && isJust lvl then
        when (isJust lvl) $ gameEvent $ StairsTaken v (fromJust lvl)
    else if isNothing lvl then
        gameEvent Escaped
    else
        return ()

clearDestination :: PlayerAction ()
clearDestination = do
    s <- get
    modify $ \s -> s { path = Nothing }

startRunning :: Point -> PlayerAction ()
startRunning to = do
    env  <- ask
    seen <- getSeen
    let p     = player (level env)
        path  = findPath (playerFinder (level env) seen to) (at p) to
    when (isJust path && length (fromJust path) > 1) $ do
        modify $ \s -> s { path = path }
        continueRunning

continueRunning :: PlayerAction ()
continueRunning = do
    env  <- ask
    seen <- getSeen
    path <- gets path
    when (isJust path) $
        if length (fromJust path) > 1 then do
            bumpAt (fromJust path !! 1)
            modify $ \s -> s { path = Just (tail (fromJust path)) }
            when (not (canAutomate env)) clearDestination
        else clearDestination

playerFinder :: DLevel
             -> [Point]
             -> (Point -> Point -> [Point])
playerFinder lvl seen = \p p' -> L.filter (`elem` seen) (dfinder lvl (at (player lvl)) p p')

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

-- update newly seen tiles at end of turn
updateSeen :: PlayerAction ()
updateSeen = do
    env <- ask
    let lvl = level env
        pl  = player lvl
    -- update seen tiles
    updateSeenDepth (depth lvl) =<< seenTiles
    -- add messages for currently seen tile
    t   <- getPlayerTile
    when (isDownStair t) $ seenMessage (StairsSeen Down)
    when (isUpStair t)   $ seenMessage (StairsSeen Up)
    let is = findItemsAt (at pl) lvl
    when (length is > 0) $ seenMessage (ItemsSeen is)

-- update heard recently moved mobs
updateHeard :: PlayerAction ()
updateHeard = do
    env <- ask
    let lvl = level env
        pl  = player lvl
    let f (GameUpdate (Moved m p)) = do
            heard <- canHear (events env) pl m
            if mobId m /= mobId pl && heard then 
                return (Just p)
            else
                return Nothing
        f otherwise = return Nothing
    heardMobs <- catMaybes <$> (mapM f . eventsSince 1 $ events env)
    modify $ \s -> s { heard = [(depth lvl, heardMobs)] }

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
    else return $ L.nub(ts ++ (filter (canSee lvl p) points))

getSeen :: PlayerAction [Point]
getSeen = asks level >>= \lvl -> seenAtDepth (depth lvl) <$> get

seenAtDepth :: Depth -> InputState -> [Point]
seenAtDepth d is = fromMaybe [] (L.lookup d (seen is))

heardAtDepth :: Depth -> InputState -> [Point]
heardAtDepth d is = fromMaybe [] (L.lookup d (heard is))

hasSeen :: Point -> PlayerAction Bool
hasSeen p = do
    lvl <- asks level
    ps  <- getSeen
    let pl = player lvl
    return $ p `elem` ps || canSee lvl pl p

getTargets :: PlayerAction [Point]
getTargets = do
    env <- getEnv 
    pl  <- getPlayer
    r   <- gets readied
    let inRange p = distance (at pl) p <= fromMaybe 0 (itemRange =<< r)
    return (L.filter (not . touching (at pl)) . L.filter inRange . L.filter (canSee (level env) pl) . L.sortBy (comparing (distance (at pl))) . map at $ mobs (level env))
