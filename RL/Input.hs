module RL.Input where

import RL.Action
import RL.UI.Common (Key(..), KeyMod)
import RL.Event
import RL.Types
import RL.Game
import RL.Pathfinder
import RL.Random

import Data.Maybe (listToMaybe, maybeToList, fromJust, isJust, isNothing, fromMaybe)

-- automate player turn
automatePlayer :: GameEnv [Event]
automatePlayer = do
    env <- ask
    p   <- asks (player . level)
    if isResting p then
        if canAutomate env then return (rest p)
        else return [StoppedResting p]
    else if isJust (destination p) then
        if canAutomate env then startRunning (fromJust (destination p))
        else return [DestinationAbrupted p (fromJust (destination p))]
    else return []

-- transform key to an event(s) within the game
keyToEvents :: Key -> [KeyMod] -> GameEnv [Event]
keyToEvents k m = do
    env <- ask
    let lvl = level env
        p   = player lvl
    if menu env == NoMenu && not (isConfused p) then do
        case k of
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
            (KeyChar 'r')     -> if canRest env then return [StartedResting p] else return [FailedRest p]
            (KeyChar 'R')     -> if canRest env then return [StartedResting p] else return [FailedRest p]
            (KeyChar '>')     -> maybeToList <$> (takeStairs Down)
            (KeyChar '<')     -> maybeToList <$> (takeStairs Up)
            (KeyChar 'i')     -> return [MenuChange Inventory]
            (KeyChar 'Q')     -> return [QuitGame]
            (KeyChar 'g')     -> return (maybeToList (pickup (level env)))
            (KeyChar ',')     -> return (maybeToList (pickup (level env)))
            (KeyChar 'w')     -> return [MenuChange Equipment]
            (KeyChar 'W')     -> return [MenuChange Equipment]
            (KeyChar 'e')     -> return [MenuChange Equipment]
            (KeyChar 'q')     -> return [MenuChange DrinkMenu]
            (KeyMouseLeft to) -> if canSee lvl p to || to `elem` seen lvl then startRunning to else return []
            otherwise         -> return []
    else if isConfused p then do
        if k == (KeyChar 'Q') then return [QuitGame]
        else moveOrAttack =<< randomDir
    else if isViewingInventory env then do
        let ch = charFromKey k
            i  = (`fromInventoryLetter` (inventory p)) =<< ch
        e <- maybe (return []) (applyItem lvl p) i
        return (e ++ [MenuChange NoMenu])
    else return []

charFromKey :: Key -> Maybe Char
charFromKey (KeyChar ch) = Just ch
charFromKey otherwise = Nothing

moveOrAttack :: Dir -> GameEnv [Event]
moveOrAttack dir = do
    env <- ask
    let lvl = level env
        p   = player lvl
    moveOrAttackAt (addDir dir (at p))

moveOrAttackAt :: Point -> GameEnv [Event]
moveOrAttackAt to = do
    env <- ask
    let lvl    = level env
        p      = player lvl
        moveE  = [Moved p to]
        stairE = maybe [] (maybeToList . stairF) $ findTileAt to lvl
        stairF = \t -> StairsTaken (fromJust (getStairDir t)) <$> getStairLvl t
    case (findMobAt to lvl, findTileAt to lvl) of
        (Just m, _) -> attack p m
        (_, Just t) -> if isPassable t then return (moveE ++ stairE) else return []
        otherwise   -> return []

pickup :: DLevel -> Maybe Event
pickup lvl = 
    let is = findItemsAt (at (player lvl)) lvl
    in  ItemPickedUp (player lvl) <$> listToMaybe is

takeStairs :: VerticalDirection -> GameEnv (Maybe Event)
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

startRunning :: Point -> GameEnv [Event]
startRunning to = ask >>= \env ->
    let p     = player (level env)
        path  = findPath (dfinder (level env)) distance to (at p)
        destE = if canAutomate env && isNothing (destination p) then [DestinationSet p to] else []
    in  if isJust path && length (fromJust path) > 1 then
            (destE ++) <$> moveOrAttackAt (fromJust path !! 1)
        else return []
