module RL.Input where

import RL.Action
import RL.UI.Common (Key(..), KeyMod)
import RL.Event
import RL.Types
import RL.Game
import RL.Pathfinder

import Data.Maybe (listToMaybe, maybeToList, fromJust, isJust, isNothing)

charFromKey :: Key -> Maybe Char
charFromKey (KeyChar ch) = Just ch
charFromKey otherwise = Nothing

-- transform key to an event(s) within the game
keyToEvents :: Key -> [KeyMod] -> GameEnv [Event]
keyToEvents k m = do
    env <- ask
    let lvl = level env
        p   = player lvl
    if menu env == NoMenu && not (canAutomate env) then do
        es <- case k of
            (KeyChar 'k')     -> moveOrAttack North
            (KeyChar 'j')     -> moveOrAttack South
            (KeyChar 'h')     -> moveOrAttack West
            (KeyChar 'l')     -> moveOrAttack East
            (KeyChar 'u')     -> moveOrAttack NE
            (KeyChar 'y')     -> moveOrAttack NW
            (KeyChar 'b')     -> moveOrAttack SW
            (KeyChar 'n')     -> moveOrAttack SE
            -- (KeyChar 'r')  -> Restart
            (KeyChar '>')     -> maybeToList <$> (takeStairs Down)
            (KeyChar '<')     -> maybeToList <$> (takeStairs Up)
            (KeyChar 'i')     -> return [MenuChange Inventory]
            (KeyChar 'q')     -> return [QuitGame]
            (KeyChar 'g')     -> return (maybeToList (pickup (level env)))
            (KeyChar ',')     -> return (maybeToList (pickup (level env)))
            (KeyChar 'w')     -> return [MenuChange Equipment]
            (KeyChar 'W')     -> return [MenuChange Equipment]
            (KeyChar 'e')     -> return [MenuChange Equipment]
            (KeyMouseLeft to) -> if canSee lvl p to || to `elem` seen lvl then automatePlayer to else return []
            otherwise         -> return []
        -- stop automating if we've seen a mob
        if isJust (destination p) then
            return $ DestinationAbrupted p (fromJust (destination p)):es
        else
            return es
    else if isAutomated env then automatePlayer (fromJust (destination p))
    else if isViewingInventory env then
        let ch = charFromKey k
            e  = if isJust ch && fromJust ch `elem` inventoryLetters then
                    let i = fromInventoryLetter (fromJust ch) (inventory p)
                    in  Equipped p <$> maybeToList i
                 else []
        in  return (e ++ [MenuChange NoMenu])
    else return []

moveOrAttack :: Dir -> GameEnv [Event]
moveOrAttack dir = do
    env <- ask
    let lvl = level env
        p   = player lvl
        to  = addDir dir (at p)
    moveOrAttackAt (addDir dir (at p))

moveOrAttackAt :: Point -> GameEnv [Event]
moveOrAttackAt to = do
    env <- ask
    let lvl = level env
        p   = player lvl
    case (findMobAt to lvl, findTileAt to lvl) of
        (Just m, _) -> attack p m
        (_, Just t) -> if isPassable t then return [Moved p to] else return []
        otherwise   -> return []

pickup :: DLevel -> Maybe Event
pickup lvl = 
    let is = findItemsAt (at (player lvl)) lvl
    in  ItemPickedUp (player lvl) <$> listToMaybe is

takeStairs :: VerticalDirection -> GameEnv (Maybe Event)
takeStairs v = do
    lvl <- asks level
    let p = player lvl
    return $ do
        t    <- findTileAt (at p) lvl
        lvl' <- getStairLvl t
        if (v == Up && isUpStair t) || (v == Down && isDownStair t) then
            Just (StairsTaken v lvl')
        else
            Nothing

automatePlayer :: Point -> GameEnv [Event]
automatePlayer to = do
    env <- ask
    p   <- asks (player . level)
    let path  = findPath (dfinder (level env)) distance to (at p)
        destE = if isNothing (destination p) then [DestinationSet p to] else []
    if isJust path && length (fromJust path) > 1 then
        (destE ++) <$> moveOrAttackAt (fromJust path !! 1)
    else
        return []
