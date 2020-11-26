module RL.Input where

import RL.Action
import RL.UI.Common (Key(..), KeyMod)
import RL.Event
import RL.Types
import RL.Game

import Data.Maybe (listToMaybe, maybeToList, fromJust, isJust)

charFromKey :: Key -> Maybe Char
charFromKey (KeyChar ch) = Just ch
charFromKey otherwise = Nothing

-- transform key to an event(s) within the game
keyToEvents :: Key -> [KeyMod] -> GameEnv [Event]
keyToEvents k m = do
    env <- ask
    let p = player (level env)
    if menu env == NoMenu then
        case k of
            (KeyChar 'k') -> moveOrAttack North
            (KeyChar 'j') -> moveOrAttack South
            (KeyChar 'h') -> moveOrAttack West
            (KeyChar 'l') -> moveOrAttack East
            (KeyChar 'u') -> moveOrAttack NE
            (KeyChar 'y') -> moveOrAttack NW
            (KeyChar 'b') -> moveOrAttack SW
            (KeyChar 'n') -> moveOrAttack SE
            -- (KeyChar 'r') -> Restart
            (KeyChar '>') -> maybeToList <$> (takeStairs Down)
            (KeyChar '<') -> maybeToList <$> (takeStairs Up)
            (KeyChar 'i') -> return [MenuChange Inventory]
            (KeyChar 'q') -> return [QuitGame]
            (KeyChar 'g') -> return (maybeToList (pickup (level env)))
            (KeyChar ',') -> return (maybeToList (pickup (level env)))
            (KeyChar 'w') -> return [MenuChange Equipment]
            (KeyChar 'W') -> return [MenuChange Equipment]
            (KeyChar 'e') -> return [MenuChange Equipment]
            otherwise     -> return []
    else
        let ch = charFromKey k
            e  = if isJust ch && fromJust ch `elem` inventoryLetters then
                    let i = fromInventoryLetter (fromJust ch) (inventory p)
                    in  Equipped p <$> maybeToList i
                 else []
        in  return (e ++ [MenuChange NoMenu])

moveOrAttack :: Dir -> GameEnv [Event]
moveOrAttack dir = do
    env <- ask
    let lvl = level env
        p   = player lvl
        to  = addDir dir (at p)
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
