module RL.Client.Input (
    UserInput(..),
    Action(..),
    Dir(..),
    user,
    isPlaying,
    charToAction,

    module RL.Client
) where

-- player input
--
-- see movePlayer & attack for movement/attack logic

import RL.Game
import RL.Client
import RL.State
import RL.Random

import Control.Monad (when)

-- user client             last action
data UserInput = UserInput Action
user           = UserInput None

-- specific actions that can have inputs on the keyboard
data Action = Move Dir | Restart | Quit | Up | Down | None
data Dir    = North | East | South | West | NE | NW | SE | SW deriving (Eq)

-- user input
instance Client UserInput where
    tick (UserInput a) = do
        case a of Move d    -> moveDir d
                  Up        -> takeStairs a
                  Down      -> takeStairs a
                  otherwise -> return ()

-- is user playing?
isPlaying :: Action -> Bool
isPlaying Quit      = False
isPlaying otherwise = True

-- Char input to Action
-- todo expose via config
charToAction :: Char -> Action
charToAction 'k'       = Move North
charToAction 'j'       = Move South
charToAction 'h'       = Move West
charToAction 'l'       = Move East
charToAction 'u'       = Move NE
charToAction 'y'       = Move NW
charToAction 'b'       = Move SW
charToAction 'n'       = Move SE
charToAction 'r'       = Restart
charToAction '>'       = Down
charToAction '<'       = Up
charToAction 'q'       = Quit
charToAction otherwise = None

-- move dir in map
moveDir :: Dir -> Game ()
moveDir NW    = movePlayer (-1, -1)
moveDir North = movePlayer ( 0, -1)
moveDir NE    = movePlayer ( 1, -1)
moveDir East  = movePlayer ( 1,  0)
moveDir SE    = movePlayer ( 1,  1)
moveDir South = movePlayer ( 0,  1)
moveDir SW    = movePlayer (-1,  1)
moveDir West  = movePlayer (-1,  0)

takeStairs :: Action -> Game ()
takeStairs a = do
    lvl <- getLevel
    p   <- getPlayer
    let t = findTileAt (at p) lvl

    case (t, a) of
        (Just (StairUp   lvl), Up)   -> changeLevel lvl
        (Just (StairDown lvl), Down) -> changeLevel lvl
        otherwise                    -> return ()

-- move player (or attack if mob present)
--            offset
movePlayer :: Point -> Game ()
movePlayer (0, 0) = return ()
movePlayer off    = do
        newloc <- getloc
        target <- getMobAt newloc
        maybe (moveToTile newloc) attack target
    where
        getloc = fmap addoff getPlayer
        addoff = addOffset off . at

-- basic attack function
attack :: Mob -> Game ()
attack target = do
    p       <- getPlayer
    dmg     <- roll (dmgd p)
    target' <- hurtMob target dmg

    sendMessage ("Player hit! " ++ (show dmg) ++ " damage")
    when (isDead target') (sendMessage ("You killed the " ++ mobName target))

-- move player to a given tile offset
moveToTile :: Point -> Game ()
moveToTile xy = do
        p <- getPlayer
        t <- getTileAt xy
        maybe (return ()) (\t -> when (isPassable t) (movePlayer p)) t
    where movePlayer = setPlayer . moveMobTo xy

-- hurt    mob    dmg
hurtMob :: Mob -> Int -> Game Mob
hurtMob target dmg = do
        ms <- getMobs
        setMobs (map (hurtMob dmg) ms)
        return target'
    where
        hurtMob  dmg m = if m == target then target' else m
        target' = target { hp = (hp target) - dmg }
