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
import RL.Command
import RL.Client
import RL.State

import Control.Monad (when)

-- user client             last action
data UserInput = UserInput Action
user           = UserInput None

-- specific actions that can have inputs on the keyboard
data Action = Move Dir | MoveV VerticalDirection | InventoryA | Restart | Quit | None
data Dir    = North | East | South | West | NE | NW | SE | SW deriving (Eq)

-- user input
instance Client UserInput where
    tick (UserInput a) = do
        updateSeen -- update seen tiles for player
        p <- getPlayer
        when (not (isDead p)) $ do
            case a of Move  d    -> moveDir d
                      MoveV v    -> dispatch (TakeStairs v)
                      InventoryA -> dispatch ToggleInventory
                      otherwise  -> return ()

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
charToAction '>'       = MoveV Down
charToAction '<'       = MoveV Up
charToAction 'i'       = InventoryA
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

-- move player (or attack if mob present)
--            offset
movePlayer :: Point -> Game ()
movePlayer = dispatch . MovePlayer
