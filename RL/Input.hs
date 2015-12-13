module RL.Input (UserInput(..), user, isPlaying) where

-- player input
--
-- see movePlayer & attack for movement/attack logic

import RL.IO
import RL.Game
import RL.Mob
import RL.Map
import RL.State
import RL.Random

import Control.Monad.Reader
import Control.Monad.State
import Graphics.Vty

-- user client             last action
data UserInput = UserInput Action

-- default user input client
user = UserInput None

-- is user playing?
isPlaying :: UserInput -> Bool
isPlaying (UserInput Quit)      = False
isPlaying (UserInput otherwise) = True

-- user input
instance Client UserInput where
    tick u = do
        a <- getAction
        case a of Move d    -> moveDir d
                  otherwise -> return ()

        return $ UserInput a

-- specific actions that can have inputs on the keyboard
data Action = Quit | Move Dir | None                          deriving (Eq)
data Dir    = North | East | South | West | NE | NW | SE | SW deriving (Eq)

-- gets game Action from user input
getAction :: GameState Action
getAction = do
        disp <- ask
        fmap getAction' (io $ nextEvent disp)
    where
        -- Event -> Action
        getAction' (EvKey (KChar c) _) = charToAction c
        getAction' otherwise           = None

-- move dir in map
moveDir :: Dir -> GameState ()
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
movePlayer :: Point -> GameState ()
movePlayer (0, 0) = return ()
movePlayer off    = do
        newloc <- getloc
        target <- getMobAt newloc
        maybe (moveToTile newloc) attack target
    where
        getloc = fmap addoff getPlayer
        addoff = addOffset off . at

-- basic attack function
attack :: Mob -> GameState ()
attack target = do
    p    <- getPlayer
    dmg  <- roll $ dmgd p

    hurtMob target dmg
    sendMessage $ "Player hit! " ++ (show dmg) ++ " damage"

-- helper functions

-- Char input to Action
-- todo expose via config
charToAction :: Char -> Action
charToAction 'q'       = Quit
charToAction 'k'       = Move North
charToAction 'j'       = Move South
charToAction 'h'       = Move West
charToAction 'l'       = Move East
charToAction 'u'       = Move NE
charToAction 'y'       = Move NW
charToAction 'b'       = Move SW
charToAction 'n'       = Move SE
charToAction otherwise = None

-- move player to a given tile offset
moveToTile :: Point -> GameState ()
moveToTile xy = do
        p <- getPlayer
        t <- getTileAt xy
        when (isPassable t) $ movePlayer p
    where movePlayer = setPlayer . moveMobTo xy

-- hurt    mob    dmg
hurtMob :: Mob -> Int -> GameState ()
hurtMob target dmg = do
        ms <- getMobs
        setMobs $ map (hurtMob dmg) ms
    where
        hurtMob  dmg m = if m == target then hurtMob' dmg m else m
        hurtMob' dmg m = m { hp = (hp target) - dmg }
