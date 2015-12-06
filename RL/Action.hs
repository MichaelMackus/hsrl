module RL.Action (Action, Dir, doAction, isPlaying) where

import RL.Draw
import RL.Game
import RL.IO
import RL.Map
import RL.Mob
import RL.Random
import RL.State

import Data.Maybe
import Control.Applicative
import Control.Monad.State
import UI.HSCurses.Curses

-- player actions

-- specific actions that can have inputs (on the keyboard)
data Action = Quit | Move Dir | None
data Dir    = North | East | South | West | NE | NW | SE | SW

-- this is triggered from the game loop for a particular action
doAction :: GameState Bool -- is playing ?
doAction = do
        a <- getAction
        case a of Move d    -> moveDir d
                  otherwise -> return ()
        isPlaying a
    where
        getAction                = io $ getAction' <$> getCh
        getAction' (KeyChar 'q') = Quit
        getAction' (KeyChar 'k') = Move North
        getAction' (KeyChar 'j') = Move South
        getAction' (KeyChar 'h') = Move West
        getAction' (KeyChar 'l') = Move East
        getAction' (KeyChar 'u') = Move NE
        getAction' (KeyChar 'y') = Move NW
        getAction' (KeyChar 'b') = Move SW
        getAction' (KeyChar 'n') = Move SE
        getAction' otherwise     = None

-- change playing status on action
isPlaying :: Action -> GameState Bool
isPlaying Quit      = return False         -- not playing
isPlaying otherwise = not <$> winCondition -- keep playing if we haven't won
    where winCondition = fmap null getMobs

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
movePlayer :: Point -> GameState ()
movePlayer (0, 0) = return () -- todo advance clock
movePlayer off    = do
        newloc <- getloc
        target <- getMobAt newloc
        maybe (moveToTile newloc) attack target
    where
        getloc = fmap addoff getPlayer
        addoff = addOffset off . at

-- move player to a given tile offset
moveToTile :: Point -> GameState ()
moveToTile xy = do
        p <- getPlayer
        t <- getTileAt xy
        when (isPassable t) $ movePlayer p
    where movePlayer = setPlayer . moveMobTo xy

-- basic attack function, TODO refactor with MobManager & AI
attack :: Mob -> GameState ()
attack target = do
        p    <- getPlayer
        dmg  <- dmgd p
        ms   <- fmap (hurtMobs dmg) getMobs
        msgs <- sendMessages ["dmg: " ++ show dmg, "hp: " ++ show (hp p)]
        game <- get
        put $ game { mobs = ms, messages = msgs }
    where
        hurtMobs dmg   = filterMobs . map (hurtMob dmg)
        hurtMob  dmg m = if matchMob m then hurtMob' dmg m else m
        hurtMob' dmg m = Mob { symbol = symbol target, at = at target, hp = (hp target) - dmg }
        matchMob     m = symbol target == symbol m
        dmgd         _ = 1 `d` 4 -- TODO abstract
