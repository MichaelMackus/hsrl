module RL.Draw ( draw, sendMessages ) where

import RL.Game
import RL.IO
import RL.Map
import RL.Mob
import RL.State

import Control.Monad.State
import UI.HSCurses.Curses

-- drawing functions
--
-- These functions aid with drawing state from the map
--
-- TODO remove hscurses

-- master draw function: draws player, mobs, and tiles
draw :: GameState ()
draw = do
    getMap >>= drawMap
    getMobsWithPlayer >>= drawMobs
    getMessages >>= drawMessages
    getSeed >>= debug
    io refresh
    clearMessages                   -- clear messages for next draw

drawMap :: Map -> GameState ()
drawMap m = io $ mapM_ drawTile (enumerateMap m)
    where drawTile (p, t) = drawTileAt p t

drawMobs :: [Mob] -> GameState ()
drawMobs = io . mapM_ drawMob
    where drawMob m = drawCharAt (at m) (symbol m)

drawTileAt :: Point -> Tile -> IO ()
drawTileAt p = drawCharAt p . fromTile

drawCharAt :: Point -> Char -> IO ()
drawCharAt (x, y) = mvAddCh' y x
    where mvAddCh' y x c = mvWAddStr stdScr y x [c]

sendMessages :: [Message] -> GameState [Message]
sendMessages msgs = fmap (++ msgs) getMessages

clearMessages :: GameState ()
clearMessages = do
    game <- get
    put $ game { messages = [] }

drawMessages :: [Message] -> GameState ()
drawMessages = mapM_ drawMessage . enumerate
    where
        drawMessage  (i,    msg) = drawMessage' (0, 15 + i) msg
        drawMessage' (x, y) msg  = io $ mvWAddStr stdScr y x msg
        enumerate                = zip [0..]
