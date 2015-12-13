module RL.Game (GameState, Game(..), Message, Client(..)) where

import RL.Mob
import RL.Map
import RL.Renderer

import Control.Monad.State
import Control.Monad.Reader
import System.Random

-- global game state
type GameState = StateT Game Renderer

-- global game
data Game = Game {
    level    :: Level,       -- current dungeon layout/mobs
    messages :: [Message],   -- things that happened so far
    seed     :: Maybe StdGen -- randomization
}

-- simple string event
type Message = String

-- represents a client that does something to the state
-- (see: RL.Input and RL.AI)
class Client c where
    -- 1 turn tick, modifies   state & Client
    tick :: Client c => c -> GameState c

-- game is renderable
instance Renderable Game where
    getSprites g = getSprites (level g) ++ getMsgSprites (messages g)

-- dungeon is renderable
instance Renderable Level where
    getSprites lvl = getMobSprite (player lvl) : map getMobSprite (mobs lvl) ++ getMapSprites (tiles lvl)

-- helper functions since map/mob isn't renderable without context

getMobSprite :: Mob -> Sprite
getMobSprite m = (at m, symbol m : [])

getMapSprites :: Map -> [Sprite]
getMapSprites m = map getMapSprites' $ enumerate m
    where
        getMapSprites' (y, ts) = ((0, y), map fromTile ts)
        enumerate              = zip [0..]

getMsgSprites :: [Message] -> [Sprite]
getMsgSprites = take 5 . map toSprite . enumerate
    where
        toSprite (i, m) = ((0, i + 15), m)
        enumerate       = zip [0..]

