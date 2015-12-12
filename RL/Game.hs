module RL.Game (Game(..), Client(..), Message, GameState) where

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
    clients  :: [Client],    -- player and AI client(s)
    messages :: [Message],   -- things that happened so far
    seed     :: Maybe StdGen -- randomization
}

-- represents AI or player client
data Client = AIClient | UIClient

-- simple string event
type Message = String

-- game is renderable
instance Renderable Game where
    getSprites g = getSprites (level g)

-- dungeon is renderable
instance Renderable Level where
    getSprites lvl = getSprite (player lvl) : map getSprite (mobs lvl) ++ getMapSprites (tiles lvl)

-- mobs are renderable
instance Renderable Mob where
    getSprite m = (at m, symbol m : [])

-- helper function since Map can't be directly rendered (its a type alias)
-- todo make Map a dedicated data type and Renderable
getMapSprites :: Map -> [Sprite]
getMapSprites m = map getMapSprites' $ enumerate m
    where
        getMapSprites' (y, ts) = ((0, y), map fromTile ts)
        enumerate              = zip [0..]
