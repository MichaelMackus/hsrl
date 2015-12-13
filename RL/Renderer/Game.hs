module RL.Renderer.Game (
    Game(..),
    Level(..),
    module RL.Renderer
) where

import RL.Game
import RL.Renderer

-- game is renderable
instance Renderable Game where
    getSprites g = getSprites (level g) ++ getMsgSprites (messages g)

-- dungeon is renderable
instance Renderable Level where
    getSprites lvl = getMobSprite (player lvl) : map getMobSprite (mobs lvl) ++ getMapSprites (tiles lvl)

-- helper functions since map/mob isn't renderable without context

getMobSprite :: Mob -> Sprite
getMobSprite m = (at m, symbol m : [])

getMapSprites :: Tiles -> [Sprite]
getMapSprites m = map getMapSprites' $ enumerate m
    where
        getMapSprites' (y, ts) = ((0, y), map fromTile ts)
        enumerate              = zip [0..]

getMsgSprites :: [Message] -> [Sprite]
getMsgSprites = take 5 . map toSprite . enumerate
    where
        toSprite (i, m) = ((0, i + 15), m)
        enumerate       = zip [0..]
