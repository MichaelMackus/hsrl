module RL.Renderer.Game (
    Env(..),
    DLevel(..),
    module RL.Renderer
) where

import RL.Game
import RL.Renderer
import RL.Util (enumerate)

-- game is renderable
instance Renderable Env where
    getSprites e = getSprites (level e) ++ getMsgSprites (messages e)

-- -- dungeon is renderable
-- instance Renderable Level where
--     getSprites lvl = getMobSprite (player lvl) : map getMobSprite (mobs lvl) ++ getMapSprites (tiles lvl)

-- dungeon is renderable
instance Renderable DLevel where
    getSprites d = getMapSprites d

-- helper functions since map/mob isn't renderable without context

getMobSprite :: Mob -> Sprite
getMobSprite m = (at m, symbol m : [])

getMapSprites :: DLevel -> [Sprite]
getMapSprites lvl = map getRowSprite . enumerate . toTiles $ iterMap sym lvl
    where
        sym p t = either id symbol (findPoint p lvl)
        getRowSprite ((y), ts) = ((0, y), ts)

getMsgSprites :: [Message] -> [Sprite]
getMsgSprites = take 10 . map toSprite . enumerate
    where
        toSprite (i, m) = ((0, i + 15), m)
