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
    getSprites e = getSprites (level e) ++ getMsgSprites (messages e) ++ getStatusSprites (level e)

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
getMapSprites lvl = map getRowSprite . enumerate . map (map fromTile) . toTiles $ iterMap sym lvl
    where
        sym p t = Other (findPoint p lvl)
        getRowSprite ((y), ts) = ((0, y), ts)

getStatusSprites :: DLevel -> [Sprite]
getStatusSprites lvl =
    let p = player lvl
        statusLine = [ "HP: " ++ show (hp p) ++ "/" ++ show (mhp p),
                       "Depth: " ++ show (depth lvl) ]
    in  mkSprites (60, 15) statusLine

getMsgSprites :: [Message] -> [Sprite]
getMsgSprites = take 10 . mkSprites (0, 15)

mkSprites :: Point -> [String] -> [Sprite]
mkSprites (offx, offy) = map toSprite . enumerate
    where
        toSprite (i, s) = ((offx, i + offy), s)
