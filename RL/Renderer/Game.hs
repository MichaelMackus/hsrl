module RL.Renderer.Game (
    Env(..),
    DLevel(..),
    toMessage,
    module RL.Renderer
) where

import RL.Game
import RL.Renderer
import RL.Util (enumerate)

import Data.Maybe (catMaybes)

-- game is renderable
instance Renderable Env where
    getSprites e = getSprites (level e) ++ getMsgSprites (events e) ++ getStatusSprites (level e)

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

getMsgSprites :: [Event] -> [Sprite]
getMsgSprites = mkSprites (0, 15) . reverse . take 10 . catMaybes . map toMessage

toMessage :: Event -> Maybe String
toMessage (Attacked attacker target dmg)
    | isPlayer attacker = Just $ "You hit the " ++ mobName target ++ " for " ++ show dmg ++ " damage"
    | isPlayer target = Just $ "You were hit by the " ++ mobName attacker ++ " for " ++ show dmg
    | otherwise = Just $ "The " ++ mobName attacker ++ " hit the " ++ mobName target ++ " for " ++ show dmg
toMessage (Died m)
    | isPlayer m = Just $ "You died!"
    -- TODO different event for killed
    | otherwise  = Just $ "You killed the " ++ mobName m
toMessage (StairsTaken Up) = Just $ "You've gone up stairs."
toMessage (StairsTaken Down) = Just $ "You've gone down stairs."
toMessage otherwise = Nothing

mkSprites :: Point -> [String] -> [Sprite]
mkSprites (offx, offy) = map toSprite . enumerate
    where
        toSprite (i, s) = ((offx, i + offy), s)
