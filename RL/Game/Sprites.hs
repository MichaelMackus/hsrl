module RL.Game.Sprites (
    Env(..),
    DLevel(..),
    toMessage,
    getSprites
) where

import RL.Game
import RL.UI.Common as UI
import RL.Util (enumerate)

import Data.Maybe (catMaybes)
import qualified Data.List as L

-- game is renderable
getSprites e = otherWindows e ++ getSprites' (level e) ++ getMsgSprites (events e) ++ getStatusSprites (level e)
    where
        -- dungeon is renderable
        getSprites' d = getMapSprites d

-- helper functions since map/mob isn't renderable without context

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

otherWindows :: Env -> [Sprite]
otherWindows e
    | isViewingInventory e =
        let lvl = level e
            inv = L.groupBy itemType (inventory (player lvl))
            eq  = L.groupBy itemType (equipment (player lvl))
        in  mkSprites (0, 0) (map showItem (concat eq ++ concat inv))
    | otherwise = []
        where showItem i = " - " ++ show i

getMsgSprites :: [Event] -> [Sprite]
getMsgSprites = mkSprites (0, 15) . reverse . take 10 . catMaybes . map toMessage

toMessage :: Event -> Maybe String
toMessage (Attacked attacker target dmg)
    | isPlayer attacker = Just $ "You hit the " ++ mobName target ++ " for " ++ show dmg ++ " damage"
    | isPlayer target = Just $ "You were hit by the " ++ mobName attacker ++ " for " ++ show dmg
    | otherwise = Just $ "The " ++ mobName attacker ++ " hit the " ++ mobName target ++ " for " ++ show dmg
toMessage (Missed attacker target)
    | isPlayer attacker = Just $ "You missed the " ++ mobName target
    | isPlayer target = Just $ "The " ++ mobName attacker ++ " missed"
    | otherwise = Just $ "The " ++ mobName attacker ++ " missed the " ++ mobName target
toMessage (Died m)
    | isPlayer m = Just $ "You died!"
    -- TODO different event for killed
    | otherwise  = Just $ "You killed the " ++ mobName m
toMessage (StairsTaken Up) = Just $ "You've gone up stairs."
toMessage (StairsTaken Down) = Just $ "You've gone down stairs."
toMessage (Waken m) = Just $ "The " ++ mobName m ++ " has waken up."
toMessage (Slept m) = Just $ "The " ++ mobName m ++ " has fell asleep."
toMessage otherwise = Nothing

mkSprites :: UI.Point -> [String] -> [Sprite]
mkSprites (offx, offy) = map toSprite . enumerate
    where
        toSprite (i, s) = ((offx, i + offy), s)
