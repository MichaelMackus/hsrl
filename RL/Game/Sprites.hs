module RL.Game.Sprites (
    Env(..),
    DLevel(..),
    toMessage,
    getSprites
) where

import RL.Game
import RL.UI.Common as UI
import RL.Util (enumerate, unenumerate2d)

import Data.Maybe (catMaybes)
import qualified Data.List as L
import qualified Data.Map as M

white = (255, 255, 255)
grey  = (125, 125, 125)
black = (0, 0, 0)

-- game is renderable
getSprites :: Env -> [Sprite]
getSprites e = getSprites' (level e) ++ getMsgSprites (events e) ++ getStatusSprites (level e) ++ otherWindows e
    where
        -- dungeon is renderable
        getSprites' d = getMapSprites d

-- helper functions since map/mob isn't renderable without context

getMapSprites :: DLevel -> [Sprite]
getMapSprites lvl = map sprite (M.toList (tiles lvl))
    where
        sprite (p, t) = if canPlayerSee p then tileSprite lvl p
                        else Sprite p " " black black
        canPlayerSee = canSee lvl (player lvl)

        tileColor Floor = white
        tileColor Cavern = grey
        tileColor Rock = grey
        tileColor (StairUp _) = white
        tileColor (StairDown _) = white

        tileSprite :: DLevel -> (Int, Int) -> Sprite
        tileSprite lvl p = case findTileOrMob p lvl of
                               Left  t -> Sprite p (fromTile t:"") (tileColor t) black
                               Right m -> Sprite p (symbol m:"")   white black

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
getMsgSprites = mkSprites (0, 15) . reverse . take 9 . catMaybes . map toMessage

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
        toSprite (i, s) = Sprite (offx, i + offy) s white black
