module RL.Sprites (
    Env(..),
    DLevel(..),
    getSprites
) where

import RL.Game
import RL.UI.Common as UI
import RL.Util (enumerate)

import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe)
import qualified Data.List as L
import qualified Data.Map as M

white  = (255, 255, 255)
grey   = (200, 200, 200)
dgrey  = (125, 125, 125)
black  = (0, 0, 0)
purple = (204,0,204)
green  = (0,204,0)
yellow = (255,255,0)
red    = (255,0,0)
blue   = (0, 0, 255)

-- game is renderable
getSprites :: Env -> [Sprite]
getSprites e = getMapSprites (level e) ++ getMsgSprites (events e) ++ getStatusSprites (level e) ++ otherWindows e

-- helper functions since map/mob isn't renderable without context

-- OLD faster func
-- getMapSprites lvl = map getRowSprite . enumerate . unenumerate2d . map sym $ M.toList (tiles lvl)
--     where
--         sym (p, t) = (p, head (spriteStr (sprite (p, t))))
--         getRowSprite ((y), ts) = Sprite (0, y) ts white black

getMapSprites :: DLevel -> [Sprite]
getMapSprites lvl = map sprite (M.toList (tiles lvl))
    where
        sprite (p, t) = if canPlayerSee p then tileOrMobSprite lvl p
                        else seenTileSprite lvl p
        canPlayerSee = canSee lvl (player lvl)

        tileColor Floor = white
        tileColor Cavern = grey
        tileColor Rock = grey
        tileColor (StairUp _) = white
        tileColor (StairDown _) = white

        mobColor "Kobold" = purple
        mobColor "Goblin" = green
        mobColor "Grid Bug" = purple
        mobColor "Orc" = yellow
        mobColor otherwise = white

        tileSprite :: DLevel -> (Int, Int) -> Maybe Sprite
        tileSprite lvl p = case findTileAt p lvl of
                               Just  t -> Just (Sprite p (fromTile t:"") (tileColor t) black)
                               Nothing -> Nothing
        itemSprite :: DLevel -> (Int, Int) -> Maybe Sprite
        itemSprite lvl p = case findItemsAt p lvl of
                               (i:_) -> Just (Sprite p (itemSymbol i:"") blue black)
                               []    -> Nothing
        mobSprite :: DLevel -> (Int, Int) -> Maybe Sprite
        mobSprite lvl p = case findTileOrMob p lvl of
                               Right m -> Just (Sprite p (symbol m:"")   (mobColor (mobName m)) black)
                               Left _  -> Nothing
        tileOrMobSprite :: DLevel -> (Int, Int) -> Sprite
        tileOrMobSprite lvl p = let sprites = [mobSprite lvl p, itemSprite lvl p, tileSprite lvl p]
                                    sprite  = listToMaybe (catMaybes sprites)
                                in  if isJust sprite then fromJust sprite
                                    else Sprite p " " black black
        seenTileSprite lvl p = if p `elem` seen lvl then stale (fromJust (listToMaybe (catMaybes [itemSprite lvl p, tileSprite lvl p])))
                               else Sprite p " " black black
        stale spr = spr { spriteFgColor = dgrey, spriteBgColor = black }

getStatusSprites :: DLevel -> [Sprite]
getStatusSprites lvl =
    let p = player lvl
        hpSprite = (mkSprite (64, 15) (show (hp p))) { spriteFgColor = hpColor }
        hpPercent = fromIntegral (hp p) / fromIntegral (mhp p)
        hpColor = if hpPercent >= 1.0 then white
                  else if hpPercent >= 0.7 then green
                  else if hpPercent >= 0.4 then yellow
                  else red
    in [ mkSprite (60, 15) "HP: ", hpSprite, mkSprite (66, 15) ("/" ++ show (mhp p)),
         mkSprite (60, 16) ("Depth: " ++ show (depth lvl)) ]

otherWindows :: Env -> [Sprite]
otherWindows e
    | isViewingInventory e =
        let lvl = level e
            inv = L.groupBy itemType (inventory (player lvl))
            eq  = L.groupBy itemType (equipmentToList (equipment (player lvl)))
        in  mkSprites (0,  0) ([ "Inventory:", " " ] ++ map showInvItem (zip inventoryLetters (concat inv))) ++
            mkSprites (40, 0) ([ "Equipped:", " " ] ++ map showItem (concat eq))
    | otherwise = []
        where showInvItem (ch, i) = ch:(showItem i)
              showItem i = " - " ++ show i

getMsgSprites :: [Event] -> [Sprite]
getMsgSprites evs = let recentMsgs = catMaybes (map toMessage (getEventsAfterTurns 2 evs))
                        staleMsgs  = catMaybes (map toMessage (getEventsAfterTurns 11 (getEventsBeforeTurns 2 evs)))
                        msgs       = zip recentMsgs (repeat white) ++ zip staleMsgs (repeat grey)
                    in  mkColoredSprites (0, 15) . reverse . take 9 $ msgs

mkSprites :: UI.Point -> [String] -> [Sprite]
mkSprites (offx, offy) = map toSprite . enumerate
    where
        toSprite (i, s) = Sprite (offx, i + offy) s white black

mkColoredSprites :: UI.Point -> [(String, Color)] -> [Sprite]
mkColoredSprites (offx, offy) = map toSprite . enumerate
    where
        toSprite (i, (s, fg)) = Sprite (offx, i + offy) s fg black

mkSprite :: UI.Point -> String -> Sprite
mkSprite xy s = Sprite xy s white black
