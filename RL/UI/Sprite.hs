module RL.UI.Sprite (
    Message(..),
    Sprite(..),
    getSprites,
    spriteAt
) where

import RL.Game
import RL.UI.Common as UI
import RL.Util (enumerate, equating)

import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe)
import qualified Data.List as L
import qualified Data.Map as M

white   = (255, 255, 255)
grey    = (200, 200, 200)
dgrey   = (125, 125, 125)
black   = (0, 0, 0)
purple  = (204,0,204)
green   = (0,204,0)
yellow  = (255,255,0)
red     = (255,0,0)
blue    = (0, 0, 255)
brown   = (153, 76, 0)
orange  = (255, 128, 0)
lyellow = (255, 255, 204)

data Message = Message {
    messagePos :: Point,
    message :: String,
    messageFgColor :: Color,
    messageBgColor :: Color
} deriving Show

data Sprite   = Sprite {
    spritePos :: Point,
    spriteChar :: Char,
    spriteFgColor :: Color,
    spriteBgColor :: Color
} deriving Show

-- game is renderable
getSprites :: Env -> [Either Message Sprite]
getSprites e = map Right (getMapSprites (level e)) ++ map Left (getMsgSprites (events e)) ++ map Left (getStatusSprites (level e)) ++ map Left (otherWindows e)

-- helper functions since map/mob isn't renderable without context

-- OLD faster func
-- getMapSprites lvl = map getRowSprite . enumerate . unenumerate2d . map sym $ M.toList (tiles lvl)
--     where
--         sym (p, t) = (p, head (spriteStr (sprite (p, t))))
--         getRowSprite ((y), ts) = Sprite (0, y) ts white black

spriteAt :: DLevel -> Point -> Sprite
spriteAt lvl p = if canPlayerSee p then tileOrMobSprite lvl p
                 else seenTileSprite lvl p
    where
        canPlayerSee p = canSee lvl (player lvl) p || canSense lvl (player lvl) p

        tileColor Floor = white
        tileColor Cavern = grey
        tileColor Rock = grey
        tileColor (StairUp _) = white
        tileColor (StairDown _) = white

        mobColor "Kobold" = purple
        mobColor "Goblin" = green
        mobColor "Grid Bug" = purple
        mobColor "Orc" = yellow
        mobColor "Zombie" = dgrey
        mobColor otherwise = white

        itemColor (Item "Blue"          (Potion _)) = blue
        itemColor (Item "Yellow"        (Potion _)) = yellow
        itemColor (Item "Black"         (Potion _)) = dgrey
        itemColor (Item "Red"           (Potion _)) = red
        itemColor (Item "White"         (Potion _)) = white
        itemColor (Item "Green"         (Potion _)) = green
        itemColor (Item "Orange"        (Potion _)) = orange
        itemColor (Item "Leather Armor" (Armor  _)) = brown
        itemColor (Item "Plate Mail"    (Armor  _)) = white
        itemColor (Item "Full Plate"    (Armor  _)) = white
        itemColor (Item "Small Shield"  (Armor  _)) = grey
        itemColor (Item "Tower Shield"  (Armor  _)) = white
        itemColor (Item n               (Armor  _)) = grey
        itemColor (Item "Quarterstaff"  (Weapon _)) = brown
        itemColor (Item "Dagger"        (Weapon _)) = dgrey
        itemColor (Item "Mace"          (Weapon _)) = grey
        itemColor (Item "Ornate Sword"  (Weapon _)) = yellow
        itemColor (Item n               (Weapon _)) = lyellow
        itemColor (Item n               (Scroll _)) = white
        itemColor otherwise = white

        tileSprite :: DLevel -> (Int, Int) -> Maybe Sprite
        tileSprite lvl p = case findTileAt p lvl of
                               Just  t -> Just (Sprite p (fromTile t) (tileColor t) black)
                               Nothing -> Nothing
        itemSprite :: DLevel -> (Int, Int) -> Maybe Sprite
        itemSprite lvl p = case findItemsAt p lvl of
                               (i:_) -> Just (Sprite p (itemSymbol i) (itemColor i) black)
                               []    -> Nothing
        mobSprite :: DLevel -> (Int, Int) -> Maybe Sprite
        mobSprite lvl p = case findTileOrMob p lvl of
                               Right m -> if isVisible m then
                                            Just (Sprite p (symbol m) (mobColor (mobName m)) black)
                                          else if isPlayer m then
                                            Just (Sprite p ' ' white (50,50,50))
                                          else
                                            Nothing
                               Left _  -> Nothing
        tileOrMobSprite :: DLevel -> (Int, Int) -> Sprite
        tileOrMobSprite lvl p = let sprites = [mobSprite lvl p, itemSprite lvl p, tileSprite lvl p]
                                    sprite  = listToMaybe (catMaybes sprites)
                                in  if isJust sprite then fromJust sprite
                                    else Sprite p ' ' black black
        seenTileSprite lvl p = if p `elem` seen lvl then stale (fromJust (listToMaybe (catMaybes [itemSprite lvl p, tileSprite lvl p])))
                               else Sprite p ' ' black black
        stale spr = spr { spriteFgColor = dgrey, spriteBgColor = black }

getMapSprites :: DLevel -> [Sprite]
getMapSprites lvl = map (spriteAt lvl . fst) . M.toList $ tiles lvl

getStatusSprites :: DLevel -> [Message]
getStatusSprites lvl =
    let p = player lvl
        hpSprite = (mkMessage (64, 15) (show (hp p))) { messageFgColor = hpColor }
        hpPercent = fromIntegral (hp p) / fromIntegral (mhp p)
        hpColor = if hpPercent >= 1.0 then white
                  else if hpPercent >= 0.7 then green
                  else if hpPercent >= 0.4 then yellow
                  else red
    in [ mkMessage (60, 15) "HP: ", hpSprite, mkMessage (66, 15) ("/" ++ show (mhp p)),
         mkMessage (60, 16) ("Depth: " ++ show (depth lvl)) ]

otherWindows :: Env -> [Message]
otherWindows e
    | isViewingInventory e =
        let lvl = level e
            inv = L.groupBy (equating itemType) (inventory (player lvl))
            eq  = L.groupBy (equating itemType) (equipmentToList (equipment (player lvl)))
        in  mkMessages (0,  0) ([ "Inventory:", " " ] ++ map showInvItem (zip inventoryLetters (concat inv))) ++
            mkMessages (40, 0) ([ "Equipped:", " " ] ++ map showItem (concat eq))
    | otherwise = []
        where showInvItem (ch, i) = ch:(showItem i)
              showItem i = " - " ++ show i

getMsgSprites :: [Event] -> [Message]
getMsgSprites evs = let recentMsgs = catMaybes (map toMessage (getEventsAfterTurns 2 evs))
                        staleMsgs  = catMaybes (map toMessage (getEventsAfterTurns 11 (getEventsBeforeTurns 2 evs)))
                        msgs       = zip recentMsgs (repeat white) ++ zip staleMsgs (repeat grey)
                    in  mkColoredMessages (0, 15) . reverse . take 9 $ msgs

mkMessages :: Point -> [String] -> [Message]
mkMessages (offx, offy) = map toSprite . enumerate
    where
        toSprite (i, s) = Message (offx, i + offy) s white black

mkColoredMessages :: Point -> [(String, Color)] -> [Message]
mkColoredMessages (offx, offy) = map toSprite . enumerate
    where
        toSprite (i, (s, fg)) = Message (offx, i + offy) s fg black

mkMessage :: Point -> String -> Message
mkMessage xy s = Message xy s white black
