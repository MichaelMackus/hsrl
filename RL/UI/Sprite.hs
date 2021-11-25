module RL.UI.Sprite (
    Message(..),
    Sprite(..),
    getSprites,
    spriteAt,
    seenWallType,
    WallType(..),
    toMessage
) where

import RL.Game
import RL.UI.Common as UI
import RL.Util (enumerate, equating, groupBy')

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
blue    = (0, 128, 255)
brown   = (153, 76, 0)
orange  = (255, 128, 0)
lyellow = (255, 255, 204)

data Message = Message {
    messagePos :: Point,
    message :: String,
    messageFgColor :: Color,
    messageBgColor :: Color
} deriving (Show, Eq)

data Sprite   = Sprite {
    spritePos :: Point,
    spriteChar :: Char,
    spriteFgColor :: Color,
    spriteBgColor :: Color
} deriving (Show, Eq)

-- game is renderable
getSprites :: Env -> [Either Message Sprite]
getSprites e = map Right (getMapSprites e) ++ map Left (getMsgSprites e) ++ map Left (getStatusSprites (level e)) ++ map Left (otherWindows e)

-- helper functions since map/mob isn't renderable without context

-- TODO messages are displaying *above* inventory
spriteAt :: Env -> Point -> Sprite
spriteAt env p = if menu env == TargetMenu && target (player lvl) == Just p then targetingSprite p
                 else if canPlayerSee p then tileOrMobSprite lvl p
                 else seenTileSprite lvl p
    where
        lvl = level env
        canPlayerSee p = canSee lvl (player lvl) p || canSense lvl (player lvl) p

        targetingSprite p = Sprite p '*' red black

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
        itemColor (Item "Bow"           (Weapon _)) = brown
        itemColor (Item "Arrow"         (Weapon _)) = brown
        itemColor (Item "Dagger"        (Weapon _)) = dgrey
        itemColor (Item "Mace"          (Weapon _)) = grey
        itemColor (Item "Ornate Sword"  (Weapon _)) = yellow
        itemColor (Item n               (Weapon _)) = lyellow
        itemColor (Item n               (Scroll _)) = white
        itemColor otherwise = white

        featureColor (Chest    _) = yellow
        featureColor (Fountain 0) = grey
        featureColor (Fountain _) = blue
        featureColor Altar        = grey

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
        featureSprite :: DLevel -> (Int, Int) -> Maybe Sprite
        featureSprite lvl p = case L.lookup p (features lvl) of
                                Just f  -> Just (Sprite p (fromFeature f) (featureColor f) black)
                                Nothing -> Nothing
        tileOrMobSprite :: DLevel -> (Int, Int) -> Sprite
        tileOrMobSprite lvl p = let sprites = [mobSprite lvl p, featureSprite lvl p, itemSprite lvl p, tileSprite lvl p]
                                    sprite  = listToMaybe (catMaybes sprites)
                                in  if isJust sprite then fromJust sprite
                                    else Sprite p ' ' black black
        seenTileSprite lvl p = if p `elem` seen lvl then stale (fromJust (listToMaybe (catMaybes [featureSprite lvl p, itemSprite lvl p, tileSprite lvl p])))
                               else Sprite p ' ' black black
        stale spr = spr { spriteFgColor = dgrey, spriteBgColor = black }

getMapSprites :: Env -> [Sprite]
getMapSprites env = map (spriteAt env . fst) . M.toList $ tiles (level env)

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
    | menu e `elem` [Inventory, ProjectileMenu] =
        let lvl = level e
            inv = groupItems (inventory (player lvl))
            eq  = groupItems (equipmentToList (equipment (player lvl)))
        in  mkMessages (0,  0) ([ "Inventory:", " " ] ++ map showInvItem (zip inventoryLetters inv)) ++
            mkMessages (40, 0) ([ "Equipped:", " " ] ++ map showItem eq)
    | otherwise = []
        where showInvItem (ch, i) = ch:(showItem i)
              p              = player (level e)
              showItem (1,i) = " - " ++ showIdentified (identified p) i
              showItem (n,i) = " - " ++ show n ++ " " ++ showIdentified (identified p) i ++ "s" -- TODO pluralize

getMsgSprites :: Env -> [Message]
getMsgSprites env = let evs        = events env
                        recentMsgs = catMaybes (map (toMessage env) (getEventsAfterTurns 2 evs))
                        staleMsgs  = catMaybes (map (toMessage env) (getEventsAfterTurns 11 (getEventsBeforeTurns 2 evs)))
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

data WallType = Wall   | WallNS | WallNE | WallNW  | WallNSE | WallNSW | WallNEW
              | WallEW | WallSE | WallSW | WallSEW | WallNESW deriving (Eq, Ord)

wallHasE WallNE    = True
wallHasE WallNSE   = True
wallHasE WallNEW   = True
wallHasE WallEW    = True
wallHasE WallSE    = True
wallHasE WallSEW   = True
wallHasE WallNESW  = True
wallHasE otherwise = False
wallHasW WallNW    = True
wallHasW WallNSW   = True
wallHasW WallNEW   = True
wallHasW WallEW    = True
wallHasW WallSW    = True
wallHasW WallSEW   = True
wallHasW WallNESW  = True
wallHasW otherwise = False
wallHasN WallNESW  = True
wallHasN t         = t <= WallNEW && t > Wall
wallHasS WallNS    = True
wallHasS WallNSE   = True
wallHasS WallNSW   = True
wallHasS t         = t <= WallNESW && t >= WallSE

-- which part of the wall is seen
seenWallType :: Env -> Point -> Maybe WallType
seenWallType env (x,y) =
    let lvl  = level env
        f p' = maybe False (not . isWall) (findTileAt p' lvl) && p' `elem` (seen lvl)
        fixWall Wall = if ((x+1),y) `elem` (seen lvl) || ((x-1),y) `elem` (seen lvl) then WallEW
                       else if (x,y+1) `elem` (seen lvl) || (x,y-1) `elem` (seen lvl) then WallNS
                       else Wall
        fixWall t    = t
    in  fixWall <$> filterWallType f (x,y) <$> wallType env (x,y)

filterWallType :: (Point -> Bool) -> Point -> WallType -> WallType
filterWallType f (x,y) t =
    let ne    = (x+1,y-1)
        nw    = (x-1,y-1)
        se    = (x+1,y+1)
        sw    = (x-1,y+1)
        north = (x,  y-1)
        south = (x,  y+1)
        west  = (x-1,y)
        east  = (x+1,y)
    in  if t == WallNESW && (f ne || f nw) && (f se || f sw) then t
        else if wallHasN t && wallHasE t && wallHasW t && ((f ne && f nw) || ((f ne || f nw) && f south)) then WallNEW
        else if wallHasS t && wallHasE t && wallHasW t && ((f se && f sw) || ((f se || f sw) && f north)) then WallSEW
        else if wallHasN t && wallHasS t && wallHasE t && ((f ne && f se) || ((f ne || f se) && f west)) then WallNSE
        else if wallHasN t && wallHasS t && wallHasW t && ((f nw && f sw) || ((f nw || f sw) && f east)) then WallNSW
        else if wallHasS t && wallHasW t && (f sw || (f east && f north)) then WallSW
        else if wallHasS t && wallHasE t && (f se || (f west && f north)) then WallSE
        else if wallHasN t && wallHasW t && (f nw || (f east && f south)) then WallNW
        else if wallHasN t && wallHasE t && (f ne || (f west && f south)) then WallNE
        else if (wallHasN t || wallHasS t) && (f west  || f east)  then WallNS
        else if (wallHasW t || wallHasE t) && (f north || f south) then WallEW
        else Wall

-- wall type for different wall tiles
wallType :: Env -> Point -> Maybe WallType
wallType env p =
    if not (maybe False isWall (findTileAt p (level env))) then Nothing
    else if wallN env p && wallS env p && wallE env p && wallW env p then Just WallNESW
    else if wallN env p && wallS env p && wallE env p then Just WallNSE
    else if wallN env p && wallS env p && wallW env p then Just WallNSW
    else if wallN env p && wallW env p && wallE env p then Just WallNEW
    else if wallS env p && wallW env p && wallE env p then Just WallSEW
    else if wallN env p && wallS env p then Just WallNS
    else if wallW env p && wallS env p then Just WallSW
    else if wallE env p && wallS env p then Just WallSE
    else if wallW env p && wallN env p then Just WallNW
    else if wallE env p && wallN env p then Just WallNE
    else if wallE env p then Just WallEW
    else if wallW env p then Just WallEW
    else if wallS env p then Just WallNS
    else if wallN env p then Just WallNS
    else Just Wall

wallN  :: Env -> Point -> Bool
wallN env (x,y) = maybe False isWall (findTileAt (x, y - 1) (level env))
wallE  :: Env -> Point -> Bool
wallE env (x,y) = maybe False isWall (findTileAt (x + 1, y) (level env))
wallS  :: Env -> Point -> Bool
wallS env (x,y) = maybe False isWall (findTileAt (x, y + 1) (level env))
wallW  :: Env -> Point -> Bool
wallW env (x,y) = maybe False isWall (findTileAt (x - 1, y) (level env))

isWall :: Tile -> Bool
isWall Rock = True
isWall otherwise = False


toMessage :: Env -> Event -> Maybe String
toMessage e (NewGame) = Just $ "You delve underground, searching for your ancestors' sword."
toMessage e (Escaped) = Just $ "There is no escape. You must avenge your ancestors!"
toMessage e (Crit attacker target)
    | isPlayer attacker = Just $ "CRITICAL HIT!"
toMessage e (Damaged attacker target dmg)
    | isPlayer attacker && isPlayer target = Just $ "You hurt yourself for " ++ show dmg ++ " damage! Be more careful!"
    | isPlayer attacker = Just $ "You hit the " ++ mobName target ++ " for " ++ show dmg ++ " damage"
    | isPlayer target = Just $ "You were hit by the " ++ mobName attacker ++ " for " ++ show dmg
    | otherwise = Just $ "The " ++ mobName attacker ++ " hit the " ++ mobName target ++ " for " ++ show dmg
toMessage e (Missed attacker target)
    | isPlayer attacker = Just $ "You missed the " ++ mobName target
    | isPlayer target = Just $ "The " ++ mobName attacker ++ " missed"
    | otherwise = Just $ "The " ++ mobName attacker ++ " missed the " ++ mobName target
toMessage e (Died m)
    | isPlayer m = Just $ "You died! Press space to quit or r to restart a new game."
    | otherwise  = Just $ "You killed the " ++ mobName m
toMessage e (StairsTaken Up _) = Just $ "You've gone up stairs."
toMessage e (StairsTaken Down _) = Just $ "You've gone down stairs."
toMessage e (Waken m) | canSee (level e) (player (level e)) (at m) = Just $ "The " ++ mobName m ++ " wakes up from their slumber."
toMessage e (Slept m) = Just $ "The " ++ mobName m ++ " has fallen asleep."
toMessage e (StairsSeen Up) = Just $ "You see stairs going up."
toMessage e (StairsSeen Down) = Just $ "You see stairs going down."
toMessage e (ItemsSeen items) = let suffix = if length items > 1 then "There are " ++ show (length items - 1) ++ " more items here." else ""
                              in  Just $ "You see a " ++ showIdentified (identified (player (level e))) (head items) ++ ". " ++ suffix
toMessage e (ItemPickedUp m item) | isPlayer m = Just $ "You have picked up a " ++ showIdentified (identified (player (level e))) item ++ "."
toMessage e (Equipped m item) | isPlayer m = Just $ "You have equipped up the " ++ showIdentified (identified (player (level e))) item ++ "."
toMessage e (EquipmentRemoved m item) | isPlayer m = Just $ "You have removed the " ++ showIdentified (identified (player (level e))) item ++ "."
-- TODO
-- toMessage e (MenuChange Inventory) = Just $ "Pick an item to use or equip. Press space to cancel."
-- toMessage e (MenuChange ProjectileMenu) = Just $ "Pick a projectile to throw. Press space to cancel."
-- toMessage e (MenuChange TargetMenu) = Just $ "Pick a target to throw at. Press space to cancel."
toMessage e (Drank           m p)      | isPlayer m = Just $ "You drank the " ++ show p ++ "."
toMessage e (Healed          m n)      | isPlayer m = Just $ "You were healed of " ++ show n ++ " points of damage."
toMessage e (GainedLife      m n)      | isPlayer m = Just $ "Praise the sun! You feel youthful."
toMessage e (GainedStrength  m n)      | isPlayer m = Just $ "You feel empowered!"
toMessage e (DrankAcid       m  )      | isPlayer m = Just $ "It BURNS!"
toMessage e (Vanished        m  )      | isPlayer m = Just $ "You can no longer see yourself!"
toMessage e (Confused        m  )      | isPlayer m = Just $ "You feel drunk."
toMessage e (Blinded         m  )      | isPlayer m = Just $ "You can no longer see your surroundings!"
toMessage e (Read            m s)      | isPlayer m = Just $ "You read the " ++ show s ++ "."
toMessage e (CastFire        m n)      | isPlayer m = Just $ "Roaring flames erupt all around you!"
toMessage e (CastLightning   m n)      | isPlayer m = Just $ "KABOOM! Lightning strikes everything around you."
toMessage e (Teleported      m p)      | isPlayer m = Just $ "You feel disoriented."
toMessage e (Mapped          lvl)                   = Just $ "You suddenly understand the layout of the current level."
toMessage e (GainedTelepathy m)        | isPlayer m = Just $ "You sense nearby danger."
toMessage e (FailedRest      m)        | isPlayer m = Just $ "You are unable to rest with the sounds of nearby monsters."
toMessage e (MissileInterrupted m)     | isPlayer m = Just $ "You are unable to concentrate on firing within the melee."
toMessage e (ThrownProjectile m i _)   | isPlayer m = Just $ "You throw the " ++ show i ++ "."
toMessage e (FiredProjectile  m l p _) | isPlayer m = Just $ "You fire the " ++ show p ++ " out of your " ++ show l ++ "."
toMessage e (BandageApplied   m)       | isPlayer m = Just $ "You apply the bandage."
toMessage e (FeatureInteracted p (Fountain 0)) = Just $ "The fountain has run dry!"
toMessage e (FeatureInteracted p (Fountain n)) = Just $ "You drink from the fountain."
toMessage e (FeatureInteracted p (Chest is)) = Just $ "You open the chest! There are " ++ show (length is) ++ " items."
toMessage e (FeatureInteracted p Altar) = Just $ "You pray to the gods."
toMessage e otherwise = Nothing
