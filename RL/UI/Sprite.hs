{-# LANGUAGE TupleSections #-}

module RL.UI.Sprite (
    gameSprites,
    SpriteEnv(..),
    Sprite(..),
    SpriteAttr(..),
    Color,
    toMessage
) where

import RL.Game
import RL.Player
import RL.UI.Common
import RL.Util

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

data SpriteEnv = SpriteEnv { spriteGame :: Env,
                             spriteIS   :: InputState,
                             spriteSeen :: [Point],
                             spriteHeard :: [Point] }

spriteLevel = level . spriteGame

gameSprites :: SpriteEnv -> [Sprite]
gameSprites env = getMapSprites env ++ getMsgSprites (spriteGame env) ++ getStatusSprites env ++ inputSprites env

inputSprites :: SpriteEnv -> [Sprite]
inputSprites env =
    case menu (spriteIS env) of
        Just TargetMenu     -> maybe [] targetMenu (target (spriteIS env))
        Just Inventory      -> inventoryMenu
        Just DropMenu       -> inventoryMenu
        Just ProjectileMenu -> inventoryMenu
        otherwise           -> []
    where targetMenu  p = [CharSprite p '*' (SpriteAttr red black)]
          inventoryMenu =
            let lvl                 = spriteLevel env
                inv                 = groupItems . L.filter (not . isGold) $ inventory (player lvl)
                eq                  = groupItems (equipmentToList (equipment (player lvl)))
                showInvItem (ch, i) = ch:(showItem i)
                p                   = player lvl
                readyStr i          = if readied (spriteIS env) == Just i then " (readied)" else ""
                showItem (1,i)      = " - " ++ showIdentified (identified p) i ++ readyStr i
                showItem (n,i)      = " - " ++ show n ++ " " ++ showIdentified (identified p) i ++ "s" ++ readyStr i -- TODO pluralize
            in  mkMessages (0,  0) ([ "Inventory:", " " ] ++ map showInvItem (zip inventoryLetters inv)) ++
                mkMessages (40, 0) ([ "Equipped:", " " ] ++ map showItem eq)
            
spriteAt :: SpriteEnv -> Point -> Sprite
spriteAt env p = if canPlayerSee p then tileOrMobSprite lvl p
                 else if p `elem` spriteSeen env then seenTileSprite lvl p
                      else heardTileSprite lvl p
    where
        lvl = spriteLevel env
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
        mobColor "Rat" = brown
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
        featureColor Campfire     = red

        tileSprite :: DLevel -> (Int, Int) -> Maybe Sprite
        tileSprite lvl p = case findTileAt p lvl of
                               Nothing -> Nothing
                               Just  t -> 
                                   let charSpr   = CharSprite p (fromTile t) (SpriteAttr (tileColor t) black)
                                       wallSpr w = WallSprite p w (SpriteAttr (tileColor t) black)
                                   in  Just $ maybe charSpr wallSpr (seenWallType env p)
        itemSprite :: DLevel -> (Int, Int) -> Maybe Sprite
        itemSprite lvl p = case findItemsAt p lvl of
                               (i:_) -> Just (CharSprite p (itemSymbol i) (SpriteAttr (itemColor i) black))
                               []    -> Nothing
        mobSprite :: DLevel -> (Int, Int) -> Maybe Sprite
        mobSprite lvl p = case findTileOrMob p lvl of
                               Right m -> if isVisible m then
                                            Just (CharSprite p (symbol m) (SpriteAttr (mobColor (mobName m)) black))
                                          else if isPlayer m then
                                            Just (CharSprite p ' ' (SpriteAttr white (50,50,50)))
                                          else
                                            Nothing
                               Left _  -> Nothing
        featureSprite :: DLevel -> (Int, Int) -> Maybe Sprite
        featureSprite lvl p = case L.lookup p (features lvl) of
                                Just f  -> Just (CharSprite p (fromFeature f) (SpriteAttr (featureColor f) black))
                                Nothing -> Nothing
        tileOrMobSprite :: DLevel -> (Int, Int) -> Sprite
        tileOrMobSprite lvl p = let sprites = [mobSprite lvl p, featureSprite lvl p, itemSprite lvl p, tileSprite lvl p]
                                    sprite  = listToMaybe (catMaybes sprites)
                                in  if isJust sprite then fromJust sprite
                                    else CharSprite p ' ' (SpriteAttr black black)
        seenTileSprite lvl p  = if p `elem` spriteSeen env then stale (fromJust (listToMaybe (catMaybes [featureSprite lvl p, itemSprite lvl p, tileSprite lvl p])))
                                else CharSprite p ' ' (SpriteAttr black black)
        heardTileSprite lvl p = if p `elem` spriteHeard env then stale (CharSprite p '?' undefined)
                                else CharSprite p ' ' (SpriteAttr black black)
        stale (CharSprite    p c _) = CharSprite    p c (SpriteAttr dgrey black)
        stale (MessageSprite p c _) = MessageSprite p c (SpriteAttr dgrey black)
        stale (WallSprite    p c _) = WallSprite    p c (SpriteAttr dgrey black)

getMapSprites :: SpriteEnv -> [Sprite]
getMapSprites env = map (spriteAt env . fst) . M.toList $ tiles (spriteLevel env)

getStatusSprites :: SpriteEnv -> [Sprite]
getStatusSprites env =
    let lvl = spriteLevel env
        p  = player lvl
        hpSprite = (MessageSprite (64, 15) (show (hp p)) (SpriteAttr hpColor black))
        hpPercent = fromIntegral (hp p) / fromIntegral (mhp p)
        hpColor = if hpPercent >= 1.0 then white
                  else if hpPercent >= 0.7 then green
                  else if hpPercent >= 0.4 then yellow
                  else red
    in [ mkMessage (60, 15) "HP: ", hpSprite, mkMessage (64 + length (show (hp p)), 15) (" / " ++ show (mhp p)),
         mkMessage (60, 16) ("AC: " ++ show (mobAC (player lvl))),
         mkMessage (60, 17) ("XP: " ++ show (xp (player lvl)) ++ " / " ++ show (expForLevel (mlvl (player lvl) + 1))),
         mkMessage (60, 18) ("GP: " ++ show (goldAmount (inventory (player lvl)))),
         mkMessage (60, 19) ("Level: " ++ show (mlvl (player lvl))),
         mkMessage (60, 20) ("Depth: " ++ show (depth lvl)),
         mkMessage (60, 21) ("Readied: " ++ maybe "- none -" show (readied (spriteIS env))) ]

getMsgSprites :: Env -> [Sprite]
getMsgSprites env = let evs        = events env
                        recentMsgs = catMaybes (map (toMessage env) (eventsSince 1 evs))
                        staleMsgs  = catMaybes (map (toMessage env) (eventsSince 10 (eventsTurnsAgo 1 evs)))
                        msgs       = zip recentMsgs (repeat white) ++ zip staleMsgs (repeat grey)
                    in  mkColoredMessages (0, 15) . takeLast 9 . wrapMessages . reverse . take 9 $ msgs

mkMessages :: Point -> [String] -> [Sprite]
mkMessages (offx, offy) = map toSprite . enumerate
    where
        toSprite (i, s) = MessageSprite (offx, i + offy) s (SpriteAttr white black)

maxMessageWidth = 59

wrapMessages :: [(String, Color)] -> [(String, Color)]
wrapMessages = foldr f []
    where f (str, clr) xs = if length str > maxMessageWidth then (map (, clr) $ wrapString str maxMessageWidth) ++ xs
                            else (str, clr):xs

mkColoredMessages :: Point -> [(String, Color)] -> [Sprite]
mkColoredMessages (offx, offy) = map toSprite . enumerate
    where
        toSprite (i, (s, fg)) = MessageSprite (offx, i + offy) s (SpriteAttr fg black)

mkMessage :: Point -> String -> Sprite
mkMessage xy s = MessageSprite xy s (SpriteAttr white black)

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
seenWallType :: SpriteEnv -> Point -> Maybe WallType
seenWallType env (x,y) =
    let lvl  = spriteLevel env
        f p' = maybe False (not . isWall) (findTileAt p' lvl) && p' `elem` (spriteSeen env)
        fixWall Wall = if ((x+1),y) `elem` (spriteSeen env) || ((x-1),y) `elem` (spriteSeen env) then WallEW
                       else if (x,y+1) `elem` (spriteSeen env) || (x,y-1) `elem` (spriteSeen env) then WallNS
                       else Wall
        fixWall t    = t
    in  fixWall <$> filterWallType f (x,y) <$> wallType lvl (x,y)

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
wallType :: DLevel -> Point -> Maybe WallType
wallType lvl p =
    if not (maybe False isWall (findTileAt p lvl)) then Nothing
    else if wallN lvl p && wallS lvl p && wallE lvl p && wallW lvl p then Just WallNESW
    else if wallN lvl p && wallS lvl p && wallE lvl p then Just WallNSE
    else if wallN lvl p && wallS lvl p && wallW lvl p then Just WallNSW
    else if wallN lvl p && wallW lvl p && wallE lvl p then Just WallNEW
    else if wallS lvl p && wallW lvl p && wallE lvl p then Just WallSEW
    else if wallN lvl p && wallS lvl p then Just WallNS
    else if wallW lvl p && wallS lvl p then Just WallSW
    else if wallE lvl p && wallS lvl p then Just WallSE
    else if wallW lvl p && wallN lvl p then Just WallNW
    else if wallE lvl p && wallN lvl p then Just WallNE
    else if wallE lvl p then Just WallEW
    else if wallW lvl p then Just WallEW
    else if wallS lvl p then Just WallNS
    else if wallN lvl p then Just WallNS
    else Just Wall

wallN  :: DLevel -> Point -> Bool
wallN lvl (x,y) = maybe False isWall (findTileAt (x, y - 1) lvl)
wallE  :: DLevel -> Point -> Bool
wallE lvl (x,y) = maybe False isWall (findTileAt (x + 1, y) lvl)
wallS  :: DLevel -> Point -> Bool
wallS lvl (x,y) = maybe False isWall (findTileAt (x, y + 1) lvl)
wallW  :: DLevel -> Point -> Bool
wallW lvl (x,y) = maybe False isWall (findTileAt (x - 1, y) lvl)

isWall :: Tile -> Bool
isWall Rock = True
isWall otherwise = False


toMessage :: Env -> Event -> Maybe String
toMessage e (GameUpdate NewGame)   = Just $ "You have spent your life in squalor and filth among the rogues. You have heard many rumors of great treasures beneath this dungeon. This is your chance to earn fame and fortune."
toMessage e (GameUpdate (Escaped)) = Just $ "There is no escape. You must redeem your ancestors!"
toMessage e (GameUpdate (Crit attacker target))
    | isPlayer attacker = Just $ "CRITICAL HIT!"
toMessage e (GameUpdate (Damaged attacker target dmg))
    | isPlayer attacker && isPlayer target = Just $ "You hurt yourself for " ++ show dmg ++ " damage! Be more careful!"
    | isPlayer attacker = Just $ "You hit the " ++ mobName target ++ " for " ++ show dmg ++ " damage"
    | isPlayer target = Just $ "You were hit by the " ++ mobName attacker ++ " for " ++ show dmg
    | otherwise = Just $ "The " ++ mobName attacker ++ " hit the " ++ mobName target ++ " for " ++ show dmg
toMessage e (GameUpdate (Missed attacker target))
    | isPlayer attacker = Just $ "You missed the " ++ mobName target
    | isPlayer target = Just $ "The " ++ mobName attacker ++ " missed"
    | otherwise = Just $ "The " ++ mobName attacker ++ " missed the " ++ mobName target
toMessage e (GameUpdate (Died m))
    | isPlayer m = Just $ "You died! Press space to quit or r to restart a new game."
    | otherwise  = Just $ "You killed the " ++ mobName m
toMessage e (GameUpdate (StairsTaken Up _)) = Just $ "You've gone up stairs."
toMessage e (GameUpdate (StairsTaken Down _)) = Just $ "You've gone down stairs."
toMessage e (GameUpdate (Waken m)) | canSee (level e) (player (level e)) (at m) = Just $ "The " ++ mobName m ++ " wakes up from their slumber."
toMessage e (GameUpdate (Slept m)) = Just $ "The " ++ mobName m ++ " has fallen asleep."
toMessage e (EventMessage (StairsSeen Up)) = Just $ "You see stairs going up."
toMessage e (EventMessage (StairsSeen Down)) = Just $ "You see stairs going down."
toMessage e (EventMessage (ItemsSeen items)) = let suffix = if length items > 1 then "There are " ++ show (length items - 1) ++ " more items here." else ""
                                               in  Just $ "You see a " ++ showIdentified (identified (player (level e))) (head items) ++ ". " ++ suffix
toMessage e (EventMessage (MenuChange Inventory)) = Just $ "Pick an item to use or equip. Press space to cancel."
toMessage e (EventMessage (MenuChange ProjectileMenu)) = Just $ "Pick a projectile to throw. Press space to cancel."
toMessage e (EventMessage (MenuChange TargetMenu)) = Just $ "Pick a target to fire at. Press r to ready something else, space to cancel."
toMessage e (EventMessage (MenuChange DropMenu)) = Just $ "Pick an item to drop onto the ground."
toMessage e (EventMessage (Readied i)) = Just $ "You have readied the " ++ show i
toMessage e (EventMessage InMelee) = Just $ "You are unable to concentrate on firing within the melee."
toMessage e (EventMessage PlayerRested)   = Just $ "You find a safe place to rest and heal your wounds."
toMessage e (EventMessage PlayerInDanger) = Just $ "You are unable to find a safe place to rest here!"
toMessage e (EventMessage (AttackOfOpportunity m t))    | isPlayer t = Just $ "The " ++ show m ++ " notices an opening and attacks!"
toMessage e (EventMessage (AttackRoll m roll 0))   | isPlayer m = Just $ "You rolled a " ++ show roll ++ " to hit"
toMessage e (EventMessage (AttackRoll m roll mod)) | isPlayer m = Just $ "You rolled a " ++ show roll ++ " to hit, with a modifier of " ++ show mod
toMessage e (EventMessage (AttackRoll m roll mod)) = Just $ "The " ++ mobName m ++ " rolled a " ++ show roll ++ " to hit"
-- toMessage e (EventMessage (PlayerRetreated m))                       = Just $ "You have successfully broke off the melee with the " ++ show m
toMessage e (EventMessage (MobFlees m))                              = Just $ "The " ++ show m ++ " flees from combat"
toMessage e (GameUpdate (ItemPickedUp m item))          | isPlayer m = Just $ "You have picked up the " ++ showIdentified (identified (player (level e))) item ++ "."
toMessage e (GameUpdate (ItemDropped  m item))          | isPlayer m = Just $ "You have dropped the " ++ showIdentified (identified (player (level e))) item ++ "."
toMessage e (GameUpdate (Equipped m item))              | isPlayer m = Just $ "You have equipped up the " ++ showIdentified (identified (player (level e))) item ++ "."
toMessage e (GameUpdate (EquipmentRemoved m item))      | isPlayer m = Just $ "You have removed the " ++ showIdentified (identified (player (level e))) item ++ "."
toMessage e (GameUpdate (Drank           m p))          | isPlayer m = Just $ "You drank the " ++ show p ++ "."
toMessage e (GameUpdate (Healed          m n))          | isPlayer m = Just $ "You were healed of " ++ show n ++ " points of damage."
toMessage e (GameUpdate (GainedLife      m n))          | isPlayer m = Just $ "Praise the sun! You feel youthful."
toMessage e (GameUpdate (GainedLevel     m lvl))        | isPlayer m = Just $ "Congratulations! You are now level " ++ show lvl
toMessage e (GameUpdate (GainedStrength  m n))          | isPlayer m = Just $ "You feel empowered!"
toMessage e (GameUpdate (DrankAcid       m  ))          | isPlayer m = Just $ "It BURNS!"
toMessage e (GameUpdate (GainedMobFlag m Invisible))    | isPlayer m = Just $ "You can no longer see yourself!"
toMessage e (GameUpdate (GainedMobFlag m ConfusedF))    | isPlayer m = Just $ "You feel drunk."
toMessage e (GameUpdate (GainedMobFlag m BlindedF))     | isPlayer m = Just $ "You can no longer see your surroundings!"
toMessage e (GameUpdate (GainedMobFlag m Sleeping))     | isPlayer m = Just $ "You fell asleep."
toMessage e (GameUpdate (GainedMobFlag m TelepathicF))  | isPlayer m = Just $ "You sense nearby danger."
toMessage e (GameUpdate (GainedMobFlag m (MappedF _)))  | isPlayer m = Just $ "You suddenly understand the layout of the current level."
toMessage e (GameUpdate (RemovedMobFlag m Invisible))   | isPlayer m = Just $ "You can see yourself again."
toMessage e (GameUpdate (RemovedMobFlag m ConfusedF))   | isPlayer m = Just $ "You feel sober."
toMessage e (GameUpdate (RemovedMobFlag m BlindedF))    | isPlayer m = Just $ "You are no longer blind."
toMessage e (GameUpdate (RemovedMobFlag m Sleeping))    | isPlayer m = Just $ "You wake up."
toMessage e (GameUpdate (RemovedMobFlag m TelepathicF)) | isPlayer m = Just $ "You stop sensing danger."
toMessage e (GameUpdate (RemovedMobFlag m (MappedF _))) | isPlayer m = Just $ "You feel forgetful."
toMessage e (GameUpdate (Read            m s))          | isPlayer m = Just $ "You read the " ++ show s ++ "."
toMessage e (GameUpdate (CastFire        m n))          | isPlayer m = Just $ "Roaring flames erupt all around you!"
toMessage e (GameUpdate (CastLightning   m n))          | isPlayer m = Just $ "KABOOM! Lightning strikes everything around you."
toMessage e (GameUpdate (Teleported      m p))          | isPlayer m = Just $ "You feel disoriented."
toMessage e (GameUpdate (ThrownProjectile m i _ _))     | isPlayer m = Just $ "You throw the " ++ show i ++ "."
toMessage e (GameUpdate (FiredProjectile  m l p _ _))   | isPlayer m = Just $ "You fire the " ++ show p ++ " out of your " ++ show l ++ "."
toMessage e (EventMessage (NoTargetsInRange))                        = Just $ "No targets in range."
toMessage e (GameUpdate (FeatureInteracted p (Fountain 0))) = Just $ "The fountain has run dry!"
toMessage e (GameUpdate (FeatureInteracted p (Fountain n))) = Just $ "You drink from the fountain."
toMessage e (GameUpdate (FeatureInteracted p (Chest is))) = Just $ "You open the chest! There are " ++ show (length is) ++ " items."
toMessage e (GameUpdate (FeatureInteracted p Altar)) = Just $ "You pray to the gods."
toMessage e (GameUpdate (Rested p d' d)) = Just $ "You fall asleep, warmed by the campfire."
toMessage e otherwise = Nothing
