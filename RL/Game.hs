module RL.Game (Env(..), Client(..), broadcastEvents, isPlaying, canAutomate, canRest, isWon, isQuit, module RL.Map, module RL.Event) where

import RL.Event
import RL.Map
import RL.Random
import RL.Util (addOrReplace)

import Data.Map (Map)
import Data.Maybe (fromMaybe, fromJust, isJust, maybeToList)
import qualified Data.List as L
import qualified Data.Map as M

data Env     = Env {
    dungeon    :: Dungeon,
    level      :: DLevel,
    events     :: [Event]
}

isPlaying :: Env -> Bool
isPlaying env = not (isDead (player (level env)) || isQuit env)

-- TODO win condition
isWon :: Env -> Bool
isWon = const False

isQuit :: Env -> Bool
isQuit e = isJust (L.find (== QuitGame) (events e))

-- checks if we are running to the destination and there are no mobs seen
canAutomate :: Env -> Bool
canAutomate env = let lvl = level env
                      p   = player lvl
                      ms  = mobs lvl
                  in  null (L.filter (canSee lvl p) (map at ms))

-- check to ensure we can rest (nearby mobs are dead)
canRest :: Env -> Bool
canRest env = let f m = distance (at m) (at p) <= hearing m
                  p   = player (level env)
              in  null (L.filter f (mobs (level env)))

-- represents a client that does something to the state
class Client c where
    -- broadcast event to client, resulting in state change within client
    broadcast :: Client c => c -> Event -> c
-- broadcast multiple events to client
broadcastEvents :: Client c => c -> [Event] -> c
broadcastEvents c []    = c
broadcastEvents c (e:t) = broadcastEvents (broadcast c e) t

instance Client Env where
    broadcast env e@NewGame                   = broadcast' (updateSeen (canSee (level env) (player (level env))) env) e
    broadcast env e@EndOfTurn                 = broadcast' (updateFlags $ updateSeen (canSee (level env) (player (level env))) env) e
    broadcast env e@(StairsTaken v lvl)       = broadcast' (changeLevel env v lvl) e
    broadcast env e@(MobSpawned m)            = broadcast' (env { level = (level env) { mobs = m:(mobs (level env)) } }) e
    broadcast env e@(ItemPickedUp m i)        = broadcast' (env { level = removePickedItem m i (level env) }) e
    broadcast env e@(Teleported m to)         = updateSeen (canSee (level env) (player (level env))) $ broadcast' env e
    broadcast env e@(Mapped lvl)              = broadcast' (updateSeen (const True) env) e
    broadcast env e@(ItemSpawned p i)         = broadcast' (env { level = (level env) { items = (p, i):(items (level env)) } }) e
    broadcast env e@(ThrownProjectile m i p)  = let is = if not (isFragile i) then (p,i):items (level env) else items (level env)
                                                in  broadcast' (env { level = (level env) { items = is } }) e
    broadcast env e@(FiredProjectile m _ i p) = let is = if not (isFragile i) then (p,i):items (level env) else items (level env)
                                                in  broadcast' (env { level = (level env) { items = is } }) e
    broadcast env e@(FeatureInteracted p f@(Chest _)) = broadcast' (env { level = (level env) { features = L.delete (p, f) (features (level env)) } }) e
    broadcast env e@(FeatureInteracted p f@(Fountain n)) = broadcast' (env { level = (level env) { features = addOrReplace p (Fountain (max 0 (n - 1))) (features (level env)) } }) e

    broadcast env e                           = broadcast' env e

broadcast' env e =
        let p       = player (level env)
            lvl     = level env
            ms'     = map (`broadcast` e) (mobs lvl)
        in  env { level = lvl { player = broadcast p e,
                                mobs   = aliveMobs ms' },
                  events = e:events env }

instance Client Mob where
    broadcast m (Moved m' to)              | m == m' && canMove m = moveMob to m
    broadcast m (Damaged _ m' dmg)         | m == m' = m { hp = max 0 (hp m - dmg) }
    broadcast m (Waken m')                 | m == m' = let fs = filter (/= Sleeping) (flags m)
                                                       in  m { flags = fs }
    broadcast m (Slept m')                 | m == m' = m { flags = L.nub (Sleeping:flags m) }
    broadcast m (ItemPickedUp m' i)        | m == m' = pickup i m
    broadcast m (Equipped m' i)            | m == m' = equip m i
    broadcast m (EquipmentRemoved m' i)    | m == m' = removeEquip m i
    broadcast m (Read     m' i)            | m == m' = (poofItem m i) { identified = L.nub (itemType i:identified m) }
    broadcast m (Teleported m' to)         | m == m' = m { at = to }
    broadcast m (Drank    m' i)            | m == m' = (poofItem m i) { identified = L.nub (itemType i:identified m) }
    broadcast m (Healed m' amt)            | m == m' = m { hp = min (mhp m) (hp m + amt) }
    broadcast m (GainedLife m' amt)        | m == m' = m { mhp = mhp m + amt, hp = mhp m + amt }
    broadcast m (GainedStrength m' str)    | m == m' = m { thac0 = thac0 m - str, strength = strength m + str }
    broadcast m (Vanished m')              | m == m' = m { flags = L.nub (Invisible:flags m) }
    broadcast m (Confused m')              | m == m' = m { flags = L.nub (ConfusedF:flags m) }
    broadcast m (Blinded m')               | m == m' = m { flags = L.nub (BlindedF:flags m) }
    broadcast m (GainedTelepathy m')       | m == m' = m { flags = L.nub (TelepathicF:flags m) }
    broadcast m (ReadiedProjectile m' i)   | m == m' = m { readied = Just i }
    broadcast m (TargetChanged   m' p)     | m == m' = m { target = Just p }
    broadcast m (ThrownProjectile m' i _)  | m == m' = m { readied = Nothing, target = Nothing, inventory = L.delete i (inventory m) }
    broadcast m (FiredProjectile m' _ i _) | m == m' = m { readied = Nothing, target = Nothing, inventory = L.delete i (inventory m) }
    broadcast m (BandageApplied  m')       | m == m' = m { inventory = L.delete (Item "Bandage" Bandage) (inventory m) }

    broadcast m otherwise = m

pickup :: Item -> Mob -> Mob
pickup i m = if i `elem` inventory m then
                 let f (n, i') = if i == i' then (n+1, i') else (n, i')
                 in  m { inventory = ungroupItems . map f $ groupItems (inventory m) }
             else m { inventory = inventory m ++ [i] }

removePickedItem :: Mob -> Item -> DLevel -> DLevel
removePickedItem m i lvl = let is  = L.delete i (findItemsAt (at m) lvl)
                           in  replaceItemsAt (at m) lvl is

equip :: Mob -> Item -> Mob
equip m i = if isLauncher i then
                let launch = launcher (equipment m)
                in  m { equipment = (equipment m) { launcher = Just i },
                        inventory = maybeToList launch ++ L.delete i (inventory m) }
            else if isWeapon i then
                let weap = wielding (equipment m)
                in  m { equipment = (equipment m) { wielding = Just i },
                        inventory = maybeToList weap ++ L.delete i (inventory m) }
            else if isArmor i then
                let s     = slot (fromJust (armorProperties i))
                    worn' = if Body == s then wearing (equipment m) else shield (equipment m)
                    eqp'  = if Body == s then (equipment m) { wearing = Just i }
                            else               (equipment m) { shield  = Just i }
                in  m { equipment = eqp',
                        inventory = maybeToList worn' ++ (L.delete i $ inventory m) }
            else m

removeEquip :: Mob -> Item -> Mob
removeEquip m i = if isLauncher i then
                      m { equipment = (equipment m) { launcher = Nothing }, inventory = i:inventory m }
                  else if isWeapon i then
                      m { equipment = (equipment m) { wielding = Nothing }, inventory = i:inventory m }
                  else if isArmor i then
                      let s     = slot (fromJust (armorProperties i))
                          eqp'  = if Body == s then (equipment m) { wearing = Nothing }
                                  else               (equipment m) { shield  = Nothing }
                      in  m { equipment = eqp', inventory = i:inventory m }
                  else m


poofItem :: Mob -> Item -> Mob
poofItem m i = m { inventory = L.delete i (inventory m) }

-- use this to change to a different dungeon level
changeLevel :: Env -> VerticalDirection -> DLevel -> Env
changeLevel env v lvl = do
        let pl   = player (level env)
            dng  = dungeon env
            lvl' = fromMaybe (level env) (placeOnStair pl $ fromMaybe lvl (atDepth (depth lvl) dng))
        if (depth lvl /= depth (level env)) then
            env { level = lvl',
                  dungeon = insertLevel (level env) $ insertLevel lvl' dng }
        else
            env
    where
        placeOnStair pl lvl =
            let isStair   = if v == Up then isDownStair else isUpStair
                t         = findTile (isStair . snd) lvl
                f  (p, _) = lvl { player = pl { at = p } }
            in  f <$> t

-- remove stale flags from mobs/player at end of turn
updateFlags :: Env -> Env
updateFlags env = let p                   = player (level env)
                      isStale Invisible   = turnsSince (== Vanished p) (events env) >= 100
                      isStale ConfusedF   = turnsSince (== Confused p) (events env) >= 10
                      isStale BlindedF    = turnsSince (== Blinded  p) (events env) >= 50
                      isStale TelepathicF = turnsSince (== GainedTelepathy p) (events env) >= 200
                      isStale otherwise   = False
                  in  env { level = (level env) { player = p { flags = L.filter (not . isStale) (flags p) } } }

-- update newly seen things at end of turn
updateSeen :: (Point -> Bool) -> Env -> Env
updateSeen f env =
    let lvl    = level env
        p      = player lvl
        points = M.keys (tiles lvl)
        seen'  = filter f points
        -- check what is on the current tile
        t      = findTileAt (at (player lvl)) lvl 
        stairE = if isDownStair (fromJust t) then [StairsSeen Down]
                 else if isUpStair (fromJust t) then [StairsSeen Up]
                 else []
        -- check if there are items here
        is     = findItemsAt (at (player lvl)) lvl
        itemE  = if length is > 0 then [ItemsSeen is] else []
        fresh  = not (recentGame p (events env)) && recentlyMoved p (events env)
        picked = recentlyPicked p (events env)
    in  env { level  = lvl { seen = L.nub (seen' ++ seen lvl) },
              events = if fresh then stairE ++ itemE ++ events env
                       else if picked then itemE ++ events env
                       else events env }

-- check if mob recently moved to this tile
recentlyMoved :: Mob -> [Event] -> Bool
recentlyMoved m es = let es' = L.takeWhile (not . f) es
                         f (Moved m' _) = m == m'
                         f otherwise    = False
                     in  (== 0) . length $ L.filter isEndOfTurn es'

-- check if recently picked up on this tile
recentlyPicked :: Mob -> [Event] -> Bool
recentlyPicked m es = let es' = L.takeWhile (not . f) es
                          f (ItemPickedUp m' _) = m == m'
                          f otherwise           = False
                      in  (== 0) . length $ L.filter isEndOfTurn es'

-- check if this is a new game since player last moved
recentGame :: Mob -> [Event] -> Bool
recentGame m es = let es' = L.takeWhile (not . g) es
                      f (Moved m' _) = m == m'
                      f otherwise    = False
                      g (NewGame)    = True
                      g otherwise    = False
                  in  (== 0) . length $ L.filter f es'
