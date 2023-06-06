module RL.Game (Env(..), Client(..), lastVisitedDay, restedThisTurn, stairsTakenThisTurn, daysSinceLastVisit, currentDay, broadcastEvents, isPlaying, canAutomate, canRest, isWon, isQuit, updateFlags, module RL.Dungeon, module RL.Event) where

import RL.Event
import RL.Dungeon
import RL.Random
import RL.Util (addOrReplace)

import Data.Map (Map)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe, fromJust, isJust, maybeToList, catMaybes)
import qualified Data.List as L
import qualified Data.Map as M

data Env = Env {
    dungeon  :: Dungeon,
    level    :: DLevel,
    events   :: [Event]
}

isPlaying :: Env -> Bool
isPlaying env = not (isDead (player (level env)) || isQuit env)

-- TODO win condition
isWon :: Env -> Bool
isWon = const False

isQuit :: Env -> Bool
isQuit e = isJust (L.find (== GameUpdate QuitGame) (events e))

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

-- get current game day
currentDay :: [Event] -> Day
currentDay es = lastRested es + 1

lastRested :: [Event] -> Day
lastRested = maybe 0 id . listToMaybe . mapMaybe f
    where f (GameUpdate (Rested _ _ d)) = Just d
          f otherwise                   = Nothing

stairsTakenThisTurn :: Env -> Bool
stairsTakenThisTurn = occurredThisTurn f . events
    where f (GameUpdate (StairsTaken _ _)) = True
          f otherwise                      = False

restedThisTurn :: Env -> Bool
restedThisTurn = occurredThisTurn f . events
    where f (GameUpdate (Rested _ _ d)) = True
          f otherwise                   = False

-- get day we last visited this level
lastVisitedDay :: Env -> [Event] -> Day
lastVisitedDay env es = if occurredSince f g es then currentDay $ eventsAfterF f es -- have we rested on this level since we changed stairs to it? Then, 
                        else currentDay $ eventsAfterF g es
    where f (GameUpdate (Rested _ d _))      | d         == depth (level env)
                                      = True
          f otherwise                 = False
          g (GameUpdate (StairsTaken _ lvl)) | depth lvl == depth (level env)
                                      = True
          g otherwise                 = False

-- get days since last visiting this level
daysSinceLastVisit :: Env -> Int
daysSinceLastVisit env = let es' = eventsTurnsAgo 1 (events env) -- don't count current visit
                         in  if not (happened f es') then 0
                             else currentDay (events env) - lastVisitedDay env es'
    where f (GameUpdate (StairsTaken _ lvl)) | depth lvl == depth (level env)
                                      = True
          f otherwise                 = False

-- represents a client that does something to the state
class Client c where
    -- broadcast event to client, resulting in state change within client
    broadcast :: Client c => c -> Event -> c
-- broadcast multiple events to client
broadcastEvents :: Client c => c -> [Event] -> c
broadcastEvents c []    = c
broadcastEvents c (e:t) = broadcastEvents (broadcast c e) t

instance Client Env where
    broadcast env e@(GameUpdate (StairsTaken v lvl)     )  = broadcast' (changeLevel env v lvl) e
    broadcast env e@(GameUpdate (MobSpawned m)          )  = broadcast' (env { level = (level env) { mobs = m:(mobs (level env)) } }) e
    broadcast env e@(GameUpdate (ItemPickedUp m i)      )  = broadcast' (env { level = removePickedItem m i (level env) }) e
    broadcast env e@(GameUpdate (ItemSpawned p i)       )  = broadcast' (env { level = (level env) { items = (p, i):(items (level env)) } }) e
    broadcast env e@(GameUpdate (ThrownProjectile m i p))  = let is = if not (isFragile i) then (p,i):items (level env) else items (level env)
                                                             in  broadcast' (env { level = (level env) { items = is } }) e
    broadcast env e@(GameUpdate (FiredProjectile m _ i p)) = let is = if not (isFragile i) then (p,i):items (level env) else items (level env)
                                                             in  broadcast' (env { level = (level env) { items = is } }) e
    broadcast env e@(GameUpdate (FeatureInteracted p f@(Chest _))) = broadcast' (env { level = (level env) { features = L.delete (p, f) (features (level env)) } }) e
    broadcast env e@(GameUpdate (FeatureInteracted p f@(Fountain n))) = broadcast' (env { level = (level env) { features = addOrReplace p (Fountain (max 0 (n - 1))) (features (level env)) } }) e

    broadcast env e = broadcast' env e

broadcast' env e =
        let p       = player (level env)
            lvl     = level env
            ms'     = map (`broadcast` e) (mobs lvl)
        in  env { level    = lvl { player = broadcast p e,
                                   mobs   = aliveMobs ms' },
                  events   = e:events env }

instance Client Mob where
    broadcast m (GameUpdate (Moved m' to)     )         | m == m' && canMove m = moveMob to m
    broadcast m (GameUpdate (Damaged _ m' dmg))         | m == m' = m { hp = max 0 (hp m - dmg) }
    broadcast m (GameUpdate (Waken m')        )         | m == m' = let fs = filter (/= Sleeping) (flags m)
                                                                    in  m { flags = fs }
    broadcast m (GameUpdate (Slept m')                ) | m == m' = m { flags = L.nub (Sleeping:flags m) }
    broadcast m (GameUpdate (ItemPickedUp m' i)       ) | m == m' = pickup i m
    broadcast m (GameUpdate (Equipped m' i)           ) | m == m' = equip m i
    broadcast m (GameUpdate (EquipmentRemoved m' i)   ) | m == m' = removeEquip m i
    broadcast m (GameUpdate (Read     m' i)           ) | m == m' = (poofItem m i) { identified = L.nub (itemType i:identified m) }
    broadcast m (GameUpdate (Teleported m' to)        ) | m == m' = m { at = to }
    broadcast m (GameUpdate (Drank    m' i)           ) | m == m' = (poofItem m i) { identified = L.nub (itemType i:identified m) }
    broadcast m (GameUpdate (Healed m' amt)           ) | m == m' = m { hp = min (mhp m) (hp m + amt) }
    broadcast m (GameUpdate (GainedLife m' amt)       ) | m == m' = m { mhp = mhp m + amt, hp = mhp m + amt }
    broadcast m (GameUpdate (GainedStrength m' str)   ) | m == m' = m { thac0 = thac0 m - str, strength = strength m + str }
    broadcast m (GameUpdate (GainedMobFlag m' f)      ) | m == m' = m { flags = L.nub (f:flags m) }
    broadcast m (GameUpdate (RemovedMobFlag m' f)     ) | m == m' = m { flags = L.delete f (flags m) }
    broadcast m (GameUpdate (ThrownProjectile m' i _) ) | m == m' = m { inventory = L.delete i (inventory m) }
    broadcast m (GameUpdate (FiredProjectile m' _ i _)) | m == m' = m { inventory = L.delete i (inventory m) }
    broadcast m (GameUpdate (BandageApplied  m')      ) | m == m' = m { inventory = L.delete (Item "Bandage" Bandage) (inventory m) }

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

turnsSinceMobF :: Mob -> MobFlag -> [Event] -> Int
turnsSinceMobF m flag = let f (GameUpdate (GainedMobFlag m' flag')) = m == m' && flag == flag'
                            f otherwise                             = False
                        in  length . L.filter isEndOfTurn . takeWhile (not . f)

-- remove stale flags from mobs/player at end of turn
updateFlags :: Env -> Env
updateFlags env = let isStale m Invisible   = turnsSinceMobF m Invisible   (events env) >= 100
                      isStale m ConfusedF   = turnsSinceMobF m ConfusedF   (events env) >= 10
                      isStale m BlindedF    = turnsSinceMobF m BlindedF    (events env) >= 50
                      isStale m TelepathicF = turnsSinceMobF m TelepathicF (events env) >= 200
                      isStale m otherwise   = False
                      removeFlag m f        = GameUpdate (RemovedMobFlag m f)
                      evF        m          = map (removeFlag m) (L.filter (isStale m) (flags m))
                      evs                   = concat $ evF (player (level env)):(map evF (mobs (level env)))
                  in  broadcastEvents env evs
