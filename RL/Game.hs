module RL.Game (Env(..), Client(..), broadcastEvents, isPlaying, canAutomate, canRest, isWon, isQuit, updateFlags, retreatedFrom, module RL.Map, module RL.Event) where

import RL.Event
import RL.Map
import RL.Random
import RL.Util (addOrReplace)

import Data.Map (Map)
import Data.Maybe (fromMaybe, fromJust, isJust, maybeToList, catMaybes)
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
    broadcast env e@(GameUpdate (ItemDropped  m i)      )  = broadcast' (env { level = (level env) { items = (at m, i):items (level env) } }) e
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
    broadcast m (GameUpdate (ItemDropped  m' i)       ) | m == m' = poofItem m i
    broadcast m (GameUpdate (Equipped m' i)           ) | m == m' = equip m i
    broadcast m (GameUpdate (EquipmentRemoved m' i)   ) | m == m' = removeEquip m i
    broadcast m (GameUpdate (Read     m' i)           ) | m == m' = (poofItem m i) { identified = L.nub (itemType i:identified m) }
    broadcast m (GameUpdate (Teleported m' to)        ) | m == m' = m { at = to }
    broadcast m (GameUpdate (Drank    m' i)           ) | m == m' = (poofItem m i) { identified = L.nub (itemType i:identified m) }
    broadcast m (GameUpdate (Healed m' amt)           ) | m == m' = m { hp = min (mhp m) (hp m + amt) }
    broadcast m (GameUpdate (GainedLife m' amt)       ) | m == m' = m { mhp = mhp m + amt, hp = mhp m + amt }
    broadcast m (GameUpdate (GainedLevel m' lvl)      ) | m == m' = m { mlvl = lvl, savingThrow = savingThrow m' - 1, thac0 = thac0 m' - 1 }
    broadcast m (GameUpdate (GainedStrength m' str)   ) | m == m' = m { strength = strength m + str }
    broadcast m (GameUpdate (GainedMobFlag m' f)      ) | m == m' = m { flags = L.nub (f:flags m) }
    broadcast m (GameUpdate (RemovedMobFlag m' f)     ) | m == m' = m { flags = L.delete f (flags m) }
    broadcast m (GameUpdate (ThrownProjectile m' i _) ) | m == m' = m { inventory = L.delete i (inventory m) }
    broadcast m (GameUpdate (FiredProjectile m' _ i _)) | m == m' = m { inventory = L.delete i (inventory m) }
    broadcast m (GameUpdate (BandageApplied  m')      ) | m == m' = m { inventory = L.delete (Item "Bandage" Bandage) (inventory m) }
    broadcast m (GameUpdate (Died m'))                  | isPlayer m && not (isPlayer m') = m { xp = xp m + xpAward m' }

    broadcast m otherwise = m

pickup :: Item -> Mob -> Mob
pickup i m =
    let inv = if not (isGold i) && i `elem` inventory m then
                  let f (n, i') = if i == i' then (n+1, i') else (n, i')
                  in  ungroupItems . map f $ groupItems (inventory m)
              else inventory m ++ [i]
        nxp = if isGold i then xp m + goldAmount [i] else xp m
    in  m { inventory = inv, xp = nxp }

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


-- get list of enmies that retreated from the melee
retreatedFrom :: Env -> Mob -> [Mob]
retreatedFrom env m = let lvl                         = level env
                          enemies                     = if isPlayer m then mobs lvl else [player lvl]
                          f (GameUpdate (Moved m' p)) = m' `elem` enemies && isVisible m' && touching (at m) (at m') && not (touching (at m) p)
                          f otherwise                 = False
                          g (GameUpdate (Moved m' p)) = findMob (mobId m') enemies
                          g otherwise                 = Nothing
                      in  if occurredThisTurn tookStairs (events env) then [] else catMaybes . map g . filterEventsThisTurn f $ events env
