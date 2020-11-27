module RL.Game (GameEnv, Env(..), Client(..), broadcastEvents, isTicking, isPlaying, isAutomated, canAutomate, isWon, isQuit, isViewingInventory, module RL.Map, module RL.Event, module Control.Monad.Reader) where

import RL.Event
import RL.Map
import RL.Random

import Control.Monad.Reader
import Data.Maybe (fromMaybe, fromJust, isJust, maybeToList)
import qualified Data.List as L
import qualified Data.Map as M

type GameEnv = ReaderT Env (Rand StdGen)
data Env     = Env {
    dungeon    :: Dungeon,
    level      :: DLevel,
    events     :: [Event],
    menu       :: Menu
}

isPlaying :: Env -> Bool
isPlaying env = not (isDead (player (level env)) || isQuit env)

-- TODO win condition
isWon :: Env -> Bool
isWon = const False

isQuit :: Env -> Bool
isQuit e = isJust (L.find (== QuitGame) (events e))

-- checks if we are running to the destination
isAutomated :: Env -> Bool
isAutomated env = let lvl = level env
                      p   = player lvl
                  in  isJust (destination p)

-- checks if we are running to the destination and there are no mobs seen
canAutomate :: Env -> Bool
canAutomate env = let lvl = level env
                      p   = player lvl
                      ms  = mobs lvl
                  in  isAutomated env && null (L.filter (canSee lvl p) (map at ms))

-- detects if we're ticking (i.e. AI and other things should be active)
isTicking :: Env -> Bool
isTicking = (== NoMenu) . menu

-- detects if we're ticking (i.e. AI and other things should be active)
isViewingInventory :: Env -> Bool
isViewingInventory e = menu e /= NoMenu

-- represents a client that does something to the state
class Client c where
    -- broadcast event to client, resulting in state change within client
    broadcast :: Client c => c -> Event -> c
-- broadcast multiple events to client
broadcastEvents :: Client c => c -> [Event] -> c
broadcastEvents c []    = c
broadcastEvents c (e:t) = broadcastEvents (broadcast c e) t

instance Client Env where
    broadcast env e@NewGame             = broadcast' (updateSeen (canSee (level env) (player (level env))) env) e
    broadcast env e@EndOfTurn           = broadcast' (updateFlags . healDamaged $ updateSeen (canSee (level env) (player (level env))) env) e
    broadcast env e@(StairsTaken v lvl) = broadcast' (changeLevel env v lvl) e
    broadcast env e@(MenuChange  m)     = broadcast' (env { menu = m }) e
    broadcast env e@(MobSpawned m)      = broadcast' (env { level = (level env) { mobs = m:(mobs (level env)) } }) e
    broadcast env e@(ItemPickedUp m i)  = broadcast' (env { level = removePickedItem m i (level env) }) e
    broadcast env e@(Teleported m to)   = updateSeen (canSee (level env) (player (level env))) $ broadcast' env e
    broadcast env e@(Mapped lvl)        = broadcast' (updateSeen (const True) env) e
    broadcast env e                     = broadcast' env e

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
    broadcast m (MobSeen  m' p)            | m == m' = m { destination = Just (at p) }
    broadcast m (MobHeard m' p)            | m == m' = m { destination = Just (at p) }
    broadcast m (DestinationSet m' p)      | m == m' = m { destination = Just p }
    broadcast m (DestinationAbrupted m' p) | m == m' = m { destination = Nothing }
    broadcast m (ItemPickedUp m' i)        | m == m' = m { inventory = inventory m ++ [i] }
    broadcast m (Equipped m' i)            | m == m' = equip m i
    broadcast m (Read     m' i)            | m == m' = poofItem m i
    broadcast m (Teleported m' to)         | m == m' = m { at = to }
    broadcast m (GainedTelepathy lvl)   | isPlayer m = m { isTelepathicOn = depth lvl:isTelepathicOn m }
    broadcast m (Drank    m' i)            | m == m' = poofItem m i
    broadcast m (Healed m' amt)            | m == m' = m { hp = min (mhp m) (hp m + amt) }
    broadcast m (GainedLife m' amt)        | m == m' = m { mhp = mhp m + amt, hp = mhp m + amt }
    broadcast m (GainedStrength m' str)    | m == m' = m { thac0 = thac0 m - str, strength = strength m + str }
    broadcast m (Vanished m')              | m == m' = m { flags = L.nub (Invisible:flags m) }
    broadcast m (Confused m')              | m == m' = m { flags = L.nub (ConfusedF:flags m) }
    broadcast m (Blinded m')               | m == m' = m { flags = L.nub (BlindedF:flags m) }

    broadcast m otherwise = m

removePickedItem :: Mob -> Item -> DLevel -> DLevel
removePickedItem m i lvl = let is  = L.delete i (findItemsAt (at m) lvl)
                           in  replaceItemsAt (at m) lvl is

equip :: Mob -> Item -> Mob
equip m i = if isWeapon i then
                let weap = wielding (equipment m)
                in  m { equipment = (equipment m) { wielding = Just i },
                        inventory = maybeToList weap ++ L.delete i (inventory m) }
            else if isArmor i then
                let s     = slot (fromJust (armorProperties i))
                    worn' = L.find   ((Just s ==) . armorSlot) (wearing (equipment m))
                    eqp'  = L.filter ((Just s /=) . armorSlot) (wearing (equipment m))
                in  m { equipment = (equipment m) { wearing = i:eqp' },
                        inventory = maybeToList worn' ++ (L.delete i $ inventory m) }
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
updateFlags env = let p                 = player (level env)
                      isStale Invisible = turnsSince (== Vanished p) (events env) >= 100
                      isStale ConfusedF = turnsSince (== Confused p) (events env) >= 10
                      isStale BlindedF  = turnsSince (== Blinded  p) (events env) >= 50
                      isStale otherwise = False
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

-- heal damaged mobs if mob not damaged 5 turns ago
healDamaged :: Env -> Env
healDamaged env =
    let p            = player (level env)
        healedP      = if isHealing p then p { hp = min (mhp p) (hp p + 1) } else p
        healedMs     = map (\m -> m { hp = min (mhp m) (hp m + 1) }) msToHeal
        msToHeal     = L.filter isHealing (mobs (level env))
        sinceHit   m = turnsSince (isDamageE m) (events env)
        isHealing  m = sinceHit m > 0 && sinceHit m `mod` 5 == 0
        isDamageE m (Damaged _ m' _) = m == m'
        isDamageE m otherwise        = False
    in  env { level = (level env) { mobs   = L.nub (healedMs ++ mobs (level env))
                                  , player = healedP} }

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
