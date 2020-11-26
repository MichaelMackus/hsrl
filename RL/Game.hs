module RL.Game (GameEnv, Env(..), Client(..), broadcastEvents, isTicking, isPlaying, isAutomated, canAutomate, isWon, isQuit, isViewingInventory, module RL.Map, module RL.Event, module Control.Monad.Reader) where

import RL.Event
import RL.Map
import RL.Random

import Control.Monad.Reader
import Data.Maybe (fromMaybe, fromJust, isNothing, isJust, maybeToList)
import qualified Data.List as L
import qualified Data.Map as M

type GameEnv = ReaderT Env (Rand StdGen)
data Env     = Env {
    dungeon  :: Dungeon,
    level    :: DLevel,
    events   :: [Event],
    menu     :: Menu
}

isPlaying :: Env -> Bool
isPlaying env = not (isDead (player (level env)) || isQuit env)

isWon :: Env -> Bool
isWon = const False

isQuit :: Env -> Bool
isQuit e = isJust (L.find (== QuitGame) (events e))

-- checks if we are running to the destination
isAutomated :: Env -> Bool
isAutomated env = let lvl = level env
                      p   = player lvl
                      ms  = mobs lvl
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
isViewingInventory e = let m = menu e
                       in  m == Inventory || m == Equipment

-- represents a client that does something to the state
class Client c where
    -- broadcast event to client, resulting in state change within client
    broadcast :: Client c => c -> Event -> c
-- broadcast multiple events to client
broadcastEvents :: Client c => c -> [Event] -> c
broadcastEvents c []    = c
broadcastEvents c (e:t) = broadcastEvents (broadcast c e) t

instance Client Env where
    broadcast env e@(StairsTaken v lvl) = broadcast' (changeLevel env v lvl) e
    broadcast env e@(MenuChange  m)     = broadcast' (env { menu = m }) e
    broadcast env e@NewGame             = broadcast' (updateSeen env) e
    broadcast env e@EndOfTurn           = broadcast' (updateSeen env) e
    broadcast env e@(MobSpawned m)      = broadcast' (env { level = (level env) { mobs = m:(mobs (level env)) } }) e
    broadcast env e@(ItemPickedUp m i)  = broadcast' (env { level = removePickedItem m i (level env) }) e
    broadcast env e                     = broadcast' env e

broadcast' env e =
        let p       = player (level env)
            lvl     = level env
            ms'     = map (`broadcast` e) (mobs lvl)
        in  env { level = lvl { player = broadcast p e,
                                mobs   = aliveMobs ms' },
                  events = e:events env }

instance Client Mob where
    broadcast m (Moved m' to)              | m == m' && canMove m = moveMob m to
    broadcast m (Damaged _ m' dmg)         | m == m' = hurtMob m dmg
    broadcast m (Waken m')                 | m == m' = let fs = filter (not . isSleeping) (flags m)
                                                       in  m { flags = fs }
    broadcast m (Slept m')                 | m == m' = m { flags = L.nub (Sleeping:flags m) }
    broadcast m (MobSeen  m' p)            | m == m' = m { destination = Just (at p) }
    broadcast m (MobHeard m' p)            | m == m' = m { destination = Just (at p) }
    broadcast m (DestinationSet m' p)      | m == m' = m { destination = Just p }
    broadcast m (DestinationAbrupted m' p) | m == m' = m { destination = Nothing }
    broadcast m (ItemPickedUp m' i)        | m == m' = m { inventory = inventory m ++ [i] }
    broadcast m (Equipped m' i)            | m == m' = equip m i
    broadcast m EndOfTurn                       = healDamaged m

    broadcast m otherwise = m

-- hurt    mob    dmg
hurtMob :: Mob -> Int -> Mob
hurtMob target dmg = target { hp = (hp target) - dmg, turnsToHeal = min 6 (turnsToHeal target + 1) }

-- heal damaged if mob not damaged 5 turns ago
healDamaged :: Mob -> Mob
healDamaged m
    | turnsToHeal m > 1 = m { turnsToHeal = turnsToHeal m - 1 } -- hasn't been 5 turns
    | hp m + 1 > mhp m  = m                                     -- can't heal more than max
    | otherwise         = m { hp = hp m + 1, turnsToHeal = 5 }

removePickedItem :: Mob -> Item -> DLevel -> DLevel
removePickedItem m i lvl = let is  = L.delete i (findItemsAt (at m) lvl)
                           in  replaceItemsAt (at m) lvl is

equip :: Mob -> Item -> Mob
equip m i = if isWeapon i then
                let weap = wielding (equipment m)
                in  m { equipment = (equipment m) { wielding = Just i },
                        inventory = maybeToList weap ++ L.delete i (inventory m) }
            else
                -- TODO replace same armor slot
                let armor = wearing (equipment m)
                in  m { equipment = (equipment m) { wearing = [i] },
                        inventory = armor ++ L.delete i (inventory m) }

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

updateSeen :: Env -> Env
updateSeen env =
    let lvl    = level env
        p      = player lvl
        points = M.keys (tiles lvl)
        seen'  = filter (canSee lvl p) points
        -- check what is on the current tile
        t      = findTileAt (at (player lvl)) lvl 
        stairE = if isDownStair (fromJust t) then [StairsSeen Down]
                 else if isUpStair (fromJust t) then [StairsSeen Up]
                 else []
        -- check if there are items here
        is     = findItemsAt (at (player lvl)) lvl
        itemE  = if length is > 0 then [ItemsSeen is] else []
    in  env { level  = lvl { seen = L.nub (seen' ++ seen lvl) },
              events = if recentlyMoved p (events env) then stairE ++ itemE ++ events env else events env }

-- moves mob, resetting the destination if we have reached it
moveMob :: Mob -> Point -> Mob
moveMob m p = let dest = if destination m == Just p then Nothing else destination m
              in  m { at = p, destination = dest }

-- check if mob recently moved to this tile
recentlyMoved :: Mob -> [Event] -> Bool
recentlyMoved m es = let es' = L.takeWhile (not . f) es
                         f (Moved m' _) = m == m'
                         f otherwise    = False
                     in  (== 0) . length $ L.filter isEndOfTurn es'
