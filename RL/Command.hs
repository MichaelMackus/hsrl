module RL.Command where

import RL.Game
import RL.State
import RL.Random

import Control.Monad (when)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)

-- commands, which produce events
class Command c where
    dispatch :: c -> Game ()

data AttackCommand = AttackMob Mob | AttackPlayer Player | Attack Mob Mob

instance Command AttackCommand where
    dispatch (AttackMob    m) = getPlayer >>= (\p -> dispatch (Attack p m))
    dispatch (AttackPlayer m) = getPlayer >>= (\p -> dispatch (Attack m p))

    -- any mob attacking another mob
    dispatch (Attack attacker target) = when (canAttack target) $ do
            let weap = weaponProperties =<< getWielding attacker

            -- attack roll
            atk     <- roll (1 `d` 20)
            if atk + (fromMaybe 0 (bonus <$> weap)) >= thac0 attacker - mobAC target then do
                -- hit!
                dmg     <- roll (maybe (baseDmg attacker) dmgd weap)
                target' <- hurtMob target dmg
                when (isDead target') (send (Died target'))
                send (Attacked attacker target' dmg)
            else
                send (Missed attacker target)
        where
            -- hurt    mob    dmg
            hurtMob :: Mob -> Int -> Game Mob
            hurtMob target dmg = do
                    p  <- getPlayer
                    if target == p then do
                        let p' = hurtMob dmg p
                        setPlayer p'
                        return p'
                    else do
                        ms <- getMobs
                        setMobs (map (hurtMob dmg) ms)
                        return target'
                where
                    hurtMob  dmg m = if m == target then target' else m
                    target' = target { hp = (hp target) - dmg }

data MoveCommand = MovePlayer Offset | MoveMob Mob Point

instance Command MoveCommand where
    dispatch (MovePlayer (0, 0)) = return ()
    dispatch (MovePlayer off)    = do
            player <- getPlayer
            when (canMove player) $ do
                let newloc = addoff off player
                target <- getMobAt newloc
                maybe (dispatch $ MoveMob player newloc) (dispatch . AttackMob) target
        where
            addoff off = addOffset off . at

    dispatch (MoveMob m loc) = when (canMove m) $ do
        target <- getMobAt loc
        t      <- getTileAt loc
        when (isNothing target && maybe False isPassable t) $ do
            p <- getPlayer
            if m == p then
                setPlayer (moveMobTo loc m)
            else
                setMob (moveMobTo loc m)
            send (Moved m loc)

data ChangeLevelCommand = TakeStairs VerticalDirection | MobTakeStairs Mob VerticalDirection

instance Command ChangeLevelCommand where
    dispatch (TakeStairs v) = getPlayer >>= (\p -> dispatch (MobTakeStairs p v))

    dispatch (MobTakeStairs m v) = when (canMove m) $ do
        t <- getTileAt (at m)
        let lvl = do
            t'   <- t
            lvl' <- getStairLvl t'
            if (v == Up && isUpStair t') || (v == Down && isDownStair t') then
                Just lvl'
            else
                Nothing

        when (isJust lvl) $ do
            send (StairsTaken v)
            changeLevel (fromJust lvl)

-- toggle sleep flag
data SleepCommand = Wake Mob | Sleep Mob

instance Command SleepCommand where
    dispatch (Wake m) = do
        let fs = filter (not . isSleeping) (flags m)
        when (length fs /= length (flags m)) $ do
            setMob (m { flags = fs })
            send (Waken m)
    dispatch (Sleep m) = do
        let fs = filter (not . isSleeping) (flags m)
        when (length fs == length (flags m)) $ do
            setMob (m { flags = (Sleeping:fs) })
            send (Slept m)

-- User Interface commands
data UICommand = ToggleInventory | Pickup

instance Command UICommand where
    dispatch ToggleInventory = do
        env <- get
        if isTicking env then
            send (Inventory ViewInventory)
        else
            send UIClosed

    dispatch Pickup = do
        lvl <- getLevel
        let is = findItemsAt (at (player lvl)) lvl
        when (not (null is)) $ do
            let i = head is
                inv = inventory (player lvl) ++ [i]
                p = (player lvl) { inventory = inv }
                lvl' = replaceItemsAt (at p) lvl (drop 1 is)
            setLevel $ lvl' { player = p }
            send (ItemPickedUp (show i))
