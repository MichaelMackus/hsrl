module RL.Action (module RL.Action, module Control.Monad.Reader) where

import RL.Event
import RL.Game
import RL.Random

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe (fromMaybe, fromJust, maybeToList, catMaybes, isJust, listToMaybe)
import qualified Data.List as L

class Monad m => GameAction m where
    getEnv :: m Env
    insertEvent  :: Event -> m ()
    insertEvent ev = insertEvents [ev]
    insertEvents :: [Event] -> m ()
    insertEvents = mapM_ insertEvent

getPlayer :: GameAction m => m Player
getPlayer = player . level <$> getEnv

getEvents :: GameAction m => m [Event]
getEvents = events <$> getEnv

insertMessage :: GameAction m => Message   -> m ()
insertMessage m = insertEvent (EventMessage m)

gameEvent  :: GameAction m => GameEvent -> m ()
gameEvent e = insertEvent (GameUpdate e)
gameEvents :: GameAction m => [GameEvent] -> m ()
gameEvents = insertEvents . map GameUpdate

-- TODO movement action based on mob speed
-- walkAt :: GameAction m => Mob -> Point -> m ()

-- TODO 
-- canMove :: GameAction m => Mob -> m Bool
-- canMove 

-- add seen message if recently seen
seenMessage :: GameAction m => Message -> m ()
seenMessage m  = getEnv >>= \env ->
    let p      = player (level env)
        fresh  = recentGame (events env) || recentlyMoved p (events env) || recentlyPicked p (events env)
        newMsg = not (occurredThisTurn (== EventMessage m) (events env))
    in  when (fresh && newMsg) $ insertMessage m

-- activated AoO for mobs that are retreating
-- TODO -2 fleeer's ac ?
-- TODO no attack retreating *unless* player movement > mob... simulataneous
-- TODO initiative, meaning both mob & player get to attack each other
-- TODO player can simply attack mob he last attacked (i.e. the "target")
--
-- TODO alternatively, we can implement individual initiative for melees, and do attackRetreating as we are, but only if player movement >= mob
attackRetreating :: (GameAction m, MonadRandom m) => Mob -> m Bool
attackRetreating m = do
    env <- getEnv
    let target = listToMaybe (retreatedFrom env m)
    -- TODO 50/50 chance to miss if < speed than target, representing initiative
    when (isJust target) $ do
        insertMessage $ AttackOfOpportunity m (fromJust target)
        attack m (wielding (equipment m)) (fromJust target)
    return $ isJust target

attack :: (GameAction m, MonadRandom m) => Mob -> Maybe Item -> Mob -> m ()
attack attacker weap target = do
    let weapProp         = weaponProperties =<< weap
        invisiblePenalty = if Invisible `elem` flags target then 4 else 0
        invisibleBonus   = if Invisible `elem` flags attacker || Sleeping `elem` flags target then 4 else 0
        weapBonus        = fromMaybe 0 (weaponBonus <$> weapProp) - invisiblePenalty + invisibleBonus
    -- attack roll
    atk <- roll (1 `d` 20)
    insertMessage (AttackRoll attacker atk weapBonus)
    if atk + weapBonus >= thac0 attacker - mobAC target || atk == 20 then do
        -- hit!
        let dmgDie  = maybe (baseDmg attacker) weaponDamage weapProp
        dmg <- (+ strength attacker) <$> roll dmgDie
        damage dmg attacker target
    else
        gameEvent (Missed attacker target)

applyItem :: (GameAction m, MonadRandom m) => DLevel -> Mob -> Item -> m ()
applyItem lvl m i =
    if isEquippable i then equip m i
    else if isDrinkable i then drinkPotion m i
    else if isReadable i then readScroll lvl m i
    else return ()

-- fire projectile toward the mob's target
-- TODO randomly hit anybody in melee
fire :: (GameAction m, MonadRandom m) => DLevel -> Mob -> Item -> Mob -> m ()
fire lvl attacker proj m = when (isProjectile proj) $ do
    let eqp = equipment attacker
        isLaunching = maybe False (`launchesProjectile` proj) (launcher eqp)
    -- 50% chance for fragile ammunition (e.g. arrows) to survive
    r <- roll (1 `d` 2)
    let broken = isFragile proj && r == 1
    -- fire projectile if proper launcher equipped
    if isLaunching then do
        gameEvent (FiredProjectile attacker (fromJust (launcher eqp)) proj (at m) broken)
        attack attacker (launcher eqp) m
    else do
        gameEvent (ThrownProjectile attacker proj (at m) broken)
        attack attacker (Just proj) m

equip :: GameAction m => Mob -> Item -> m ()
equip m i = do
    let wield  = L.filter isTwoHanded (catMaybes [wielding (equipment m)])
        shld   = fromJust (shield (equipment m))
    when (isTwoHanded i && isShielded m) $ gameEvent (EquipmentRemoved m shld)
    when (isShield    i && handsFull  m) $ gameEvents (map (EquipmentRemoved m) wield)
    gameEvent (Equipped m i)

drinkPotion :: (GameAction m, MonadRandom m) => Mob -> Item -> m ()
drinkPotion m i = do
    gameEvent (Drank m i)
    case potionType i of
         Just Healing -> do
             healed <- roll (1 `d` 8)
             gameEvent (Healed m healed)
         Just Life -> gameEvent (GainedLife m 1)
         Just Acid -> do
             dmg <- roll (1 `d` 6)
             gameEvent (DrankAcid m)
             damage dmg m m
         Just Strength     -> gameEvent (GainedStrength m 1)
         Just Invisibility -> gameEvent $ GainedMobFlag m Invisible
         Just Confusion    -> gameEvent $ GainedMobFlag m ConfusedF
         Just Darkness     -> gameEvent $ GainedMobFlag m BlindedF
         otherwise         -> return ()

-- TODO change to be independent of player/mob
readScroll :: (GameAction m, MonadRandom m) => DLevel -> Mob -> Item -> m ()
readScroll lvl m i = do
    gameEvent (Read m i)
    case scrollType i of
         Just Fire -> do
             dmg <- roll (6 `d` 6)
             let p    = player lvl
                 ms   = L.filter (\m -> withinFov lvl p (at m)) (mobs lvl)
             gameEvent (CastFire m dmg)
             mapM_ (damage dmg p) ms
         Just Lightning -> do
             dmg <- roll (6 `d` 6)
             let p    = player lvl
                 ms   = L.filter (\m -> withinFov lvl p (at m)) (mobs lvl)
             gameEvent (CastLightning m dmg)
             mapM_ (damage dmg p) ms
         Just Teleport     -> do
            p <- randomPassable lvl
            gameEvents $ maybeToList (Teleported m <$> p)
         Just Mapping      -> gameEvent $ GainedMobFlag m (MappedF (depth lvl))
         Just Telepathy    -> gameEvent $ GainedMobFlag m TelepathicF
         otherwise         -> return ()


damage :: GameAction m => Int -> Mob -> Mob -> m ()
damage dmg attacker target = do
    gameEvent (Damaged attacker target dmg)
    when (isDead (target { hp = hp target - dmg })) $ gameEvent (Died target)
