import RL.Client.AI
import RL.Client.Input
import RL.Client.Time
import RL.Game
import RL.Game.Sprites
import RL.Generator.Dungeon
import RL.Generator.Mobs
import RL.UI
import RL.State

import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import System.Environment
import System.Random

-- main game loop
--
-- first part of returned tuple is whether player has won or not
gameLoop :: (Env -> IO ()) -> (Env -> IO Action) -> Env -> IO (Bool, Env)
gameLoop draw nextAction env = do
    draw env            -- draw to screen
    a <- nextAction env -- wait for user input, and transform into Action

    let turn = do
            tick (nextTurn a)
            dead <- isDead <$> getPlayer
            return (isPlaying a && not dead)

        (playing, env') = runGame turn env
        (won, _)        = runGame isGameWon env'

    if playing && not won then
        gameLoop draw nextAction env'
    else
        return (won, env')

main = do
        -- allow user to customize display if supported
        flags <- getFlags
        let initUI = if "vty" `elem` flags || "tty" `elem` flags then initTTYUI
                     else initDefaultUI

        -- initialize game & launch game loop
        ui        <- initUI defaultUIConfig
        e         <- nextLevel conf
        (won, e') <- gameLoop (uiRender ui . getSprites) (getAction ui) e

        uiEnd ui

        -- print latest status messages
        mapM_ putStrLn (catMaybes (reverse (take 9 (map toMessage (events e')))))

        -- print final text
        if won then
            putStrLn "Congratulations, you won the game!"
        else
            putStrLn "Goodbye!"
    where
        conf = DungeonConfig {
            dwidth = 80,
            dheight = 15,
            maxTries = 10,
            prevLevel = Nothing,
            maxDepth  = 5,
            mobConfig = MobConfig {
                maxMobs   = 5,
                maxMTries = 5,
                difficultyRange = (2, 0)
            },
            itemConfig = ItemConfig {
                maxItems = 10
            },
            playerConfig = PlayerConfig {
                playerHp = 12,
                playerFov = 5
            }
        }

        getFlags =
            map (dropWhile (== '-')) . filter ("-" `isInfixOf`) <$> getArgs


getAction :: UI -> Env -> IO Action
getAction disp env = do
        k <- uiInput disp
        return (toAction $ fst k)
    where
        -- gets game Action from user input
        toAction :: Key -> Action
        toAction (KeyChar c) = charToAction c
        toAction  KeyUp      = Move North
        toAction  KeyRight   = Move East
        toAction  KeyLeft    = Move West
        toAction  KeyDown    = Move South
        toAction  KeyQuit    = Quit
        toAction otherwise   = None

-- generate a new level
nextLevel :: DungeonConfig -> IO Env
nextLevel conf = do
        g <- newStdGen
        let (lvl, s) = runGenerator levelGenerator conf (initState g)

        return (mkEnv lvl (gen s))
    where
        mkEnv lvl g = Env {
            dungeon = DTip lvl,
            level   = lvl,
            rng     = g,
            events  = [],
            menu    = NoMenu
        }
