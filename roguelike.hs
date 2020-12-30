import RL.AI
import RL.Input
import RL.Game
import RL.Generator.Dungeon
import RL.Generator.Mobs
import RL.UI
import RL.Random

import Data.List (isInfixOf)
import Data.Ratio
import System.Environment
import System.Exit (exitSuccess)

helpMessages = [ "Usage: hsrl [--vty|--tty] [TILESET_PATH]"
                ,""
                ,"TILESET_PATH\tPath to custom tileset. By default uses res/image/Anno_16x16.png"
                ,"--vty or --tty\tSet to terminal mode (must be built with the vty build flag)." ]

-- main game loop
--
-- first part of returned tuple is whether player has won or not
gameLoop :: (Env -> IO ()) -> (Env -> IO [Event]) -> (Env -> IO Env) -> Env -> IO (Bool, Env)
gameLoop draw inputEvents endTurn env = do
    draw env              -- draw to screen
    es <- inputEvents env -- wait for user input, and transform into Action

    -- broadcast input events & then process end of turn
    env' <- endTurn (broadcastEvents env es)
    let playing = not (isDead (player (level env')) || isQuit env')
    if playing then
        gameLoop draw inputEvents endTurn env'
    else
        return (isQuit env', env')

doEndTurn :: MobConfig -> Env -> IO Env
doEndTurn conf env =
        if isTicking env then do
            env'    <- doAI env
            -- spawned <- spawnMobs env'
            let spawned = []
            return $ broadcastEvents env' (spawned ++ [EndOfTurn])
        else return env
    where
        doAI env = let ms = mobs (level env)
                   in  go ms env
            where go []    env = return env
                  go (m:t) env = do
                    es <- evalRand (runReaderT (automate m) env) <$> newStdGen
                    go t (broadcastEvents env es)

getEvents :: UI -> Env -> IO [Event]
getEvents disp env = do
    if not (isAutomated env) then do
        (k, m) <- uiInput disp
        return . evalRand (runReaderT (keyToEvents k m) env) =<< newStdGen
    else
        return . evalRand (runReaderT automatePlayer env)    =<< newStdGen

main = do
    -- allow user to customize display if supported, or tileset
    flags <- map (dropWhile (== '-')) . filter ("-" `isInfixOf`)         <$> getArgs
    args  <- filter (not . ("-" `isInfixOf`)) <$> getArgs

    when ("help" `elem` flags || "h" `elem` flags) $ do
        mapM_ putStrLn helpMessages
        exitSuccess

    let uiConfig = if not (null args) then defaultUIConfig { tilePath = head args }
                   else defaultUIConfig
    ui    <- if "vty" `elem` flags || "tty" `elem` flags then initTTYUI uiConfig
             else initDefaultUI uiConfig

    -- initialize game & launch game loop
    let newGame = do
        conf       <- mkDefaultConf
        e          <- (`broadcast` NewGame) <$> nextLevel conf
        (quit, e') <- gameLoop (uiRender ui) (getEvents ui) (doEndTurn (mobConfig conf)) e
        uiRender ui e' -- render last frame

        let waitForQuit = do
            -- wait for one last button press
            (k, m) <- uiInput ui
            if k == KeyChar ' ' || k == KeyQuit || k == KeyChar 'q' || k == KeyChar 'Q' || k == KeyChar 'r' || k == KeyEscape then return k
            else waitForQuit
        k <- if quit then return KeyQuit else waitForQuit
        if k == KeyChar 'r' then newGame
        else uiEnd ui
    newGame

    -- putStrLn "Your inventory:"
    -- putStrLn "---------------"
    -- mapM_ putStrLn (map ((" - " ++) . itemTrueName) (inventory (player (level (e')))))
    -- putStrLn ""
    -- putStrLn "Latest status messages:"
    -- putStrLn "-----------------------"
    -- mapM_ putStrLn (reverse (take 9 (catMaybes (map toMessage (events e')))))
    -- putStrLn ""

defaultUIConfig = UIConfig { columns = 80
                           , rows = 24
                           , uiTitle = "Dungeons of Haskell"
                           , tilePath = "res/image/Anno_16x16.png"
                           , tileSize = (16, 16)
                           , fullscreen = False }

mkDefaultConf = do
    itemApps <- randomItemAppearances
    return $ DungeonConfig {
        dwidth = 80,
        dheight = 15,
        maxTries = 10,
        prevLevel = Nothing,
        maxDepth  = 10,
        mobConfig = MobConfig {
            maxMobs = 10,
            minMobs = 4,
            mobGenChance = (1 % 3),
            mobSleepingChance = (1 % 2),
            difficultyRange = (2, 0)
        },
        itemConfig = ItemConfig {
            maxItems = 10,
            minItems = 3,
            itemGenChance = (1 % 5),
            itemAppearances = itemApps
        },
        playerConfig = PlayerConfig {
            playerHp = 12,
            playerFov = 5,
            playerItems = dagger:(replicate 3 (Item "Healing" (Potion Healing)))
        }
    }

-- generate a new level
nextLevel :: DungeonConfig -> IO Env
nextLevel conf = do
        g <- newStdGen
        let (lvl, s) = runGenerator levelGenerator conf (initState g)

        return (mkEnv lvl (gen s))
    where
        mkEnv lvl g = Env {
            dungeon    = DTip lvl,
            level      = lvl,
            events     = [],
            menu       = NoMenu
        }
