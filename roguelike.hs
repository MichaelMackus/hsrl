import RL.AI
import RL.Input
import RL.Game
import RL.Generator.Dungeon
import RL.Generator.Mobs
import RL.UI
import RL.Random

import Data.List (isInfixOf, filter)
import Data.Maybe (catMaybes)
import System.Environment


-- main game loop
--
-- first part of returned tuple is whether player has won or not
gameLoop :: (Env -> IO ()) -> (Env -> IO [Event]) -> Env -> IO (Bool, Env)
gameLoop draw inputEvents env = do
    draw env              -- draw to screen
    es <- inputEvents env -- wait for user input, and transform into Action

    -- broadcast input events & then process end of turn
    env' <- endTurn (broadcastEvents env es)
    let playing = not (isDead (player (level env')) || isQuit env')
    if playing then
        gameLoop draw inputEvents env'
    else
        return (isQuit env', env')

endTurn :: Env -> IO Env
endTurn env =
        if isTicking env then do
            env'    <- doAI env
            spawned <- spawnMobs env'
            return $ broadcastEvents env' (spawned ++ [EndOfTurn])
        else return env
    where
        spawnMobs env = do
            -- spawn new mobs
            let maxMobs   = 10   -- TODO make this configurable
                maxMTries = 5    -- TODO make this configurable
                ms        = mobs (level env)
            r <- roll (1 `d` 10) -- 10% chance to spawn new mob
            if (length ms < maxMobs && r == 1) then do
                g   <- newStdGen
                let s = mkGenState (level env) g
                    -- TODO save config somewhere..
                    (newMs', _) = runGenerator mobGenerator (MobConfig (length ms + 1) maxMTries (2,0)) s
                    spawned = filter (not . (`elem` ms)) newMs'
                return (map MobSpawned spawned)
            else return []
        doAI env = let ms = mobs (level env)
                   in  go ms env
            where go []    env = return env
                  go (m:t) env = do
                    es <- evalRand (runReaderT (automate m) env) <$> newStdGen
                    go t (broadcastEvents env es)

getEvents :: UI -> Env -> IO [Event]
getEvents disp env = do
    (k, m) <- if not (canAutomate env) then uiInput disp else return (KeyUnknown, [])
    g      <- newStdGen
    return $ evalRand (runReaderT (keyToEvents k m) env) g

main = do
    -- allow user to customize display if supported
    flags <- map (dropWhile (== '-')) . filter ("-" `isInfixOf`) <$> getArgs
    ui    <- if "vty" `elem` flags || "tty" `elem` flags then initTTYUI defaultUIConfig
             else initDefaultUI defaultUIConfig

    -- initialize game & launch game loop
    let newGame = do
        e          <- (`broadcast` NewGame) <$> nextLevel defaultConf
        (quit, e') <- gameLoop (uiRender ui) (getEvents ui) e
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

defaultConf = DungeonConfig {
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
        maxItems = 10,
        randomItemNames = []
    },
    playerConfig = PlayerConfig {
        playerHp = 12,
        playerFov = 5
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
            dungeon = DTip lvl,
            level   = lvl,
            events  = [],
            menu    = NoMenu
        }

-- generate mobs if necessary
-- mobSpawner :: Env -> Generator MobConfig DLevel [Mob]
-- mobSpawner 
