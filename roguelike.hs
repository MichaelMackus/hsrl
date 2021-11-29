import RL.AI
import RL.Player
import RL.Game
import RL.Generator.Dungeon
import RL.Generator.Mobs
import RL.Generator.Features
import RL.UI
import RL.UI.Sprite
import RL.Random

import Control.Monad.State
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ratio
import System.Environment
import System.Exit (exitSuccess)
import qualified Data.List as L

helpMessages = [ "Usage: hsrl [--vty|--tty] [TILESET_PATH]"
                ,""
                ,"TILESET_PATH\tPath to custom tileset. By default uses res/image/Anno_16x16.png"
                ,"--vty or --tty\tSet to terminal mode (must be built with the vty build flag)." ]

type Game = StateT GameState IO
data GameState = GameState { envState :: Env, inputState :: InputState, aiState :: [(Id, AIState)] }

-- main game loop
--
-- return value is True if player quit
gameLoop :: UI -> Game Bool
gameLoop disp = do
        doPlayerAction automatePlayer
        renderMap

        -- handle user input
        ready <- readyForInput <$> gets inputState
        when ready $ do
            (k, km) <- liftIO $ uiInput disp
            doPlayerAction (handleInput k km)

        -- handle AI
        ticking <- isTicking <$> gets inputState
        when ticking $ do
            ms <- gets (mobs . level . envState)
            mapM_ (doAI automate . mobId) ms
            -- spawnMobs -- TODO
            endTurn

        playing <- isPlaying <$> gets envState
        if playing then
            gameLoop disp
        else
            isQuit <$> gets envState
    where
        doAI :: AIAction a -> Id -> Game ()
        doAI k mid = do
                s <- L.lookup mid <$> gets aiState
                doAI' (fromMaybe (defaultAIState mid) s)
            where
                doAI' :: AIState -> Game ()
                doAI' aist = do
                    s <- get
                    g <- liftIO newStdGen
                    let (evs, aist') = runAI k (envState s) aist g
                    put $ s { aiState = map (\(i, s) -> if i == mid then (i, aist') else (i, s)) (aiState s), envState = broadcastEvents (envState s) evs }
        doPlayerAction :: PlayerAction a -> Game ()
        doPlayerAction k = doA k >> doA updateSeen
            where doA :: PlayerAction a -> Game ()
                  doA k = do
                    s <- get
                    g <- liftIO newStdGen
                    let (evs, is') = execPlayerAction k (envState s) (inputState s) g
                    put $ s { inputState = is', envState = broadcastEvents (envState s) evs }
        endTurn :: Game ()
        endTurn = modify $ \s -> s { envState = broadcastEvents (updateFlags (envState s)) [GameUpdate EndOfTurn] }
        renderMap = do
            s <- get
            liftIO (uiRender disp (gameSprites (spriteEnv s)))

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
        e          <- nextLevel conf
        (quit, s') <- runStateT (gameLoop ui) (defaultGameState e)
        uiRender ui (gameSprites (spriteEnv s')) -- render last frame

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

defaultGameState :: Env -> GameState
defaultGameState e = GameState (broadcast e (GameUpdate NewGame)) is []
    where is = defaultInputState { readied = listToMaybe (L.filter ((== "Dagger") . itemDescription) (inventory (player (level e)))) }

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
        -- TODO start player with ~3 throwing daggers, then we can nerf dagger range?
        -- TODO add dip command, which wastes/dilutes potion if not acid/poison?
        -- TODO allow throwing of potions at monsters to observe effects safely
        -- TODO add quiver/fire command
        -- TODO torches?
        -- TODO bundle of arrows/other items
        -- TODO gold & level up via gold
        -- TODO monster stashes/hordes
        -- TODO monster drops/items
        -- TODO ranged monsters
        playerConfig = PlayerConfig {
            playerHp = 12,
            playerFov = 5,
            -- playerItems = dagger:(replicate 3 (Item "Magic Draught" Draught)) -- TODO make fountains give magic draught
            playerItems = replicate 3 dagger
        },
        featureConfig = FeatureConfig {
            maxFeatures = 5,
            cellFeatureChance = 1 % 7,
            fItemAppearances = itemApps
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
            events     = []
        }

spriteEnv :: GameState -> SpriteEnv
spriteEnv (GameState env is _) = let ps = seenAtDepth (depth (level env)) is
                                 in  SpriteEnv env is ps
