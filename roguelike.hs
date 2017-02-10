import RL.Client.AI
import RL.Client.Input
import RL.Client.Time
import RL.Game
import RL.Generator.Dungeon
import RL.Generator.Mobs
import RL.Renderer.Game
import RL.State

import Data.Maybe (catMaybes)
import Graphics.Vty
import System.Random

-- main game loop
--
-- first part of returned tuple is whether player has won or not
gameLoop :: (Env -> IO ()) -> (Env -> IO Action) -> Env -> IO (Bool, Env)
gameLoop draw nextAction env = do
    draw env            -- draw to screen
    a <- nextAction env -- wait for user input, and transform into Action

    let turn = do
            tick (UserInput a) -- user movement
            tick AI            -- AI
            tick end           -- mob cleanup, new spawns, healing, etc.

            dead <- isDead <$> getPlayer
            return (isPlaying a && not dead)

        (playing, env') = runGame turn env
        (won, _)        = runGame isGameWon env'

    if playing && not won then
        gameLoop draw nextAction env'
    else
        return (won, env')

main = do
        vty       <- mkRenderer -- initialize VTY renderer
        e         <- nextLevel conf
        (won, e') <- gameLoop (`render` vty) (nextAction vty) e

        shutdown vty

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
                maxMTries = 5
            },
            playerConfig = PlayerConfig {
                playerHp = 12,
                playerFov = 5
            }
            -- TODO ItemConfig
        }


nextAction :: Vty -> Env -> IO Action
nextAction vty env = do
        e <- nextEvent vty
        return (toAction e)
    where
        -- gets game Action from user input
        toAction :: InputEvent -> Action
        toAction (EvKey (KChar c) _) = charToAction c
        toAction (EvKey otherwise _) = None

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
            events  = []
        }
