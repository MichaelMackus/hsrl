import RL.Client.AI
import RL.Client.Input
import RL.Game
import RL.Generator.Dungeon
import RL.Renderer.Game
import RL.State

import Graphics.Vty
import System.Random

-- main game loop
--
-- first part of returned tuple is whether player has won or not
gameLoop :: (Env -> IO ()) -> IO Action -> Env -> IO (Bool, Env)
gameLoop draw nextAction env = do
    draw env           -- draw to screen
    a <- nextAction    -- wait for user input, and transform into Action

    let turn = do
            tick (UserInput a) -- user movement
            tick AI            -- AI
            return (isPlaying a)

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
        mapM_ putStrLn (reverse (take 9 (messages e')))

        -- print final text
        if won then
            putStrLn "Congratulations, you won the game!"
        else
            putStrLn "Goodbye!"
    where
        conf = GenConfig 80 15 10


nextAction :: Vty -> IO Action
nextAction vty = toAction <$> nextEvent vty
    where
        -- gets game Action from user input
        toAction :: Event -> Action
        toAction (EvKey (KChar c) _) = charToAction c
        toAction (EvKey otherwise _) = None

-- generate a new level
nextLevel :: GenConfig -> IO Env
nextLevel conf = do
        g <- newStdGen
        let (lvl, g') = generateLevel conf g

        return (mkEnv lvl g')
    where
        mkEnv lvl g = Env {
            dungeon  = DTip lvl,
            level    = lvl,
            rng      = g,
            messages = []
        }
