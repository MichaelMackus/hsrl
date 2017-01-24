import RL.Client.AI
import RL.Client.Input
import RL.Game
import RL.Generator.Dungeon
import RL.Renderer.Game
import RL.State

import Control.Monad.State
import Graphics.Vty
import System.Random

-- main game loop
gameLoop :: (Env -> IO ()) -> IO Action -> Env -> IO ()
gameLoop draw nextAction env = do
    draw env           -- draw to screen
    a <- nextAction    -- wait for user input, and transform into Action

    let turn = do
            u   <- tick a user   -- user movement
            ai  <- tick a AI     -- AI
            won <- isGameWon

            if won then do
                sendMessage "Congratulations, you won the game!"
                return False
            else
                return (isPlaying u)
        (playing, env') = runGame turn env

    when playing (gameLoop draw nextAction env')

main = do
        vty <- mkRenderer -- initialize VTY renderer
        e   <- nextLevel conf
        gameLoop (`render` vty) (nextAction vty) e
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
