import RL.Client.AI
import RL.Client.Input
import RL.Game
import RL.Generator.Dungeon
import RL.Generator.DLevel
import RL.Renderer.Game
import RL.State

import Control.Monad.State
import Graphics.Vty
import System.Random

-- generate a new level
nextLevel :: GenConfig -> IO Env
nextLevel conf = mkEnv =<< ioGenerator_ levelGenerator conf
    where
        mkEnv lvl = newStdGen >>= (\g -> return $ Env {
            dungeon  = DTip lvl,
            level    = lvl,
            rng      = g,
            messages = []
        })

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

nextAction :: Vty -> IO Action
nextAction vty = toAction <$> nextEvent vty
    where
        -- gets game Action from user input
        toAction :: Event -> Action
        toAction (EvKey (KChar c) _) = charToAction c
        toAction (EvKey otherwise _) = None

main = do
        vty <- mkRenderer -- initialize VTY renderer

        let conf = GenConfig 80 15 10
        e <- nextLevel conf
        gameLoop (`render` vty) (nextAction vty) e
