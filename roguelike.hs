import RL.Action
import RL.Game
import RL.Renderer
import RL.Setup
import RL.State

import Control.Monad.State
import Control.Monad.Reader

-- main game loop
gameLoop :: GameState ()
gameLoop = do
    draw                     -- draw to screen
    playing <- doAction      -- handle user input
    when (playing) gameLoop  -- keep playing

-- master draw function: draws player, mobs, and tiles
draw :: GameState ()
draw = do
    -- l <- getLevel
    -- m <- getMessages
    m <- getMap
    lift $ render m
    -- renderMsgs m

-- run game in black box IO action
runGame :: Game -> GameState () -> IO ()
runGame g gs = do
        vty <- mkRenderer
        runReaderT gameState vty
        return ()
    where
        gameState  = runStateT gameState' g
        gameState' = setupGame >> gs >> lift killRenderer

main = runGame defaultGame gameLoop
