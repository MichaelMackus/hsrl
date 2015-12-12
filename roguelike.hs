import RL.Game
import RL.Map
import RL.Renderer
import RL.Setup
import RL.State

import Control.Monad.State
import Control.Monad.Reader

-- main game loop
gameLoop :: GameState ()
gameLoop = do
    render                -- draw to screen
    -- todo map do workers
    -- playing <- doAction   -- handle user input
    when True gameLoop -- keep playing

-- run game in black box IO action
runGame :: Game -> GameState () -> IO ()
runGame g gs = do
        vty <- mkRenderer        -- initialize VTY renderer
        runReaderT gameState vty -- run the Renderer
        return ()
    where
        -- run game                       Game state
        gameState  = runStateT gameState' g
        -- wrap        (pre)   gameLoop   (post)
        gameState' = setupGame >> gs >> killRenderer

main = runGame defaultGame gameLoop
