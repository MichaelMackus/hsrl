import RL.Game
import RL.IO
import RL.Map
import RL.Renderer
import RL.Setup
import RL.State

import Control.Monad.State
import Control.Monad.Reader
import Graphics.Vty

-- main game loop
gameLoop :: GameState ()
gameLoop = do
    draw               -- draw to screen
    -- todo map tick clients
    disp <- ask
    io $ do
        e <- nextEvent disp
        return ()

draw :: GameState()
draw = do
    game <- get -- from state
    disp <- ask -- from reader
    io $ render game disp

killGame :: GameState()
killGame = do
    disp <- ask
    io $ killRenderer disp

-- run game in black box IO action
runGame :: Game -> GameState () -> IO ()
runGame g gs = do
        vty <- mkRenderer        -- initialize VTY renderer
        runReaderT gameState vty -- run the Renderer
        return ()
    where
        -- run game                       Game state
        gameState  = runStateT gameState' g
        -- wrap        (pre)   gameLoop  (post)
        gameState' = setupGame >> gs >> killGame

main = runGame defaultGame gameLoop
