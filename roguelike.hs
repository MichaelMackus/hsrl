import RL.Client.AI
import RL.Client.Input
import RL.Game
import RL.Setup
import RL.Setup.Dungeon
import RL.Renderer.Game

import Control.Monad.State
import Control.Monad.Reader

-- main game loop
gameLoop :: GameState ()
gameLoop = do
    draw               -- draw to screen
    u   <- tick user   -- user movement
    ai  <- tick AI     -- AI
    won <- isGameWon

    if won then
        sendMessage "Congratulations, you won the game!"
    else
        when (isPlaying u) gameLoop

draw :: GameState ()
draw = do
    game <- get        -- game state
    disp <- ask        -- disp reader
    io $ render game disp

main = runGame defaultGame gameLoop
