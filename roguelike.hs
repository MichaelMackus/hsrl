import RL.AI
import RL.Game
import RL.IO
import RL.Input
import RL.Renderer
import RL.Setup
import RL.State

import Control.Monad.State
import Control.Monad.Reader

-- main game loop
gameLoop :: GameState ()
gameLoop = do
    draw                        -- draw to screen
    u  <- tick user             -- user movement
    ai <- tick AI               -- AI
    when (isPlaying u) gameLoop

draw :: GameState ()
draw = do
    game <- get        -- game state
    disp <- ask        -- disp reader
    io $ render game disp

shutdown :: r -> GameState r
shutdown r = do
        disp <- ask
        msgs <- getMessages
        io $ killRenderer disp                       -- shutdown display
        unless (null msgs) (shutdownMsg $ head msgs) -- last game message
        return r                                     -- unmodified result
    where
        shutdownMsg msg = io $ putStrLn ("Last message: " ++ msg)

main = runGame defaultGame gameLoop

-- run game in black box IO action
--
-- This initializes the Reader and State in order to give the gameLoop a
-- renderer & persistent game state.  You can call any function which runs
-- within GameState with this.
--
-- For example: "runGame defaultGame (roll $ 1 `d` 20)" will generate a random
-- IO Int from 1-20 (albeit rather inefficiently ;))
runGame :: Game -> GameState r -> IO r
runGame g gs = do
        vty    <- mkRenderer               -- initialize VTY renderer
        (r, s) <- runReaderT gameState vty -- run the Renderer
        return r
    where
        -- run game                       initial Game
        gameState  = runStateT gameState' g
        -- wrap        (pre)   gameLoop  (post)
        gameState' = setupGame >> gs >>= shutdown
