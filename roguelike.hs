import RL.Action
import RL.Draw
import RL.Game
import RL.Setup
import RL.State

import Control.Monad.State
import UI.HSCurses.Curses

-- main game loop
gameLoop :: GameState ()
gameLoop = do
    draw                     -- draw to screen
    playing <- doAction      -- handle user input
    when (playing) gameLoop  -- keep playing

-- run game in black box IO action
runGame :: Game -> GameState () -> IO ()
runGame g gs = runGameState >>= endGame
    where
        runGameState = runStateT initialState g
        initialState = setupGame >> gs

-- cleanup
endGame :: (a, Game) -> IO ()
endGame gs = endWin

main = runGame defaultGame gameLoop
