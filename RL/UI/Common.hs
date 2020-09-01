module RL.UI.Common where

type Sprite   = (Point, String) -- horizontal string somewhere on the screen
type Point    = (Int, Int)
data Key      = KeyChar Char | KeyUp | KeyDown | KeyRight | KeyLeft |
                KeyEnter | KeyEscape | KeyBackspace |
                KeyMouseLeft Point | KeyMouseRight Point | KeyQuit |
                KeyUnknown
    deriving (Show)

data UIConfig = UIConfig { columns :: Int
                         , rows    :: Int
                         -- below is for GUI-based UIs (e.g. SDL)
                         , initMouse  :: Bool
                         , uiTitle    :: String
                         , fontPath   :: FilePath
                         , fontSize   :: Int
                         , fullscreen :: Bool }

data UI = UI { uiEnd    :: IO ()              -- shut down
             , uiRender :: [Sprite] -> IO ()  -- main render function
             , uiInput  :: IO (Key) -- wait on input from keyboard and return the single key inputted
             }
