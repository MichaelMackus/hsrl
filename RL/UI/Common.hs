module RL.UI.Common where

import RL.Game

type Color    = (Int, Int, Int) -- RGB color value
data Key      = KeyChar Char | KeyUp | KeyDown | KeyRight | KeyLeft |
                KeyEnter | KeyEscape | KeyBackspace |
                KeyMouseLeft Point | KeyMouseRight Point | KeyQuit |
                KeyUnknown
    deriving (Show, Eq)
data KeyMod   = KeyModAlt | KeyModCtrl | KeyModShift | KeyModSuper deriving (Show, Eq)

data UIConfig = UIConfig { columns :: Int
                         , rows    :: Int
                         -- below is for GUI-based UIs (e.g. SDL)
                         , uiTitle    :: String
                         , fontPath   :: FilePath
                         , fontSize   :: Int
                         , fullscreen :: Bool }

data UI = UI { uiEnd    :: IO ()              -- shut down
             , uiRender :: Env -> IO ()       -- main render function
             , uiInput  :: IO (Key, [KeyMod]) -- wait on input from keyboard and return the single key inputted
             }
