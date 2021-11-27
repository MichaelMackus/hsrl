module RL.UI.Common where

import RL.Game
import RL.UI.Sprite

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
                         , tilePath   :: FilePath
                         , tileSize   :: (Int, Int)
                         , fullscreen :: Bool }

data UI = UI { uiEnd    :: IO ()              -- shut down
             , uiRender :: [Sprite] -> IO ()  -- main render function
             , uiInput  :: IO (Key, [KeyMod]) -- wait on input from keyboard and return the single key inputted
             }
