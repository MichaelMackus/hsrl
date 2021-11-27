module RL.UI.Common where

import Control.Monad.State
import Data.IORef
import Data.Maybe (listToMaybe)

import RL.Game
import RL.UI.Sprite

import qualified Data.List as L

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

-- monad for rendering - this way we only draw what has changed
type RenderM = StateT [Sprite] IO

-- make a new renderer in the IO monad
mkRenderer :: (a -> RenderM ()) -> IO (a -> IO ())
mkRenderer k = do
    ref <- newIORef ([] :: [Sprite])
    return $ \x -> do
        s  <- readIORef ref
        s' <- execStateT (k x) s
        writeIORef ref s'
        return ()

drawChanged :: ([Sprite] -> IO ()) -> [Sprite] -> RenderM ()
drawChanged render newS = do
    changedSprs <- getChanged newS
    liftIO $ render changedSprs
    modify $ \s -> updateChanged s changedSprs

getChanged :: [Sprite] -> RenderM [Sprite]
getChanged newS = gets $ \s -> changed s newS

changed :: [Sprite] -> [Sprite] -> [Sprite]
changed s newS = filter (\spr -> not (spr `elem` s)) newS

updateChanged :: [Sprite] -> [Sprite] -> [Sprite]
updateChanged s newS = L.nubBy f (newS ++ s)
    where f a b = spritePos a == spritePos b

spriteAt :: Point -> [Sprite] -> Maybe Sprite
spriteAt p = listToMaybe . filter f
    where f spr = spritePos spr == p

