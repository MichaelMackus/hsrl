module RL.Renderer (
    Renderer,
    Display,
    Sprite,
    Renderable(..),
    render,
    killRenderer,
    mkRenderer,
    getInput,
    Key(..)
) where

-- Basic VTY (virtual terminal) renderer.
--
-- The master "render" function takes a Renderable thing and the Display to
-- output onto.
--
-- The Renderable thing returns Sprites through "getSprites" which are strings
-- somewhere on the screen.

import RL.Client.Input
import RL.Game

import Control.Monad.Reader

#if defined(vty)
import Graphics.Vty
#else
import qualified UI.HSCurses.Curses as Curses
#endif

type Renderer = ReaderT Display IO
type Sprite   = (Point, String) -- string somewhere on the screen
data Key      = KeyChar Char | KeyUnknown

class Renderable r where
    getSprites :: r -> [Sprite]

-- game is renderable
-- instance Renderable (Game a) where
    -- getSprites g = getSprites (level g) ++ getMsgSprites (messages g)
    -- getSprites g = [((0, 0), show $ gets dungeon)]

-- main render function
render :: Renderable r => r -> Display -> IO ()
-- initialize rendering
mkRenderer :: IO Display
-- shut down
killRenderer :: Display -> IO ()
-- wait on input from keyboard and return the single key inputted
getInput :: Display -> IO Key

#if defined(vty)
type Display = Vty
mkRenderer = do
    cfg <- standardIOConfig
    mkVty cfg

render r disp = do
    let layers = map getImage $ getSprites r
    update disp $ picForLayers layers
killRenderer = shutdown

getImage :: Sprite -> Image
getImage ((0, 0), str) = string defAttr str
getImage ((x, y), str) = translate x y $ getImage ((0, 0), str)

getInput vty = do
        e <- nextEvent vty
        return (toKey e)
    where
        -- gets game Action from user input
        toKey :: Event -> Action
        toKey (EvKey (KChar c) _) = KeyChar c
        toKey (EvKey otherwise _) = KeyUnknown
#else
type Display = Curses.Window -- curses window
mkRenderer = do
    Curses.initCurses
    Curses.echo False
    Curses.cursSet Curses.CursorInvisible
    return Curses.stdScr
render r w = do
    let sprites = getSprites r
    Curses.wclear w
    forM_ sprites $ \((x,y), str) -> do
        Curses.mvWAddStr w y x str
    Curses.refresh
killRenderer w = Curses.endWin
getInput w = do
    ch <- Curses.getCh
    case ch of
        (Curses.KeyChar ch) -> return (KeyChar ch)
        otherwise           -> return KeyUnknown
#endif
