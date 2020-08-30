module RL.Renderer (
    Renderer,
    Display,
    Sprite,
    Renderable(..),
    render,
    killRenderer,
    mkRenderer,
    getAction
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
import UI.HSCurses.Curses as Curses
#endif

type Renderer = ReaderT Display IO
type Sprite   = (Point, String) -- string somewhere on the screen

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
-- wait on user input & return the action chosen
getAction :: Display -> Env -> IO Action

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

getAction vty env = do
        e <- nextEvent vty
        return (toAction e)
    where
        -- gets game Action from user input
        toAction :: Event -> Action
        toAction (EvKey (KChar c) _) = charToAction c
        toAction (EvKey otherwise _) = None
#else
type Display = Window -- curses window
mkRenderer = do
    Curses.initCurses
    Curses.echo False
    Curses.cursSet Curses.CursorInvisible
    return Curses.stdScr
render r w = do
    let sprites = getSprites r
    wclear w
    forM_ sprites $ \((x,y), str) -> do
        Curses.mvWAddStr w y x str
    Curses.refresh
killRenderer w = Curses.endWin
getAction w env = do
    ch <- Curses.getCh
    case ch of
        (Curses.KeyChar ch) -> return (charToAction ch)
        otherwise           -> return None
#endif
