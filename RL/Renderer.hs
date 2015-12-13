module RL.Renderer (
    Renderer,
    Display,
    Sprite,
    Renderable(..),
    render,
    killRenderer,
    mkRenderer
) where

-- Basic VTY (virtual terminal) renderer.
--
-- The master "render" function takes a Renderable thing and the Display to
-- output onto.
--
-- The Renderable thing returns Sprites through "getSprites" which are strings
-- somewhere on the screen.

import Graphics.Vty

import Control.Monad.Reader

type Renderer = ReaderT Display IO

type Display  = Vty
type Sprite   = (Point, String) -- string somewhere on the screen
type Point    = (Int, Int)      -- cheap cop out, todo move Point declaration

class Renderable r where
    getSprites :: r -> [Sprite]

-- main render function
render :: Renderable r => r -> Display -> IO ()
render r disp = do
    let layers = map getImage $ getSprites r
    update disp $ picForLayers layers

-- shut down
killRenderer :: Display -> IO ()
killRenderer = shutdown

getImage :: Sprite -> Image
getImage ((0, 0), str) = string defAttr str
getImage ((x, y), str) = translate x y $ getImage ((0, 0), str)

-- -- render message panel
-- renderMsgs :: [Message] -> Renderer ()
-- renderMsgs ms = do
--     let msgImg = map ms msgToImg
--         pic    = picForLayers msgImg
--     vty <- ask
--     update vty pic

mkRenderer :: IO Vty
mkRenderer = do
    cfg <- standardIOConfig
    mkVty cfg
