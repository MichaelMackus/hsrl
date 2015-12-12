module RL.Renderer (Renderer, Display, Sprite, Renderable(..), render, killRenderer, mkRenderer) where

import Graphics.Vty

import Control.Monad.Reader

type Renderer = ReaderT Display IO

type Display  = Vty
type Sprite   = (Point, String)
type Point    = (Int, Int) -- cheap cop out, todo move Point declaration

class Renderable r where
    getSprite  :: r -> Sprite
    getSprite  = head . getSprites

    getSprites :: r -> [Sprite]
    getSprites r = getSprite r : []

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

-- msgToImg :: [Message] -> Image
-- msgToImg = take 5 $ map toImg
--     where
--         toImg            = toImg' . enumerate
--         toImg'    (i, m) = translateY (0, i + 15) $ string defAttr m
--         enumerate        = zip [0..]
