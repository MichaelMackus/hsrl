module RL.Renderer (Renderer, render, killRenderer, mkRenderer) where

import RL.Map
import RL.Mob

import Control.Monad.Reader
import Graphics.Vty

-- renderer monad       layers env
--
-- TODO Manages a list of renderable Image layers.
-- type Renderer = StateT [Image] Display
type Renderer = Display

-- reader monad        VTY terminal environment
type Display = ReaderT Vty IO

-- render dungeon level
render :: Map -> Renderer ()
render lvl = do
        -- let pic = picForLayers layers
        let pic = picForImage $ levelToImg lvl
        vty <- ask
        liftIO $ update vty pic
        -- todo refresh ??
    -- where layers = [ levelToImg lvl,
    --                  mobsToImg  ms ]

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

-- shut down
killRenderer :: Renderer ()
killRenderer = do
    vty <- ask
    liftIO $ shutdown vty

levelToImg :: Map -> Image
levelToImg = vertCat . map toImg . map toStr
    where
        toImg = string defAttr
        toStr = map fromTile

-- msgToImg :: [Message] -> Image
-- msgToImg = take 5 $ map toImg
--     where
--         toImg            = toImg' . enumerate
--         toImg'    (i, m) = translateY (0, i + 15) $ string defAttr m
--         enumerate        = zip [0..]

mobsToImg :: [Mob] -> Image
mobsToImg = vertCat . map mobToImg

-- takes a mob and creates an (offset) Image
mobToImg :: Mob -> Image
mobToImg m = offsetMobImg (at m) $ mobImg m
    where
        mobImg              = char defAttr . symbol
        offsetMobImg (x, y) = translate x y

setupRenderer :: IO ()
setupRenderer = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    let line0 = string (defAttr ` withForeColor ` green) "first line"
        line1 = string (defAttr ` withBackColor ` blue) "second line"
        img = line0 <-> line1
        pic = picForImage img
    update vty pic
    e <- nextEvent vty
    shutdown vty
    print ("Last event was: " ++ show e)
