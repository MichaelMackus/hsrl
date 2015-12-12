module RL.Renderer (render, killRenderer, mkRenderer) where

import RL.Game
import RL.IO
import RL.Map
import RL.Mob
import RL.State

import Control.Monad.Reader
import Graphics.Vty

-- render dungeon level
render :: GameState ()
render = do
    lvl  <- getLevel
    msgs <- getMessages
    disp <- ask

    io $ do
        let layers = mobsToLayers (mobs lvl) ++ [levelToImg lvl] -- todo messages
        update disp $ picForLayers layers
        refresh disp

-- shut down
killRenderer :: GameState ()
killRenderer = do
    vty <- ask
    liftIO $ shutdown vty

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

levelToImg :: Level -> Image
levelToImg = mapToImg . tiles
    where
        toImg = string defAttr
        toStr = map fromTile

mapToImg :: Map -> Image
mapToImg = vertCat . map toImg . map toStr
    where
        toImg = string defAttr
        toStr = map fromTile

-- msgToImg :: [Message] -> Image
-- msgToImg = take 5 $ map toImg
--     where
--         toImg            = toImg' . enumerate
--         toImg'    (i, m) = translateY (0, i + 15) $ string defAttr m
--         enumerate        = zip [0..]

mobsToLayers :: [Mob] -> [Image]
mobsToLayers = map mobToImg

-- takes a mob and creates an (offset) Image
mobToImg :: Mob -> Image
mobToImg m = offsetMobImg (at m) $ mobImg m
    where
        mobImg              = char defAttr . symbol
        offsetMobImg (x, y) = translate x y
