{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module RL.UI.SDL where

import RL.UI.Common

import Control.Monad (forM_)
import SDL
import SDL.Font
import qualified Data.Text as T

data SDLUI = SDLUI { window :: Window
                   , renderer :: Renderer
                   , font :: Font }

black = V4 0   0   0   255
white = V4 255 255 255 255

sdlUI cfg = do
    let maxWidth  = fromIntegral $ fontSize cfg * columns cfg
        maxHeight = fromIntegral $ fontSize cfg * rows cfg
        windowConfig = defaultWindow { windowResizable = False,
                                       windowInitialSize = V2 maxWidth maxHeight,
                                       windowMode = if fullscreen cfg then Fullscreen else Windowed }
    window   <- createWindow (T.pack $ uiTitle cfg) windowConfig
    renderer <- createRenderer window (-1) defaultRenderer
    SDL.Font.initialize
    font     <- load (fontPath cfg) (fontSize cfg)
    return UI
        { uiRender = \sprites -> do
            -- clear
            clear renderer
            rendererDrawColor renderer $= black
            -- draw sprites
            forM_ sprites $ \((x,y), str) -> do
                s    <- blended font white (T.pack str)
                tex  <- createTextureFromSurface renderer s
                (TextureInfo _ _ w h) <- queryTexture tex
                let fwidth = fontSize cfg
                    fheight = fontSize cfg
                    dest = Rectangle (P (V2 (fromIntegral $ x * fwidth) (fromIntegral $ y * fheight))) (V2 (fromIntegral w) (fromIntegral h))
                copy renderer tex Nothing (Just dest)
                freeSurface s
                destroyTexture tex
            -- update renderer
            present renderer
        , uiEnd = return ()
        , uiInput = do
            -- TODO
            return KeyUnknown
            -- e <- nextEvent disp
            -- case e of
            --     (EvKey (KChar c)        _) -> return (KeyChar c)
            --     (EvKey KUp              _) -> return KeyUp
            --     (EvKey KDown            _) -> return KeyDown
            --     (EvKey KRight           _) -> return KeyRight
            --     (EvKey KLeft            _) -> return KeyLeft
            --     (EvKey KEnter           _) -> return KeyEnter
            --     (EvKey KEsc             _) -> return KeyEscape
            --     (EvKey KBS              _) -> return KeyBackspace
            --     (EvMouseDown x y BLeft  _) -> return (KeyMouseLeft (x, y))
            --     (EvMouseDown x y BRight _) -> return (KeyMouseRight (x, y))
            --     otherwise                  -> return KeyUnknown
        }
