{-# LANGUAGE TupleSections #-}
module RL.UI.SDL where

import RL.UI.Common

import Control.Monad (forM_, forM)
import Control.Concurrent (threadDelay)
import Data.Char (toUpper, toLower)
import Data.List (foldl')
import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Foreign.C.String (peekCString)
import SDL
import SDL.Font
import qualified Data.Text as T
import qualified SDL.Raw as Raw

black = V4 0   0   0   255
white = V4 255 255 255 255

initSdlUI :: UIConfig -> IO UI
initSdlUI cfg = do
    SDL.initialize [InitVideo]
    let maxWidth  = fromIntegral $ fontSize cfg * columns cfg
        maxHeight = fromIntegral $ fontSize cfg * rows cfg
        windowConfig = defaultWindow { windowResizable = False,
                                       windowInitialSize = V2 maxWidth maxHeight,
                                       windowMode = if fullscreen cfg then Fullscreen else Windowed }
        rendererConfig = defaultRenderer { rendererType = AcceleratedVSyncRenderer }
    window   <- createWindow (T.pack $ uiTitle cfg) windowConfig
    renderer <- createRenderer window (-1) rendererConfig
    Raw.startTextInput
    SDL.Font.initialize
    font     <- load (fontPath cfg) (fontSize cfg)
    return (sdlUI window renderer font cfg)

sdlUI :: Window -> Renderer -> Font -> UIConfig -> UI
sdlUI window renderer font cfg = UI
    { uiRender = \sprites -> do
        -- clear
        clear renderer
        rendererDrawColor renderer $= black
        -- draw sprites
        forM_ sprites $ \((x,y), str) -> do
            s    <- shaded font white black (T.pack str)
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
    , uiEnd = do
        -- cleanup SDL - properly closes window & ends so we can debug via ghci
        Raw.stopTextInput
        destroyRenderer renderer
        destroyWindow window
        SDL.Font.free font
        SDL.Font.quit
        SDL.quit
    , uiInput = do
        es   <- (:) <$> waitEvent <*> pollEvents
        mods <- toMods <$> getModState
        let k = foldl' f Nothing es
            f def e = case eventToKey e of
                        Nothing           -> def
                        Just (KeyChar ch) -> if KeyModShift `elem` mods then Just (KeyChar (toUpper ch))
                                             else Just (KeyChar (toLower ch))
                        Just k            -> Just k
            waitForInput = uiInput (sdlUI window renderer font cfg)
        maybe waitForInput (return . (,mods)) k
    }

toMods :: KeyModifier -> [KeyMod]
toMods mod = let shift = keyModifierLeftShift mod || keyModifierRightShift mod || keyModifierCapsLock mod
                 ctrl  = keyModifierLeftCtrl  mod || keyModifierRightCtrl  mod
                 alt   = keyModifierLeftAlt   mod || keyModifierRightAlt   mod
                 super = keyModifierLeftGUI   mod || keyModifierRightGUI   mod
             in  catMaybes [if shift then Just KeyModShift else Nothing
                           ,if ctrl  then Just KeyModCtrl  else Nothing
                           ,if alt   then Just KeyModAlt   else Nothing
                           ,if super then Just KeyModSuper else Nothing]

eventToKey :: Event -> Maybe Key
eventToKey e = case e of
    (Event _ (KeyboardEvent ed)) | keyboardEventKeyMotion ed == Pressed ->
        case keysymKeycode (keyboardEventKeysym ed) of
            KeycodeReturn    -> Just KeyEnter
            KeycodeUp        -> Just KeyUp
            KeycodeDown      -> Just KeyDown
            KeycodeLeft      -> Just KeyLeft
            KeycodeRight     -> Just KeyRight
            KeycodeEscape    -> Just KeyEscape
            KeycodeBackspace -> Just KeyBackspace
            (Keycode k)      -> let ch = toEnum (fromIntegral k) :: Char
                                in  if isAlpha k  then Just (KeyChar ch) -- Ctrl doesn't get TextInputEvent
                                    else               Nothing           -- So we can get capitalized symbol from TextInputEvent
    (Event _ (TextInputEvent (TextInputEventData _ t))) -> Just (textToKey t)
    (Event _ (MouseButtonEvent ed))
        | mouseButtonEventMotion ed == Released && mouseButtonEventButton ed == ButtonLeft ->
            let (P (V2 x y)) = mouseButtonEventPos ed
            in  Just (KeyMouseLeft (fromIntegral x, fromIntegral y))
        | mouseButtonEventMotion ed == Released && mouseButtonEventButton ed == ButtonRight ->
            let (P (V2 x y)) = mouseButtonEventPos ed
            in  Just (KeyMouseRight (fromIntegral x, fromIntegral y))
    (Event _ QuitEvent)        -> Just KeyQuit
    otherwise                  -> Nothing -- don't advance turn for non-user input events (such as window events)

-- check if keycode is alpha character
isAlpha :: Integral a => a -> Bool
isAlpha k = let x = fromIntegral k
            in (x >= fromEnum 'a' && x <= fromEnum 'z') ||
               (x >= fromEnum 'A' && x <= fromEnum 'Z')

textToKey :: T.Text -> Key
textToKey t = let str = T.unpack t
              in  if null str then KeyUnknown else KeyChar (head str)
