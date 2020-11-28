{-# LANGUAGE TupleSections #-}
module RL.UI.SDL where

import RL.UI.Common
import RL.UI.SDL.Image
import RL.Util

import Codec.Picture
import Control.Monad (forM_)
import Data.Char (toUpper, toLower)
import Data.IORef
import Data.List (foldl', lookup)
import Data.Maybe (catMaybes, isJust, fromJust)
import Foreign.C.Types (CInt)
import SDL
import qualified Data.Text as T
import qualified SDL.Raw as Raw

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
    font     <- mkTransparent <$> loadImage (fontPath cfg)
    atlasRef <- newIORef []
    Raw.startTextInput
    return (sdlUI window renderer (mkFontAtlas renderer font atlasRef) cfg)

fromColor (r,g,b) = V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255

sdlUI :: Window -> Renderer -> (Color -> Color -> IO Texture) -> UIConfig -> UI
sdlUI window renderer getFont cfg = UI
    { uiRender = \sprites -> do
        -- clear
        clear renderer
        rendererDrawColor renderer $= V4 0 0 0 255
        let maxWidth  = fromIntegral $ fontSize cfg * columns cfg
            maxHeight = fromIntegral $ fontSize cfg * rows cfg
        -- fontT <- createTexture renderer RGBA8888 TextureAccessStreaming (V2 maxWidth maxHeight)
        -- fontT <- createTextureFromSurface renderer fontS
        -- draw sprites
        forM_ sprites $ \(Sprite (x,y) str fg bg) -> do
            let fwidth = fromIntegral (fontSize cfg)
                fheight = fromIntegral (fontSize cfg)
                x' = fromIntegral x * fwidth
                y' = fromIntegral y * fwidth
                dest  = Rectangle (P (V2 x' y')) (V2 fwidth fheight)
                srcs  = map lookupChar str
                dests = offsetDest str dest
            fontT <- getFont fg bg
            forM_ (zip srcs dests) $ \(src, dest) -> copy renderer fontT (Just src) (Just dest)
        -- update renderer
        present renderer
    , uiEnd = do
        -- cleanup SDL - properly closes window & ends so we can debug via ghci
        Raw.stopTextInput
        -- TODO cleanup fonts
        destroyRenderer renderer
        destroyWindow window
        SDL.quit
    , uiInput = do
        es   <- (:) <$> waitEvent <*> pollEvents
        mods <- toMods <$> getModState
        let k = foldl' f Nothing es
            f def e = case eventToKey e of
                        Nothing                -> def
                        Just (KeyChar ch)      -> if KeyModShift `elem` mods then Just (KeyChar (toUpper ch))
                                                  else Just (KeyChar (toLower ch))
                        Just (KeyMouseLeft  p) -> Just $ KeyMouseLeft  (translateToWorld cfg p)
                        Just (KeyMouseRight p) -> Just $ KeyMouseRight (translateToWorld cfg p)
                        Just k                 -> Just k
            waitForInput = uiInput (sdlUI window renderer getFont cfg)
        maybe waitForInput (return . (,mods)) k
    }

type Atlas = [((Color, Color), Texture)]
mkFontAtlas :: Renderer -> Image PixelRGBA8 -> IORef Atlas -> (Color -> Color -> IO Texture)
mkFontAtlas r i atlasRef fg bg = do
    atlas <- readIORef atlasRef
    let tex = lookup (fg, bg) atlas
    if isJust tex then return (fromJust tex)
    else do
        -- change color of image
        let f (PixelRGBA8 _ _ _ 0) = toPixel bg
            f otherwise            = toPixel fg
        s   <- createSurfaceFromImage (pixelMap f i)
        tex <- createTextureFromSurface r s
        writeIORef atlasRef $ ((fg,bg), tex):atlas
        return tex

toPixel :: Color -> PixelRGBA8
toPixel (r,g,b) = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255

-- color :: Texture -> Image PixelRGBA8 -> Color -> Color -> IO ()
-- color t font fg bg = do
--     (pxs, pitch) <- lockTexture t Nothing
--     -- TODO alignment?
--     forM_ 
--     unlockTexture t

lookupChar :: Char -> Rectangle CInt
lookupChar ch
    | ch >= '`' && ch <= 'o' = xyToRect 6 (fromEnum ch - fromEnum '`')
    | ch >= 'p' && ch <= '~' = xyToRect 7 (fromEnum ch - fromEnum 'p')
    | ch >= '@' && ch <= 'O' = xyToRect 4 (fromEnum ch - fromEnum '@')
    | ch >= 'P' && ch <= '_' = xyToRect 5 (fromEnum ch - fromEnum 'P')
    | ch >= ' ' && ch <= '/' = xyToRect 2 (fromEnum ch - fromEnum ' ')
    | ch >= '0' && ch <= '?' = xyToRect 3 (fromEnum ch - fromEnum '0')
    | otherwise = xyToRect 0 15

offsetDest :: Num a => String -> Rectangle a -> [Rectangle a]
offsetDest str (Rectangle (P (V2 x y)) (V2 w h)) = map f (enumerate str)
    where f (i, ch) = let x' = x + fromIntegral i * w
                          y' = y
                      in  Rectangle (P (V2 x' y')) (V2 w h)

-- TODO don't hardcode font width/height
xyToRect :: Int -> Int -> Rectangle CInt
xyToRect y x = let s  = 16
                   x' = fromIntegral x * s
                   y' = fromIntegral y * s
               in  Rectangle (P $ V2 x' y') (V2 s s)

toMods :: KeyModifier -> [KeyMod]
toMods mod = let shift = keyModifierLeftShift mod || keyModifierRightShift mod || keyModifierCapsLock mod
                 ctrl  = keyModifierLeftCtrl  mod || keyModifierRightCtrl  mod
                 alt   = keyModifierLeftAlt   mod || keyModifierRightAlt   mod
                 super = keyModifierLeftGUI   mod || keyModifierRightGUI   mod
             in  catMaybes [if shift then Just KeyModShift else Nothing
                           ,if ctrl  then Just KeyModCtrl  else Nothing
                           ,if alt   then Just KeyModAlt   else Nothing
                           ,if super then Just KeyModSuper else Nothing]

-- translate point from pixel coordinates to tile (world) coordinates
translateToWorld :: UIConfig -> (Int, Int) -> (Int, Int)
translateToWorld cfg (x, y) = let x' = floor (fromIntegral x / fromIntegral (fontSize cfg))
                                  y' = floor (fromIntegral y / fromIntegral (fontSize cfg))
                              in  (x', y')

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
    (Event _ (MouseButtonEvent ed)) -- TODO transform from pixel coordinates to tile coordinates
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
