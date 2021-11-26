{-# LANGUAGE TupleSections #-}
module RL.UI.SDL where

import RL.Game hiding (Event)
import RL.UI.Common
import RL.UI.SDL.Image
import RL.UI.Sprite
import RL.Util

import Codec.Picture
import Control.Monad
import Data.Char (toUpper, toLower)
import Data.IORef
import Data.List (foldl')
import Data.Maybe (catMaybes, isJust, fromJust)
import Foreign.C.Types (CInt)
import SDL hiding (Point)
import qualified Data.Text as T
import qualified SDL.Raw as Raw

initSdlUI :: UIConfig -> IO UI
initSdlUI cfg = do
    SDL.initialize [InitVideo]
    let maxWidth  = fromIntegral $ (fst (tileSize cfg)) * columns cfg
        maxHeight = fromIntegral $ (snd (tileSize cfg)) * rows cfg
        windowConfig = defaultWindow { windowResizable = False,
                                       windowInitialSize = V2 maxWidth maxHeight,
                                       windowMode = if fullscreen cfg then Fullscreen else Windowed }
        rendererConfig = defaultRenderer { rendererType = AcceleratedVSyncRenderer }
    window   <- createWindow (T.pack $ uiTitle cfg) windowConfig
    renderer <- createRenderer window (-1) rendererConfig
    tiles    <- mkTransparent <$> loadImage (tilePath cfg)
    atlasRef <- newIORef []
    Raw.startTextInput
    return (sdlUI window renderer (mkFontAtlas renderer tiles atlasRef) cfg)

fromColor (r,g,b) = V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255

sdlUI :: Window -> Renderer -> (Color -> Color -> IO Texture) -> UIConfig -> UI
sdlUI window renderer getFont cfg = UI
    { uiRender = \env -> do
        -- clear
        clear renderer
        rendererDrawColor renderer $= V4 0 0 0 255
        let fontW    = fst (tileSize cfg)
            fontH    = snd (tileSize cfg)
            fontS    = (fontW, fontH)
        -- draw sprites
        forM_ (getSprites env) $ \spr ->
            case spr of
                Left (Message (x,y) str fg bg) -> do
                    let x' = fromIntegral x * fromIntegral fontW
                        y' = fromIntegral y * fromIntegral fontH
                        dest  = Rectangle (P (V2 x' y')) (V2 (fromIntegral fontW) (fromIntegral fontH))
                        srcs  = map (lookupChar fontS) str
                        dests = offsetDest str dest
                    fontT <- getFont fg bg
                    forM_ (zip srcs dests) $ \(src, dest) -> copy renderer fontT (Just src) (Just dest)
                Right s@(Sprite (x,y) ch fg bg) -> do
                    let x'    = fromIntegral x * fromIntegral fontW
                        y'    = fromIntegral y * fromIntegral fontH
                        dest  = Rectangle (P (V2 x' y')) (V2 (fromIntegral fontW) (fromIntegral fontH))
                        sprCh = tileSymbol env (spritePos s) (spriteChar s)
                        src   = lookupChar fontS sprCh
                    -- TODO mask not working any more?
                    fontT <- getFont (tileColor sprCh fg) (tileColor sprCh bg)
                    copy renderer fontT (Just src) (Just dest)
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
mkFontAtlas rend i atlasRef fg@(r,g,b) bg = do
    atlas <- readIORef atlasRef
    let tex = lookup (fg, bg) atlas
    if isJust tex then return (fromJust tex)
    else do
        -- change color of image
        let f (PixelRGBA8 _  _  _  0) = toPixel bg
            f (PixelRGBA8 r' g' b' a) = PixelRGBA8 (mask r r') (mask g g') (mask b b') a
            -- use color mask to allow color range in tile map
            mask c c' = fromIntegral $ if c' == 0 then c else floor $ (fromIntegral c / 255) * fromIntegral c'
        s   <- createSurfaceFromImage (pixelMap f i)
        tex <- createTextureFromSurface rend s
        writeIORef atlasRef $ ((fg,bg), tex):atlas
        return tex

toPixel :: Color -> PixelRGBA8
toPixel (r,g,b) = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255

-- custom color overrides for SDL frontend
tileColor :: Char -> Color -> Color
-- tileColor '.' (0,0,0) = (0,0,0)
-- tileColor '.' fg      = (100,100,100)
-- tileColor '#' (0,0,0) = (100,100,100)
-- tileColor '#' fg      = fg
tileColor ch  color   = color

-- lookup char in tile map
lookupChar :: (Int, Int) -> Char -> Rectangle CInt
lookupChar s ch = let tileCols = 16 -- TODO don't hardcode
                      col = fromEnum ch `mod` tileCols
                      row = floor $ fromIntegral (fromEnum ch) / fromIntegral tileCols
                  in  xyToRect s row col


offsetDest :: Num a => String -> Rectangle a -> [Rectangle a]
offsetDest str (Rectangle (P (V2 x y)) (V2 w h)) = map f (enumerate str)
    where f (i, ch) = let x' = x + fromIntegral i * w
                          y' = y
                      in  Rectangle (P (V2 x' y')) (V2 w h)

xyToRect :: (Int, Int) -> Int -> Int -> Rectangle CInt
xyToRect (w, h) y x = let x' = fromIntegral x * fromIntegral w
                          y' = fromIntegral y * fromIntegral h
                      in  Rectangle (P $ V2 x' y') (V2 (fromIntegral w) (fromIntegral h))

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
translateToWorld cfg (x, y) = let w  = fromIntegral . fst $ tileSize cfg
                                  h  = fromIntegral . snd $ tileSize cfg
                                  x' = floor (fromIntegral x / w)
                                  y' = floor (fromIntegral y / h)
                              in  (x', y')

eventToKey :: Event -> Maybe Key
eventToKey e = case e of
    (Event _ (KeyboardEvent ed)) | keyboardEventKeyMotion ed == Pressed ->
        case keysymKeycode (keyboardEventKeysym ed) of
            KeycodeReturn    -> Just KeyEnter
            KeycodeReturn2   -> Just KeyEnter
            KeycodeKPEnter   -> Just KeyEnter
            KeycodeUp        -> Just KeyUp
            KeycodeDown      -> Just KeyDown
            KeycodeLeft      -> Just KeyLeft
            KeycodeRight     -> Just KeyRight
            KeycodeEscape    -> Just KeyEscape
            KeycodeBackspace -> Just KeyBackspace
            KeycodeTab       -> Just (KeyChar '\t')
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

-- converts an ASCII char to tile
tileSymbol :: Env -> Point -> Char -> Char
tileSymbol env p '#' =
    let wallChar = seenWallType env p
    in  case wallChar of
            Just WallNESW -> xyToChar 12 14
            Just WallNSE  -> xyToChar 12 12
            Just WallNSW  -> xyToChar 11 9
            Just WallNEW  -> xyToChar 12 10
            Just WallSEW  -> xyToChar 12 11
            Just WallNS   -> xyToChar 11 10
            Just WallSW   -> xyToChar 11 11
            Just WallSE   -> xyToChar 12 9
            Just WallNW   -> xyToChar 11 12
            Just WallNE   -> xyToChar 12 8
            Just WallEW   -> xyToChar 12 13
            Just Wall     -> xyToChar 11 0
            Nothing       -> '#'
tileSymbol env p '{' = xyToChar 14 2
tileSymbol env p '=' = xyToChar 14 9
-- tileSymbol env p '=' = xyToChar 13 1
tileSymbol env p '_' = xyToChar 14 3
tileSymbol env p '0' = xyToChar 13 3
tileSymbol env p ch  = ch
--fromFeature Altar = '⛩'
--fromFeature (_) = '◛'
--fromFeature (_) = '⌸'
--fromFeature (_) = '⌸'
--fromFeature (_) = '⍯'
--fromFeature (_) = 'Ω'
--fromFeature (Door _) = '⌻'

xyToChar :: Int -> Int -> Char
xyToChar row col = toEnum (col + (row * 16)) -- TODO don't hardcode 16 cols

