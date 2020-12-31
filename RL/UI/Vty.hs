module RL.UI.Vty (vtyUI) where

import RL.Game
import RL.UI.Common
import RL.UI.Sprite
import RL.Util

import Data.Either (lefts)
import Data.Maybe (fromJust)
import Graphics.Vty

vtyUI :: UIConfig -> IO UI
vtyUI cfg = do
    vtyCfg <- (\c -> c { mouseMode = Just True }) <$> standardIOConfig
    out    <- outputForConfig vtyCfg
    (curCols, curRows) <- displayBounds out
    when (curRows < rows cfg || curCols < columns cfg)
        $ error "Terminal too small for VTY window!"
    disp   <- mkVty vtyCfg
    return UI
        { uiRender = \env ->
            let bg      = Background ' ' (withBackColor defAttr (rgbColor 0 0 0))
                msgImgs = map msgToImage . lefts $ getSprites env
                mapImg  = vertCat $ map (tileToImage env) (enumerate (toTiles $ level env))
                layers  = msgImgs ++ [mapImg]
            in  update disp $ (picForLayers layers) { picBackground = bg }
        , uiEnd = shutdown disp
        , uiInput = do
            let f e = case e of
                        (EvKey (KChar c)        mods) -> return ((KeyChar c), toKeyMods mods)
                        (EvKey KUp              mods) -> return (KeyUp, toKeyMods mods)
                        (EvKey KDown            mods) -> return (KeyDown, toKeyMods mods)
                        (EvKey KRight           mods) -> return (KeyRight, toKeyMods mods)
                        (EvKey KLeft            mods) -> return (KeyLeft, toKeyMods mods)
                        (EvKey KEnter           mods) -> return (KeyEnter, toKeyMods mods)
                        (EvKey KEsc             mods) -> return (KeyEscape, toKeyMods mods)
                        (EvKey KBS              mods) -> return (KeyBackspace, toKeyMods mods)
                        (EvMouseDown x y BLeft  mods) -> return ((KeyMouseLeft (x, y)), toKeyMods mods)
                        (EvMouseDown x y BRight mods) -> return ((KeyMouseRight (x, y)), toKeyMods mods)
                        otherwise                     -> nextEvent disp >>= f
            nextEvent disp >>= f
        }

tileToImage :: Env -> (Int, [Tile]) -> Image
tileToImage env (y, row) = flattenSprites . map spr $ enumerate row
    where spr (x, _)     = let s = spriteAt env (x, y)
                           in  s { spriteChar = unicodeSymbol env (x, y) (spriteChar s) } -- TODO CLI switch for unicode

msgToImage :: Message -> Image
msgToImage msg = let (x,y) = messagePos msg
                 in  translate x y $ string (color msg) (message msg)
    where color spr = withForeColor (withBackColor defAttr (uncurry3 rgbColor (messageBgColor spr))) (uncurry3 rgbColor (messageFgColor spr))

flattenSprites :: [Sprite] -> Image
flattenSprites = horizCat . map msgToImage . foldr f []
    where f c []    = [mkMessage c]
          f c (h:t) = if spriteFgColor c == messageFgColor h && spriteBgColor c == messageBgColor h then (concatMsg c h):t
                      else (mkMessage c):h:t

mkMessage :: Sprite -> Message
mkMessage (Sprite _ ch c c') = Message (0,0) (ch:"") c c'

concatMsg :: Sprite -> Message -> Message
concatMsg spr msg = msg { message = spriteChar spr:message msg }

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

toKeyMods = map toKeyMod
    where toKeyMod MShift = KeyModShift -- TODO no shift
          toKeyMod MCtrl  = KeyModCtrl
          toKeyMod MMeta  = KeyModAlt
          toKeyMod MAlt   = KeyModSuper

-- converts an ASCII char to unicode
unicodeSymbol :: Env -> Point -> Char -> Char
unicodeSymbol env p '#' =
    let wallChar = seenWallType env p
    in  case wallChar of
            Just WallNESW -> '╋'
            Just WallNSE  -> '┣'
            Just WallNSW  -> '┫'
            Just WallNEW  -> '┻'
            Just WallSEW  -> '┳'
            Just WallNS   -> '┃'
            Just WallSW   -> '┓'
            Just WallSE   -> '┏'
            Just WallNW   -> '┛'
            Just WallNE   -> '┗'
            Just WallEW   -> '━'
            Just Wall     -> '#'
            Nothing       -> ' '
unicodeSymbol env p '{' = '⌠'
unicodeSymbol env p '=' = '⚌'
unicodeSymbol env p '_' = 'π'
unicodeSymbol env p '0' = 'Θ'
unicodeSymbol env p ch  = ch
--fromFeature Altar = '⛩'
--fromFeature (_) = '◛'
--fromFeature (_) = '⌸'
--fromFeature (_) = '⌸'
--fromFeature (_) = '⍯'
--fromFeature (_) = 'Ω'
--fromFeature (Door _) = '⌻'
