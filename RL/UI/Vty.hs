module RL.UI.Vty (vtyUI) where

import RL.Game
import RL.UI.Common
import RL.UI.Sprite
import RL.Util

import Control.Monad
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
        { uiRender = \sprs ->
            let bg     = Background ' ' (withBackColor defAttr (rgbColor 0 0 0))
                layers = map sprToImage sprs
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

-- TODO more performance if we use string for each row in map
sprToImage :: Sprite -> Image
sprToImage (CharSprite    (x, y) ch  attr) = translate x y $ char   (sprColor attr) (unicodeSymbol ch)
sprToImage (MessageSprite (x, y) str attr) = translate x y $ string (sprColor attr) str
sprToImage (WallSprite    (x, y) w   attr) = translate x y $ char   (sprColor attr) (wallSymbol w)

sprColor :: SpriteAttr -> Attr
sprColor attr = withForeColor (withBackColor defAttr (uncurry3 rgbColor (bgColor attr))) (uncurry3 rgbColor (fgColor attr))

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

toKeyMods = map toKeyMod
    where toKeyMod MShift = KeyModShift -- TODO no shift
          toKeyMod MCtrl  = KeyModCtrl
          toKeyMod MMeta  = KeyModAlt
          toKeyMod MAlt   = KeyModSuper

-- converts an ASCII char to unicode
wallSymbol :: WallType -> Char
wallSymbol WallNESW = '╋'
wallSymbol WallNSE  = '┣'
wallSymbol WallNSW  = '┫'
wallSymbol WallNEW  = '┻'
wallSymbol WallSEW  = '┳'
wallSymbol WallNS   = '┃'
wallSymbol WallSW   = '┓'
wallSymbol WallSE   = '┏'
wallSymbol WallNW   = '┛'
wallSymbol WallNE   = '┗'
wallSymbol WallEW   = '━'
wallSymbol Wall     = '#'

unicodeSymbol :: Char -> Char
unicodeSymbol '{' = '⌠'
unicodeSymbol '=' = '⚌'
unicodeSymbol '_' = 'π'
unicodeSymbol '0' = 'Θ'
unicodeSymbol ch  = ch
--fromFeature Altar = '⛩'
--fromFeature (_) = '◛'
--fromFeature (_) = '⌸'
--fromFeature (_) = '⌸'
--fromFeature (_) = '⍯'
--fromFeature (_) = 'Ω'
--fromFeature (Door _) = '⌻'
