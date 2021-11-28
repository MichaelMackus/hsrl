module RL.UI.Vty (vtyUI) where

import RL.Game
import RL.UI.Common
import RL.UI.Sprite
import RL.Util

import Control.Monad
import Data.Either (lefts)
import Data.Maybe (fromJust)
import Graphics.Vty
import qualified Data.List as L

vtyUI :: UIConfig -> IO UI
vtyUI cfg = do
    vtyCfg <- (\c -> c { mouseMode = Just True }) <$> standardIOConfig
    out    <- outputForConfig vtyCfg
    (curCols, curRows) <- displayBounds out
    when (curRows < rows cfg || curCols < columns cfg)
        $ error "Terminal too small for VTY window!"
    disp   <- mkVty vtyCfg
    return UI
        { uiRender = \spr -> let pic = picForLayers . map sprToImage . condenseSprites $ spr
                                 bg  = Background ' ' (withBackColor defAttr (rgbColor 0 0 0))
                             in  update disp (pic { picBackground = bg })
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

-- condense chars next to each other to message sprite (for VTY performance)
condenseSprites :: [Sprite] -> [Sprite]
condenseSprites sprs = map fromVtySprite                $
                        concat . map (foldr f [])       $
                        map (L.sortBy (comparing sprX)) $
                        L.groupBy (equating sprY)       $
                        L.sortBy (comparing sprY)       $ 
                        map vtySprite sprs
    where sprY (VtyS (x,y) _ _) = y
          sprX (VtyS (x,y) _ _) = x
          f m (m':xs) = if canCombine m m' then (combine m m'):xs else m:m':xs
          f m []      = [m]

data VtySprite = VtyS Point String SpriteAttr

canCombine :: VtySprite -> VtySprite -> Bool
canCombine (VtyS _ _ attr) (VtyS _ _ attr') = attr == attr'

combine :: VtySprite -> VtySprite -> VtySprite
combine (VtyS pos s attr) (VtyS pos' s' attr') = if pos < pos' then VtyS pos (s ++ s') attr
                                                 else VtyS pos' (s' ++ s) attr

vtySprite :: Sprite -> VtySprite
vtySprite (CharSprite    pos ch  attr) = VtyS pos (unicodeSymbol ch:"") attr
vtySprite (MessageSprite pos str attr) = VtyS pos str attr
vtySprite (WallSprite    pos w   attr) = VtyS pos (wallSymbol w:"") attr

fromVtySprite :: VtySprite -> Sprite
fromVtySprite (VtyS pos (s:"") attr) = CharSprite pos s attr
fromVtySprite (VtyS pos str    attr) = MessageSprite pos str attr

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
