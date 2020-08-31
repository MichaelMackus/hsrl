module RL.UI.Vty where

import RL.UI.Common

import Control.Monad (when)
import Graphics.Vty

instance UI Vty where
    uiInit cfg = do
        vtyCfg <- standardIOConfig
        out    <- outputForConfig vtyCfg
        (curCols, curRows) <- displayBounds out
        print (curRows, curCols)
        when (curRows < rows cfg || curCols < columns cfg)
            $ error "Terminal too small for VTY window!"
        mkVty vtyCfg
    uiRender disp r =
        let layers = map getImage $ getSprites r
        in  update disp $ picForLayers layers
    uiEnd = shutdown
    uiInput disp = do
        e <- nextEvent disp
        case e of
            (EvKey (KChar c)        _) -> return (KeyChar c)
            (EvKey KUp              _) -> return KeyUp
            (EvKey KDown            _) -> return KeyDown
            (EvKey KRight           _) -> return KeyRight
            (EvKey KLeft            _) -> return KeyLeft
            (EvKey KEnter           _) -> return KeyEnter
            (EvKey KEsc             _) -> return KeyEscape
            (EvKey KBS              _) -> return KeyBackspace
            (EvMouseDown x y BLeft  _) -> return (KeyMouseLeft (x, y))
            (EvMouseDown x y BRight _) -> return (KeyMouseRight (x, y))
            otherwise                  -> return KeyUnknown

getImage :: Sprite -> Image
getImage ((0, 0), str) = string defAttr str
getImage ((x, y), str) = translate x y $ getImage ((0, 0), str)
