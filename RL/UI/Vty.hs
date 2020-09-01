module RL.UI.Vty (vtyUI) where

import RL.UI.Common

import Control.Monad (when)
import Graphics.Vty

vtyUI :: UIConfig -> IO UI
vtyUI cfg = do
    vtyCfg <- standardIOConfig
    out    <- outputForConfig vtyCfg
    (curCols, curRows) <- displayBounds out
    when (curRows < rows cfg || curCols < columns cfg)
        $ error "Terminal too small for VTY window!"
    disp   <- mkVty vtyCfg
    return UI
        { uiRender = \sprites ->
            let layers = reverse $ map getImage sprites
            in  update disp $ picForLayers layers
        , uiEnd = shutdown disp
        , uiInput = do
            e <- nextEvent disp
            case e of
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
                otherwise                     -> return (KeyUnknown, [])
        }

toKeyMods = map toKeyMod
    where toKeyMod MShift = KeyModShift -- TODO no shift
          toKeyMod MCtrl  = KeyModCtrl
          toKeyMod MMeta  = KeyModAlt
          toKeyMod MAlt   = KeyModSuper

getImage :: Sprite -> Image
getImage ((0, 0), str) = string defAttr str
getImage ((x, y), str) = translate x y $ getImage ((0, 0), str)
