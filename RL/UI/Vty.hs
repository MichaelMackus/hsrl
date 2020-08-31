module RL.UI.Vty where

import RL.UI.Common

import Graphics.Vty

instance UI Vty where
    uiInit = do
        cfg <- standardIOConfig
        mkVty cfg
    uiRender disp r =
        let layers = map getImage $ getSprites r
        in  update disp $ picForLayers layers
    uiEnd = shutdown
    uiInput disp = do
        e <- nextEvent disp
        case e of
            (EvKey (KChar c) _) -> return (KeyChar c)
            (EvKey otherwise _) -> return KeyUnknown

getImage :: Sprite -> Image
getImage ((0, 0), str) = string defAttr str
getImage ((x, y), str) = translate x y $ getImage ((0, 0), str)
