module RL.UI.Vty (vtyUI) where

import RL.UI.Common
import RL.Util

import Control.Monad (when)
import Graphics.Vty
import qualified Data.List as L

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
            let layers  = reverse (map getImage (condenseSprites sprites))
                picture = (picForLayers layers) { picBackground = bg }
                bg      = Background ' ' (withBackColor defAttr (rgbColor 0 0 0))
            in  update disp picture
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

-- helper function to condense sprites on the same row & attributes together
condenseSprites = concat . map spriteConcat . reverse . groupBy2 f
    where f spr spr'           = spriteY spr == spriteY spr' && colorsMatch spr spr'
          g spr spr'           = spriteX spr + 1 == spriteX spr' && colorsMatch spr spr'
          spriteX              = fst . spritePos
          spriteY              = snd . spritePos
          colorsMatch spr spr' = spriteFgColor spr == spriteFgColor spr' && spriteBgColor spr == spriteBgColor spr'

spriteConcat :: [Sprite] -> [Sprite]
spriteConcat [] = []
spriteConcat (s:xs) = reverse (go [] s xs)
    where go acc s [] = s:acc
          go acc s (s':xs) = if comp s s' then go acc (s { spriteStr = spriteStr s ++ spriteStr s' }) xs
                             else go (s:acc) s' xs
          comp spr spr' = spriteX spr + length (spriteStr spr) == spriteX spr'
          spriteX = fst . spritePos

getImage :: Sprite -> Image
getImage spr = let (x, y) = spritePos spr
                   (fr,fg,fb) = spriteFgColor spr
                   (br,bg,bb) = spriteBgColor spr
                   attr = withBackColor (withForeColor defAttr (rgbColor fr fg fb)) (rgbColor br bg bb)
               in  translate x y (string attr (spriteStr spr))

-- group by non-adjacent
-- https://stackoverflow.com/questions/53377577/groupby-function-which-groups-non-adjacent-elements
groupBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy2 = go [] where
  go acc comp [] = acc
  go acc comp (h:t) =
    let (hs, nohs) = L.partition (comp h) t
    in go ((h:hs):acc) comp nohs

groupSequential :: (a -> a -> Bool) -> [a] -> [[a]]
groupSequential comp = go [] where
    go acc [] = acc
    go acc xs@(p:ps) =
        let h = reverse (L.foldl' f [p] ps)
            t = drop (length h) xs
            f xs@(x:_) x' = if comp x x' then x':xs
                            else xs
        in  go (acc ++ [h]) t
