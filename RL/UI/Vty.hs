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

-- faster version but no colors
-- condenseSprites :: [Sprite] -> [Sprite]
-- condenseSprites = map getRowSprite . enumerate . unenumerate2d . map sym
--     where
--         sym spr = (spritePos spr, head (spriteStr spr))
--         getRowSprite ((y), ts) = Sprite (0, y) ts (255,255,255) (0,0,0)

-- helper function to condense sprites on the same row & attributes together
-- TODO improve performance
condenseSprites = concat . map (map spriteConcat . L.groupBy g) . groupBy2 f
    where f spr spr'  = let (x,  y ) = spritePos spr
                            (x', y') = spritePos spr'
                        in  y == y' && spriteFgColor spr == spriteFgColor spr' && spriteBgColor spr == spriteBgColor spr'
          g spr spr'  = let (x,  y ) = spritePos spr
                            (x', y') = spritePos spr'
                        in  x + 1 == x' && spriteFgColor spr == spriteFgColor spr' && spriteBgColor spr == spriteBgColor spr'

-- concat sprite strings discarding positions of tails
spriteConcat :: [Sprite] -> Sprite
spriteConcat [] = error "Empty sprite list for sprite concat"
spriteConcat (spr:xs) = foldr f spr xs
    where f spr' spr = spr { spriteStr = spriteStr spr ++ spriteStr spr' }

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
