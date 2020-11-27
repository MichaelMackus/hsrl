module RL.UI.Vty (vtyUI) where

import RL.UI.Common
import RL.Util

import Control.Monad (when)
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
        { uiRender = \sprites ->
            let image         = vertCat (map flattenSprites rows)
                bg            = Background ' ' (withBackColor defAttr (rgbColor 0 0 0))
                (lenX, lenY)  = (maxX sprites, maxY sprites)
                initRows      = replicate lenY (replicate lenX (' ', (0,0,0), (0,0,0)))
                rows          = L.foldl' f initRows sprites
                f l s         = let row      = map g (zip [0..] (l !! spriteY s))
                                    g (x,ch) = let x' = x - spriteX s
                                               in if x >= spriteX s && x' < length (spriteStr s) then
                                                    (spriteStr s !! x', spriteFgColor s, spriteBgColor s)
                                                  else ch
                                in  replaceAt l (spriteY s) row

            in  update disp $ (picForImage image) { picBackground = bg }
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

type CharSprite = (Char, RL.UI.Common.Color, RL.UI.Common.Color)

flattenSprites :: [CharSprite] -> Image
flattenSprites = horizCat . map toImage . foldr f []
    where f c []    = [toSprite c]
          f c (h:t) = if fgClr c == spriteFgColor h && bgClr c == spriteBgColor h then (concatSpr c h:t)
                      else toSprite c:h:t
          toImage spr = string (color spr) (spriteStr spr)
          fgClr  (ch, c, c') = c
          bgClr  (ch, c, c') = c'
          color  spr         = withForeColor (withBackColor defAttr (uncurry3 rgbColor (spriteBgColor spr))) (uncurry3 rgbColor (spriteFgColor spr))
          ch     (c , _, _ ) = c

toSprite :: CharSprite -> Sprite
toSprite (ch, c, c') = Sprite (-1,-1) (ch:"") c c'

concatSpr :: CharSprite -> Sprite -> Sprite
concatSpr (c, _, _) spr = spr { spriteStr = c:spriteStr spr }

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

toKeyMods = map toKeyMod
    where toKeyMod MShift = KeyModShift -- TODO no shift
          toKeyMod MCtrl  = KeyModCtrl
          toKeyMod MMeta  = KeyModAlt
          toKeyMod MAlt   = KeyModSuper

maxX :: [Sprite] -> Int
maxX = foldr f 0
    where f s x = let x' = spriteX s + length (spriteStr s)
                  in  max x x'

maxY :: [Sprite] -> Int
maxY = foldr f 0
    where f s y = let y' = spriteY s + 1
                  in  max y y'

replaceAt :: [a] -> Int -> a -> [a]
replaceAt l i a | i >= length l = error "Index greater than or equal length"
                | otherwise     = let (xs,_:ys) = splitAt i l
                                  in  xs ++ a:ys

spriteX = fst . spritePos
spriteY = snd . spritePos

condenseSprites = concat . map spriteConcat . reverse . groupBy2 f
    where f spr spr'           = spriteY spr == spriteY spr' && colorsMatch spr spr'
          colorsMatch spr spr' = spriteFgColor spr == spriteFgColor spr' && spriteBgColor spr == spriteBgColor spr'

spriteConcat :: [Sprite] -> [Sprite]
spriteConcat [] = []
spriteConcat (s:xs) = reverse (go [] s xs)
    where go acc s [] = s:acc
          go acc s (s':xs) = if comp s s' then go acc (s { spriteStr = spriteStr s ++ spriteStr s' }) xs
                             else go (s:acc) s' xs
          comp spr spr' = spriteX spr + length (spriteStr spr) == spriteX spr'

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
