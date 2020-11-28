module RL.UI.SDL.Image
    ( loadImage
     ,mkTransparent
     ,createSurfaceFromImage
     ,regionRect
    ) where

import Codec.Picture
import Control.Monad (forM, when)
import Foreign.C.Types (CInt)
import SDL
import System.Endian
import qualified Data.IntSet as I
import qualified Data.Vector.Storable as V

loadImage :: String -> IO (Image PixelRGBA8)
loadImage f = do
    r <- readImage f
    case r of
        Left  e -> error e
        Right i -> do
            return (convertRGBA8 i)

mkTransparent :: Image PixelRGBA8 -> Image PixelRGBA8
mkTransparent i = let p = pixelAt i 0 0
                  in  pixelMap (\p' -> if p' == p then PixelRGBA8 0 0 0 0 else p') i

createSurfaceFromImage :: Image PixelRGBA8 -> IO Surface
createSurfaceFromImage i = do
    pixels <- V.thaw (imageData i)
    let (w, h) = (fromIntegral (imageWidth i), fromIntegral (imageHeight i))
    createRGBSurfaceFrom pixels (V2 w h) (fromIntegral w*4) systemFormat

regionRect :: Integral a => (Int, Int) -> Image PixelRGBA8 -> Rectangle a
regionRect (x,y) i = let (w,h) = (fromIntegral (imageWidth i), fromIntegral (imageHeight i))
                     in  Rectangle (fromXY (x,y)) (V2 w h)


systemFormat :: PixelFormat
systemFormat = case getSystemEndianness of
                    LittleEndian -> ABGR8888
                    BigEndian    -> RGBA8888

fromXY :: Integral a => (Int, Int) -> Point V2 a
fromXY (x, y) = P (V2 (fromIntegral x) (fromIntegral y))

toXY :: Integral a => Point V2 a -> (Int, Int)
toXY (P (V2 x y)) = (fromIntegral x, fromIntegral y)
