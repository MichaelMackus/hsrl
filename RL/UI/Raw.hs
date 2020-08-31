{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module RL.UI.Raw where

import RL.UI.Common

import Control.Monad (forM_)
import Data.Maybe (listToMaybe, fromMaybe)
import System.IO
import qualified Data.List as L

-- fallback terminal UI
instance UI () where
    uiInit = hSetBuffering stdin NoBuffering
    uiRender w r =
        let sprites         = getSprites r
            (lenX, lenY)    = (maxX sprites + 1, maxY sprites + 1)
            initRows        = replicate lenY (replicate lenX ' ')
            rows            = foldr f initRows sprites
            f ((x,y),str) l = let row      = map g (zip [0..] (l !! y))
                                  g (i,ch) = if ch == ' '
                                                && i >= x
                                                && i < length str then str !! (i - x) else ch
                              in  if y < length l then replaceAt l y row else l
        in  putStr (unlines rows)
    uiEnd w = return ()
    uiInput w = getChar >>= return . KeyChar

maxY :: [Sprite] -> Int
maxY = fromMaybe 0 . listToMaybe . reverse . L.sort . map toYs
    where toYs ((_,y),_) = y
maxX :: [Sprite] -> Int
maxX = fromMaybe 0 . listToMaybe . reverse . L.sort . map toXs
    where toXs ((x,_),str) = x + length str

replaceAt :: [a] -> Int -> a -> [a]
replaceAt l i a | i >= length l = error "Index greater than or equal length"
                | otherwise     = let (xs,_:ys) = splitAt i l
                                  in  xs ++ a:ys
