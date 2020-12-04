module RL.Util where

import Data.Maybe (isJust)
import Data.Foldable (foldl')
import qualified Data.List as L

-- helper functions

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

enumerate1 :: [a] -> [(Int, a)]
enumerate1 = zip [1..]


enumerate2d :: [[a]] -> [((Int, Int), a)]
enumerate2d = concat . map enumerateRow . zip [0..] where
    enumerateRow (y, r) = map (\(x, t) -> ((x, y), t)) $ zip [0..] r

unenumerate :: [(Int, a)] -> [a]
unenumerate [] = error "unenumerate requires a list"
unenumerate it = snd $ unzip it

unenumerate2d :: [((Int, Int), a)] -> [[a]]
unenumerate2d = map (map snd) . L.groupBy groupYs . L.sortBy comparePs
    where groupYs ((_,y), _) ((_,y'), _) = y == y'
          comparePs ((x,y), _) ((x',y'), _) = compare y y' `mappend` compare x x'

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery x l
    | x > 0     = (take x l) : (splitEvery x (drop x l))
    | otherwise = error "splitEvery requires non-negative number"

comparing :: Ord o => (a -> o) -> a -> a -> Ordering
comparing f = \a b -> compare (f a) (f b)

equating :: Eq b => (a -> b) -> a -> a -> Bool
equating f = \a b -> f a == f b

-- lookup closest to int, fallback to int - 1 (repeatedly)
lookupMax :: (Num a, Eq a) => a -> [(a, b)] -> Maybe b
lookupMax n xs
    | isJust (lookup n xs) = lookup n xs
    | otherwise            = lookupMax (n - 1) xs

takeWhiles :: Eq a => ([a] -> Bool) -> [a] -> [a]
takeWhiles f = go []
    where
        go accum [] = accum
        go accum (x:xs) =
           if f accum then
               go (accum ++ [x]) xs
           else
               accum

dropWhiles :: Eq a => ([a] -> Bool) -> [a] -> [a]
dropWhiles f = go []
    where
        go accum [] = []
        go accum (x:xs) =
            if f (x:accum) then
                go (x:accum) xs
            else
                x:xs

translateList :: ([a] -> b -> [a]) -> [b] -> [a]
translateList = flip foldl' []

addOrReplace :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
addOrReplace key value assoc = (key,value):(L.filter ((key /=).fst) assoc)

-- group by non-adjacent
-- https://stackoverflow.com/questions/53377577/groupby-function-which-groups-non-adjacent-elements
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f l = reverse (go [] f l) where
  go acc comp [] = acc
  go acc comp (h:t) =
    let (hs, nohs) = L.partition (comp h) t
    in go ((h:hs):acc) comp nohs
