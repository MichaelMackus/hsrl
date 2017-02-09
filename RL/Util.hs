module RL.Util where

import Data.Maybe (isJust)
import Data.Foldable (foldl')

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
unenumerate2d [] = error "unenumerate2 requires a list"
unenumerate2d it = splitEvery maxX . snd $ unzip it
    where maxX = (+1) . fst . fst $ last it

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery x l
    | x > 0     = (take x l) : (splitEvery x (drop x l))
    | otherwise = error "splitEvery requires non-negative number"

comparing :: Ord o => (a -> o) -> a -> a -> Ordering
comparing f = \a b -> compare (f a) (f b)

-- lookup closest to int, fallback to int - 1 (repeatedly)
lookupMax :: (Num a, Eq a) => a -> [(a, b)] -> Maybe b
lookupMax n xs
    | isJust (lookup n xs) = lookup n xs
    | otherwise            = lookupMax (n - 1) xs

takeWhiles :: Eq a => ([a] -> Bool) -> [a] -> [a]
takeWhiles f = fold []
    where
        g accum x = if f accum then (x:accum) else accum
        fold accum (x:xs) =
            let accum' = g accum x
            in  if f accum then
                    fold accum' xs
                else
                    accum

-- strict version of takeWhiles
takeWhiles' :: ([a] -> Bool) -> [a] -> [a]
takeWhiles' f = translateList g
    where g accum x = if f accum then (x:accum) else accum

translateList :: ([a] -> b -> [a]) -> [b] -> [a]
translateList = flip foldl' []
