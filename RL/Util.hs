module RL.Util where

-- helper functions

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

enumerate2 :: [[a]] -> [((Int, Int), a)]
enumerate2 = concat . map enumerateRow . zip [0..] where
    enumerateRow (y, r) = map (\(x, t) -> ((x, y), t)) $ zip [0..] r

unenumerate :: [(Int, a)] -> [a]
unenumerate [] = error "unenumerate requires a list"
unenumerate it = snd $ unzip it

unenumerate2 :: [((Int, Int), a)] -> [[a]]
unenumerate2 [] = error "unenumerate2 requires a list"
unenumerate2 it = splitEvery maxX . snd $ unzip it
    where maxX = fst . fst $ last it

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery x l
    | x > 0     = (take x l) : (splitEvery x (drop x l))
    | otherwise = error "splitEvery requires non-negative number"
