module RL.Types where

data Dice  = D Int Sides
type Sides = Int

-- construct dice from number of dice & sides, used like: 2 `d` 4 or 1 `d` 20
d :: Int -> Sides -> Dice
d n ns = D n ns

type Point   = (Int, Int)

-- dungeon cell box (w x h)
type Dimension = (Width, Height)
type Width     = Int
type Height    = Int

-- used like: 2 `x` 4
x :: Int -> Int -> Dimension
x w h = (w, h)

-- Point helpers
type Slope = Double

-- get y-intercept
intercept :: Point -> Slope -> Double
intercept (x, y) m = fromIntegral y - (m * fromIntegral x)

-- slope of two points
slope :: Point -> Point -> Slope
slope (x1, y1) (x2, y2) = fromIntegral ys / fromIntegral xs
    where xs = abs $ x1 - x2
          ys = abs $ y1 - y2

-- distance between points
distance :: (Floating a, Ord a) => Point -> Point -> a
distance (x1, y1) (x2, y2) = sqrt (xs^2 + ys^2)
    where xs = abs (fromIntegral x1 - fromIntegral x2)
          ys = abs (fromIntegral y1 - fromIntegral y2)
