module RL.Types where

import Data.List (sort,unfoldr)

type Depth    = Int
data Dice     = D Int Sides Modifier deriving (Eq, Ord)
type Sides    = Int
type Modifier = Int

type AC = Int

-- construct dice from number of dice & sides, used like: 2 `d` 4 or 1 `d` 20 `plus` 5
d :: Int -> Sides -> Dice
d n ns = D n ns 0
plus :: Dice -> Int -> Dice
plus (D n ns m) m' = D n ns (m + m')

maxD :: Dice -> Int
maxD (D n ns _) = ns * n

type Point  = (Int, Int)
type Path   = [Point]
type Offset = Point
type Difficulty = Int

data VerticalDirection = Up | Down deriving (Eq, Show)
data Dir = North | East | South | West | NE | NW | SE | SW deriving (Eq, Enum, Show)

-- dungeon cell box (w x h)
type Dimension = (Width, Height)
type Width     = Int
type Height    = Int

-- used like: 2 `x` 4
x :: Int -> Int -> Dimension
x w h = (w, h)

-- Point helpers
type Slope = Double

addDir :: Dir -> Point -> Point
addDir North (ox, oy) = (ox, oy - 1)
addDir South (ox, oy) = (ox, oy + 1)
addDir East  (ox, oy) = (ox + 1, oy)
addDir West  (ox, oy) = (ox - 1, oy)
addDir NE    (ox, oy) = (ox + 1, oy - 1)
addDir SE    (ox, oy) = (ox + 1, oy + 1)
addDir NW    (ox, oy) = (ox - 1, oy - 1)
addDir SW    (ox, oy) = (ox - 1, oy + 1)

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

-- bresenham's from https://wiki.haskell.org/Bresenham%27s_line_drawing_algorithm
line :: Point -> Point -> [Point]
line pa@(xa,ya) pb@(xb,yb) = map maySwitch . unfoldr go $ (x1,y1,0)
  where
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch = if steep then (\(x,y) -> (y,x)) else id
    [(x1,y1),(x2,y2)] = sort [maySwitch pa, maySwitch pb]
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, error)
        | xTemp > x2 = Nothing
        | otherwise  = Just ((xTemp, yTemp), (xTemp + 1, newY, newError))
        where
          tempError = error + deltay
          (newY, newError) = if (2*tempError) >= deltax
                            then (yTemp+ystep,tempError-deltax)
                            else (yTemp,tempError)
