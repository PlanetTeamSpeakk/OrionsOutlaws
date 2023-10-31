-- | A module containing miscellaneous utility functions.
module Game.OrionsOutlaws.Util.Util (module Game.OrionsOutlaws.Util.Util) where

import Data.Time.Clock.POSIX  (getPOSIXTime)
import System.Random          (randomRIO)
import Data.Functor           ((<&>))

-- | Linearly interpolates between the given values.
lerp :: Num a => a -> a -> a -> a
lerp a b t = a + (b - a) * t

-- | Returns the current time in milliseconds.
msTime :: IO Integer
msTime = round . (* 1000) <$> getPOSIXTime

-- | Returns a random element from the given list.
randomElem :: [a] -> IO a
randomElem xs = randomRIO (0, length xs - 1) <&> (xs !!)

-- | Maps all elements of the given list to a pair of the element and its index.
enumerate :: [a] -> [(Int, a)]
enumerate = enumerateFrom 0

-- | Maps all elements of the given list to a pair of the element and its index,
--   starting from the given index.
enumerateFrom :: Int -> [a] -> [(Int, a)]
enumerateFrom i = zip [i..]

-- | Ensures the given list is of at least the given length
--   by appending the given element @n - m@ times,
--   where @n@ is the given length and @m@ is the current length.
rpad :: Int -> a -> [a] -> [a]
rpad n x xs = take n $ xs ++ repeat x

-- | Ensures the given list is of at least the given length
--   by prepending it @n - m@ times with the given element,
--   where @n@ is the given length and @m@ is the current length.
lpad :: Int -> a -> [a] -> [a]
lpad n x xs = replicate (n - length xs) x ++ xs

-- | Rotates the given point with the given angle around the origin.
rotatePoint :: Point -> Radians -> Point
rotatePoint (x, y) r = (x * cos r - y * sin r, x * sin r + y * cos r)

-- | Rotates the given point with the given angle around the given center.
rotatePointAround :: Point -> Center -> Radians -> Point
rotatePointAround (x, y) (cx, cy) r = (x' + cx, y' + cy)
  where (x', y') = rotatePoint (x - cx, y - cy) r

-- | Calculates the angle between the two given points.
pointAngle :: Point -> Point -> Radians
pointAngle (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1)

-- | Calculates the squared distance between two points.
distanceSq :: Point -> Point -> Float
distanceSq (x1, y1) (x2, y2) = (x2 - x1) ** 2 + (y2 - y1) ** 2

-- | Calculates the distance between two points.
distance :: Point -> Point -> Float
distance p1 p2 = sqrt $ distanceSq p1 p2

-- | Some point consisting of an x and y.
type Point   = (Float, Float)
-- | A point that acts as a center point.
type Center  = Point
-- | An angle in radians.
type Radians = Float
