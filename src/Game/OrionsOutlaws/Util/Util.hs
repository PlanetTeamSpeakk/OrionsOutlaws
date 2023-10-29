module Game.OrionsOutlaws.Util.Util (module Game.OrionsOutlaws.Util.Util) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (randomRIO)

lerp :: Num a => a -> a -> a -> a
lerp a b t = a + (b - a) * t

msTime :: IO Integer
msTime = round . (* 1000) <$> getPOSIXTime

randomElem :: [a] -> IO a
randomElem xs = do
  i <- randomRIO (0, length xs - 1)
  return $ xs !! i

enumerate :: [a] -> [(Int, a)]
enumerate = enumerateFrom 0

enumerateFrom :: Int -> [a] -> [(Int, a)]
enumerateFrom i = zip [i..]

rpad :: Int -> a -> [a] -> [a]
rpad n x xs = take n $ xs ++ repeat x

lpad :: Int -> a -> [a] -> [a]
lpad n x xs = replicate (n - length xs) x ++ xs
