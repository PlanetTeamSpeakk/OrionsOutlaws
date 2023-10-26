module Game.OrionsOutlaws.Util (module Game.OrionsOutlaws.Util) where

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
